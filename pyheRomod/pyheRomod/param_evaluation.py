import pandas as pd
import numpy as np
import math
import warnings
import ast
from collections import OrderedDict
from .param_definition import UnevalParameters, UnevalInit, UnevalStartingValues, UnevalInflow
from .expansion import construct_expand_df_py, get_states_to_expand_py, complete_stl_py # Added
from .state_definition import UnevalStateList # Added
import scipy.stats

PYHEROMOD_CONFIG = { "inf_parameter": "warning" }
C_COMPLEMENT_MARKER = -np.pi

class ExpressionHacker(ast.NodeTransformer):
    def __init__(self, discount_method="start"): self.discount_method = discount_method
    def visit_Call(self, node):
        func_name = node.func.id if isinstance(node.func, ast.Name) else None
        if func_name in ("dispatch_strategy_py", "by_strategy_py"):
            if not any(kw.arg == "_strategy" for kw in node.keywords): node.keywords.insert(0, ast.keyword("_strategy", ast.Name("strategy", ast.Load())))
        if func_name == "by_group_py":
            if not any(kw.arg == "_group" for kw in node.keywords): node.keywords.insert(0, ast.keyword("_group", ast.Name("group", ast.Load())))
        if func_name == "discount_py":
            offset = {"start": 1.0, "end": 0.0, "midpoint": 0.5}.get(self.discount_method)
            if offset is None: raise ValueError(f"Invalid discounting method: {self.discount_method}")
            time_expr = ast.BinOp(ast.Name("markov_cycle", ast.Load()), ast.Sub(), ast.Constant(offset))
            if any(kw.arg == "time" for kw in node.keywords): [setattr(kw, 'value', time_expr) for kw in node.keywords if kw.arg == "time"]
            else: node.keywords.append(ast.keyword("time", time_expr))
        return self.generic_visit(node)

def apply_expression_hacks_py(expr_str: str, discount_method: str) -> str:
    if not expr_str.strip(): return expr_str
    try: return ast.unparse(ast.fix_missing_locations(ExpressionHacker(discount_method).visit(ast.parse(expr_str))))
    except Exception as e: warnings.warn(f"AST transformation failed for '{expr_str}': {e}. Using original.", UserWarning); return expr_str

def discount_py(x, r, time, first=False):
    r_scalar = float(r.iloc[0] if isinstance(r, pd.Series) else r[0]) if isinstance(r, (pd.Series, np.ndarray)) and r.size > 0 else float(r)
    if not (0 <= r_scalar <= 1): raise ValueError("Discount rate 'r' must be 0-1.")
    return x / ((1 + r_scalar) ** (time + (1 if first else 0)))

def vswitch_py(switch_on, **cases):
    if isinstance(switch_on, pd.Series):
        if all(not isinstance(v, pd.Series) for v in cases.values()): return switch_on.map(cases)
        res = pd.Series(index=switch_on.index, dtype=object)
        for k, v_series in cases.items(): res[switch_on == k] = v_series[switch_on == k] if isinstance(v_series, pd.Series) else v_series
        try: return pd.to_numeric(res)
        except (ValueError, TypeError): return res
    return cases.get(switch_on)

def dispatch_strategy_py(_strategy=None, **strats): return vswitch_py(_strategy, **strats) if _strategy is not None else (_raise := ValueError("_strategy missing"))
def by_strategy_py(_strategy=None, **strats): return dispatch_strategy_py(_strategy=_strategy, **strats)
def by_group_py(_group=None, **groups): return vswitch_py(_group, **groups) if _group is not None else (_raise := ValueError("_group missing"))

_EVAL_GLOBALS = None
def get_eval_globals():
    global _EVAL_GLOBALS
    if _EVAL_GLOBALS is None:
        _EVAL_GLOBALS = {"np": np, "math": math, "pd": pd, **{name: getattr(np, name) for name in ["log", "exp", "sqrt", "abs"]},
                         **{name: getattr(scipy.stats, name) for name in ["norm", "beta", "gamma", "poisson", "binom", "lognorm", "expon"]},
                         "dispatch_strategy_py": dispatch_strategy_py, "by_strategy_py": by_strategy_py, "by_group_py": by_group_py, "discount_py": discount_py,
                         "C_complement": C_COMPLEMENT_MARKER}
        r_dist_map = {
            "rnorm": ("norm", ("loc", "scale")), "rbeta": ("beta", ("a", "b")),
            "rgamma": ("gamma", ("a", "scale")), "rlnorm": ("lognorm", ("s", "scale"))
        }
        for r_name_loop, (scipy_name_loop, shape_arg_names_loop) in r_dist_map.items():
            scipy_dist_func_loop = getattr(scipy.stats, scipy_name_loop)
            if r_name_loop == "rlnorm":
                _EVAL_GLOBALS[r_name_loop] = lambda n, meanlog, sdlog, _dist_func=scipy_dist_func_loop: \
                                        _dist_func.rvs(s=sdlog, scale=np.exp(meanlog), size=n)
            elif r_name_loop == "rgamma":
                def rgamma_wrapper(n, shape, rate=None, scale_param=None, _dist_func=scipy_dist_func_loop):
                    final_scale = (1.0 / rate) if rate is not None else (scale_param if scale_param is not None else 1.0)
                    if rate is not None and scale_param is not None: raise ValueError("Provide either rate or scale for rgamma, not both.")
                    return _dist_func.rvs(a=shape, scale=final_scale, size=n)
                _EVAL_GLOBALS[r_name_loop] = rgamma_wrapper
            else:
                _EVAL_GLOBALS[r_name_loop] = lambda n, *args, _dist_func=scipy_dist_func_loop, _arg_names=shape_arg_names_loop: \
                                        _dist_func.rvs(**dict(zip(_arg_names, args)), size=n)
    return _EVAL_GLOBALS

def check_vars_table(df, varnames, vartype):
    txt_map = {"parameter": "parameter", "init": "initial probability for state", "value": "value", "inflow": "inflow"}
    txt = txt_map.get(vartype, vartype)
    for varname in varnames:
        if varname not in df: warnings.warn(f"Var '{varname}' not in results for checking.", UserWarning); continue
        col = pd.to_numeric(df[varname], errors='coerce')
        if col.isna().any(): raise ValueError(f"Error in {txt} '{varname}', NA values.")
        if PYHEROMOD_CONFIG["inf_parameter"] != "ignore" and np.isinf(col).any():
            msg = f"Error in {txt} '{varname}', infinite value(s) detected."
            if PYHEROMOD_CONFIG["inf_parameter"] == "warning": warnings.warn(msg, UserWarning)
            elif PYHEROMOD_CONFIG["inf_parameter"] == "error": raise ValueError(msg)

def safe_eval_python(df_in, exprs, vartype="parameter", check=True):
    df = df_in.copy()
    glbls = get_eval_globals()
    for name, expr_str in exprs.items():
        try:
            val = eval(expr_str, glbls, df)
            if np.isscalar(val) or (isinstance(val, np.ndarray) and val.ndim == 0): df[name] = val
            elif isinstance(val, (list, tuple, np.ndarray, pd.Series)):
                if len(val) == len(df): df[name] = val
                elif len(val) == 1 and not isinstance(val, pd.Series): df[name] = val[0]
                else: raise ValueError(f"Expr for '{name}' len {len(val)} != df len {len(df)}.")
            else: raise TypeError(f"Expr for '{name}' type {type(val)} unsupported.")
        except Exception as e:
            error_detail = str(e)
            if isinstance(e, NameError):
                error_detail = f"reference to undefined variable (NameError: {e})."
            txt_map = {"parameter": "parameter", "init": "initial probability for state", "value": "value", "inflow": "inflow"}
            vartype_str = txt_map.get(vartype, vartype)
            raise RuntimeError(f"Error in {vartype_str} '{name}': {error_detail}") from e
    if check: check_vars_table(df, list(exprs.keys()), vartype)
    return df

def eval_parameters_python(
    param_obj: UnevalParameters,
    state_list_obj: UnevalStateList, # For determining expansion needs
    model_cycles: int,
    strategy_names: list[str],
    group_names: list[str] = None,
    state_time_limit_config = None, # Passed to complete_stl_py
    transition_matrix_expressions: OrderedDict[str,str] = None, # For get_states_to_expand
    disc_method: str = 'start'
):
    """
    Evaluates parameters, constructing the primary run_d DataFrame which may include expanded states.
    """
    if not isinstance(param_obj, UnevalParameters): raise TypeError("param_obj must be UnevalParameters.")
    if not isinstance(state_list_obj, UnevalStateList): raise TypeError("state_list_obj must be UnevalStateList.")
    if model_cycles <= 0: model_cycles = 1

    # 1. Determine which states need expansion
    states_to_expand_dict = get_states_to_expand_py(
        param_obj=param_obj,
        state_list_obj=state_list_obj,
        transition_matrix_expressions=transition_matrix_expressions
    )

    # 2. Determine max state_time for each state, per strategy
    all_state_names = state_list_obj.get_state_names()
    state_time_limits_per_strategy = complete_stl_py(
        state_time_limit=state_time_limit_config,
        state_names=all_state_names,
        strategy_names=strategy_names,
        model_cycles=model_cycles,
        state_groups_df=None # Assuming state_groups_df is handled elsewhere or not used directly here yet
    )

    # 3. Construct the model_time base DataFrame
    # R's model_time is 1-based for cycles. Markov cycle is 0-based for time in discount.
    # Python equivalent: model_time from 1 to N_cycles. markov_cycle from 0 to N_cycles-1
    model_time_df_base = pd.DataFrame({
        'model_time': np.arange(1, model_cycles + 1).astype(float),
        'markov_cycle': np.arange(0, model_cycles).astype(float)
    })

    # 4. Construct the expanded run_d DataFrame
    run_d = construct_expand_df_py(
        states_to_expand_dict=states_to_expand_dict,
        state_time_limits_per_strategy=state_time_limits_per_strategy,
        model_time_df=model_time_df_base,
        strategy_names=strategy_names,
        state_names=all_state_names,
        group_names=group_names
    )
    if run_d.empty and model_cycles > 0 and strategy_names and all_state_names :
        warnings.warn("construct_expand_df_py returned empty despite inputs; parameter eval might fail or be empty.", UserWarning)
        # Fallback to a minimal structure if truly empty, to allow some processing, though likely indicates an issue.
        # This might occur if no states, no strategies, or no model_cycles.
        # The individual checks in construct_expand_df_py should prevent this if inputs are valid.
        # If it's still empty, it implies one of the core iterables (strategies, states, model_time) was effectively empty.
        # For safety, we'll return an empty DataFrame with expected columns if run_d is empty.
        return pd.DataFrame(columns=list(param_obj.expressions.keys()) + ['model_time', 'markov_cycle', 'strategy', 'group', 'state_name', 'state_time', 'strategy_id', 'group_id', 'state_id'])


    # 5. Prepare expressions and evaluate
    # Note: apply_expression_hacks_py needs to be aware of the column names in run_d (e.g. 'strategy', 'group', 'markov_cycle')
    exprs_to_eval = OrderedDict()
    for name, expr_str in param_obj.expressions.items():
        exprs_to_eval[name] = apply_expression_hacks_py(expr_str, disc_method)

    # Add 'state_day', 'state_week', 'state_month', 'state_year' if not present, derived from 'state_time'
    # Assuming state_time is in days for these conversions. This matches R's default behavior.
    if 'state_time' in run_d:
        if 'state_day' not in run_d: run_d['state_day'] = run_d['state_time']
        if 'state_week' not in run_d: run_d['state_week'] = run_d['state_time'] / 7.0
        if 'state_month' not in run_d: run_d['state_month'] = run_d['state_time'] / (365.25 / 12.0)
        if 'state_year' not in run_d: run_d['state_year'] = run_d['state_time'] / 365.25

    evaluated_params_df = safe_eval_python(run_d, exprs_to_eval, "parameter")

    return evaluated_params_df


def eval_init_python(init_obj: UnevalInit,
                     evaluated_params_df: pd.DataFrame,
                     individual_level: bool = False,
                     disc_method: str = 'start'):
    """
    Evaluates initial state probabilities or counts.
    Uses the subset of evaluated_params_df for the first model_time and state_time=0 (non-expanded view).
    """
    if not isinstance(init_obj, UnevalInit): raise TypeError("init_obj must be an UnevalInit instance.")
    if evaluated_params_df.empty:
        warnings.warn("eval_init: evaluated_params_df is empty. Cannot evaluate initial states.", UserWarning)
        return pd.Series(dtype=float, index=list(init_obj.expressions.keys()))

    # Context for init is typically model_time=1 and state_time=0 (representing the start of the model, before any state-specific time accrues)
    # If multiple strategies/groups, we need one init vector per strategy/group combination.
    # The original R code implies init is per strategy/group.

    # Filter for model_time == 1 and state_time == 0.0
    # If 'state_time' is not in evaluated_params_df (e.g., if no expansion happened and it wasn't added),
    # then just filter by model_time == 1.
    if 'state_time' in evaluated_params_df.columns:
        ctx_df_base = evaluated_params_df[
            (evaluated_params_df['model_time'] == 1) &
            (evaluated_params_df['state_time'] == 0.0)
        ]
    else: # Should not happen if eval_parameters_python is used, as it adds state_time
        ctx_df_base = evaluated_params_df[evaluated_params_df['model_time'] == 1]


    if ctx_df_base.empty:
        # Fallback: if state_time=0.0 is missing for model_time=1 (e.g. if all states expand immediately from ST=0 upwards)
        # try just model_time=1 and take the row with the minimum state_time present for mt=1 for each strat/group.
        # This is a bit of a guess if state_time=0 is truly absent.
        warnings.warn("eval_init: No data for model_time == 1 and state_time == 0.0. Trying with min state_time at model_time == 1.", UserWarning)
        mt1_df = evaluated_params_df[evaluated_params_df['model_time'] == 1]
        if mt1_df.empty: raise ValueError("eval_init: No data for model_time == 1 at all.")
        if 'state_time' in mt1_df.columns and ('strategy' in mt1_df.columns and 'group' in mt1_df.columns) :
             ctx_df_base = mt1_df.loc[mt1_df.groupby(['strategy', 'group'])['state_time'].idxmin()]
        elif 'state_time' in mt1_df.columns and 'strategy' in mt1_df.columns: # Only strategy
             ctx_df_base = mt1_df.loc[mt1_df.groupby('strategy')['state_time'].idxmin()]
        else: # No strategy/group, just take first row of model_time 1
            ctx_df_base = mt1_df.iloc[[0]]

        if ctx_df_base.empty:
             raise ValueError("eval_init: Could not determine a context DataFrame from evaluated_params_df for model_time == 1.")


    exprs = OrderedDict()
    for n, e_str in init_obj.expressions.items():
        exprs[n] = apply_expression_hacks_py(e_str, disc_method)

    # We need to evaluate init for each unique strategy/group combination present in ctx_df_base
    # The result should be a DataFrame: index=MultiIndex(strategy, group), columns=state_names

    all_init_vectors = []
    group_cols = []
    if 'strategy' in ctx_df_base.columns: group_cols.append('strategy')
    if 'group' in ctx_df_base.columns: group_cols.append('group')

    if not group_cols: # Single context
        # This path is if evaluated_params_df had no strategy/group columns (e.g. very simple model eval)
        # Or if ctx_df_base somehow ended up with only one row after filtering.
        single_ctx_row = ctx_df_base.iloc[[0]].reset_index(drop=True)
        init_wide_df_row = safe_eval_python(single_ctx_row, exprs, "init", check=False)
        vec = init_wide_df_row[list(init_obj.expressions.keys())].iloc[0].astype(float)

        # ... (rest of validation logic from original, applied to vec) ...
        if vec.isna().any(): raise ValueError(f"NA in init states: {vec[vec.isna()].index.tolist()}.")
        comp_mask = np.isclose(vec, C_COMPLEMENT_MARKER)
        if comp_mask.sum() > 1: raise ValueError("Error in initial probabilities: complement marker 'C_complement' can only be used for at most one state.")
        if comp_mask.sum() == 1:
            sum_others = vec[~comp_mask].sum()
            if np.isnan(sum_others): raise ValueError("Cannot calc complement due to NaNs.")
            vec[comp_mask] = 1.0 - sum_others

        neg_mask = vec < 0
        if neg_mask.any() and (individual_level or not np.allclose(vec[neg_mask], 0)):
            raise ValueError(f"Error in initial probabilities: probabilities are negative for states: {vec[neg_mask].index.tolist()}.")
        vec[np.isclose(vec, 0) & neg_mask] = 0.0

        if individual_level:
            if (vec < 0).any(): raise ValueError(f"Negative init (post-fix): {vec[vec < 0].index.tolist()}.")
            if ((vec < -1e-9) | (vec > 1.0 + 1e-9)).any(): raise ValueError(f"Error in initial probabilities: probabilities are outside range [0,1] for states: {vec[(vec < -1e-9) | (vec > 1.0 + 1e-9)].index.tolist()}.")
            if not np.isclose(vec.sum(), 1.0): raise ValueError(f"Error in initial probabilities: values do not sum to 1 (sum is {vec.sum()}).")
        if not individual_level and np.isclose(vec.sum(), 0): warnings.warn("Warning: Initial probabilities/counts sum to zero.", UserWarning)
        # For a single vector result, returning a Series is fine.
        # If the caller expects a DataFrame indexed by strategy/group, this path needs adjustment or clarification.
        # The original R version returned a named vector.
        return vec


    # Loop through unique strategy/group combinations from the context
    # This ensures we get one init vector per combination.
    processed_indices = []

    # Get unique strategy/group combinations to iterate over
    # We only need one row from ctx_df_base for each unique combination as context
    if not group_cols: # Should have been handled by the earlier `if not group_cols:` block
        # This part of the code should ideally not be reached if group_cols is empty,
        # as the single context case is handled above. Adding error for safety.
        raise RuntimeError("eval_init_python: Reached multi-group processing logic with no group_cols defined.")

    # Take the first row for each unique strategy/group combination
    # This ensures that the context for evaluating init expressions is consistent for each combo,
    # and avoids using multiple (potentially slightly different if parameters varied by state_name)
    # rows from the original run_d for the same init vector.
    unique_ctx_rows_df = ctx_df_base.drop_duplicates(subset=group_cols).reset_index(drop=True)

    for _, row_ctx in unique_ctx_rows_df.iterrows():
        # Create a single-row DataFrame for evaluation context for this strat/group
        current_eval_ctx = pd.DataFrame([row_ctx])

        init_wide_df_row = safe_eval_python(current_eval_ctx, exprs, "init", check=False)
        vec = init_wide_df_row[list(init_obj.expressions.keys())].iloc[0].astype(float)

        if vec.isna().any(): raise ValueError(f"NA in init states for {row_ctx[group_cols] if group_cols else 'context'}: {vec[vec.isna()].index.tolist()}.")
        comp_mask = np.isclose(vec, C_COMPLEMENT_MARKER)
        if comp_mask.sum() > 1: raise ValueError(f"Complement marker error for {row_ctx[group_cols] if group_cols else 'context'}.")
        if comp_mask.sum() == 1:
            sum_others = vec[~comp_mask].sum()
            if np.isnan(sum_others): raise ValueError(f"NaN sum for complement for {row_ctx[group_cols] if group_cols else 'context'}.")
            vec[comp_mask] = 1.0 - sum_others

        neg_mask = vec < 0
        if neg_mask.any() and (individual_level or not np.allclose(vec[neg_mask], 0)):
            raise ValueError(f"Negative probs for {row_ctx[group_cols] if group_cols else 'context'}: {vec[neg_mask].index.tolist()}.")
        vec[np.isclose(vec, 0) & neg_mask] = 0.0

        if individual_level:
            if (vec < 0).any(): raise ValueError(f"Negative init (post-fix) for {row_ctx[group_cols] if group_cols else 'context'}.")
            if ((vec < -1e-9) | (vec > 1.0 + 1e-9)).any(): raise ValueError(f"Probs outside [0,1] for {row_ctx[group_cols] if group_cols else 'context'}.")
            if not np.isclose(vec.sum(), 1.0): raise ValueError(f"Probs do not sum to 1 for {row_ctx[group_cols] if group_cols else 'context'} (sum {vec.sum()}).")
        if not individual_level and np.isclose(vec.sum(), 0): warnings.warn(f"Init sum to zero for {row_ctx[group_cols] if group_cols else 'context'}.", UserWarning)

        # Store the vector with its strategy/group identifiers
        id_tuple = tuple(row_ctx[gc] for gc in group_cols)
        processed_indices.append(id_tuple)
        all_init_vectors.append(vec)

    if not all_init_vectors and not unique_ctx_rows_df.empty : # Should not happen if unique_ctx_rows_df was not empty
         warnings.warn("eval_init: No init vectors were generated despite having unique contexts.", UserWarning)
    elif not all_init_vectors and unique_ctx_rows_df.empty and not ctx_df_base.empty:
         warnings.warn("eval_init: No init vectors generated; unique_ctx_rows_df was empty but ctx_df_base was not (unexpected).", UserWarning)
    elif not all_init_vectors: # Generic catch-all if it's empty for other reasons (e.g. ctx_df_base was empty)
        warnings.warn("eval_init: No init vectors were generated (final check).", UserWarning)
        return pd.DataFrame(columns=list(init_obj.expressions.keys()))


    init_results_df = pd.concat(all_init_vectors, axis=1).T
    if group_cols:
        init_results_df.index = pd.MultiIndex.from_tuples(processed_indices, names=group_cols)
    init_results_df.columns = list(init_obj.expressions.keys())

    # If only one strategy/group was processed, R behavior is often to return a Series/vector.
    # For consistency, let's decide if this function should always return a DataFrame or simplify.
    # Current R code returns a numeric vector. If there are multiple strategies/groups,
    # this Python port might need to be called per strategy/group by the caller, or this function
    # needs to clearly document its multi-strategy/group return format (e.g., DataFrame indexed by strat/group).
    # For now, returning the DataFrame. If it's a single row, squeeze might convert to Series.
    if len(init_results_df) == 1 and len(group_cols) > 0 : # If only one strat/group combo, but it was named
        return init_results_df # Still return DF to keep structure, caller can .iloc[0]
    elif len(init_results_df) == 1 and not group_cols: # Only one row and no strat/group context
        return init_results_df.iloc[0] # Return Series

    return init_results_df


def eval_starting_values_python(sv_obj: UnevalStartingValues,
                                evaluated_params_df: pd.DataFrame,
                                disc_method: str = 'start'):
    """
    Evaluates starting values for states. These are typically time-dependent (on model_time) but not state_time dependent.
    So, context is evaluated_params_df filtered for state_time == 0.0 (or min state_time if 0.0 is absent).
    """
    if not isinstance(sv_obj, UnevalStartingValues): raise TypeError("sv_obj must be UnevalStartingValues.")
    if evaluated_params_df.empty:
        warnings.warn("eval_starting_values: evaluated_params_df is empty.", UserWarning)
        return pd.DataFrame(columns=list(sv_obj.expressions.keys()))

    # Context: typically model_time dependent, but for state_time = 0 (or minimum if 0 is not present for expanded states)
    # Use a local variable 'ctx_df' for the context to avoid confusion with the input 'evaluated_params_df'
    # This was the source of the NameError 'params_df' if it was mistyped later.
    # The original code was trying to use 'params_df' which was not defined in this scope.
    # It should have been 'evaluated_params_df'.

    # Initialize ctx_df to ensure it's defined before conditional assignments
    ctx_df = pd.DataFrame()

    if 'state_time' in evaluated_params_df.columns:
        ctx_df = evaluated_params_df[evaluated_params_df['state_time'] == 0.0].reset_index(drop=True)
        if ctx_df.empty and not evaluated_params_df.empty : # Fallback if state_time=0.0 is missing
            warnings.warn("eval_starting_values: No data for state_time == 0.0. Trying with min state_time per model_time/strategy/group.", UserWarning)
            group_cols_sv = [col for col in ['model_time', 'strategy', 'group'] if col in evaluated_params_df.columns]
            if group_cols_sv :
                 ctx_df = evaluated_params_df.loc[evaluated_params_df.groupby(group_cols_sv)['state_time'].idxmin()].reset_index(drop=True)
            elif not evaluated_params_df.empty: # If no group_cols but df is not empty, take first row of min state_time over all
                 ctx_df = evaluated_params_df.loc[[evaluated_params_df['state_time'].idxmin()]].reset_index(drop=True)
            # If evaluated_params_df was empty to begin with, ctx_df remains empty, handled below.

    elif not evaluated_params_df.empty: # No 'state_time' column, but df has data
        ctx_df = evaluated_params_df.reset_index(drop=True)
        # This implies no expansion, so all rows are effectively at an equivalent "base" state time.

    # If after all attempts, ctx_df is still empty (and evaluated_params_df wasn't initially empty)
    if ctx_df.empty and not evaluated_params_df.empty:
         # This case might occur if evaluated_params_df had state_time but all were filtered out and fallbacks failed
         # or if it had no state_time and was structured unexpectedly.
         warnings.warn("eval_starting_values: Context DataFrame is empty after filtering attempts. Using first row of evaluated_params_df as a last resort.", UserWarning)
         ctx_df = evaluated_params_df.iloc[[0]].reset_index(drop=True) # Absolute fallback if any data exists

    if ctx_df.empty: # This means evaluated_params_df was initially empty or became empty and couldn't be recovered
        warnings.warn("eval_starting_values: Context DataFrame is definitively empty. Returning empty DataFrame.", UserWarning)
        id_cols_for_empty_return = [col for col in ['model_time', 'strategy', 'group'] if col in evaluated_params_df.columns]
        return pd.DataFrame(columns=id_cols_for_empty_return + list(sv_obj.expressions.keys()))

    exprs = OrderedDict()
    for n, e_str in sv_obj.expressions.items():
        exprs[n] = apply_expression_hacks_py(e_str, disc_method)

    res_df = safe_eval_python(ctx_df, exprs, "value")

    # Ensure all original context columns (model_time, strategy, group, etc.) are preserved
    # and that results are aligned with the context. safe_eval_python returns a df with context + new value columns.
    # We only need the value columns + the identifying columns.
    id_cols = [col for col in ['model_time', 'strategy', 'group', 'strategy_id', 'group_id', 'state_id', 'state_name', 'markov_cycle','state_time'] if col in res_df.columns]
    val_cols = list(sv_obj.expressions.keys())

    final_res_df = res_df[id_cols + val_cols]

    # R heRomod sets the last value to 0 for each starting value.
    # This needs to be done per strategy/group if they exist.
    if not final_res_df.empty:
        group_by_cols_sv = [col for col in ['strategy', 'group'] if col in final_res_df.columns]
        if not group_by_cols_sv: # If no strategy/group, apply to the whole series
            for val_col in val_cols:
                if pd.api.types.is_numeric_dtype(final_res_df[val_col]):
                    final_res_df.loc[final_res_df['model_time'] == final_res_df['model_time'].max(), val_col] = 0.0
        else:
            def set_last_to_zero(series_group):
                # series_group is a Series (one of the val_cols, grouped by strategy/group)
                # It's indexed by the original index of final_res_df for that group.
                # We need to find the row(s) within this group that correspond to the max model_time for this group.
                # The group's `final_res_df` subset is available via `final_res_df.loc[series_group.index]`
                group_df_subset = final_res_df.loc[series_group.index]
                max_mt_in_group = group_df_subset['model_time'].max()
                # Get original index locations where model_time is max for this group
                idx_to_zero_out = group_df_subset[group_df_subset['model_time'] == max_mt_in_group].index

                # Create a temporary series to modify, then update the main DataFrame
                # This avoids SettingWithCopyWarning if series_group was a view
                modified_series = series_group.copy()
                if pd.api.types.is_numeric_dtype(modified_series):
                     modified_series.loc[idx_to_zero_out] = 0.0
                return modified_series

            for val_col in val_cols:
                 if pd.api.types.is_numeric_dtype(final_res_df[val_col]):
                    final_res_df[val_col] = final_res_df.groupby(group_by_cols_sv, group_keys=False)[val_col].apply(set_last_to_zero)

    return final_res_df


def eval_inflow_python(inflow_obj: UnevalInflow,
                       evaluated_params_df: pd.DataFrame,
                       disc_method: str = 'start'):
    """
    Evaluates state inflows. Inflows can depend on model_time and state_time.
    The result is aggregated per model_time (summing over state_times).
    """
    if not isinstance(inflow_obj, UnevalInflow): raise TypeError("inflow_obj must be an UnevalInflow instance.")

    if evaluated_params_df.empty:
        warnings.warn("eval_inflow: evaluated_params_df is empty. Returning empty DataFrame.", UserWarning)
        # Return structure should match expected output: model_time, strategy, group, and inflow state columns
        id_cols_empty = [c for c in ['model_time', 'strategy', 'group'] if c in evaluated_params_df.columns] # Check if these were even there
        return pd.DataFrame(columns=id_cols_empty + list(inflow_obj.expressions.keys()))

    exprs = OrderedDict()
    for n, e_str in inflow_obj.expressions.items():
        exprs[n] = apply_expression_hacks_py(e_str, disc_method)

    # Inflows are evaluated over the full evaluated_params_df (which includes all model_times and state_times)
    wide_df_eval = safe_eval_python(evaluated_params_df, exprs, "inflow", check=False)

    # Columns to identify each unique time point before aggregation: model_time, strategy, group
    id_vars_for_pivot = [col for col in ['model_time', 'strategy', 'group', 'strategy_id', 'group_id'] if col in wide_df_eval.columns]

    # Columns that represent the inflow values for different states
    value_vars_for_pivot = list(inflow_obj.expressions.keys())

    # Check for NAs and negatives in the evaluated inflow values *before* aggregation
    for v_col in value_vars_for_pivot:
        if wide_df_eval[v_col].isna().any():
            raise ValueError(f"NA in inflow state '{v_col}' before aggregation. Check expressions/parameters.")
        if pd.api.types.is_numeric_dtype(wide_df_eval[v_col]) and (wide_df_eval[v_col] < 0).any():
            raise ValueError(f"Negative inflow value(s) detected in state '{v_col}' before aggregation.")

    # Aggregation: sum inflows over state_time for each model_time, strategy, group combination.
    # If no strategy/group, then just aggregate over model_time.
    if not id_vars_for_pivot: # Should not happen with a proper run_d
        warnings.warn("eval_inflow: No ID variables (model_time, strategy, group) found for pivoting. Aggregating globally.", UserWarning)
        # This case would mean wide_df_eval is just value columns, sum them all.
        # Result would be a single row Series. This is likely not intended.
        aggregated_inflows = wide_df_eval[value_vars_for_pivot].sum().to_frame().T
    elif not any(item in ['strategy', 'group'] for item in id_vars_for_pivot) and 'model_time' in id_vars_for_pivot:
        # Only model_time is an ID var
        aggregated_inflows = wide_df_eval.groupby('model_time')[value_vars_for_pivot].sum().reset_index()
    else: # Group by model_time and any of strategy, group
        pivot_group_cols = [col for col in id_vars_for_pivot if col != 'model_time'] # model_time is the primary index for aggregation here
        if not pivot_group_cols : # Only model_time
             aggregated_inflows = wide_df_eval.groupby('model_time')[value_vars_for_pivot].sum().reset_index()
        else:
             all_group_cols_for_pivot = ['model_time'] + pivot_group_cols
             aggregated_inflows = wide_df_eval.groupby(all_group_cols_for_pivot, as_index=False)[value_vars_for_pivot].sum()


    # Ensure all inflow states defined in inflow_obj are columns in the result, even if they summed to 0 everywhere
    for s_name in inflow_obj.expressions.keys():
        if s_name not in aggregated_inflows.columns:
            aggregated_inflows[s_name] = 0.0

    # Ensure correct column order: ID cols first, then inflow value cols in original order
    final_id_cols = [col for col in id_vars_for_pivot if col in aggregated_inflows.columns] # Get actual ID cols present
    final_value_cols = [val_col for val_col in value_vars_for_pivot if val_col in aggregated_inflows.columns]

    return aggregated_inflows[final_id_cols + final_value_cols]

def eval_starting_values_python(sv_obj: UnevalStartingValues,
                                evaluated_params_df: pd.DataFrame,
                                disc_method: str = 'start'):
    ctx_df = params_df[params_df['state_time'] == 1].reset_index(drop=True)
    if ctx_df.empty:
        warnings.warn("No data available in evaluated_params_df for state_time == 1.", UserWarning)
        return pd.DataFrame(columns=list(sv_obj.expressions.keys()))
    exprs = OrderedDict((n, apply_expression_hacks_py(e, disc_method)) for n, e in sv_obj.expressions.items())
    res_df = safe_eval_python(ctx_df, exprs, "value")
    res_df = res_df[list(sv_obj.expressions.keys())]
    if not res_df.empty: res_df.iloc[-1] = 0.0
    return res_df

def eval_inflow_python(inflow_obj, params_df, strategy_name='', group_name='', disc_method='start'):
    if params_df.empty: warnings.warn("eval_inflow: params_df empty.", UserWarning); return pd.DataFrame()
    exprs = OrderedDict((n, apply_expression_hacks_py(e, disc_method)) for n, e in inflow_obj.expressions.items())
    wide_df = safe_eval_python(params_df, exprs, "inflow", check=False)

    cols_to_melt = ['model_time', 'state_time'] + list(inflow_obj.expressions.keys())
    if any(c not in wide_df for c in cols_to_melt): raise KeyError(f"Cols missing for melt: {[c for c in cols_to_melt if c not in wide_df]}.")

    long_df = wide_df[cols_to_melt].melt(id_vars=['model_time', 'state_time'], var_name='.state', value_name='.value')
    long_df['.state'] = long_df['.state'].astype(str)

    if long_df['.value'].isna().any(): raise ValueError(f"NA in inflow: \n{long_df[long_df['.value'].isna()].head()}.")
    num_vals = pd.to_numeric(long_df['.value'], errors='coerce')
    if (num_vals < 0).any(): raise ValueError("negative value(s) detected")
    long_df['.value'] = num_vals

    pivot = pd.pivot_table(long_df, index='model_time', columns='.state', values='.value', fill_value=0.0, aggfunc="sum")
    for s_name in inflow_obj.expressions.keys():
        if s_name not in pivot: pivot[s_name] = 0.0
    return pivot[list(inflow_obj.expressions.keys())].reset_index()
