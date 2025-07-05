import ast
import warnings
from collections import OrderedDict
from collections.abc import Iterable
import pandas as pd
from .param_definition import UnevalCollection, UnevalParameters
from .state_definition import State, StateTransition, UnevalStateList


STATE_TIME_VARS = {"state_time", "state_day", "state_week", "state_month", "state_year", "state_hour"}

class _StateTimeVarFinder(ast.NodeVisitor):
    def __init__(self, state_time_vars_set: set[str]):
        self.state_time_vars_set = state_time_vars_set
        self.found = False

    def visit_Name(self, node):
        if node.id in self.state_time_vars_set:
            self.found = True
        self.generic_visit(node)

    def visit_Attribute(self, node):
        self.generic_visit(node)

def has_state_time_in_expr(expr_str: str, state_time_vars_set: set[str] = STATE_TIME_VARS) -> bool:
    if not expr_str or not isinstance(expr_str, str) or not expr_str.strip():
        return False
    try:
        tree = ast.parse(expr_str)
        finder = _StateTimeVarFinder(state_time_vars_set)
        finder.visit(tree)
        return finder.found
    except SyntaxError:
        return False
    except Exception as e:
        warnings.warn(f"Could not parse expression string for state_time_var check: '{expr_str}'. Error: {e}", UserWarning)
        return False

def has_state_time_py_for_uneval_collection(collection: UnevalCollection, state_time_vars_set: set[str] = STATE_TIME_VARS) -> bool:
    if not isinstance(collection, UnevalCollection):
        warnings.warn(f"Expected UnevalCollection, got {type(collection)}.", UserWarning)
        return False
    for expr_str in collection.expressions.values():
        if has_state_time_in_expr(expr_str, state_time_vars_set):
            return True
    return False

def has_state_time_py_for_state(state: State, state_time_vars_set: set[str] = STATE_TIME_VARS) -> bool:
    if not isinstance(state, State):
        warnings.warn(f"Expected State object, got {type(state)}.", UserWarning)
        return False
    return has_state_time_py_for_uneval_collection(state, state_time_vars_set)

def has_state_time_py_for_state_transition(transition: StateTransition, state_time_vars_set: set[str] = STATE_TIME_VARS) -> bool:
    if not isinstance(transition, StateTransition):
        warnings.warn(f"Expected StateTransition object, got {type(transition)}.", UserWarning)
        return False
    return has_state_time_py_for_uneval_collection(transition, state_time_vars_set)

def has_state_time_py_for_uneval_state_list(
    state_list: UnevalStateList,
    state_time_vars_set: set[str] = STATE_TIME_VARS
) -> dict[str, bool]:
    if not isinstance(state_list, UnevalStateList):
        raise TypeError("Expected UnevalStateList object.")
    state_names = state_list.get_state_names()
    expansion_needed_for_state: dict[str, bool] = {name: False for name in state_names}
    for state_name, state_obj in state_list.states.items():
        if has_state_time_py_for_state(state_obj, state_time_vars_set):
            expansion_needed_for_state[state_name] = True
    if state_list.transitions:
        transition_uses_st_vars = [
            has_state_time_py_for_state_transition(trans, state_time_vars_set)
            for trans in state_list.transitions
        ]
        from_states_of_expanding_transitions = []
        expand_all_states_due_to_transition = False
        for i, trans_uses_vars in enumerate(transition_uses_st_vars):
            if trans_uses_vars:
                from_state = state_list.transitions[i].from_state
                if from_state is None:
                    expand_all_states_due_to_transition = True
                    break
                else:
                    if isinstance(from_state, str):
                        from_states_of_expanding_transitions.append(from_state)
        if expand_all_states_due_to_transition:
            for state_name in state_names:
                expansion_needed_for_state[state_name] = True
        else:
            for state_name_to_expand in from_states_of_expanding_transitions:
                if state_name_to_expand in expansion_needed_for_state:
                    expansion_needed_for_state[state_name_to_expand] = True
    return expansion_needed_for_state

class _ExpressionVariableVisitor(ast.NodeVisitor):
    def __init__(self):
        self.variable_names: set[str] = set()
    def visit_Name(self, node):
        self.variable_names.add(node.id)
        self.generic_visit(node)

def get_vars_from_expr(expr_str: str) -> set[str]:
    if not expr_str or not isinstance(expr_str, str) or not expr_str.strip():
        return set()
    try:
        tree = ast.parse(expr_str)
        visitor = _ExpressionVariableVisitor()
        visitor.visit(tree)
        return visitor.variable_names
    except SyntaxError: return set()
    except Exception as e:
        warnings.warn(f"Could not parse expr string for var extraction: '{expr_str}'. Err: {e}", UserWarning)
        return set()

def trace_st_dependency_py(
    expressions: OrderedDict[str, str],
    extras_st_dependency: dict[str, bool] = None,
    state_time_vars_set: set[str] = STATE_TIME_VARS
) -> dict[str, bool]:
    if extras_st_dependency is None: extras_st_dependency = {}

    all_item_names = list(expressions.keys())

    st_dependency_map: dict[str, bool] = {}
    for var in state_time_vars_set: st_dependency_map[var] = True
    for var, is_st_dep in extras_st_dependency.items(): st_dependency_map[var] = is_st_dep
    for name in all_item_names:
        if name not in st_dependency_map:
            st_dependency_map[name] = False

    made_change_in_pass = True
    max_passes = len(all_item_names) + 1
    current_pass = 0

    while made_change_in_pass and current_pass < max_passes:
        made_change_in_pass = False
        current_pass += 1
        for name, expr_str in expressions.items():
            if st_dependency_map.get(name, False): continue

            if has_state_time_in_expr(expr_str, state_time_vars_set):
                if not st_dependency_map.get(name, False):
                    st_dependency_map[name] = True
                    made_change_in_pass = True
                continue

            referenced_vars = get_vars_from_expr(expr_str)
            for ref_var in referenced_vars:
                if st_dependency_map.get(ref_var, False):
                    if not st_dependency_map.get(name, False):
                        st_dependency_map[name] = True
                        made_change_in_pass = True
                        break

    if current_pass >= max_passes and made_change_in_pass:
        warnings.warn("trace_st_dependency_py: Max passes reached, potential circular dependency.", UserWarning)

    return {name: st_dependency_map.get(name, False) for name in all_item_names}

def get_states_to_expand_py(
    param_obj: UnevalParameters,
    state_list_obj: UnevalStateList,
    transition_matrix_expressions: OrderedDict[str, str] = None,
    state_time_vars_set: set[str] = STATE_TIME_VARS
) -> dict[str, bool]:
    if not isinstance(state_list_obj, UnevalStateList):
        raise TypeError("state_list_obj must be an UnevalStateList.")
    if not isinstance(param_obj, UnevalParameters):
        raise TypeError("param_obj must be an UnevalParameters instance.")
    if transition_matrix_expressions is not None and not isinstance(transition_matrix_expressions, OrderedDict):
        raise TypeError("transition_matrix_expressions must be an OrderedDict or None.")

    state_names = state_list_obj.get_state_names()
    expansion_needed: dict[str, bool] = {name: False for name in state_names}

    st_dep_params = trace_st_dependency_py(
        param_obj.expressions,
        extras_st_dependency=None,
        state_time_vars_set=state_time_vars_set
    )

    for state_name, state_obj in state_list_obj.states.items():
        state_val_st_dep_results = trace_st_dependency_py(
            state_obj.expressions,
            extras_st_dependency=st_dep_params,
            state_time_vars_set=state_time_vars_set
        )
        if any(state_val_st_dep_results.values()):
            expansion_needed[state_name] = True

    expand_all_due_to_sl_transition_val = False
    temp_expansion_from_sl_trans_val = {name: False for name in state_names}

    for trans_obj in state_list_obj.transitions:
        if not trans_obj.expressions: continue

        trans_val_st_dep_results = trace_st_dependency_py(
            trans_obj.expressions,
            extras_st_dependency=st_dep_params,
            state_time_vars_set=state_time_vars_set
        )
        if any(trans_val_st_dep_results.values()):
            if trans_obj.from_state is None:
                expand_all_due_to_sl_transition_val = True
                break
            elif trans_obj.from_state in temp_expansion_from_sl_trans_val:
                temp_expansion_from_sl_trans_val[trans_obj.from_state] = True

    if expand_all_due_to_sl_transition_val:
        for name in state_names: expansion_needed[name] = True
    else:
        for name, should_expand in temp_expansion_from_sl_trans_val.items():
            if should_expand: expansion_needed[name] = True

    if transition_matrix_expressions:
        prob_exprs_st_dep = trace_st_dependency_py(
            transition_matrix_expressions,
            extras_st_dependency=st_dep_params,
            state_time_vars_set=state_time_vars_set
        )

        for from_state_name in state_names:
            if expansion_needed[from_state_name]: continue
            for trans_key, is_st_dep in prob_exprs_st_dep.items():
                if is_st_dep:
                    if trans_key.startswith(from_state_name + "_to_"):
                        expansion_needed[from_state_name] = True
                        break
    return expansion_needed

def _check_scl_py(scl_dict: dict[str, int], all_state_names: list[str], model_cycles: int):
    """Helper to validate a state_time_limit dictionary for a single strategy."""
    if not isinstance(scl_dict, dict):
        raise TypeError("'state_time_limit' for a strategy must be a dictionary.")
    if not all(isinstance(k, str) for k in scl_dict.keys()):
        raise TypeError("State names in 'state_time_limit' must be strings.")

    processed_scl_dict = {}
    for k_s, v_s in scl_dict.items():
        if isinstance(v_s, float) and v_s.is_integer():
            processed_scl_dict[k_s] = int(v_s)
        elif isinstance(v_s, int):
            processed_scl_dict[k_s] = v_s
        else:
            raise TypeError(f"Limit for state '{k_s}' must be an integer or whole number float, got {type(v_s)}.")

    for state_name, limit in processed_scl_dict.items():
        if state_name not in all_state_names:
            raise ValueError(f"State '{state_name}' in 'state_time_limit' is not a defined model state.")
        if limit < 0: # Allow 0, meaning it exists for one cycle at state_time = 0
            raise ValueError(f"Limit for state '{state_name}' ({limit}) must be non-negative.")
        if limit > model_cycles :
             warnings.warn(f"Limit for state '{state_name}' ({limit}) exceeds model_cycles ({model_cycles}). Capping at model_cycles for now.", UserWarning)
    return processed_scl_dict

def complete_stl_py(
    state_time_limit,
    state_names: list[str],
    strategy_names: list[str],
    model_cycles: int,
    state_groups_df: pd.DataFrame = None
) -> dict[str, dict[str, int]]:
    if not strategy_names: return {}
    if not state_names: return {strat: {} for strat in strategy_names}

    default_limit_for_all_states = model_cycles

    result_limits: dict[str, dict[str, int]] = {
        strat_name: {st_name: default_limit_for_all_states for st_name in state_names}
        for strat_name in strategy_names
    }

    scl_processed = False
    if state_time_limit is None:
        scl_processed = True
    elif isinstance(state_time_limit, (int, float)):
        if isinstance(state_time_limit, float) and not state_time_limit.is_integer():
            raise ValueError("Global 'state_time_limit' if float must be a whole number.")
        limit_val = int(state_time_limit)
        if limit_val < 0: raise ValueError("Global 'state_time_limit' must be non-negative.") # Allow global 0

        effective_limit = limit_val
        if limit_val > model_cycles:
            warnings.warn(f"Global 'state_time_limit' ({limit_val}) exceeds model_cycles ({model_cycles}). Using model_cycles.", UserWarning)
            effective_limit = model_cycles

        for strat_name in strategy_names:
            for st_name in state_names:
                result_limits[strat_name][st_name] = effective_limit
        scl_processed = True
    elif isinstance(state_time_limit, dict):
        if not state_time_limit:
            scl_processed = True
        else:
            first_val_key = next(iter(state_time_limit.keys()))
            first_val_scl = state_time_limit[first_val_key]

            if isinstance(first_val_scl, (int, float)):
                scl_for_all_strats_orig: dict[str, any] = state_time_limit
                scl_for_all_strats = _check_scl_py(scl_for_all_strats_orig, state_names, model_cycles)
                for strat_name in strategy_names:
                    for st_name, limit_val in scl_for_all_strats.items():
                        if st_name in result_limits[strat_name]:
                             result_limits[strat_name][st_name] = min(limit_val, model_cycles)
                scl_processed = True
            elif isinstance(first_val_scl, dict):
                scl_per_strat: dict[str, dict[str, any]] = state_time_limit
                for strat_name_key, single_strat_scl_orig in scl_per_strat.items():
                    if strat_name_key not in strategy_names:
                        raise ValueError(f"Strategy '{strat_name_key}' in 'state_time_limit' is not a defined model strategy.")

                    single_strat_scl_processed = _check_scl_py(single_strat_scl_orig, state_names, model_cycles)
                    for st_name, limit_val in single_strat_scl_processed.items():
                        if st_name in result_limits[strat_name_key]:
                            result_limits[strat_name_key][st_name] = min(limit_val, model_cycles)
                scl_processed = True

    if state_time_limit is not None and not scl_processed:
        raise TypeError("'state_time_limit' must be None, int, dict (state->limit), or dict (strat->dict(state->limit)).")

    if state_groups_df is not None and not state_groups_df.empty:
        if not {'name', 'group', 'share'}.issubset(state_groups_df.columns):
            raise ValueError("state_groups_df must contain columns 'name', 'group', 'share'.")
        try:
            share_bool = pd.to_numeric(state_groups_df['share'], errors='raise').fillna(0) != 0
        except Exception as e:
            raise ValueError(f"Could not convert 'share' column in state_groups_df to boolean/numeric: {e}")

        state_groups_df_proc = state_groups_df.assign(share_bool=share_bool)

        for strat_name in strategy_names:
            current_strat_limits_dict = result_limits[strat_name]
            for group_id, group_data in state_groups_df_proc.groupby('group'):
                shared_states_in_group = group_data[group_data['share_bool']]['name'].tolist()
                if not shared_states_in_group: continue

                current_limits_for_group_states = [current_strat_limits_dict[s] for s in shared_states_in_group if s in current_strat_limits_dict]
                if not current_limits_for_group_states: continue
                max_limit_for_group = max(current_limits_for_group_states)

                for state_in_group_to_update in shared_states_in_group:
                    if state_in_group_to_update in current_strat_limits_dict:
                         result_limits[strat_name][state_in_group_to_update] = max_limit_for_group
    return result_limits

def construct_expand_df_py(
    states_to_expand_dict: dict[str, bool],
    state_time_limits_per_strategy: dict[str, dict[str, int]],
    model_time_df: pd.DataFrame,
    strategy_names: list[str],
    state_names: list[str],
    group_names: list[str] = None,
    n_strategies: int = None,
    n_states: int = None,
    n_groups: int = None
) -> pd.DataFrame:
    """
    Constructs the core DataFrame used for model evaluation, incorporating state expansion.

    Args:
        states_to_expand_dict: Output of get_states_to_expand_py.
                                Dict mapping state_name -> bool (True if expansion needed).
        state_time_limits_per_strategy: Output of complete_stl_py.
                                        Dict mapping strategy_name -> dict (state_name -> max_state_time).
        model_time_df: DataFrame with 'model_time' and 'markov_cycle' columns.
        strategy_names: List of strategy names.
        state_names: List of state names.
        group_names: Optional list of group names. If None, a single default group is assumed.
        n_strategies: Optional count of strategies (for pre-allocation/validation).
        n_states: Optional count of states (for pre-allocation/validation).
        n_groups: Optional count of groups (for pre-allocation/validation).

    Returns:
        pd.DataFrame with columns: model_time, markov_cycle, strategy, group, state_name, state_time.
                                   Plus factorized IDs: strategy_id, group_id, state_id.
    """
    if n_strategies is None: n_strategies = len(strategy_names)
    if n_states is None: n_states = len(state_names)

    if group_names is None or not group_names:
        group_names_internal = ["_default_group_"]
        if n_groups is None: n_groups = 1
    else:
        group_names_internal = group_names
        if n_groups is None: n_groups = len(group_names_internal)

    if not strategy_names:
        warnings.warn("construct_expand_df_py: strategy_names is empty. Returning empty DataFrame.", UserWarning)
        return pd.DataFrame(columns=['model_time', 'markov_cycle', 'strategy', 'group', 'state_name', 'state_time', 'strategy_id', 'group_id', 'state_id'])
    if not state_names:
        warnings.warn("construct_expand_df_py: state_names is empty. Returning empty DataFrame.", UserWarning)
        return pd.DataFrame(columns=['model_time', 'markov_cycle', 'strategy', 'group', 'state_name', 'state_time', 'strategy_id', 'group_id', 'state_id'])


    all_rows = []

    for mt_idx, mt_row in model_time_df.iterrows():
        model_time_val = mt_row['model_time']
        markov_cycle_val = mt_row['markov_cycle']
        for strat_idx, strat_name in enumerate(strategy_names):
            for group_idx, group_name in enumerate(group_names_internal):
                for state_idx, current_state_name in enumerate(state_names):
                    should_expand_this_state = states_to_expand_dict.get(current_state_name, False)

                    max_st_for_this_state_strat = 0
                    if strat_name in state_time_limits_per_strategy and \
                       current_state_name in state_time_limits_per_strategy[strat_name]:
                       max_st_for_this_state_strat = state_time_limits_per_strategy[strat_name][current_state_name]

                    if should_expand_this_state and max_st_for_this_state_strat > 0:
                        # Expand this state from state_time 0 to max_st_for_this_state_strat (inclusive of 0)
                        # Note: R's seq.int(0, N) gives N+1 elements. Python's range(0, N+1) does the same.
                        # R heRomod uses state_time starting at 0 for expanded states.
                        for st_val in range(max_st_for_this_state_strat + 1): # +1 to include the max limit itself
                            all_rows.append({
                                'model_time': model_time_val,
                                'markov_cycle': markov_cycle_val,
                                'strategy': strat_name,
                                'group': group_name,
                                'state_name': current_state_name,
                                'state_time': float(st_val) # Ensure float like R
                            })
                    else:
                        # Not expanding, or limit is 0. Single entry with state_time = 0.
                        all_rows.append({
                            'model_time': model_time_val,
                            'markov_cycle': markov_cycle_val,
                            'strategy': strat_name,
                            'group': group_name,
                            'state_name': current_state_name,
                            'state_time': 0.0 # Ensure float
                        })

    if not all_rows: # Should not happen if model_time_df, strategies, states are non-empty
        return pd.DataFrame(columns=['model_time', 'markov_cycle', 'strategy', 'group', 'state_name', 'state_time', 'strategy_id', 'group_id', 'state_id'])

    res_df = pd.DataFrame(all_rows)

    # Add factorized IDs
    res_df['strategy_id'] = pd.factorize(res_df['strategy'])[0]
    res_df['group_id'] = pd.factorize(res_df['group'])[0]
    res_df['state_id'] = pd.factorize(res_df['state_name'])[0]

    return res_df
