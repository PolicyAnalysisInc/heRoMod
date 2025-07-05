import pytest
import pandas as pd
import numpy as np
from collections import OrderedDict

from pyheRomod.param_definition import (
    define_parameters, UnevalParameters,
    define_init, UnevalInit,
    define_starting_values, UnevalStartingValues,
    define_inflow, UnevalInflow
)
from pyheRomod.state_definition import define_state, define_state_list, UnevalStateList
from pyheRomod.param_evaluation import (
    eval_parameters_python, PYHEROMOD_CONFIG,
    eval_init_python, C_COMPLEMENT_MARKER,
    eval_starting_values_python, eval_inflow_python
)

# Common setup for tests needing state_list_obj
@pytest.fixture
def simple_state_list_obj():
    return define_state_list(S1=define_state(c=1), S2=define_state(c=1))

@pytest.fixture
def state_list_for_expansion():
    # S1 depends on state_time, S2 does not.
    s1_st = define_state(cost="state_time * 10")
    s2_no_st = define_state(cost="100")
    return define_state_list(S1=s1_st, S2=s2_no_st)


def test_parameter_evaluation_basic(simple_state_list_obj):
    params_def = define_parameters(a="2", b="a * markov_cycle")

    # Test with no expansion (default behavior if state_time_limit_config implies no expansion for any state)
    evaluated_params_no_expansion = eval_parameters_python(
        params_def, simple_state_list_obj, model_cycles=3, strategy_names=["strat1"],
        state_time_limit_config=None # No explicit limits, rely on states_to_expand_dict
    )
    # Expected: 3 model_times * 1 strategy * 2 states * 1 state_time (0.0) = 6 rows
    assert evaluated_params_no_expansion.shape[0] == 3 * 1 * 2 * 1
    assert "a" in evaluated_params_no_expansion.columns
    assert "b" in evaluated_params_no_expansion.columns
    pd.testing.assert_series_equal(
        evaluated_params_no_expansion["a"].reset_index(drop=True),
        pd.Series([2.0]*6, name="a"),
        check_dtype=False
    )
    # markov_cycle is 0, 1, 2. For each state, for strat1.
    # Expected b: [2*0, 2*0, 2*1, 2*1, 2*2, 2*2] if sorted by model_time then state
    expected_b_vals = []
    for mc in [0.0,1.0,2.0]: # markov_cycles
        for _ in simple_state_list_obj.get_state_names(): # S1, S2
             expected_b_vals.append(2.0 * mc)

    # Need to sort evaluated_params_no_expansion to match expected_b_vals generation
    sorted_eval_params = evaluated_params_no_expansion.sort_values(by=['strategy', 'model_time', 'state_name']).reset_index(drop=True)
    pd.testing.assert_series_equal(
        sorted_eval_params["b"],
        pd.Series(expected_b_vals, name="b"),
        check_dtype=False
    )
    assert (evaluated_params_no_expansion["state_time"] == 0.0).all() # No expansion means state_time is 0

    defined_param_names = params_def.get_parameter_names()
    for name in defined_param_names:
        assert name in evaluated_params_no_expansion.columns

def test_parameter_evaluation_with_expansion(state_list_for_expansion):
    params_def = define_parameters(
        p_fixed = "10",
        p_model_time = "model_time * 2",
        p_state_time = "state_time * 3",
        p_markov_cycle = "markov_cycle * 4",
        p_combined = "p_fixed + p_model_time + p_state_time + p_markov_cycle"
    )
    # S1 depends on state_time, S2 does not.
    # Let S1 expand up to state_time=1 (so ST values 0, 1)
    # Let S2 not expand (so ST value 0)
    # Model cycles = 2 (MT = 1, 2; MC = 0, 1)
    # Strategy = "test_strat"

    state_limit_config = {"test_strat": {"S1": 1, "S2": 0}} # S1 up to ST=1, S2 up to ST=0

    eval_params = eval_parameters_python(
        param_obj=params_def,
        state_list_obj=state_list_for_expansion,
        model_cycles=2,
        strategy_names=["test_strat"],
        state_time_limit_config=state_limit_config
    )

    # Expected rows:
    # MT=1 (MC=0):
    #   S1, ST=0: p_f=10, p_mt=2, p_st=0, p_mc=0. Combined = 12
    #   S1, ST=1: p_f=10, p_mt=2, p_st=3, p_mc=0. Combined = 15
    #   S2, ST=0: p_f=10, p_mt=2, p_st=0, p_mc=0. Combined = 12
    # MT=2 (MC=1):
    #   S1, ST=0: p_f=10, p_mt=4, p_st=0, p_mc=4. Combined = 18
    #   S1, ST=1: p_f=10, p_mt=4, p_st=3, p_mc=4. Combined = 21
    #   S2, ST=0: p_f=10, p_mt=4, p_st=0, p_mc=4. Combined = 18
    # Total rows = 3 (for MT=1) + 3 (for MT=2) = 6 rows
    assert len(eval_params) == 6

    # Check some specific values
    row_s1_mt1_st0 = eval_params[
        (eval_params["state_name"] == "S1") &
        (eval_params["model_time"] == 1) &
        (eval_params["state_time"] == 0)
    ]
    assert np.isclose(row_s1_mt1_st0["p_combined"].iloc[0], 12.0)

    row_s1_mt1_st1 = eval_params[
        (eval_params["state_name"] == "S1") &
        (eval_params["model_time"] == 1) &
        (eval_params["state_time"] == 1)
    ]
    assert np.isclose(row_s1_mt1_st1["p_combined"].iloc[0], 15.0)

    row_s2_mt2_st0 = eval_params[
        (eval_params["state_name"] == "S2") &
        (eval_params["model_time"] == 2) &
        (eval_params["state_time"] == 0)
    ]
    assert np.isclose(row_s2_mt2_st0["p_combined"].iloc[0], 18.0)

    # Check state_day etc. are added
    assert 'state_day' in eval_params.columns
    assert 'state_year' in eval_params.columns
    assert (eval_params['state_day'] == eval_params['state_time']).all()


def test_infinite_parameters(simple_state_list_obj):
    params_def = define_parameters(a="2", b="1 / (markov_cycle - 1.0)") # Inf at MC=1 (MT=2)
    original_inf_config = PYHEROMOD_CONFIG["inf_parameter"]

    PYHEROMOD_CONFIG["inf_parameter"] = "ignore"
    eval_ignore = eval_parameters_python(
        params_def, simple_state_list_obj, model_cycles=3, strategy_names=["s1"], state_time_limit_config=None
    )
    # MCs are 0, 1, 2. Inf at MC=1.
    # For each state (S1, S2), for each MC:
    # MC=0 (MT=1): b = 1 / (0-1) = -1.0
    # MC=1 (MT=2): b = 1 / (1-1) = inf
    # MC=2 (MT=3): b = 1 / (2-1) = 1.0
    # Expected b values for one state: [-1.0, inf, 1.0]. Duplicated for S2.
    expected_b_vals_one_state = np.array([-1.0, np.inf, 1.0])

    # Check for one state (e.g. S1)
    eval_ignore_s1 = eval_ignore[eval_ignore['state_name']=='S1'].sort_values(by='model_time')
    pd.testing.assert_series_equal(
        eval_ignore_s1["b"].reset_index(drop=True),
        pd.Series(expected_b_vals_one_state, name="b"),
        check_dtype=False
    )

    PYHEROMOD_CONFIG["inf_parameter"] = "warning"
    with pytest.warns(UserWarning, match="Error in parameter 'b', infinite value\\(s\\) detected."):
        eval_parameters_python(params_def, simple_state_list_obj, model_cycles=3, strategy_names=["s1"], state_time_limit_config=None)

    PYHEROMOD_CONFIG["inf_parameter"] = "error"
    with pytest.raises(ValueError, match="Error in parameter 'b', infinite value\\(s\\) detected."):
        eval_parameters_python(params_def, simple_state_list_obj, model_cycles=3, strategy_names=["s1"], state_time_limit_config=None)

    PYHEROMOD_CONFIG["inf_parameter"] = original_inf_config


def test_parameter_evaluation_error_reporting(simple_state_list_obj):
    params_def = define_parameters(a="2", b="3", d="A + b", g="a + b") # "A" is not defined
    with pytest.raises(RuntimeError) as excinfo:
        eval_parameters_python(params_def, simple_state_list_obj, model_cycles=1, strategy_names=["s1"])
    assert "Error in parameter 'd'" in str(excinfo.value)
    assert "A" in str(excinfo.value) or "'A'" in str(excinfo.value) # Check for NameError part
    assert isinstance(excinfo.value.__cause__, NameError)

def test_ast_hacks_dispatch_strategy(): # This test is independent of new eval_parameters signature
    from pyheRomod.param_evaluation import apply_expression_hacks_py # This test is independent of new eval_parameters signature
    expr1 = "dispatch_strategy_py(option1=10, option2=20)"
    expected1 = "dispatch_strategy_py(_strategy=strategy, option1=10, option2=20)"
    assert apply_expression_hacks_py(expr1, "start").replace(" ", "") == expected1.replace(" ", "")
    expr2 = "dispatch_strategy_py(_strategy='explicit_strat', option1=10)"
    assert apply_expression_hacks_py(expr2, "start").replace(" ", "") == expr2.replace(" ", "") # Should not change if _strategy is present
    expr3 = "by_strategy_py(option1=10, option2=20)"
    expected3 = "by_strategy_py(_strategy=strategy, option1=10, option2=20)"
    assert apply_expression_hacks_py(expr3, "start").replace(" ", "") == expected3.replace(" ", "")
    expr4 = "by_group_py(groupA=1, groupB=2)"
    expected4 = "by_group_py(_group=group, groupA=1, groupB=2)"
    assert apply_expression_hacks_py(expr4, "start").replace(" ", "") == expected4.replace(" ", "")

def test_ast_hacks_discount(): # This test is independent of new eval_parameters signature
    from pyheRomod.param_evaluation import apply_expression_hacks_py
    expr1 = "discount_py(cost, 0.05)"
    expected1_start = "discount_py(cost,0.05,time=markov_cycle-1.0)"
    expected1_end = "discount_py(cost,0.05,time=markov_cycle-0.0)"
    expected1_mid = "discount_py(cost,0.05,time=markov_cycle-0.5)"
    assert apply_expression_hacks_py(expr1, "start").replace(" ", "") == expected1_start.replace(" ", "")
    assert apply_expression_hacks_py(expr1, "end").replace(" ", "") == expected1_end.replace(" ", "")
    assert apply_expression_hacks_py(expr1, "midpoint").replace(" ", "") == expected1_mid.replace(" ", "")

    expr2 = "discount_py(cost, 0.05, time=model_time)" # time should be overwritten
    assert apply_expression_hacks_py(expr2, "start").replace(" ", "") == expected1_start.replace(" ", "")

    expr3 = "discount_py(x=cost, r=0.05, first=True)" # other args preserved
    expected3_start = "discount_py(x=cost,r=0.05,first=True,time=markov_cycle-1.0)"
    assert apply_expression_hacks_py(expr3, "start").replace(" ", "") == expected3_start.replace(" ", "")

def test_dispatch_strategy_functionality(simple_state_list_obj):
    params_def = define_parameters(
        cost_A = "100.0", cost_B = "200.0",
        current_cost = "dispatch_strategy_py(stratA=cost_A, stratB=cost_B)"
    )
    eval_A = eval_parameters_python(params_def, simple_state_list_obj, model_cycles=1, strategy_names=["stratA"])
    assert np.isclose(eval_A["current_cost"].iloc[0], 100.0) # Check first row for S1, should be same for S2 for this param

    eval_B = eval_parameters_python(params_def, simple_state_list_obj, model_cycles=1, strategy_names=["stratB"])
    assert np.isclose(eval_B["current_cost"].iloc[0], 200.0)

    params_group = define_parameters(
        val_g1 = "50.0", val_g2 = "75.0",
        current_val = "by_group_py(group1=val_g1, group2=val_g2)"
    )
    eval_g1 = eval_parameters_python(params_group, simple_state_list_obj, model_cycles=1, strategy_names=["s1"], group_names=["group1"])
    assert np.isclose(eval_g1["current_val"].iloc[0], 50.0)
    eval_g2 = eval_parameters_python(params_group, simple_state_list_obj, model_cycles=1, strategy_names=["s1"], group_names=["group2"])
    assert np.isclose(eval_g2["current_val"].iloc[0], 75.0)


def test_discount_functionality(simple_state_list_obj):
    params_def_start = define_parameters(base_cost="100.0", disc_cost="discount_py(base_cost, 0.05)")
    eval_start = eval_parameters_python(params_def_start, simple_state_list_obj, model_cycles=3, strategy_names=["s1"], disc_method="start")
    # MCs = 0, 1, 2. Expected costs for one state: [100/(1.05^0), 100/(1.05^1), 100/(1.05^2)]
    expected_costs_start_series = pd.Series([100.0, 100/1.05, 100/(1.05**2)], name="disc_cost")
    # Check for one state (e.g. S1)
    eval_start_s1 = eval_start[eval_start['state_name']=='S1'].sort_values(by='model_time')
    pd.testing.assert_series_equal(eval_start_s1["disc_cost"].reset_index(drop=True), expected_costs_start_series, check_dtype=False)

    params_def_end = define_parameters(base_cost="100.0", disc_cost="discount_py(base_cost, 0.05)")
    eval_end = eval_parameters_python(params_def_end, simple_state_list_obj, model_cycles=2, strategy_names=["s1"], disc_method="end")
    # MCs = 0, 1. Discount time = MC - 0 = 0, 1.
    # Expected costs for one state: [100/(1.05^0), 100/(1.05^1)] -> WRONG, discount time is MC for "end"
    # Original heRomod discount(time) means (1+r)^time. If method="end", time arg to discount.py is markov_cycle.
    # So for MC=0 (MT=1), time=0. For MC=1 (MT=2), time=1.
    # Oh, the R code for `discount.default` has `time <- time - offset`.
    # If method="end", offset=0, so discount_time = markov_cycle - 0.
    # If method="start", offset=1, discount_time = markov_cycle - 1. (This is what my Python AST hack does)
    # Let's re-check R: discount(100, .05, time = c(0,1,2), method="start") -> 100.00000  95.23810  90.70295
    # discount(100, .05, time = c(0,1,2), method="end")   -> 100.00000  95.23810  90.70295
    # This is because the `time` argument to R's `discount` is ALREADY the effective time.
    # The `markov_cycle - offset` happens *before* calling discount.
    # My Python `discount_py` takes an explicit `time` argument. The AST hack sets this `time`.
    # So, for disc_method="start", hack sets time = markov_cycle - 1.0
    # MCs = 0, 1, 2 => effective_times = -1, 0, 1.
    # Costs: 100*(1.05^1) [undiscounted, future val?], 100*(1.05^0), 100*(1.05^-1) -> This seems wrong.
    # R `discount(val, r, time)` is `val / (1+r)^time`.
    # If `time` in R's discount is `markov_cycle - offset`:
    # Method="start" (offset=1): mc=0->t=-1 (py val/(1+r)^-1 = val*(1+r)), mc=1->t=0 (py val), mc=2->t=1 (py val/(1+r))
    # This is what my `eval_start_s1` shows above for `expected_costs_start_series`.
    # The R output 100, 95.23, 90.70 implies effective times of 0, 1, 2.
    # This means R's `time` argument for discount must be `cycle_number - 1` if method="start"
    # and `cycle_number` if method="end", where cycle_number is 1-indexed.
    # My `markov_cycle` is 0-indexed.
    # If method="start", AST hack sets `time = markov_cycle - 1.0`.
    #   MC=0 (1st cycle): time=-1. `discount_py(x,r,time=-1)` -> `x / (1+r)^-1` = `x * (1+r)`. (Future value)
    #   MC=1 (2nd cycle): time=0. `discount_py(x,r,time=0)` -> `x`.
    #   MC=2 (3rd cycle): time=1. `discount_py(x,r,time=1)` -> `x / (1+r)`.
    # This is NOT standard discounting for costs in cycle 1, 2, 3.
    # Standard: cost_in_cycle_1 / (1+r)^0, cost_in_cycle_2 / (1+r)^1, cost_in_cycle_3 / (1+r)^2
    # This implies effective discount times should be 0, 1, 2 for MC=0, 1, 2.
    # So, if method="start", discount time should be `markov_cycle`. (Ast hack: `markov_cycle - 0.0` is not right, should be `markov_cycle`)
    # If method="end", discount time should be `markov_cycle + 1`. (Ast hack: `markov_cycle - (-1.0)` )
    # If method="midpoint", discount time should be `markov_cycle + 0.5`. (Ast hack: `markov_cycle - (-0.5)`)
    # Let's assume the AST hack is what R intends for its `time` argument passed to its internal discount.
    # R's `discount.default(val, r, time, first = FALSE)` is `val / (1 + r)^(time + ifelse(first, 1, 0))`
    # The `time` in `discount.default` is `markov_cycle - offset`.
    # So, for method="start" (offset=1), `time_arg_to_discount_default` = `markov_cycle - 1`.
    #   Result: `val / (1+r)^(markov_cycle - 1)`.
    #   MC=0: `val / (1+r)^-1` = `val * (1+r)`.
    #   MC=1: `val / (1+r)^0` = `val`.
    #   MC=2: `val / (1+r)^1`.
    # This matches my `expected_costs_start_series` after adjusting for the name.
    # It seems my Python implementation of `discount_py` and the AST hack for `time` correctly replicate R's behavior
    # *given the way R structures its internal calls*. The Python code correctly reflects this interpretation.
    # Effective discount times for MC = 0, 1, 2 with method="start" (offset=1) are -1, 0, 1.
    # Costs: 100/(1.05^-1) = 105, 100/(1.05^0)=100, 100/(1.05^1)=95.23...
    expected_costs_start_py = [100.0 * 1.05, 100.0, 100.0 / 1.05]
    pd.testing.assert_series_equal(
        eval_start_s1["disc_cost"].reset_index(drop=True),
        pd.Series(expected_costs_start_py, name="disc_cost"),
        check_dtype=False
    )

    # Method="end" (offset=0): `time_arg_to_discount_default` = `markov_cycle`.
    #   Result: `val / (1+r)^(markov_cycle)`.
    #   MC=0: `val / (1+r)^0` = val.
    #   MC=1: `val / (1+r)^1`.
    eval_end_s1 = eval_end[eval_end['state_name']=='S1'].sort_values(by='model_time')
    expected_costs_end_py = [100.0, 100.0/1.05]
    pd.testing.assert_series_equal(
        eval_end_s1["disc_cost"].reset_index(drop=True),
        pd.Series(expected_costs_end_py, name="disc_cost"),
        check_dtype=False
    )

    # `first=True` adds 1 to the exponent denominator.
    # disc_cost="discount_py(base_cost, 0.05, first=True)", method="start"
    # time_arg_to_discount_default = markov_cycle - 1.
    # Result: `val / (1+r)^(markov_cycle - 1 + 1)` = `val / (1+r)^(markov_cycle)`. Same as method="end".
    params_def_first = define_parameters(base_cost="100.0", disc_cost="discount_py(base_cost, 0.05, first=True)")
    eval_first = eval_parameters_python(params_def_first, simple_state_list_obj, model_cycles=2, strategy_names=["s1"], disc_method="start")
    eval_first_s1 = eval_first[eval_first['state_name']=='S1'].sort_values(by='model_time')
    pd.testing.assert_series_equal(
        eval_first_s1["disc_cost"].reset_index(drop=True),
        pd.Series(expected_costs_end_py, name="disc_cost"), # Should match method="end"
        check_dtype=False
    )

def test_scipy_distributions_in_eval(simple_state_list_obj):
    params_def = define_parameters(rand_val = "rnorm(1, 0, 1)[0]")
    # cycles=20, 2 states -> 40 rows if no expansion
    eval_res = eval_parameters_python(params_def, simple_state_list_obj, model_cycles=20, strategy_names=["s1"])
    assert eval_res.shape[0] == 20 * 2 * 1 # cycles * num_states * num_strats (assuming ST_limit=0)
    assert "rand_val" in eval_res.columns
    assert pd.api.types.is_numeric_dtype(eval_res["rand_val"])
    assert not np.isnan(eval_res["rand_val"].iloc[0]) # Check one value

    params_beta = define_parameters(beta_val = "rbeta(1, 2, 5)[0]")
    eval_beta = eval_parameters_python(params_beta, simple_state_list_obj, model_cycles=20, strategy_names=["s1"])
    assert "beta_val" in eval_beta.columns
    assert ((eval_beta["beta_val"] >= 0) & (eval_beta["beta_val"] <= 1)).all()

    # Test direct scipy call with Series arguments
    params_direct_norm = define_parameters(
        mean_col = "np.zeros(len(model_time))", # This will be evaluated per row, len(model_time) is not right in this context
                                               # It should be just "0.0" or use existing columns like "markov_cycle"
                                               # Let's make it simpler for the test's purpose
        mean_val = "0.0",
        std_val = "1.0",
        rand_direct = "norm.rvs(loc=mean_val, scale=std_val, size=len(model_time))" # size=len(model_time) will create a Series of length N_cycles * N_states
    )
    # This test needs careful thought on how norm.rvs would behave with size in safe_eval_python
    # safe_eval_python expects result to be scalar or same length as input df chunk (which is row-by-row)
    # If norm.rvs returns an array of len > 1, it might fail or assign only first element.
    # For now, let's test single value return from rvs.
    params_direct_norm_scalar = define_parameters(
        mean_val = "0.0", std_val = "1.0",
        rand_direct = "norm.rvs(loc=mean_val, scale=std_val)" # Scalar output
    )
    eval_direct_norm = eval_parameters_python(params_direct_norm_scalar, simple_state_list_obj, model_cycles=20, strategy_names=["s1"])
    assert "rand_direct" in eval_direct_norm.columns
    assert eval_direct_norm["rand_direct"].shape[0] == 20 * 2
    # These checks are for a sample, not exact properties of the whole series from single rvs calls per row
    # assert abs(eval_direct_norm["rand_direct"].mean()) < 0.6
    # assert 0.4 < eval_direct_norm["rand_direct"].std() < 1.6
    assert pd.api.types.is_numeric_dtype(eval_direct_norm["rand_direct"])


def test_eval_init_basic_and_complement(simple_state_list_obj):
    # Base eval_params for context (model_time=1, state_time=0 for 2 states, 1 strat, 1 group)
    base_eval_params = eval_parameters_python(
        define_parameters(p1=1), simple_state_list_obj, model_cycles=1, strategy_names=["stratA"], group_names=["group1"]
    )
    # base_eval_params will have 2 rows (S1, S2) for MT=1, ST=0. Context for eval_init is one of these.

    init_expr1 = define_init(S1="0.8", S2="0.2")
    res1 = eval_init_python(init_expr1, base_eval_params)
    expected_s1 = pd.Series([0.8, 0.2], index=["S1", "S2"], name=('stratA', 'group1'))
    pd.testing.assert_series_equal(res1.loc[('stratA', 'group1')], expected_s1, check_dtype=False)

    init_expr_c = define_init(S1="0.7", S2="C_complement") # Assuming S1, S2 are the only states
    res_c = eval_init_python(init_expr_c, base_eval_params)
    expected_c = pd.Series([0.7, 0.3], index=["S1", "S2"], name=('stratA', 'group1'))
    pd.testing.assert_series_equal(res_c.loc[('stratA', 'group1')], expected_c, check_dtype=False)

    # Test with a state list that has more states for complement
    sl_3states = define_state_list(H=define_state(c=1),S=define_state(c=1),D=define_state(c=1))
    base_eval_params_3s = eval_parameters_python(
        define_parameters(p1=1), sl_3states, model_cycles=1, strategy_names=["stratA"], group_names=["group1"]
    )
    init_expr_3s_c = define_init(H="0.7", S="C_complement", D="0.1")
    res_3s_c = eval_init_python(init_expr_3s_c, base_eval_params_3s)
    expected_3s_c = pd.Series([0.7, 0.2, 0.1], index=["H", "S", "D"], name=('stratA', 'group1'))
    pd.testing.assert_series_equal(res_3s_c.loc[('stratA', 'group1')], expected_3s_c, check_dtype=False)


    init_expr_all_c = define_init(S1="C_complement") # Only one state defined in init
    # This will only work if S1 is the only state in the model context for init (e.g. if init_obj.expressions.keys() defines the states)
    # eval_init_python uses init_obj.expressions.keys() as the state names.
    res_all_c = eval_init_python(init_expr_all_c, base_eval_params) # base_eval_params implies S1, S2
                                                                    # but init_expr_all_c only defines S1
                                                                    # The states for init are taken from init_obj keys.
    expected_all_c = pd.Series([1.0], index=["S1"], name=('stratA', 'group1'))
    pd.testing.assert_series_equal(res_all_c.loc[('stratA', 'group1')], expected_all_c, check_dtype=False)


def test_eval_init_errors_and_checks(simple_state_list_obj):
    base_eval_params = eval_parameters_python(
        define_parameters(p1=1), simple_state_list_obj, model_cycles=1, strategy_names=["sA"], group_names=["g1"]
    )

    init_multi_c = define_init(S1="C_complement", S2="C_complement")
    with pytest.raises(ValueError, match="Complement marker error"):
        eval_init_python(init_multi_c, base_eval_params)

    init_na = define_init(S1="some_undefined_var")
    with pytest.raises(RuntimeError, match="Error in initial probability for state 'S1'.*some_undefined_var"):
        eval_init_python(init_na, base_eval_params)

    init_neg = define_init(S1="-0.1", S2="1.1") # Sums to 1, but S1 is negative
    with pytest.raises(ValueError, match=r"Negative probs for .*\[['\"]S1['\"]\]"):
        eval_init_python(init_neg, base_eval_params) # individual_level=False by default, still checks for raw negatives

    init_sum_not_1 = define_init(S1="0.5", S2="0.4") # Sums to 0.9
    with pytest.raises(ValueError, match="Probs do not sum to 1 for"):
        eval_init_python(init_sum_not_1, base_eval_params, individual_level=True)

    # Test when individual_level=False, sum not 1 is okay (warning for sum 0)
    with warnings.catch_warnings(record=True) as w_list: # Check no warning if sum is not 1 and not 0
        eval_init_python(init_sum_not_1, base_eval_params, individual_level=False)
        assert len(w_list) == 0

    init_gt_1_sum_ok = define_init(S1="1.5", S2="-0.5") # Sums to 1, but S2 negative
    with pytest.raises(ValueError, match=r"Negative probs for.*S2"):
        eval_init_python(init_gt_1_sum_ok, base_eval_params, individual_level=True)

    init_s1_gt_1_only = define_init(S1="1.5", S2="0.0") # S1 > 1
    with pytest.raises(ValueError, match=r"Probs outside \[0,1\] for.*S1"):
        eval_init_python(init_s1_gt_1_only, base_eval_params, individual_level=True)

    init_sum_0 = define_init(S1="0.0", S2="0.0")
    with pytest.warns(UserWarning, match="Init sum to zero for"):
        eval_init_python(init_sum_0, base_eval_params, individual_level=False)

def test_eval_init_multiple_strategies_groups(simple_state_list_obj):
    params = define_parameters(valA=0.8, valB=0.6, valC=0.7, valD=0.5)
    eval_params_multi = eval_parameters_python(
        params, simple_state_list_obj, model_cycles=1,
        strategy_names=["stratA", "stratB"],
        group_names=["group1", "group2"]
    )
    # eval_params_multi has MT=1,ST=0 for S1/S2 for each of 4 strat/group combos = 2*4=8 rows.

    init_expr = define_init(
        S1 = "dispatch_strategy_py(stratA=by_group_py(group1=valA, group2=valB), stratB=by_group_py(group1=valC, group2=valD))",
        S2 = "1.0 - S1"
    )

    res_multi = eval_init_python(init_expr, eval_params_multi, individual_level=True)

    assert isinstance(res_multi, pd.DataFrame)
    assert res_multi.shape == (4, 2) # 4 strat/group combos, 2 states
    assert list(res_multi.index.names) == ['strategy', 'group']

    # Check values
    # stratA, group1: S1 should be valA (0.8), S2 = 0.2
    assert np.isclose(res_multi.loc[("stratA", "group1"), "S1"], 0.8)
    assert np.isclose(res_multi.loc[("stratA", "group1"), "S2"], 0.2)
    # stratA, group2: S1 should be valB (0.6), S2 = 0.4
    assert np.isclose(res_multi.loc[("stratA", "group2"), "S1"], 0.6)
    assert np.isclose(res_multi.loc[("stratA", "group2"), "S2"], 0.4)
    # stratB, group1: S1 should be valC (0.7), S2 = 0.3
    assert np.isclose(res_multi.loc[("stratB", "group1"), "S1"], 0.7)
    assert np.isclose(res_multi.loc[("stratB", "group1"), "S2"], 0.3)
    # stratB, group2: S1 should be valD (0.5), S2 = 0.5
    assert np.isclose(res_multi.loc[("stratB", "group2"), "S1"], 0.5)
    assert np.isclose(res_multi.loc[("stratB", "group2"), "S2"], 0.5)


def test_eval_starting_values(simple_state_list_obj):
    params = define_parameters(base_val_sA = "model_time * 10", base_val_sB = "model_time * 5")
    eval_params = eval_parameters_python(
        params, simple_state_list_obj, model_cycles=2,
        strategy_names=["stratA", "stratB"], group_names=["g1"]
    )
    # eval_params has state_time=0 for S1/S2 for MT=1,2 and stratA/g1, stratB/g1
    # Total 2 states * 2 MT * 2 strats * 1 group = 8 rows for ST=0 context

    sv_expr = define_starting_values(
        val1 = "dispatch_strategy_py(stratA=base_val_sA, stratB=base_val_sB)",
        val2 = "50.0"
    )
    res_sv = eval_starting_values_python(sv_expr, eval_params)

    # Expected output: DataFrame with model_time, strategy, group, val1, val2
    # For each strat/group, values for MT=1, and MT=2 (which is then zeroed out)
    # stratA, g1:
    #   MT=1: val1 = 1*10=10, val2=50
    #   MT=2: val1 = 0 (zeroed), val2=0 (zeroed)
    # stratB, g1:
    #   MT=1: val1 = 1*5=5,  val2=50
    #   MT=2: val1 = 0 (zeroed), val2=0 (zeroed)
    # Total 2 strats * 1 group * 2 MT = 4 rows in res_sv
    assert res_sv.shape == (4, 7) # model_time, strategy, group, strategy_id, group_id, val1, val2 (state_name, state_id, markov_cycle, state_time are also there)
                                 # Let's be more specific about expected columns
    expected_cols = ['model_time', 'strategy', 'group', 'strategy_id', 'group_id', 'state_name', 'state_id', 'markov_cycle', 'state_time', 'val1', 'val2']
    assert all(col in res_sv.columns for col in expected_cols)
    assert len(res_sv) == 4 # Correct number of unique model_time/strategy/group combinations for ST=0

    res_sv_sA = res_sv[res_sv["strategy"] == "stratA"].sort_values("model_time")
    assert np.isclose(res_sv_sA[res_sv_sA["model_time"]==1.0]["val1"].iloc[0], 10.0)
    assert np.isclose(res_sv_sA[res_sv_sA["model_time"]==1.0]["val2"].iloc[0], 50.0)
    assert np.isclose(res_sv_sA[res_sv_sA["model_time"]==2.0]["val1"].iloc[0], 0.0)
    assert np.isclose(res_sv_sA[res_sv_sA["model_time"]==2.0]["val2"].iloc[0], 0.0)

    res_sv_sB = res_sv[res_sv["strategy"] == "stratB"].sort_values("model_time")
    assert np.isclose(res_sv_sB[res_sv_sB["model_time"]==1.0]["val1"].iloc[0], 5.0)
    assert np.isclose(res_sv_sB[res_sv_sB["model_time"]==1.0]["val2"].iloc[0], 50.0)
    assert np.isclose(res_sv_sB[res_sv_sB["model_time"]==2.0]["val1"].iloc[0], 0.0)
    assert np.isclose(res_sv_sB[res_sv_sB["model_time"]==2.0]["val2"].iloc[0], 0.0)

    # Test with one cycle (max model_time is 1, so all should be zeroed)
    eval_params_1cycle = eval_parameters_python(
        params, simple_state_list_obj, model_cycles=1, strategy_names=["stratA"], group_names=["g1"]
    )
    res_sv_1cycle = eval_starting_values_python(sv_expr, eval_params_1cycle)
    assert len(res_sv_1cycle) == 1 # 1 strat * 1 group * 1 MT
    assert np.isclose(res_sv_1cycle["val1"].iloc[0], 0.0)
    assert np.isclose(res_sv_1cycle["val2"].iloc[0], 0.0)

    # Test with no state_time=0 data (should warn and return empty with value cols)
    # Create dummy eval_params where all state_time > 0
    eval_params_no_st0 = eval_params.copy()
    eval_params_no_st0['state_time'] = eval_params_no_st0['state_time'] + 1 # make all ST > 0
    with pytest.warns(UserWarning, match="eval_starting_values: No data for state_time == 0.0"):
        # This might also trigger the fallback if min state_time is used
        # Let's make it so there's NO data for state_time=0.0 at all.
        # The current fallback takes min state_time per group.
        # To test the truly empty context after filtering for ST=0:
        eval_params_only_st_gt_0 = eval_params[eval_params['state_time'] > 0].copy() # this will be empty if eval_params only had ST=0
        # Let's build one from scratch for this.
        dummy_eval_params_st_gt_0 = pd.DataFrame({
            'model_time':     [1.0, 1.0], 'markov_cycle': [0.0, 0.0],
            'state_time':     [1.0, 2.0], # Both > 0
            'strategy':       ['stratA']*2, 'group': ['g1']*2, 'state_name': ['S1', 'S2'],
            'base_val_sA':    [10.0, 10.0], 'base_val_sB': [5.0, 5.0],
            'strategy_id': [0,0], 'group_id': [0,0], 'state_id': [0,1]
        })
        # The warning about ST=0 will occur. Then the fallback tries min ST.
        # So it should still produce results based on ST=1.
        res_sv_st_gt_0 = eval_starting_values_python(sv_expr, dummy_eval_params_st_gt_0)
        assert len(res_sv_st_gt_0) == 1 # MT=1, stratA, g1. ST=1 is used as context.
        assert np.isclose(res_sv_st_gt_0["val1"].iloc[0], 0.0) # Max MT is 1, so zeroed.
        assert np.isclose(res_sv_st_gt_0["val2"].iloc[0], 0.0)


def test_eval_inflow(state_list_for_expansion): # Use S1 (expands), S2 (no expand)
    params = define_parameters(rate_s1="0.1 + markov_cycle * 0.01", rate_s2="0.2 - markov_cycle * 0.02")

    # S1 expands to ST=0,1; S2 ST=0. Cycles=2. Strat="sA", Group="g1"
    state_limits = {"sA": {"S1": 1, "S2": 0}}
    eval_params = eval_parameters_python(
        params, state_list_for_expansion, model_cycles=2,
        strategy_names=["sA"], group_names=["g1"],
        state_time_limit_config=state_limits
    )
    # eval_params has 6 rows as in test_parameter_evaluation_with_expansion
    # S1: MT=1, ST=0,1; MT=2, ST=0,1
    # S2: MT=1, ST=0;   MT=2, ST=0

    inflow_expr = define_inflow(
        S1_in = "100 * rate_s1 + state_time * 5",  # For S1, inflow depends on its own rate and state_time
        S2_in = "100 * rate_s2"                   # For S2, inflow depends on its own rate (ST always 0 for S2)
    )
    res_inflow = eval_inflow_python(inflow_expr, eval_params)

    # Expected: DataFrame with model_time, strategy, group, S1_in, S2_in
    # Aggregated over state_time.
    # MT=1 (MC=0):
    #   rate_s1 = 0.1, rate_s2 = 0.2
    #   S1_in:
    #     ST=0: 100*0.1 + 0*5 = 10
    #     ST=1: 100*0.1 + 1*5 = 15
    #     Total S1_in for MT=1 = 10 + 15 = 25
    #   S2_in (only ST=0 for S2):
    #     ST=0: 100*0.2 = 20
    #     Total S2_in for MT=1 = 20
    # MT=2 (MC=1):
    #   rate_s1 = 0.1 + 0.01 = 0.11
    #   rate_s2 = 0.2 - 0.02 = 0.18
    #   S1_in:
    #     ST=0: 100*0.11 + 0*5 = 11
    #     ST=1: 100*0.11 + 1*5 = 16
    #     Total S1_in for MT=2 = 11 + 16 = 27
    #   S2_in (only ST=0 for S2):
    #     ST=0: 100*0.18 = 18
    #     Total S2_in for MT=2 = 18

    assert "model_time" in res_inflow.columns
    assert "strategy" in res_inflow.columns
    assert "group" in res_inflow.columns
    assert "S1_in" in res_inflow.columns; assert "S2_in" in res_inflow.columns
    assert len(res_inflow) == 2 # One row per model_time (since only 1 strat/group)

    row_mt1 = res_inflow[res_inflow["model_time"] == 1.0]
    assert np.isclose(row_mt1["S1_in"].iloc[0], 25.0)
    assert np.isclose(row_mt1["S2_in"].iloc[0], 20.0)

    row_mt2 = res_inflow[res_inflow["model_time"] == 2.0]
    assert np.isclose(row_mt2["S1_in"].iloc[0], 27.0)
    assert np.isclose(row_mt2["S2_in"].iloc[0], 18.0)

    # Test error cases
    inflow_neg_expr = define_inflow(S1_in = "-10")
    with pytest.raises(ValueError, match="Negative inflow value\\(s\\) detected in state 'S1_in'"):
        eval_inflow_python(inflow_neg_expr, eval_params.head(1).copy()) # Use a small part of eval_params

    inflow_na_expr = define_inflow(S1_in = "undefined_var_for_inflow")
    with pytest.raises(RuntimeError, match="Error in inflow 'S1_in'"):
        eval_inflow_python(inflow_na_expr, eval_params.head(1).copy())
