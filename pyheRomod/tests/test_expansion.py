import pytest
from pyheRomod.expansion import (
    STATE_TIME_VARS,
    has_state_time_in_expr,
    has_state_time_py_for_uneval_collection,
    has_state_time_py_for_state,
    has_state_time_py_for_state_transition,
    has_state_time_py_for_uneval_state_list,
    get_vars_from_expr,
    trace_st_dependency_py,
    _check_scl_py,
    complete_stl_py,
    get_states_to_expand_py,
    construct_expand_df_py
)
from pyheRomod.param_definition import UnevalCollection, UnevalParameters, define_parameters
from pyheRomod.state_definition import State, StateTransition, UnevalStateList, define_state, define_state_transition, define_state_list
from collections import OrderedDict
import pandas as pd


def test_has_state_time_in_expr():
    assert has_state_time_in_expr("cost + state_time", STATE_TIME_VARS) == True
    assert has_state_time_in_expr("cost + model_time", STATE_TIME_VARS) == False
    assert has_state_time_in_expr("state_day * 5", STATE_TIME_VARS) == True
    assert has_state_time_in_expr("my_state_time_var", STATE_TIME_VARS) == False
    assert has_state_time_in_expr("100", STATE_TIME_VARS) == False
    assert has_state_time_in_expr("", STATE_TIME_VARS) == False
    assert has_state_time_in_expr("some_func(state_week)", STATE_TIME_VARS) == True
    assert has_state_time_in_expr("cost + state_time)", STATE_TIME_VARS) == False

def test_has_state_time_for_uneval_collection():
    coll1 = UnevalCollection(a="state_time * 2", b="100")
    assert has_state_time_py_for_uneval_collection(coll1) == True
    coll2 = UnevalCollection(a="model_time * 2", b="param_b")
    assert has_state_time_py_for_uneval_collection(coll2) == False
    coll_empty = UnevalCollection()
    assert has_state_time_py_for_uneval_collection(coll_empty) == False

def test_has_state_time_for_state_and_transition():
    s1_st = define_state(cost="state_time * 10")
    s2_no_st = define_state(cost="100")
    assert has_state_time_py_for_state(s1_st) == True
    assert has_state_time_py_for_state(s2_no_st) == False
    t1_st = define_state_transition(prob="0.1 * state_week")
    t2_no_st = define_state_transition(prob="0.5")
    assert has_state_time_py_for_state_transition(t1_st) == True
    assert has_state_time_py_for_state_transition(t2_no_st) == False

def test_has_state_time_for_uneval_state_list():
    sA_st = define_state(value="state_time * 10")
    sA_no_st = define_state(value="100")
    sB_no_st = define_state(value="0.8")
    sC_st = define_state(value="state_month + 5")
    sD_no_st = define_state(value="1")

    t_A_B_st = define_state_transition(from_state="A", to_state="B", prob="0.1 * state_day")
    t_BC_no_st = define_state_transition(from_state="B", to_state="C", prob="0.5")
    t_ANY_A_st = define_state_transition(to_state="A", prob="state_time * 0.01")
    t_A_ANY_no_st = define_state_transition(from_state="A", prob="0.7")

    sl1 = define_state_list(A=sA_st, B=sB_no_st)
    res1 = has_state_time_py_for_uneval_state_list(sl1)
    assert res1 == {"A": True, "B": False}

    sl2 = define_state_list(t_A_B_st, A=sA_no_st, B=sB_no_st)
    res2 = has_state_time_py_for_uneval_state_list(sl2)
    assert res2 == {"A": True, "B": False}

    sl3 = define_state_list(t_ANY_A_st, A=sA_no_st, B=sB_no_st)
    res3 = has_state_time_py_for_uneval_state_list(sl3)
    assert res3 == {"A": True, "B": True}

    sl4 = define_state_list(t_A_B_st, t_BC_no_st, A=sA_st, B=sB_no_st, C=sC_st)
    res4 = has_state_time_py_for_uneval_state_list(sl4)
    assert res4 == {"A": True, "B": False, "C": True}

    sl5 = define_state_list(t_A_ANY_no_st, A=sA_st, B=sB_no_st)
    res5 = has_state_time_py_for_uneval_state_list(sl5)
    assert res5 == {"A": True, "B": False}

    sl6 = define_state_list(t_BC_no_st, A=sA_no_st, B=sB_no_st, C=sD_no_st)
    res6 = has_state_time_py_for_uneval_state_list(sl6)
    assert res6 == {"A": False, "B": False, "C": False}

    sl_empty = define_state_list()
    res_empty = has_state_time_py_for_uneval_state_list(sl_empty)
    assert res_empty == {}

def test_get_vars_from_expr():
    assert get_vars_from_expr("a + b * 2 - c") == {"a", "b", "c"}
    assert get_vars_from_expr("my_func(var1, var2)") == {"my_func", "var1", "var2"}
    assert get_vars_from_expr("100 + 20.5") == set()
    assert get_vars_from_expr("") == set()
    assert get_vars_from_expr("state_time") == {"state_time"}
    assert get_vars_from_expr("cost + (param_x / param_y)") == {"cost", "param_x", "param_y"}
    assert get_vars_from_expr("a + b)") == set()

def test_trace_st_dependency_py():
    exprs1 = OrderedDict(a="state_time * 2", b="cost_a")
    res1 = trace_st_dependency_py(exprs1)
    assert res1 == {"a": True, "b": False}
    exprs2 = OrderedDict(a="state_time * 2", b="a + 5")
    res2 = trace_st_dependency_py(exprs2)
    assert res2 == {"a": True, "b": True}
    exprs3 = OrderedDict(a="cost_x", b="a + 5")
    res3 = trace_st_dependency_py(exprs3)
    assert res3 == {"a": False, "b": False}
    exprs4 = OrderedDict(a="cost_x * factor", b="a + 5")
    extras4 = {"cost_x": True, "factor": False}
    res4 = trace_st_dependency_py(exprs4, extras_st_dependency=extras4)
    assert res4 == {"a": True, "b": True}
    exprs5 = OrderedDict(a="cost_y * factor")
    extras5 = {"cost_y": False, "factor": False}
    res5 = trace_st_dependency_py(exprs5, extras_st_dependency=extras5)
    assert res5 == {"a": False}
    exprs6 = OrderedDict(a="state_time", b="a", c="b", d="c", e="d + 10")
    res6 = trace_st_dependency_py(exprs6)
    assert res6 == {"a": True, "b": True, "c": True, "d": True, "e": True}
    exprs7 = OrderedDict(e="d + 10", d="c", c="b", b="a", a="state_time")
    res7 = trace_st_dependency_py(exprs7)
    assert res7 == {"e": True, "d": True, "c": True, "b": True, "a": True}
    exprs8 = OrderedDict(
        st_val = "state_time", dep_on_st = "st_val + 1",
        indep_val = "100", dep_on_indep = "indep_val * 2"
    )
    res8 = trace_st_dependency_py(exprs8)
    assert res8 == {"st_val": True, "dep_on_st": True, "indep_val": False, "dep_on_indep": False}
    exprs9 = OrderedDict(x="param_a + extra_st_var", param_a="10")
    extras9 = {"extra_st_var": True}
    res9 = trace_st_dependency_py(exprs9, extras_st_dependency=extras9)
    assert res9 == {"x": True, "param_a": False}

    exprs_circ1 = OrderedDict(a="b", b="a")
    res_circ1 = trace_st_dependency_py(exprs_circ1)
    assert res_circ1 == {"a": False, "b": False}

    exprs_circ2 = OrderedDict(a="b", b="a + state_time")
    res_circ2 = trace_st_dependency_py(exprs_circ2)
    assert res_circ2 == {"a": True, "b": True}

def test_get_states_to_expand_py():
    params_no_st = define_parameters(p1="10", p2="p1 * 2")
    params_with_st = define_parameters(p1="state_time + 5", p2="p1 * 2")
    sA_no_st = define_state(val="p1")
    sB_no_st = define_state(val="p2")
    sA_st_val = define_state(val="p1 + state_time")
    sC_st_val = define_state(val="p_st_extra + state_time")

    t_val_st = define_state_transition(from_state="A", to_state="B", cost_tx="p1 + state_day")
    t_val_any_st = define_state_transition(to_state="A", cost_tx="state_week")
    tm_expr_no_st = OrderedDict(A_to_B="p1 / 100", B_to_A="p2 / 100")
    tm_expr_A_st = OrderedDict(A_to_B="p1 * state_time / 100", B_to_A="p2 / 100")

    sl1 = define_state_list(A=sA_no_st, B=sB_no_st)
    res1 = get_states_to_expand_py(params_no_st, sl1, tm_expr_no_st)
    assert res1 == {"A": False, "B": False}

    res2 = get_states_to_expand_py(params_with_st, sl1, tm_expr_no_st)
    assert res2 == {"A": True, "B": True}

    sl3 = define_state_list(A=sA_st_val, B=sB_no_st)
    res3 = get_states_to_expand_py(params_no_st, sl3, tm_expr_no_st)
    assert res3 == {"A": True, "B": False}

    sl4 = define_state_list(t_val_st, A=sA_no_st, B=sB_no_st)
    res4 = get_states_to_expand_py(params_no_st, sl4, tm_expr_no_st)
    assert res4 == {"A": True, "B": False}

    sl5 = define_state_list(t_val_any_st, A=sA_no_st, B=sB_no_st)
    res5 = get_states_to_expand_py(params_no_st, sl5, tm_expr_no_st)
    assert res5 == {"A": True, "B": True}

    sl6 = define_state_list(A=sA_no_st, B=sB_no_st)
    res6 = get_states_to_expand_py(params_no_st, sl6, tm_expr_A_st)
    assert res6 == {"A": True, "B": False}

    params_complex = define_parameters(p_st_extra="state_hour", p_normal="10")
    sl_complex = define_state_list(t_val_st, A=sA_no_st, B=sB_no_st, C=sC_st_val)
    tm_expr_B_st = OrderedDict(A_to_B="p_normal/100", B_to_C="p_st_extra * 0.1")
    res_complex = get_states_to_expand_py(params_complex, sl_complex, tm_expr_B_st)
    assert res_complex == {"A": True, "B": True, "C": True}

    sl8 = define_state_list(A=sA_st, B=sB_no_st)
    res8 = get_states_to_expand_py(params_no_st, sl8, transition_matrix_expressions=None)
    assert res8 == {"A": True, "B": False}

def test_complete_stl_py():
    state_names = ["A", "B", "C"]
    strategy_names = ["strat1", "strat2"]
    model_cycles = 10

    res_default = complete_stl_py(None, state_names, strategy_names, model_cycles)
    for strat in strategy_names:
        for state in state_names: assert res_default[strat][state] == model_cycles
    res_int = complete_stl_py(5, state_names, strategy_names, model_cycles)
    for strat in strategy_names:
        for state in state_names: assert res_int[strat][state] == 5
    with pytest.warns(UserWarning, match="Global 'state_time_limit' .* exceeds model_cycles"):
        res_int_capped = complete_stl_py(15, state_names, strategy_names, model_cycles)
    for strat in strategy_names:
        for state in state_names: assert res_int_capped[strat][state] == model_cycles

    scl_dict_state = {"A": 3, "C": 7}
    res_dict_state = complete_stl_py(scl_dict_state, state_names, strategy_names, model_cycles)
    for strat in strategy_names:
        assert res_dict_state[strat]["A"] == 3
        assert res_dict_state[strat]["B"] == model_cycles
        assert res_dict_state[strat]["C"] == 7

    scl_dict_state_cap = {"A": 12, "B": 3}
    with pytest.warns(UserWarning, match="Limit for state 'A' .* exceeds model_cycles"):
         res_dict_state_c = complete_stl_py(scl_dict_state_cap, state_names, strategy_names, model_cycles)
    for strat in strategy_names:
        assert res_dict_state_c[strat]["A"] == model_cycles
        assert res_dict_state_c[strat]["B"] == 3
        assert res_dict_state_c[strat]["C"] == model_cycles

    scl_dict_strat = {"strat1": {"A": 2, "B": 4}, "strat2": {"B": 6, "C": 8}}
    res_dict_strat = complete_stl_py(scl_dict_strat, state_names, strategy_names, model_cycles)
    assert res_dict_strat["strat1"]["A"] == 2; assert res_dict_strat["strat1"]["B"] == 4
    assert res_dict_strat["strat1"]["C"] == model_cycles
    assert res_dict_strat["strat2"]["A"] == model_cycles
    assert res_dict_strat["strat2"]["B"] == 6; assert res_dict_strat["strat2"]["C"] == 8

    state_groups = pd.DataFrame({"name": ["A", "B", "C"], "group": ["grp1", "grp1", "grp2"], "share": [True, True, False]})
    scl_for_groups = {"A": 7}
    res_groups = complete_stl_py(scl_for_groups, state_names, strategy_names, model_cycles, state_groups_df=state_groups)
    for strat in strategy_names:
        assert res_groups[strat]["A"] == 10; assert res_groups[strat]["B"] == 10
        assert res_groups[strat]["C"] == model_cycles

    scl_for_groups_B_high = {"B": 12}
    with pytest.warns(UserWarning, match="Limit for state 'B' .* exceeds model_cycles"):
        res_groups_B_high = complete_stl_py(scl_for_groups_B_high, state_names, strategy_names, model_cycles, state_groups_df=state_groups)
    for strat in strategy_names:
        assert res_groups_B_high[strat]["A"] == 10; assert res_groups_B_high[strat]["B"] == 10

    with pytest.raises(ValueError, match="State 'strat1' in 'state_time_limit' is not a defined model state."):
        complete_stl_py({"strat1": 5}, state_names, strategy_names, model_cycles)

    with pytest.raises(TypeError, match="State names in 'state_time_limit' must be strings"):
        complete_stl_py({123: 5}, state_names, strategy_names, model_cycles)
    with pytest.raises(ValueError, match="State 'X' in 'state_time_limit' is not a defined model state"):
        complete_stl_py({"X": 5}, state_names, strategy_names, model_cycles)
    with pytest.raises(ValueError, match="Limit for state 'A' .* must be positive"):
        complete_stl_py({"A": 0}, state_names, strategy_names, model_cycles)
    with pytest.raises(ValueError, match="Strategy 'stratX' in 'state_time_limit' is not a defined model strategy"):
        complete_stl_py({"stratX": {"A":5}}, state_names, strategy_names, model_cycles)
    with pytest.raises(ValueError, match="state_groups_df must contain columns 'name', 'group', 'share'"):
        complete_stl_py(None, state_names, strategy_names, model_cycles, pd.DataFrame({"foo":["bar"]}))

class TestConstructExpandDfPy:
    model_time_df_simple = pd.DataFrame({'model_time': [0, 1], 'markov_cycle': [0, 1]})
    states_all = ["X", "Y"]
    strats_all = ["std", "new"]

    def test_construct_expand_df_py_no_expansion(self):
        states_to_expand = {"X": False, "Y": False}
        state_time_limits = {
            "std": {"X": 0, "Y": 0},
            "new": {"X": 0, "Y": 0}
        }

        df = construct_expand_df_py(
            states_to_expand, state_time_limits, self.model_time_df_simple,
            self.strats_all, self.states_all
        )
        # Expected rows: model_times * strategies * states * 1 (for state_time=0)
        # 2 * 2 * 2 * 1 = 8 rows
        assert len(df) == 8
        assert (df['state_time'] == 0.0).all()
        assert set(df['model_time']) == {0, 1}
        assert set(df['strategy']) == {"std", "new"}
        assert set(df['state_name']) == {"X", "Y"}
        assert set(df['group']) == {"_default_group_"}
        # Check IDs by comparing underlying numpy arrays
        np.testing.assert_array_equal(df['strategy_id'].to_numpy(), pd.factorize(df['strategy'])[0])
        np.testing.assert_array_equal(df['group_id'].to_numpy(), pd.factorize(df['group'])[0])
        np.testing.assert_array_equal(df['state_id'].to_numpy(), pd.factorize(df['state_name'])[0])


    def test_construct_expand_df_py_all_expand(self):
        states_to_expand = {"X": True, "Y": True}
        state_time_limits = { # Max state_time (inclusive for 0 to N)
            "std": {"X": 1, "Y": 2}, # X expands to ST=0,1 (2 rows); Y to ST=0,1,2 (3 rows)
            "new": {"X": 0, "Y": 1}  # X expands to ST=0 (1 row);    Y to ST=0,1 (2 rows)
        }
        df = construct_expand_df_py(
            states_to_expand, state_time_limits, self.model_time_df_simple,
            self.strats_all, self.states_all
        )

        # Calculations for expected rows:
        # For each model_time (2 of them):
        #  Strategy "std":
        #    State "X": expands to ST 0, 1 (2 distinct state_time values)
        #    State "Y": expands to ST 0, 1, 2 (3 distinct state_time values)
        #    Total distinct (strat, state, state_time) combinations for "std" = 2 + 3 = 5
        #  Strategy "new":
        #    State "X": expands to ST 0 (1 distinct state_time value)
        #    State "Y": expands to ST 0, 1 (2 distinct state_time values)
        #    Total distinct (strat, state, state_time) combinations for "new" = 1 + 2 = 3
        # Total rows = (5 (std) + 3 (new)) * 2 (model_times) * 1 (group) = 8 * 2 = 16 rows
        assert len(df) == 16

        # Check specific expansions
        df_std_X = df[(df['strategy'] == 'std') & (df['state_name'] == 'X')]
        assert len(df_std_X) == 2 * 2 # 2 model_times, 2 state_times (0,1)
        assert set(df_std_X['state_time']) == {0.0, 1.0}

        df_std_Y = df[(df['strategy'] == 'std') & (df['state_name'] == 'Y')]
        assert len(df_std_Y) == 2 * 3 # 2 model_times, 3 state_times (0,1,2)
        assert set(df_std_Y['state_time']) == {0.0, 1.0, 2.0}

        df_new_X = df[(df['strategy'] == 'new') & (df['state_name'] == 'X')]
        assert len(df_new_X) == 2 * 1 # 2 model_times, 1 state_time (0)
        assert set(df_new_X['state_time']) == {0.0}

        df_new_Y = df[(df['strategy'] == 'new') & (df['state_name'] == 'Y')]
        assert len(df_new_Y) == 2 * 2 # 2 model_times, 2 state_times (0,1)
        assert set(df_new_Y['state_time']) == {0.0, 1.0}

    def test_construct_expand_df_py_mixed_expansion(self):
        states_to_expand = {"X": True, "Y": False} # Only X expands
        state_time_limits = {
            "std": {"X": 1, "Y": 5}, # Y limit is ignored as it's not expanding
            "new": {"X": 2, "Y": 5}  # Y limit is ignored
        }
        df = construct_expand_df_py(
            states_to_expand, state_time_limits, self.model_time_df_simple,
            self.strats_all, self.states_all
        )
        # Calculations for expected rows:
        # For each model_time (2 of them):
        #  Strategy "std":
        #    State "X": expands to ST 0, 1 (2 state_times)
        #    State "Y": not expanding (ST 0 only) (1 state_time)
        #    Total for "std" per model_time = 2 + 1 = 3
        #  Strategy "new":
        #    State "X": expands to ST 0, 1, 2 (3 state_times)
        #    State "Y": not expanding (ST 0 only) (1 state_time)
        #    Total for "new" per model_time = 3 + 1 = 4
        # Total rows = (3 (std) + 4 (new)) * 2 (model_times) = 7 * 2 = 14 rows
        assert len(df) == 14
        assert (df[df['state_name'] == 'Y']['state_time'] == 0.0).all()
        df_std_X = df[(df['strategy'] == 'std') & (df['state_name'] == 'X')]
        assert set(df_std_X['state_time']) == {0.0, 1.0}
        df_new_X = df[(df['strategy'] == 'new') & (df['state_name'] == 'X')]
        assert set(df_new_X['state_time']) == {0.0, 1.0, 2.0}

    def test_construct_expand_df_py_with_groups(self):
        states_to_expand = {"X": True, "Y": False}
        state_time_limits = {
            "std": {"X": 1, "Y": 0},
            "new": {"X": 0, "Y": 0}
        }
        groups = ["g1", "g2"]
        df = construct_expand_df_py(
            states_to_expand, state_time_limits, self.model_time_df_simple,
            self.strats_all, self.states_all, group_names=groups
        )
        # Expected rows:
        # For each model_time (2):
        #  For each group (2):
        #   Strat "std":
        #     State "X": expands ST 0,1 (2 state_times)
        #     State "Y": no expand (1 state_time)
        #     Total for "std" per group, per model_time = 2 + 1 = 3
        #   Strat "new":
        #     State "X": no expand (limit 0) (1 state_time)
        #     State "Y": no expand (1 state_time)
        #     Total for "new" per group, per model_time = 1 + 1 = 2
        # Total = ( (3_std + 2_new) * 2_groups ) * 2_model_times = (5 * 2) * 2 = 20 rows
        assert len(df) == 20
        assert set(df['group']) == {"g1", "g2"}
        assert set(df[(df['strategy']=='std') & (df['state_name']=='X')]['state_time']) == {0.0, 1.0}
        assert (df[(df['strategy']=='new') & (df['state_name']=='X')]['state_time'] == 0.0).all()
        # Check group_id factors correctly
        expected_group_id_g1 = df[df['group'] == 'g1']['group_id'].iloc[0]
        expected_group_id_g2 = df[df['group'] == 'g2']['group_id'].iloc[0]
        assert expected_group_id_g1 != expected_group_id_g2
        assert (df[df['group'] == 'g1']['group_id'] == expected_group_id_g1).all()
        assert (df[df['group'] == 'g2']['group_id'] == expected_group_id_g2).all()


    def test_construct_expand_df_py_edge_cases(self):
        empty_mt_df = pd.DataFrame({'model_time': [], 'markov_cycle': []})
        df = construct_expand_df_py({}, {}, empty_mt_df, self.strats_all, self.states_all)
        assert len(df) == 0

        with pytest.warns(UserWarning, match="strategy_names is empty"):
            df = construct_expand_df_py({}, {}, self.model_time_df_simple, [], self.states_all)
            assert len(df) == 0

        with pytest.warns(UserWarning, match="state_names is empty"):
            df = construct_expand_df_py({}, {}, self.model_time_df_simple, self.strats_all, [])
            assert len(df) == 0

        states_to_expand = {"X": True, "Y": True}
        state_time_limits = { "std": {"X": 0, "Y": 0}, "new": {"X": 0, "Y": 0} }
        df = construct_expand_df_py(
            states_to_expand, state_time_limits, self.model_time_df_simple,
            self.strats_all, self.states_all
        )
        assert len(df) == 8 # 2 mt * 2 strat * 2 state * 1 st_val (0, because limit is 0)
        assert (df['state_time'] == 0.0).all()

        # Test that if a state is in states_to_expand but not in state_time_limits, its limit is 0
        states_to_expand_X_only = {"X": True, "Y": False}
        stl_Y_missing_X = {"std": {"Y":1}, "new": {"Y":1}} # X's limit will default to 0
        df_X_limit_zero = construct_expand_df_py(
            states_to_expand_X_only, stl_Y_missing_X, self.model_time_df_simple,
            self.strats_all, self.states_all
        )
        # X should expand but with max_st = 0 (so only state_time=0 for X)
        # Y should not expand (only state_time=0 for Y)
        # Total 2mt * 2strat * 2state * 1st_val = 8 rows
        assert len(df_X_limit_zero) == 8
        assert (df_X_limit_zero['state_time'] == 0.0).all()

        # Test that if a state is in state_time_limits for a strategy, but that strategy isn't in states_to_expand_dict for that state
        # This scenario is covered by the logic: if states_to_expand[state] is False, it won't expand regardless of limits.
        # If states_to_expand[state] is True, but limit for *that strat* is missing, it defaults to 0 for *that strat*.
        states_to_expand_XY = {"X": True, "Y": True}
        stl_X_std_missing_Y_new_missing = {
            "std": {"Y": 2}, # X limit for std is missing, defaults to 0
            "new": {"X": 1}  # Y limit for new is missing, defaults to 0
        }
        df_mixed_missing_limits = construct_expand_df_py(
            states_to_expand_XY, stl_X_std_missing_Y_new_missing, self.model_time_df_simple,
            self.strats_all, self.states_all
        )
        # Expected for each model_time:
        # Strat std:
        #   X: expand, limit 0 -> st=0 (1 row)
        #   Y: expand, limit 2 -> st=0,1,2 (3 rows)
        #   Total std = 1+3=4
        # Strat new:
        #   X: expand, limit 1 -> st=0,1 (2 rows)
        #   Y: expand, limit 0 -> st=0 (1 row)
        #   Total new = 2+1=3
        # Grand total = (4+3) * 2mt = 14 rows
        assert len(df_mixed_missing_limits) == 14
        assert set(df_mixed_missing_limits[(df_mixed_missing_limits.strategy=='std') & (df_mixed_missing_limits.state_name=='X')].state_time) == {0.0}
        assert set(df_mixed_missing_limits[(df_mixed_missing_limits.strategy=='std') & (df_mixed_missing_limits.state_name=='Y')].state_time) == {0.0,1.0,2.0}
        assert set(df_mixed_missing_limits[(df_mixed_missing_limits.strategy=='new') & (df_mixed_missing_limits.state_name=='X')].state_time) == {0.0,1.0}
        assert set(df_mixed_missing_limits[(df_mixed_missing_limits.strategy=='new') & (df_mixed_missing_limits.state_name=='Y')].state_time) == {0.0}
