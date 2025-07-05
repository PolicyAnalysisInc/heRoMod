import pytest
from collections import OrderedDict
from pyheRomod.state_definition import State, define_state, UnevalStateList, define_state_list, StateTransition, define_state_transition
from pyheRomod.param_definition import UnevalCollection

def test_define_state():
    state1 = define_state(cost="100", utility="0.85", extra_val="cost * 0.1")
    assert isinstance(state1, State)
    assert isinstance(state1, UnevalCollection)
    assert list(state1.expressions.keys()) == ["cost", "utility", "extra_val"]
    assert state1.expressions["cost"] == "100"
    assert state1.expressions["utility"] == "0.85"
    assert state1.expressions["extra_val"] == "cost * 0.1"
    assert state1.get_value_names() == ["cost", "utility", "extra_val"]
    state2 = define_state(val=10)
    assert state2.expressions["val"] == "10"

def test_define_state_list_basic():
    s1 = define_state(cost="100", utility="1.0")
    s2 = define_state(cost="200", utility="0.5")
    state_list = define_state_list(healthy=s1, sick=s2)
    assert isinstance(state_list, UnevalStateList)
    assert state_list.get_state_count() == 2
    assert state_list.get_state_names() == ["healthy", "sick"]
    assert state_list.states["healthy"] is s1
    assert state_list.states["sick"] is s2
    assert state_list.get_state_value_names() == ["cost", "utility"]

def test_define_state_list_errors():
    s1 = define_state(cost="100", utility="1.0")
    s_diff_names = define_state(cost="200", benefit="0.5")
    s_diff_len = define_state(cost="300")
    with pytest.raises(ValueError, match="State value names differ between states"):
        define_state_list(s1=s1, s_diff=s_diff_names)
    with pytest.raises(ValueError, match="State value names differ between states"):
        define_state_list(s1=s1, s_len=s_diff_len)
    with pytest.raises(TypeError, match="Keyword argument 's2_bad' is not a State object. Got <class 'str'>."):
        define_state_list(s1=s1, s2_bad="not a state")
    with pytest.raises(TypeError, match=r"Obj for state 's2' not State."):
         UnevalStateList(states=OrderedDict([("s1",s1), ("s2", "not a state")]))

def test_uneval_state_list_helpers():
    s1 = define_state(cost="10", util="1")
    s2 = define_state(cost="20", util="0.5")
    state_list_obj = UnevalStateList(OrderedDict([("healthy",s1), ("sick",s2)]))
    assert state_list_obj.get_state_names() == ["healthy", "sick"]
    assert state_list_obj.get_state_count() == 2
    assert state_list_obj.get_state_value_names() == ["cost", "util"]
    empty_list = UnevalStateList(OrderedDict())
    assert empty_list.get_state_names() == []
    assert empty_list.get_state_count() == 0
    assert empty_list.get_state_value_names() == []

def test_state_modify():
    s1 = define_state(cost="100", utility="0.8")
    s1_modified = s1.modify(cost="150", utility="0.75")
    assert isinstance(s1_modified, State)
    assert s1_modified is not s1
    assert s1_modified.expressions["cost"] == "150"
    assert s1_modified.expressions["utility"] == "0.75"
    assert s1.expressions["cost"] == "100"
    assert s1.expressions["utility"] == "0.8"
    s1_mod_cost = s1.modify(cost="200")
    assert s1_mod_cost.expressions["cost"] == "200"
    assert s1_mod_cost.expressions["utility"] == "0.8"
    with pytest.raises(ValueError, match="Cannot add new value 'new_val'. Only existing values can be modified."):
        s1.modify(new_val="10")
    with pytest.warns(UserWarning, match="Expression for 'cost' in modify was not a string/literal"):
        s1_mod_non_str = s1.modify(cost={"complex": "value"})
    assert s1_mod_non_str.expressions["cost"] == "{'complex': 'value'}"

def test_define_state_transition():
    t1 = define_state_transition(from_state="A", to_state="B", prob="0.5", cost_tx="10")
    assert isinstance(t1, StateTransition)
    assert t1.from_state == "A"
    assert t1.to_state == "B"
    assert list(t1.expressions.keys()) == ["prob", "cost_tx"]
    assert t1.expressions["prob"] == "0.5"
    t_any_from = define_state_transition(to_state="D", prob="0.1")
    assert t_any_from.from_state is None
    assert t_any_from.to_state == "D"
    t_any_to = define_state_transition(from_state="C", prob="0.2")
    assert t_any_to.from_state == "C"
    assert t_any_to.to_state is None
    with pytest.raises(ValueError, match="cannot be the same if both specified"):
        define_state_transition(from_state="A", to_state="A", prob="0.1")
    with pytest.raises(TypeError, match="'from_state' must be a string or None"):
        define_state_transition(from_state=123, to_state="A", prob="0.1")
    with pytest.raises(TypeError, match="'to_state' must be a string or None"):
        define_state_transition(from_state="A", to_state=123, prob="0.1")

def test_define_state_list_with_transitions():
    sA = define_state(cost="10")
    sB = define_state(cost="20")
    sC = define_state(cost="0")
    t_A_B = define_state_transition(from_state="A", to_state="B", prob="0.3")
    t_A_C = define_state_transition(from_state="A", to_state="C", prob="0.1")
    t_B_C = define_state_transition(from_state="B", to_state="C", prob="0.5")
    state_list_1 = define_state_list(t_A_B, A=sA, B=sB, C=sC)
    assert len(state_list_1.states) == 3
    assert len(state_list_1.transitions) == 1
    assert state_list_1.transitions[0] is t_A_B
    state_list_2 = define_state_list(t_A_B, t_A_C, t_B_C, A=sA, B=sB, C=sC)
    assert len(state_list_2.transitions) == 3
    assert t_A_C in state_list_2.transitions
    t_A_D_bad = define_state_transition(from_state="A", to_state="D_undefined", prob="0.1")
    with pytest.raises(ValueError, match="Invalid 'to_state' in transition 0: 'D_undefined'"):
        define_state_list(t_A_D_bad, A=sA)
    t_D_A_bad = define_state_transition(from_state="D_undefined", to_state="A", prob="0.1")
    with pytest.raises(ValueError, match="Invalid 'from_state' in transition 0: 'D_undefined'"):
        define_state_list(t_D_A_bad, A=sA)
    with pytest.raises(TypeError, match="Positional argument at index 0 is not a StateTransition object"):
        define_state_list("not_a_transition", A=sA)
    with pytest.raises(TypeError, match="State object found at positional argument 0"):
        define_state_list(sA, B=sB)

def test_uneval_state_list_modify():
    sA_v1 = define_state(cost="10", utility="1.0")
    sB_v1 = define_state(cost="20", utility="0.5")
    t_A_B_v1 = define_state_transition(from_state="A", to_state="B", prob="0.3")

    state_list_v1 = define_state_list(t_A_B_v1, A=sA_v1, B=sB_v1)
    assert state_list_v1.get_state_count() == 2
    assert len(state_list_v1.transitions) == 1

    sA_v2 = define_state(cost="15", utility="0.9")
    state_list_v2 = state_list_v1.modify(A=sA_v2)
    assert state_list_v2 is not state_list_v1
    assert state_list_v2.states["A"] is sA_v2
    assert state_list_v2.states["B"] is sB_v1
    assert len(state_list_v2.transitions) == 1

    sC_v1 = define_state(cost="0", utility="0.0")
    state_list_v3 = state_list_v2.modify(C=sC_v1)
    assert state_list_v3.get_state_count() == 3
    assert "C" in state_list_v3.states
    assert state_list_v3.states["C"] is sC_v1

    t_B_C_v1 = define_state_transition(from_state="B", to_state="C", prob="0.6")
    state_list_v4 = state_list_v3.modify(t_B_C_v1)
    assert len(state_list_v4.transitions) == 2
    assert t_B_C_v1 in state_list_v4.transitions

    sB_v2 = define_state(cost="25", utility="0.4")
    sD_v1 = define_state(cost="5", utility="0.1")
    t_C_D_v1 = define_state_transition(from_state="C", to_state="D", prob="0.7")

    state_list_v4_plus_D = state_list_v4.modify(D=sD_v1)
    state_list_v5 = state_list_v4_plus_D.modify(t_C_D_v1, B=sB_v2)

    assert state_list_v5.states["B"] is sB_v2
    assert "D" in state_list_v5.states
    assert len(state_list_v5.transitions) == 3
    assert t_C_D_v1 in state_list_v5.transitions

    with pytest.raises(TypeError, match="Object for state 'A' in modify is not a State instance"):
        state_list_v1.modify(A="not a state")

    with pytest.raises(TypeError, match="Positional argument 0 in modify is not a StateTransition instance"):
        state_list_v1.modify("not a transition")

    t_A_X_bad = define_state_transition(from_state="A", to_state="X_undefined", prob="0.1")
    with pytest.raises(ValueError, match="Invalid 'to_state' in transition 1: 'X_undefined'"):
        state_list_v1.modify(t_A_X_bad)

    sA_v3_bad_names = define_state(cost_new="12", utility_new="0.88")
    with pytest.raises(ValueError, match="State value names differ between states"):
        state_list_v1.modify(A=sA_v3_bad_names)

def test_state_transition_modify():
    t1 = define_state_transition(from_state="A", to_state="B", prob="0.5", cost_tx="10")

    t1_modified = t1.modify(prob="0.6", cost_tx="12")
    assert isinstance(t1_modified, StateTransition)
    assert t1_modified is not t1
    assert t1_modified.from_state == "A"
    assert t1_modified.to_state == "B"
    assert t1_modified.expressions["prob"] == "0.6"
    assert t1_modified.expressions["cost_tx"] == "12"

    assert t1.expressions["prob"] == "0.5"
    assert t1.expressions["cost_tx"] == "10"

    t1_mod_prob = t1.modify(prob="0.7")
    assert t1_mod_prob.expressions["prob"] == "0.7"
    assert t1_mod_prob.expressions["cost_tx"] == "10"

    with pytest.raises(ValueError, match="Cannot add new value 'new_val'.*Only existing values can be modified."):
        t1.modify(new_val="100")

    t_any = define_state_transition(prob="p_generic")
    t_any_mod = t_any.modify(prob="p_specific")
    assert t_any_mod.from_state is None
    assert t_any_mod.to_state is None
    assert t_any_mod.expressions["prob"] == "p_specific"
