import pytest
from collections import OrderedDict
from pyheRomod.param_definition import (
    define_parameters, modify, UnevalParameters, UnevalCollection
)

def get_custom_print_output(obj):
    if isinstance(obj, UnevalParameters):
        if not obj.expressions: return "0 unevaluated parameters."
        count = len(obj.expressions)
        param_type = "parameter" if count == 1 else "parameters"
        header = f"{count} unevaluated {param_type}.\n\n"
        lines = [f"{name} = {expr}" for name, expr in obj.expressions.items()]
        return header + "\n".join(lines)
    return repr(obj)

def test_parameter_definition_and_update():
    par1 = define_parameters(a=1234, b="a + 543")
    assert isinstance(par1, UnevalParameters)
    assert list(par1.expressions.keys()) == ["a", "b"]
    assert par1.expressions["a"] == "1234"; assert par1.expressions["b"] == "a + 543"
    assert get_custom_print_output(par1) == "2 unevaluated parameters.\n\na = 1234\nb = a + 543"
    with pytest.raises(ValueError, match="Parameter name 'markov_cycle' is a reserved keyword."):
        define_parameters(markov_cycle="432")
    mod_par1 = modify(par1, a="4321")
    assert mod_par1.expressions["a"] == "4321"; assert mod_par1.expressions["b"] == "a + 543"
    mod_par2 = modify(par1, a="4321", c="333")
    assert list(mod_par2.expressions.keys()) == ["a", "b", "c"]
    assert mod_par2.expressions["a"] == "4321"; assert mod_par2.expressions["c"] == "333"
    with pytest.raises(ValueError, match="Parameter name 'markov_cycle' is a reserved keyword."):
        modify(par1, markov_cycle="474")
    with pytest.raises(ValueError, match="Parameter name 'markov_cycle' is a reserved keyword."):
        modify(par1, b="a-1", markov_cycle="474")
    assert par1.get_parameter_names() == ["a", "b"]
    empty_params = define_parameters()
    assert get_custom_print_output(empty_params) == "0 unevaluated parameters."
    par1_modified_empty = modify(par1)
    assert par1_modified_empty.expressions == par1.expressions
    assert par1_modified_empty is not par1

def test_reserved_names_in_definition():
    with pytest.raises(ValueError, match="Parameter names cannot be empty"): define_parameters(a="1", **{"": "empty_val"})
    with pytest.raises(ValueError, match="Parameter name 'markov_cycle' is a reserved keyword."): define_parameters(a="1", markov_cycle="432")
    with pytest.raises(ValueError, match="Parameter name 'C' is a reserved keyword."): define_parameters(a="1", C="some_C_val")
    with pytest.warns(UserWarning, match="Parameter name '.b' starts with a typically reserved prefix"): define_parameters(a="1", **{".b": "val"})
    with pytest.warns(UserWarning, match="Parameter name '_b' starts with a typically reserved prefix"): define_parameters(a="1", _b="val")

def test_direct_value_conversion_and_type_in_expressions():
    p = define_parameters(num_int=10, num_float=20.5, is_true=True, expr_str="num_int * 2")
    assert p.expressions["num_int"] == "10"; assert p.expressions["num_float"] == "20.5"
    assert p.expressions["is_true"] == "True"; assert p.expressions["expr_str"] == "num_int * 2"
    p_mod = modify(p, num_int=100, new_val=False)
    assert p_mod.expressions["num_int"] == "100"; assert p_mod.expressions["new_val"] == "False"
    assert p_mod.expressions["num_float"] == "20.5"; assert p_mod.expressions["is_true"] == "True"

def test_modify_kwargs_processing():
    p_initial = define_parameters(p1 = "10")
    p_modified = modify(p_initial, p2 = 20, p1 = True)
    assert p_modified.expressions["p1"] == "True"; assert p_modified.expressions["p2"] == "20"
    with pytest.raises(ValueError, match="Parameter name 'C' is a reserved keyword."): modify(p_initial, C="bad")
    with pytest.warns(UserWarning, match="Parameter name '_new' starts with a typically reserved prefix"): modify(p_initial, _new="a_warning")
