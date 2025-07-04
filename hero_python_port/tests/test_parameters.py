# hero_python_port/tests/test_parameters.py

import pytest
from hero_py.parameters import Parameters

def test_parameters_creation_empty():
    """Test creating a Parameters object with no initial params."""
    params = Parameters()
    assert params.names == []
    assert repr(params) == "Parameters({})"

def test_parameters_creation_with_values():
    """Test creating Parameters with initial fixed values."""
    params = Parameters(cost_A=100, prob_B=0.75)
    assert "cost_A" in params.names
    assert "prob_B" in params.names
    assert len(params.names) == 2

    # Test direct access to definition (not evaluated value for callables)
    assert params["cost_A"] == 100
    assert params.get_definition("cost_A") == 100
    assert params["prob_B"] == 0.75
    assert params.get_definition("prob_B") == 0.75

def test_add_param_fixed():
    """Test adding a fixed parameter."""
    params = Parameters()
    params.add_param("p1", 10)
    assert params["p1"] == 10
    assert params.names == ["p1"]

    params.add_param("p2", 0.5)
    assert params["p2"] == 0.5
    assert params.names == ["p1", "p2"]

    # Test updating a parameter
    params.add_param("p1", 20)
    assert params["p1"] == 20
    assert params.names == ["p1", "p2"] # Order should be preserved from first add

def test_get_param_value_fixed():
    """Test get_param_value for fixed values."""
    params = Parameters(val_1=123, val_2=45.6)
    assert params.get_param_value("val_1") == 123
    assert params.get_param_value("val_2", cycle=5) == 45.6 # Cycle shouldn't affect fixed

def test_get_param_value_callable_simple_cycle():
    """Test get_param_value for a callable dependent on cycle."""
    params = Parameters(rate=lambda cycle: 0.1 * cycle)
    assert params.get_param_value("rate", cycle=0) == 0.0
    assert params.get_param_value("rate", cycle=5) == 0.5
    assert params.get_param_value("rate", cycle=10) == 1.0

def test_get_param_value_callable_with_params_context():
    """Test get_param_value for a callable dependent on other params (via context)."""
    # This test highlights the need for a robust evaluation order or context passing
    # The current Parameters.get_param_value is simplistic for inter-dependencies
    params = Parameters(
        base_cost=100,
        multiplier=2,
        total_cost=lambda p_ctx, cycle: p_ctx['base_cost'] * p_ctx['multiplier'] + cycle
    )
    # Direct call to get_param_value for 'total_cost' requires current_params context
    # This would typically be handled by evaluate_all or a similar orchestrator

    # Test with evaluate_all which handles the context
    evaluated = params.evaluate_all(cycle=5)
    assert evaluated["base_cost"] == 100
    assert evaluated["multiplier"] == 2
    assert evaluated["total_cost"] == (100 * 2) + 5 # 205

    # Test that order is respected in evaluate_all
    params_ordered = Parameters(
        dependent_val=lambda p_ctx, c: p_ctx['base_val'] * 2,
        base_val=50
    )
    # If base_val is defined after dependent_val, evaluate_all must process in order
    evaluated_ordered = params_ordered.evaluate_all()
    assert evaluated_ordered['base_val'] == 50
    assert evaluated_ordered['dependent_val'] == 100


def test_evaluate_all_simple():
    params = Parameters(
        p1 = 10,
        p2 = lambda cycle: cycle * 2,
        p3 = lambda p_ctx, cycle: p_ctx['p1'] + p_ctx['p2']
    )
    results_cycle0 = params.evaluate_all(cycle=0)
    assert results_cycle0['p1'] == 10
    assert results_cycle0['p2'] == 0
    assert results_cycle0['p3'] == 10 # 10 + 0

    results_cycle5 = params.evaluate_all(cycle=5)
    assert results_cycle5['p1'] == 10
    assert results_cycle5['p2'] == 10 # 5 * 2
    assert results_cycle5['p3'] == 20 # 10 + 10

def test_get_param_value_not_defined():
    """Test error when getting an undefined parameter."""
    params = Parameters()
    with pytest.raises(KeyError, match="Parameter 'undef_param' not defined."):
        params.get_param_value("undef_param")

def test_parameter_order():
    """Test that parameter definition order is maintained."""
    params = Parameters()
    params.add_param("z", 1)
    params.add_param("a", 2)
    params.add_param("m", 3)
    assert params.names == ["z", "a", "m"]

    # Test creation order
    params2 = Parameters(c=10, b=20, a=30)
    assert params2.names == ["c", "b", "a"]

# More tests needed for:
# - More complex callable dependencies and error handling in get_param_value
# - Type checking of parameter definitions if desired
# - Behavior of __getitem__ with callables (currently returns the callable)
# - Robustness of inter-parameter dependency in get_param_value without evaluate_all context
#   (This is currently a design weakness in direct get_param_value calls for dependent params)

```
This file `test_parameters.py` includes basic tests for creating `Parameters` objects, adding parameters, and evaluating them (both fixed and callable). It also includes a test for `evaluate_all` which is important for inter-dependent parameters. It notes areas for further testing as the `Parameters` class becomes more sophisticated.

Now for `hero_python_port/tests/test_state.py`.
