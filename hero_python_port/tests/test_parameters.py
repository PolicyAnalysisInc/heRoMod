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

# --- Tests for sample_and_evaluate_all (PSA functionality) ---
import numpy as np # For np.random.Generator
from hero_py.distributions import Normal, Uniform, Beta # Import distribution classes

def test_evaluate_all_deterministic_with_dist_objects():
    """
    Test that evaluate_all_deterministic returns distribution objects themselves, not samples.
    """
    norm_dist = Normal(mean=10, std=2)
    params = Parameters(
        p_norm=norm_dist,
        p_fixed=100,
        p_callable=lambda p,c: p['p_fixed'] + c # Depends on a fixed value
    )

    deterministic_eval = params.evaluate_all_deterministic(cycle=5)

    assert deterministic_eval['p_fixed'] == 100
    assert deterministic_eval['p_norm'] is norm_dist # Should be the object itself
    assert deterministic_eval['p_callable'] == 105 # 100 + 5

def test_sample_and_evaluate_all_basic_sampling():
    """Test sampling from defined distributions."""
    rng = np.random.default_rng(seed=123)
    params = Parameters(
        cost_treatment=Normal(mean=1000, std=100),
        prob_success=Beta(alpha=80, beta=20) # Mean approx 0.8
    )

    n_samples = 5
    samples_cost = []
    samples_prob = []
    for _ in range(n_samples):
        iteration_params = params.sample_and_evaluate_all(rng_engine=rng, cycle=0)
        samples_cost.append(iteration_params['cost_treatment'])
        samples_prob.append(iteration_params['prob_success'])

    assert len(samples_cost) == n_samples
    assert len(samples_prob) == n_samples
    # Check if samples are somewhat reasonable (not all same, within some range)
    assert np.std(samples_cost) > 0
    assert np.std(samples_prob) > 0
    assert all(500 < c < 1500 for c in samples_cost) # Rough check based on mean/std
    assert all(0.6 < p < 0.95 for p in samples_prob) # Rough check for Beta(80,20)

def test_sample_and_evaluate_all_mixed_types():
    """Test sampling with fixed, probabilistic, and callable dependent parameters."""
    rng = np.random.default_rng(seed=456)
    params = Parameters(
        base_utility=Uniform(min_val=0.5, max_val=0.9),
        fixed_ κόστος=500, # Using a non-ascii name to test robustness if needed
        qaly_modifier=lambda p_ctx, cycle: p_ctx['base_utility'] * (1 - cycle * 0.01),
        final_cost=lambda p_ctx, _: p_ctx['fixed_ κόστος'] + p_ctx.get('sampled_add_on', 0) # depends on another sampled
    )
    # Add another probabilistic param that final_cost depends on, but defined later in order
    # This will test if the context is correctly built up.
    # This is not ideal for definition order, but sample_and_evaluate_all should handle it.
    # Actually, for callables, the context `p_ctx` only contains items defined *before* it in `_order`.
    # So, `final_cost` cannot depend on `sampled_add_on` if `sampled_add_on` is added after `final_cost`.
    # Let's redefine with correct order for dependency.

    params_ordered = Parameters(
        base_utility=Uniform(min_val=0.5, max_val=0.9),
        fixed_cost=500,
        sampled_add_on=Normal(mean=50, std=10), # Defined before final_cost
        qaly_modifier=lambda p_ctx, cycle: p_ctx['base_utility'] * (1 - cycle * 0.01),
        final_cost=lambda p_ctx, _: p_ctx['fixed_cost'] + p_ctx['sampled_add_on']
    )

    iteration_params_c0 = params_ordered.sample_and_evaluate_all(rng_engine=rng, cycle=0)

    assert 0.5 <= iteration_params_c0['base_utility'] <= 0.9
    assert iteration_params_c0['fixed_cost'] == 500
    assert isinstance(iteration_params_c0['sampled_add_on'], float)
    # qaly_modifier at cycle 0 should be equal to base_utility
    assert iteration_params_c0['qaly_modifier'] == iteration_params_c0['base_utility']
    assert iteration_params_c0['final_cost'] == 500 + iteration_params_c0['sampled_add_on']

    iteration_params_c1 = params_ordered.sample_and_evaluate_all(rng_engine=rng, cycle=1) # New samples
    assert 0.5 <= iteration_params_c1['base_utility'] <= 0.9
    assert iteration_params_c1['fixed_cost'] == 500
    # qaly_modifier at cycle 1 should be base_utility * 0.99
    assert np.isclose(iteration_params_c1['qaly_modifier'], iteration_params_c1['base_utility'] * 0.99)
    assert iteration_params_c1['final_cost'] == 500 + iteration_params_c1['sampled_add_on']

    # Ensure samples are different across iterations (highly probable for continuous distributions)
    assert iteration_params_c0['base_utility'] != iteration_params_c1['base_utility']
    assert iteration_params_c0['sampled_add_on'] != iteration_params_c1['sampled_add_on']


def test_sample_and_evaluate_all_requires_rng_engine():
    params = Parameters(p1=Normal(0,1))
    with pytest.raises(TypeError, match="rng_engine must be an instance of numpy.random.Generator"):
        params.sample_and_evaluate_all(rng_engine=None, cycle=0) # type: ignore
    with pytest.raises(TypeError, match="rng_engine must be an instance of numpy.random.Generator"):
        params.sample_and_evaluate_all(rng_engine="not_rng", cycle=0) # type: ignore

def test_deterministic_eval_for_psa_param_definition_phase():
    """
    When defining a model, evaluate_all_deterministic should return the distribution
    objects themselves, not samples, as this is used by the main simulation loop
    when not running a PSA.
    """
    norm_dist = Normal(mean=5, std=1)
    beta_dist = Beta(alpha=1, beta=1)
    params = Parameters(
        p_norm = norm_dist,
        p_beta = beta_dist,
        p_fixed = 10,
        p_callable = lambda p_ctx, cycle: p_ctx['p_fixed'] + cycle
    )

    eval_results = params.evaluate_all_deterministic(cycle=2)

    assert eval_results['p_norm'] is norm_dist
    assert eval_results['p_beta'] is beta_dist
    assert eval_results['p_fixed'] == 10
    assert eval_results['p_callable'] == 12 # 10 + 2

# --- Tests for DSA Parameter Set Generation ---
from hero_py.dsa_definitions import DSAParameterRange

def test_add_param_with_dsa_range():
    params = Parameters()
    dsa_r = DSAParameterRange(low=80, high=120)
    params.add_param("cost_A", 100, dsa_range=dsa_r)

    assert params.get_param_definition("cost_A") == 100
    assert params.get_dsa_definition("cost_A") is dsa_r
    assert params.get_dsa_definition("cost_A").low == 80

    # Update param without DSA range, should remove it
    params.add_param("cost_A", 105)
    assert params.get_dsa_definition("cost_A") is None

    # Add it back
    params.add_param("cost_A", 100, dsa_range=dsa_r)
    assert params.get_dsa_definition("cost_A") is dsa_r

    # Test initialization with DSA range
    params_init = Parameters(cost_B=(200, DSAParameterRange(low=150, high=250)))
    assert params_init.get_param_definition("cost_B") == 200
    assert params_init.get_dsa_definition("cost_B").high == 250


def test_get_dsa_iteration_parameter_sets_fixed_param():
    dsa_r = DSAParameterRange(low=80, high=120) # Baseline from main def: 100
    params = Parameters(
        cost_A=100,
        cost_B=200,
        dsa_range_for_cost_A=dsa_r # Not how it's linked, use add_param or init tuple
    )
    params.add_param("cost_A", 100, dsa_range=dsa_r) # Correctly link DSA range

    dsa_sets = params.get_dsa_iteration_parameter_sets("cost_A", cycle=0)

    assert len(dsa_sets) == 3
    levels = [s[0] for s in dsa_sets]
    assert levels == ["low", "baseline", "high"]

    # Low set
    assert dsa_sets[0][1]["cost_A"] == 80
    assert dsa_sets[0][1]["cost_B"] == 200 # Other params at baseline
    # Baseline set
    assert dsa_sets[1][1]["cost_A"] == 100
    assert dsa_sets[1][1]["cost_B"] == 200
    # High set
    assert dsa_sets[2][1]["cost_A"] == 120
    assert dsa_sets[2][1]["cost_B"] == 200

def test_get_dsa_iteration_parameter_sets_callable_param():
    # Varied param is callable, other is fixed
    # cost_A = lambda p,c: p['base_A'] + c*10. Baseline at c=1: 50 + 10 = 60
    dsa_r_A = DSAParameterRange(low=45, high=75) # Explicit baseline in DSARange will be used by its get_values_for_dsa
                                                # but Parameters.get_dsa_iteration_parameter_sets
                                                # recalculates actual baseline.
    params = Parameters(base_A=50)
    params.add_param("cost_A", lambda p,c: p['base_A'] + c*10, dsa_range=dsa_r_A)
    params.add_param("fixed_B", 300)

    dsa_sets_c1 = params.get_dsa_iteration_parameter_sets("cost_A", cycle=1)

    # Low set for cost_A
    assert dsa_sets_c1[0][1]["cost_A"] == 45
    assert dsa_sets_c1[0][1]["base_A"] == 50 # Other params at baseline
    assert dsa_sets_c1[0][1]["fixed_B"] == 300
    # Baseline set for cost_A
    assert dsa_sets_c1[1][1]["cost_A"] == 60 # 50 + 1*10
    assert dsa_sets_c1[1][1]["base_A"] == 50
    # High set for cost_A
    assert dsa_sets_c1[2][1]["cost_A"] == 75
    assert dsa_sets_c1[2][1]["base_A"] == 50

def test_get_dsa_iteration_parameter_sets_other_param_callable():
    # Varied param is fixed, another param is callable
    dsa_r_B = DSAParameterRange(low=250, high=350)
    params = Parameters(base_A=50)
    params.add_param("cost_A", lambda p,c: p['base_A'] + c*10)
    params.add_param("fixed_B", 300, dsa_range=dsa_r_B) # Varying fixed_B

    dsa_sets_c2 = params.get_dsa_iteration_parameter_sets("fixed_B", cycle=2)

    # Low set for fixed_B
    assert dsa_sets_c2[0][1]["fixed_B"] == 250
    assert dsa_sets_c2[0][1]["base_A"] == 50
    assert dsa_sets_c2[0][1]["cost_A"] == 70 # 50 + 2*10 (cost_A at its baseline for c=2)
    # Baseline set
    assert dsa_sets_c2[1][1]["fixed_B"] == 300
    assert dsa_sets_c2[1][1]["cost_A"] == 70
    # High set
    assert dsa_sets_c2[2][1]["fixed_B"] == 350
    assert dsa_sets_c2[2][1]["cost_A"] == 70


def test_get_dsa_iteration_parameter_sets_with_probabilistic_as_other():
    # If a non-varied parameter has a ProbabilisticDistribution as its main definition,
    # evaluate_all_deterministic will return the distribution object itself.
    norm_dist = Normal(mean=10, std=1)
    params = Parameters(
        p_varied=("A", DSAParameterRange(low=5, high=15)), # Baseline for p_varied is "A"
        p_prob = norm_dist
    )
    dsa_sets = params.get_dsa_iteration_parameter_sets("p_varied")

    assert dsa_sets[0][1]["p_varied"] == 5
    assert dsa_sets[0][1]["p_prob"] is norm_dist # Other param is the dist object
    assert dsa_sets[1][1]["p_varied"] == "A"
    assert dsa_sets[1][1]["p_prob"] is norm_dist
    assert dsa_sets[2][1]["p_varied"] == 15
    assert dsa_sets[2][1]["p_prob"] is norm_dist

def test_get_dsa_iteration_parameter_sets_no_dsa_def():
    params = Parameters(cost_A=100)
    with pytest.raises(KeyError, match="Parameter 'cost_A' has no DSA range defined."):
        params.get_dsa_iteration_parameter_sets("cost_A")

    with pytest.raises(KeyError, match="Parameter 'cost_X' not defined."): # Param itself not defined
        params.get_dsa_iteration_parameter_sets("cost_X")

def test_dsa_range_baseline_override_in_get_values():
    # DSAParameterRange can take an explicit baseline.
    # get_values_for_dsa in DSAParameterRange uses it if present.
    # Parameters.get_dsa_iteration_parameter_sets recalculates actual baseline of the *varied* param.

    # Case 1: DSARange has baseline, it should be used for that point by DSAParameterRange
    dsa_r_explicit_baseline = DSAParameterRange(low=0.1, high=0.3, baseline=0.25)
    points = dsa_r_explicit_baseline.get_values_for_dsa(actual_baseline_value=0.2) # actual baseline ignored by this
    assert points["low"] == 0.1
    assert points["baseline"] == 0.25 # Uses its own baseline
    assert points["high"] == 0.3

    # Case 2: DSARange has no baseline, uses provided actual_baseline_value
    dsa_r_no_baseline = DSAParameterRange(low=0.1, high=0.3)
    points2 = dsa_r_no_baseline.get_values_for_dsa(actual_baseline_value=0.22)
    assert points2["low"] == 0.1
    assert points2["baseline"] == 0.22 # Uses the actual one
    assert points2["high"] == 0.3

    # Test how Parameters.get_dsa_iteration_parameter_sets uses this.
    # It always determines the 'actual_baseline_for_varied_param' from the main definition.
    # Then DSAParameterRange.get_values_for_dsa decides which baseline to use for the "baseline" point.
    params = Parameters()
    params.add_param("my_param", 0.2, dsa_range=dsa_r_explicit_baseline) # main def is 0.2

    dsa_sets = params.get_dsa_iteration_parameter_sets("my_param")
    # The "baseline" run for my_param should use the dsa_r_explicit_baseline's baseline (0.25)
    assert dsa_sets[1][1]["my_param"] == 0.25

    params2 = Parameters()
    params2.add_param("my_param2", 0.22, dsa_range=dsa_r_no_baseline) # main def is 0.22
    dsa_sets2 = params2.get_dsa_iteration_parameter_sets("my_param2")
    # The "baseline" run for my_param2 should use its main definition (0.22)
    assert dsa_sets2[1][1]["my_param2"] == 0.22

```
This file `test_parameters.py` includes basic tests for creating `Parameters` objects, adding parameters, and evaluating them (both fixed and callable). It also includes a test for `evaluate_all` which is important for inter-dependent parameters. It notes areas for further testing as the `Parameters` class becomes more sophisticated.

Now for `hero_python_port/tests/test_state.py`.
