# User Guide: Defining and Using Parameters in `hero_py`

Parameters are the backbone of any health economic model, representing inputs like costs, probabilities, utilities, rates, etc. `hero_py` provides a flexible `Parameters` class to manage these. This guide explains how to define and use parameters effectively.

## 1. Basic Parameter Definition

You create a `Parameters` object by passing parameter names and their definitions as keyword arguments:

```python
from hero_py import Parameters

# Define fixed numerical parameters
params = Parameters(
    cost_drug_a = 1500.0,
    probability_response = 0.75,
    cycle_length_years = 1.0
)

print(params.get_param_definition("cost_drug_a"))
# Output: 1500.0

print(params.names)
# Output: ['cost_drug_a', 'probability_response', 'cycle_length_years']
# (Order is preserved from Python 3.7+)
```
You can also add parameters after initialization using `add_param()`:
```python
params.add_param("utility_healthy", 0.95)
print(params.get_param_definition("utility_healthy"))
# Output: 0.95
```

## 2. Callable (Dynamic) Parameters

Parameters can be defined using Python `lambda` functions or regular functions. This allows for:
*   **Time-dependent parameters:** Values that change based on the simulation cycle.
*   **Inter-dependent parameters:** Values that depend on other parameters.

When a callable is used, it **must** accept two arguments:
1.  `p_ctx` (or any name): A dictionary containing already evaluated parameters for the current context (cycle and PSA iteration).
2.  `cycle` (or any name): The current simulation cycle number (0-indexed).

```python
from hero_py.distributions import Normal # For later examples

params_dynamic = Parameters(
    base_cost = 100,
    inflation_rate = 0.02,
    # Time-dependent cost (increases each cycle due to inflation)
    cost_maintenance = lambda p_ctx, cycle: p_ctx['base_cost'] * ((1 + p_ctx['inflation_rate']) ** cycle),

    # Inter-dependent parameter
    prob_event = 0.1,
    relative_risk_treatment_b = 0.5, # RR for treatment B vs. base event probability
    prob_event_treatment_b = lambda p_ctx, cycle: p_ctx['prob_event'] * p_ctx['relative_risk_treatment_b']
)

# To get evaluated values:
# For deterministic runs (or when preparing DSA sets):
eval_deterministic_c0 = params_dynamic.evaluate_all_deterministic(cycle=0)
print(f"Cost maintenance at C0: {eval_deterministic_c0['cost_maintenance']:.2f}")
# Output: Cost maintenance at C0: 100.00

eval_deterministic_c5 = params_dynamic.evaluate_all_deterministic(cycle=5)
print(f"Cost maintenance at C5: {eval_deterministic_c5['cost_maintenance']:.2f}")
# Output: Cost maintenance at C5: 110.41 (approx)
print(f"Prob event TxB: {eval_deterministic_c0['prob_event_treatment_b']:.3f}")
# Output: Prob event TxB: 0.050
```
**Important:** Parameters are evaluated in the order they are defined. A callable parameter can only depend on other parameters that were defined *before* it in the `Parameters` object.

## 3. Probabilistic Parameters (for PSA)

For Probabilistic Sensitivity Analysis (PSA), parameters can be defined using distribution helper classes from `hero_py.distributions`.

```python
from hero_py.distributions import Normal, Beta, Uniform
import numpy as np # For random number generator

psa_params = Parameters(
    cost_consultation = Normal(mean=150, std=25),
    utility_sick = Beta(alpha=8, beta=2), # Mean = 0.8
    adherence_rate = Uniform(min_val=0.6, max_val=0.9)
)

# These definitions are distribution objects themselves:
print(psa_params.get_param_definition("cost_consultation"))
# Output: Normal(mean=150, std=25)

# To get sampled values (used internally by run_psa):
rng = np.random.default_rng(seed=123)
sampled_values_iter1 = psa_params.sample_and_evaluate_all(rng_engine=rng, cycle=0)
print("\nSampled values (iteration 1):")
for name, val in sampled_values_iter1.items():
    print(f"  {name}: {val:.3f}")

sampled_values_iter2 = psa_params.sample_and_evaluate_all(rng_engine=rng, cycle=0) # New samples
print("\nSampled values (iteration 2):")
for name, val in sampled_values_iter2.items():
    print(f"  {name}: {val:.3f}")
```
When `run_psa` is called, it uses the `sample_and_evaluate_all()` method internally for each iteration. If a callable parameter depends on a probabilistic parameter, it will use the *sampled value* of that probabilistic parameter in its calculation for that PSA iteration.

## 4. DSA Parameter Ranges

For Deterministic Sensitivity Analysis (DSA), you can associate a low and high value range with a parameter.

```python
from hero_py.dsa_definitions import DSAParameterRange

dsa_params = Parameters()
dsa_params.add_param(
    name="p_success_rate",
    definition=0.7, # Baseline definition
    dsa_range=DSAParameterRange(low=0.5, high=0.9)
)

dsa_params.add_param(
    name="cost_treatment",
    definition=lambda p_ctx, c: 1000 + c*5, # Baseline is callable
    dsa_range=DSAParameterRange(low=800, high=1200) # Low/High for DSA are fixed
)

# Retrieve DSA definition:
print(f"\nDSA def for p_success_rate: {dsa_params.get_dsa_definition('p_success_rate')}")
# Output: DSA def for p_success_rate: DSAParameterRange(low=0.5, high=0.9, baseline=None)

# Generate parameter sets for DSA on 'p_success_rate':
# This shows how run_dsa would get parameter sets.
dsa_sets = dsa_params.get_dsa_iteration_parameter_sets(param_name_to_vary="p_success_rate", cycle=0)
for level, param_set in dsa_sets:
    print(f"\nDSA Level: {level}")
    print(f"  p_success_rate: {param_set['p_success_rate']}")
    print(f"  cost_treatment (baseline at cycle 0): {param_set['cost_treatment']}")
    # Output for cost_treatment will be 1000 for all levels as it's not the varied param
```
The `run_dsa` function uses `get_dsa_iteration_parameter_sets()` to get the low, baseline, and high value scenarios for each parameter being varied.

## 5. Parameter Evaluation Context

*   **`evaluate_all_deterministic(cycle)`:** Used for standard model runs and by DSA.
    *   Evaluates fixed values and callables (passing context of prior params and cycle).
    *   Returns `ProbabilisticDistribution` objects as-is (it does not sample them).
    *   Can take DSA override values for a specific parameter.
*   **`sample_and_evaluate_all(rng_engine, cycle)`:** Used by PSA.
    *   Samples from `ProbabilisticDistribution` objects.
    *   Evaluates fixed values and callables (passing context of prior *sampled* or fixed params and cycle).

This system provides a flexible way to define various types of parameters needed for health economic modeling and sensitivity analyses. Refer to the API documentation for `hero_py.parameters.Parameters` and the distribution classes in `hero_py.distributions` for more details.
```

**2. Create `hero_python_port/docs/guides/` directory (if not existing) and add `.gitkeep`:**
(This step might be redundant if the directory was implicitly created by the file above, but good for explicit tracking).
