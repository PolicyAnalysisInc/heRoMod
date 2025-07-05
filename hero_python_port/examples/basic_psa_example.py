# hero_python_port/examples/basic_psa_example.py
"""
Example demonstrating a basic Probabilistic Sensitivity Analysis (PSA) using hero_py.
It uses a simple 2-state Markov model.
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import sys # For checking interactive session

from hero_py import (
    Parameters,
    State,
    TransitionMatrix,
    Strategy,
    run_simulation # Though PSA runs simulations internally
)
from hero_py.distributions import Normal, Beta, Uniform # For probabilistic params
from hero_py.sensitivity_analysis import run_psa, PSAResults # Import PSA functions

def run_basic_psa_example():
    """Defines, runs, and shows results for a basic PSA."""

    print("--- Defining Basic PSA Example ---")

    # 1. Define Global Parameters with Probabilistic Distributions
    # Some parameters will be fixed, others will be sampled.
    psa_params = Parameters(
        # Probabilistic parameters
        p_h_to_s = Beta(alpha=2, beta=18),        # Mean = 2/(2+18) = 0.1
        cost_healthy_dist = Normal(mean=100, std=10),
        cost_sick_dist = Normal(mean=500, std=50),
        effect_healthy_val = Uniform(min_val=0.8, max_val=1.0), # Fixed name for effect
        effect_sick_val = Uniform(min_val=0.4, max_val=0.7),    # Fixed name for effect

        # Deterministic parameter (can be used by callables)
        fixed_discount_rate = 0.035, # Example, not directly used by simple state values here

        # Callables that will use the *sampled* values from above
        # The 'p' in lambda p,c: ... will be a dict of sampled/fixed values for that iteration
        cost_healthy_state = lambda p, c: p['cost_healthy_dist'],
        cost_sick_state    = lambda p, c: p['cost_sick_dist'],
        # Effects are directly sampled, but could also be callable
        # For this example, we'll have State objects directly reference effect_healthy_val and effect_sick_val
    )
    print("PSA Parameters defined with distributions.")

    # 2. Define Health States
    # State values will now use the callable definitions in psa_params,
    # which in turn use the sampled probabilistic parameters.
    healthy_state = State(
        name="Healthy",
        cost = lambda p, c: p['cost_healthy_state'], # This will resolve to the sampled cost_healthy_dist
        qaly = lambda p, c: p['effect_healthy_val']  # Directly use the sampled effect value
    )
    sick_state = State(
        name="Sick",
        cost = lambda p, c: p['cost_sick_state'],
        qaly = lambda p, c: p['effect_sick_val']
    )
    model_states = [healthy_state, sick_state]

    # 3. Define Transition Matrix
    # Transition probabilities can also be probabilistic
    tm = TransitionMatrix(state_names=["Healthy", "Sick"])
    tm.set_row_probs(
        from_state="Healthy",
        to_state_definitions={"Sick": lambda p, c: p['p_h_to_s']}, # Uses sampled p_h_to_s
        complement_to_state="Healthy"
    )
    tm.set_row_probs(
        from_state="Sick",
        to_state_definitions={"Healthy": 0.05}, # Fixed P(Healthy | Sick) for simplicity
        complement_to_state="Sick"
    )

    # 4. Define Strategy
    psa_strategy = Strategy(
        name="TreatmentPSA",
        states=model_states,
        transition_definition=tm
    )
    print(f"Strategy for PSA: {psa_strategy.name}")

    # 5. Run PSA
    num_cycles = 20
    num_iterations = 1000 # Number of PSA samples
    initial_population = {"Healthy": 1000, "Sick": 0}
    value_attributes_to_track = ["cost", "qaly"] # Must match attribute names in State

    # Use the fixed discount rate defined in parameters for consistency,
    # or define it directly here.
    # For this example, let's assume discount rates are fixed for the PSA.
    discount_r = {"cost": 0.035, "qaly": 0.035}
    # If discount_rates were also probabilistic, they'd be part of psa_params.

    print(f"\nRunning PSA with {num_iterations} iterations for {num_cycles} cycles...")
    psa_results = run_psa(
        strategy=psa_strategy,
        global_parameters=psa_params,
        num_cycles_for_sim=num_cycles,
        num_iterations=num_iterations,
        initial_population_dist=initial_population,
        value_attributes=value_attributes_to_track,
        discount_rates=discount_r,
        cost_attr_name="cost", # Specify for CE plane/CEAC
        effect_attr_name="qaly", # Specify for CE plane/CEAC
        seed=12345 # For reproducibility
    )
    print("PSA run complete.")

    # 6. Review PSA Results
    print("\n--- PSA Results ---")

    # Summary statistics of outputs (costs and QALYs)
    output_summary = psa_results.summary(percentiles=[0.025, 0.975])
    print("\nSummary of PSA Outputs (Cost and QALYs):")
    print(output_summary)

    # Parameter samples (first 5 iterations)
    param_samples_df = psa_results.get_parameter_samples()
    print("\nParameter Samples (first 5 iterations):")
    print(param_samples_df.head())

    # Output results (costs and QALYs for first 5 iterations)
    outputs_df = psa_results.get_outputs()
    print("\nPSA Outputs (cost & QALYs - first 5 iterations):")
    print(outputs_df.head())

    # Cost-Effectiveness Plane Data
    ce_plane_df = psa_results.get_ce_plane_data()
    if ce_plane_df is not None:
        print("\nCE Plane Data (first 5 iterations):")
        print(ce_plane_df.head())

        # Plot CE Plane
        fig_ce, ax_ce = plt.subplots()
        psa_results.plot_ce_plane(
            ax=ax_ce,
            title=f"Cost-Effectiveness Plane: {psa_strategy.name}",
            wtp_threshold=30000, # Optionally draw a WTP line
            scatter_kwargs={'alpha': 0.5, 's': 10}
        )
        if not hasattr(sys, 'ps1'):
            plt.savefig("basic_psa_example_ce_plane.png")
            print("\nCE Plane plot saved to basic_psa_example_ce_plane.png")
        else:
            plt.show()
        plt.close(fig_ce)


    # Cost-Effectiveness Acceptability Curve (CEAC) Data
    wtp_thresholds_for_ceac = np.linspace(0, 100000, 21) # 0 to 100k, 21 points
    # For a single strategy, CEAC is prob NMB > 0 (vs. doing nothing at 0 cost, 0 QALY)
    ceac_df = psa_results.calculate_ceac_data(comparator_psa_results=None, wtp_thresholds=wtp_thresholds_for_ceac)
    if ceac_df is not None:
        print("\nCEAC Data:")
        print(ceac_df)

        # Plot CEAC
        fig_ceac, ax_ceac = plt.subplots()
        psa_results.plot_ceac(
            comparator_psa_results=None,
            wtp_thresholds=wtp_thresholds_for_ceac,
            ax=ax_ceac,
            title=f"CEAC: {psa_strategy.name} (vs. Do Nothing)"
        )
        if not hasattr(sys, 'ps1'):
            plt.savefig("basic_psa_example_ceac.png")
            print("CEAC plot saved to basic_psa_example_ceac.png")
        else:
            plt.show()
        plt.close(fig_ceac)

if __name__ == "__main__":
    run_basic_psa_example()
```

**2. `hero_python_port/docs/examples/psa_example.md`:**
