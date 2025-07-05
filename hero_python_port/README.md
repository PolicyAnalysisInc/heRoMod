# `hero_py`: A Python Library for Health Economic Modeling (Port of R's heRomod)

## Overview

`hero_py` is a Python library currently under development, aiming to provide core functionalities for health economic modeling, inspired by the R package `heRomod`. This project seeks to bring robust and flexible modeling tools to the Python ecosystem.

The primary goal is to enable users to define, run, and analyze various types of health economic models, including:
*   Cohort-based Markov models
*   Partitioned Survival Models (PSMs)
*   Probabilistic Sensitivity Analysis (PSA)
*   Deterministic Sensitivity Analysis (DSA)
*   Model Calibration (initial implementation)
*   Value of Information (EVPI, initial EVPPI using linear regression)
*   Scenario Analysis (parameter-based)

## Current Status: Alpha - Under Active Development

`hero_py` is in its early stages of development. Core components for basic Markov models, PSMs, PSA, and DSA are being implemented. The API is subject to change. **This is not yet ready for production use.**

## Project Goals
*   Provide a Pythonic API for defining model parameters, states, transitions, and strategies.
*   Implement core simulation engines for cohort-based models.
*   Support key health economic analyses (PSA, DSA, Calibration, VoI - planned).
*   Integrate well with the PyData ecosystem (NumPy, Pandas, SciPy).
*   Offer clear documentation and examples.

## Getting Started (for Developers/Testers)

### Prerequisites
*   Python 3.8+
*   Git (for cloning)

### Environment Setup
1.  Clone this repository (or the repository where `hero_python_port/` resides).
2.  Navigate to the `hero_python_port/` directory.
3.  Create and activate a Python virtual environment:
    ```bash
    python -m venv venv
    source venv/bin/activate  # On Windows: venv\Scripts\activate
    ```
4.  Install dependencies:
    ```bash
    pip install -r requirements.txt
    ```

### Running Tests
Tests are written using `pytest`. From the `hero_python_port/` directory:
```bash
pytest
```

### Basic Usage Example (Conceptual - API is evolving)
```python
# (This example will be fleshed out as Phase 1 components become more stable
#  and a simple end-to-end run is possible. For now, refer to individual
#  class tests in tests/ for usage of Parameters, State, etc.)

# from hero_py import Parameters, State, TransitionMatrix, Strategy, run_simulation

# # 1. Define Parameters
# params = Parameters(
#     p_transition = 0.1,
#     cost_healthy = 100,
#     cost_sick = 500
# )

# # 2. Define States
# healthy = State("Healthy", cost=lambda p,c: p['cost_healthy'])
# sick = State("Sick", cost=lambda p,c: p['cost_sick'])

# # 3. Define Transition Matrix
# tm = TransitionMatrix(state_names=["Healthy", "Sick"])
# tm.set_row_probs("Healthy", {"Sick": lambda p,c: p['p_transition']}, complement_to_state="Healthy")
# tm.set_row_probs("Sick", {"Sick": 0.9}, complement_to_state="Healthy") # Incorrect example, should be "Healthy" to "Sick"

# # 4. Define Strategy
# my_strategy = Strategy("MyTreatment", states=[healthy, sick], transition_definition=tm)

# # 5. Run Simulation
# # sim_output = run_simulation(...)
# # print(sim_output.summary())
# # ax = sim_output.plot_cohort_trace()
# # if ax: ax.figure.show() # To display plot if running interactively
#
# # For PSA results (assuming psa_results is a PSAResults object):
# # print(psa_results.summary())
# # ax_ce = psa_results.plot_ce_plane()
# # if ax_ce: ax_ce.figure.show()
# # ax_ceac = psa_results.plot_ceac(comparator_psa_results=None, wtp_thresholds=[0, 20000, 50000])
# # if ax_ceac: ax_ceac.figure.show()
#
# # For DSA results (assuming dsa_results is a DSAResults object):
# # print(dsa_results.summary(output_format="impact_from_baseline"))
# # ax_tornado = dsa_results.plot_tornado(outcome_attribute="cost") # Assuming 'cost' was tracked
# # if ax_tornado: ax_tornado.figure.show()
#
# # calibration_results = calibrate_model(...)
# # print(calibration_results.get_summary_df())
#
# # evpi_df = calculate_evpi(...)
# # print(evpi_df)
# # evppi_df = calculate_evppi(...)
# # print(evppi_df)
#
# # high_cost_scenario = Scenario("High Cost", parameter_overrides={"cost_treatment": 1500})
# # scenario_results = run_scenario_analysis(..., scenarios=[high_cost_scenario])
# # print(scenario_results.get_comparison_summary())
```

## Project Documentation & Planning
*   **Project Plan:** [PROJECT_PLAN.md](PROJECT_PLAN.md) - Detailed phases and tasks for the porting effort.
*   **Python Port Roadmap:** [ROADMAP_PYTHON.md](ROADMAP_PYTHON.md) - Long-term vision for `hero_py`.
*   **Development Log (Jules):** [JULES_ACTIVITY_LOG.md](JULES_ACTIVITY_LOG.md) - Log of significant development actions.
*   **Project Status Overview:** [PYTHON_PORT_STATUS.md](PYTHON_PORT_STATUS.md) - High-level progress.

## Contributing
This project is currently in early development by AI agent Jules. Contribution guidelines will be established as the project matures. For now, feedback and suggestions can be directed through the current interaction channel.

## License
*(To be determined for the Python port - original heRomod is GPL-3. Consider compatible licenses like GPL-3 or Apache 2.0 / MIT if wider adoption is desired and original authors agree/code is sufficiently distinct).*

---
This README provides a basic entry point for anyone looking at the `hero_python_port` directory. It will need to be updated significantly as the project progresses.
