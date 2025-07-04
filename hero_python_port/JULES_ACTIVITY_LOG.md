# Jules' Activity Log for `hero_py` Development

This log tracks significant actions, decisions, and phase completions undertaken by Jules (the AI agent) during the development of the `hero_py` Python library.

## Initial Setup & Planning (Phases 0)
*   **Date:** (Assumed Start Date)
*   **Actions:**
    *   Analyzed R `heRomod` package structure and functionality based on available files.
    *   Created initial planning documents:
        *   `PROJECT_PLAN.md`: Outlined phased approach for porting, scope, toolkit.
        *   `ROADMAP_PYTHON.md`: Drafted long-term vision for the Python library.
        *   `AGENTS.md` (in R repo): Provided guidance for Python porting.
    *   Set up the basic directory structure for `hero_python_port/` (including `hero_py/`, `tests/`, `docs/`, `examples/`, `.gitignore`).

## Phase 1: Core Model Engine & Definition API
*   **Date:** (Assumed Completion Date for Phase 1)
*   **Features Implemented:**
    *   `Parameters` class: Manages fixed and callable parameter definitions with ordered, context-aware evaluation (`evaluate_all_deterministic`).
    *   `State` class: Defines health states with fixed or callable attributes (cost, utility), evaluated in context.
    *   `TransitionMatrix` class: Basic implementation for defining transition matrices with fixed/callable probabilities and initial support for complement logic (`set_row_probs`, `get_matrix`).
    *   `Strategy` class: Combines states, transition definitions, and allows for strategy-specific parameters with override logic (`get_combined_parameters`).
    *   `run_simulation` function (initial): Basic cohort simulation loop for Markov models, including cycle iteration, population tracking, value accumulation, and simple discounting.
    *   `SimulationOutput` class: Stores results from `run_simulation`.
    *   Unit tests (`pytest`) created for all new classes and `run_simulation`.
    *   `requirements.txt` initialized.
*   **Key Decisions:**
    *   Adopted lambda functions/callables as the primary Pythonic way to define dynamic parameters and state attributes, with context (`params_dict`, `cycle`) passed during evaluation.
    *   Chose `set_row_probs` with a `complement_to_state` argument as the initial API for `TransitionMatrix`.

## Phase 2: Partitioned Survival Models (PSM)
*   **Date:** (Assumed Completion Date for Phase 2)
*   **Features Implemented:**
    *   `hero_py/survival.py`:
        *   `SurvivalDistribution` ABC.
        *   Concrete `ExponentialSurvival`, `WeibullSurvival`, `LogNormalSurvival` classes using `scipy.stats` backend, with methods for `S(t)`, `pdf(t)`, `h(t)`.
    *   `hero_py/psm.py`:
        *   `PartitionedSurvivalModelDefinition` (PSMDef) class to hold PFS/OS curves and state role mappings.
        *   `PSMDef.get_state_proportions(t)` method to calculate cohort fractions in PFS, Progressed, Dead states.
    *   `Strategy` class updated: `transition_definition` can now be `PSMDef`; `model_type` attribute added; validation for PSM states.
    *   `run_simulation` updated: Handles `model_type="psm"` by using `PSMDef.get_state_proportions()` for cohort transitions.
    *   Unit tests for new survival and PSM classes, and for PSM simulation.
*   **Key Decisions:**
    *   Selected `scipy.stats` for initial parametric survival curve evaluation.
    *   PSM state proportions calculated based on `min(S_PFS, S_OS)` for the progression-free state.

## Phase 3: Probabilistic Sensitivity Analysis (PSA)
*   **Date:** (Assumed Completion Date for Phase 3)
*   **Features Implemented:**
    *   `hero_py/distributions.py`: `ProbabilisticDistribution` ABC and concrete classes (`Normal`, `Beta`, `Uniform`, `Gamma`, `LogNormal`) with `sample()` methods.
    *   `Parameters` class updated:
        *   Can store `ProbabilisticDistribution` objects.
        *   New `sample_and_evaluate_all()` method to draw samples and evaluate dependent callables.
    *   `hero_py/sensitivity_analysis.py`:
        *   `PSAResults` class: Stores PSA outputs (value attributes and parameter samples per iteration) in DataFrames. Includes methods for `get_outputs()`, `get_parameter_samples()`, `get_ce_plane_data()`, `calculate_ceac_data()`.
        *   `run_psa()` function: Performs PSA iterations, manages parameter sampling (including layered global/strategy params), runs simulations, and collects results into `PSAResults`.
    *   Unit tests for probabilistic parameter definitions, sampling, `run_psa`, and `PSAResults` methods.
*   **Key Decisions:**
    *   PSA parameter sampling and evaluation occur based on the combined, ordered definitions from global and strategy-specific parameters for each iteration.

## Phase 4: Deterministic Sensitivity Analysis (DSA)
*   **Date:** (Assumed Completion Date for Phase 4)
*   **Features Implemented:**
    *   `hero_py/dsa_definitions.py`: `DSAParameterRange(low, high, baseline=None)` class.
    *   `Parameters` class updated:
        *   `add_param` accepts optional `dsa_range`.
        *   `get_dsa_iteration_parameter_sets()` method generates low/baseline/high parameter sets for a specified parameter.
        *   `evaluate_all_deterministic` modified to accept DSA override values.
    *   `hero_py/sensitivity_analysis.py`:
        *   `DSAResults` class: Stores DSA outputs in a DataFrame with MultiIndex (parameter, level). Includes `get_results()` and `get_tornado_data()`.
        *   `run_dsa()` function: Performs one-way DSA, generates parameter sets, runs simulations, stores results.
    *   Unit tests for DSA definitions, parameter set generation, `run_dsa`, and `DSAResults`.
*   **Documentation Created (Initial):** `README.md` (for `hero_python_port`), `JULES_ACTIVITY_LOG.md` (this file), `PYTHON_PORT_STATUS.md`. `ROADMAP_PYTHON.md` updated.
*   **Key Decisions:**
    *   DSA parameter sets generated by varying one parameter while holding others at their deterministic baseline values (which can include callable evaluations or distribution objects if those are the baselines).

## Phase 5: Initial Results, Summaries & Basic Plotting
*   **Date:** (Current Date of this log entry)
*   **Features Implemented:**
    *   `SimulationOutput` enhancements:
        *   `summary()` method providing total discounted/undiscounted values per attribute.
        *   `get_cohort_trace()` accessor with normalization option.
        *   `get_aggregated_cycle_values_for_attribute()` accessor.
        *   `plot_cohort_trace()` method using `matplotlib`.
    *   `PSAResults` enhancements:
        *   `summary()` method providing descriptive statistics (mean, std, quantiles) for output attributes.
        *   `plot_ce_plane()` method.
        *   `plot_ceac()` method.
    *   `DSAResults` enhancements:
        *   `summary()` method providing impact from baseline or percent change summaries.
        *   `get_tornado_data()` refined to use the new summary method.
        *   `plot_tornado()` method.
    *   `matplotlib` added to `requirements.txt`.
    *   Unit tests updated/added for new summary and plotting methods (checking execution and return types).
*   **Documentation Updates:**
    *   Docstrings for all new public methods.
    *   `PYTHON_PORT_STATUS.md` updated to reflect Phase 5 completion.
    *   `JULES_ACTIVITY_LOG.md` updated (this entry).
    *   `hero_python_port/README.md` updated with conceptual examples of summary/plot calls.
    *   `hero_python_port/ROADMAP_PYTHON.md` updated to reflect new capabilities.
*   **Key Decisions:**
    *   Chose `matplotlib` for initial basic plotting methods directly within result objects.
---
