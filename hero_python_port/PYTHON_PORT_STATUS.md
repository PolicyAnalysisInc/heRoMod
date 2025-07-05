# `hero_py` - Python Port of heRomod: Project Status

## 1. Project Intention
To create `hero_py`, a Python library offering core equivalent functionality to the `heRomod` R package for health economic modeling. The aim is a Pythonic, user-friendly, and robust library for the PyData ecosystem.

## 2. High-Level Structure
The Python port is being built with a modular, object-oriented design:
*   **Core Classes:** `Parameters`, `State`, `TransitionMatrix` (for Markov), `PartitionedSurvivalModelDefinition` (for PSM), `Strategy`.
*   **Simulation Engine:** `run_simulation` function capable of handling both Markov and PSM strategies.
*   **Analysis Modules:** Separate classes/functions for PSA (`run_psa`, `PSAResults`) and DSA (`run_dsa`, `DSAResults`).
*   **Output Objects:** Dedicated classes like `SimulationOutput`, `PSAResults`, `DSAResults` to store and provide access to results, including summary and basic plotting methods.
*   **Supporting Modules:** `distributions.py` for probabilistic parameter definitions, `dsa_definitions.py` for DSA range specifications.

## 3. Key Libraries Selected (Initial Stack)
*   **Core:** Python 3.8+
*   **Data Handling:** `pandas`, `numpy`
*   **Statistics/Optimization:** `scipy`
*   **Survival Analysis (Parametric Evaluation):** `scipy.stats` (initially), `lifelines` / `scikit-survival` are considerations for future fitting capabilities.
*   **Plotting:** `matplotlib` (for initial basic plots).
*   **Testing:** `pytest`

## 4. Roadmap
See [ROADMAP_PYTHON.md](ROADMAP_PYTHON.md) for the long-term vision and development goals.

## 5. Current Progress against Project Plan ([PROJECT_PLAN.md](PROJECT_PLAN.md))

*   **Phase 0: Setup & Planning:** **COMPLETED**
    *   Initial analysis, creation of `AGENTS.md` (R repo), `PROJECT_PLAN.md`, `ROADMAP_PYTHON.md`.
    *   Setup of `hero_python_port/` proxy repository structure.

*   **Phase 1: Core Model Engine & Definition API:** **COMPLETED**
    *   Implemented `Parameters`, `State`, `TransitionMatrix`, `Strategy` classes with core logic.
    *   Implemented basic cohort `run_simulation` function.
    *   Core unit tests established.

*   **Phase 2: Partitioned Survival Models (PSM):** **COMPLETED**
    *   `SurvivalDistribution` classes (Exponential, Weibull, LogNormal using `scipy.stats`).
    *   `PartitionedSurvivalModelDefinition` class.
    *   `Strategy` class updated to handle PSM definitions.
    *   `run_simulation` updated to execute PSM strategies.
    *   Unit tests for survival distributions, PSM definitions, and PSM simulation.

*   **Phase 3: Probabilistic Sensitivity Analysis (PSA):** **COMPLETED**
    *   `ProbabilisticDistribution` helper classes (Normal, Beta, etc.) in `distributions.py`.
    *   `Parameters.sample_and_evaluate_all()` method for generating PSA parameter sets.
    *   `run_psa()` function.
    *   `PSAResults` class for storing and accessing PSA outputs (including CE plane data, CEAC data calculation).
    *   Unit tests for PSA functionality.

*   **Phase 4: Deterministic Sensitivity Analysis (DSA):** **COMPLETED**
    *   `DSAParameterRange` helper class in `dsa_definitions.py`.
    *   `Parameters` class updated to store DSA ranges and provide `get_dsa_iteration_parameter_sets()`.
    *   `run_dsa()` function.
    *   `DSAResults` class for storing DSA outputs, including `get_tornado_data()` method.
    *   Unit tests for DSA functionality.

*   **Phase 5: Initial Results, Summaries & Basic Plotting:** **COMPLETED**
    *   Enhanced `SimulationOutput` with `summary()`, `get_cohort_trace()`, `get_aggregated_cycle_values_for_attribute()`, and `plot_cohort_trace()`.
    *   Enhanced `PSAResults` with `summary()`, `plot_ce_plane()`, `plot_ceac()`.
    *   Enhanced `DSAResults` with `summary()`, and `plot_tornado()`.
    *   Unit tests for new summary and plotting methods (execution checks).
    *   `matplotlib` added to `requirements.txt`.

*   **Phase 6: Further Analyses:** **IN PROGRESS**
    *   **Task 6.1: Model Calibration module:** COMPLETED.
    *   **Task 6.2: EVPPI/EVPI calculations:** COMPLETED.
    *   Task 6.3: Individual-level simulation (microsimulation): PENDING.
    *   **Task 6.4: Value-Based Pricing (VBP):** COMPLETED.
    *   **Task 6.5: Scenario Analysis features:** COMPLETED.

*   **Phase 7: Documentation, Examples, and Refinement:** **IN PROGRESS**
    *   **Task 7.1: Sphinx Documentation Setup:** COMPLETED.
    *   **Task 7.2: Initial Tutorials & Examples:** COMPLETED.
    *   **Task 7.3: Further detailed examples for DSA, Calibration, VoI, VBP, Scenarios:** COMPLETED.
    *   **Task 7.4: Comprehensive user guides (topical):** IN PROGRESS - Drafted guides: "Defining and Using Parameters", "Understanding and Using SimulationOutput Results", "Probabilistic Sensitivity Analysis (PSA) Workflow", "Deterministic Sensitivity Analysis (DSA) and Tornado Plots".
    *   **Task 7.5: Full API reference review and generation:** IN PROGRESS - Docstring enhancements applied to `simulation.py`, `parameters.py`, `sensitivity_analysis.py`. API review for `strategy.py` completed. Actual Sphinx build and full review still pending.

*   **Phase 8: Performance Optimization & Advanced C++ Porting:** **PENDING**

---
*This document provides a snapshot of the project's status and will be updated as development progresses.*
