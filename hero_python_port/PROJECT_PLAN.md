# Project Plan: Porting heRomod (R) to Python (`hero_py`)

This document outlines the project plan for porting the `heRomod` R package to a new Python library, tentatively named `hero_py`. This plan is based on the initial analysis and guidance provided in `AGENTS.md` (in the original R repository).

## 1. Project Goal

To create a Python library that offers core equivalent functionality to the `heRomod` R package, enabling users to define, run, and analyze health economic models in Python. The Python library should aim for a user-friendly, "Pythonic" API while maintaining the robustness and flexibility of the original R package.

## 2. Core Team & Roles
*(To be filled in by the project team)*

## 3. Scope

### 3.1. In Scope (based on `heRomod` functionality and `AGENTS.md` porting strategy):
*   **Core Model Engine:** Cohort-based Markov model simulation, partitioned survival models (PSM).
*   **Model Definition API:** Pythonic equivalents for defining parameters, states, transitions, strategies.
*   **Key Analyses:**
    *   Probabilistic Sensitivity Analysis (PSA).
    *   Deterministic Sensitivity Analysis (DSA).
    *   Model Calibration.
    *   Expected Value of Information (EVPPI/EVPI - initial focus on EVPPI as per `heRomod`).
*   **Basic Results:** Access to counts, costs, effects per cycle/state. Summaries and simple plots.
*   **Individual-level simulation:** Support for microsimulation.

### 3.2. Out of Scope (for initial versions, potential future enhancements):
*   Full reproduction of all `run_hero_*` specific wrapper functionalities initially.
*   Advanced plotting features beyond essential visualizations.
*   Direct port of R-specific NSE mechanisms if a more Pythonic alternative is chosen.
*   Full tabular input/output parity initially (focus on programmatic model definition first).
*   Value-Based Pricing (VBP) - can be added in a later phase.
*   Complex Rcpp code porting if Python alternatives with acceptable performance exist for initial phases.

## 4. Toolkit & Dependencies (Initial)
*   **Python Version:** Python 3.8+
*   **Core Data Handling:** `pandas`, `numpy`
*   **Statistical Functions/Distributions:** `scipy.stats`
*   **Optimization (for Calibration):** `scipy.optimize`
*   **Survival Analysis (for PSM):** `lifelines` or `scikit-survival` (to be evaluated for best fit)
*   **Plotting:** `matplotlib` (core), `seaborn` (statistical). `plotnine` as an option for `ggplot2`-like API.
*   **Testing Framework:** `pytest`
*   **Documentation Generator:** `Sphinx`
*   **Package Management:** (To be decided - `pip` with `requirements.txt` initially, consider `Poetry` or `PDM` later)
*   **C++ Interop (if needed for Rcpp porting):** `pybind11`

## 5. Project Phases & Timeline (High-Level Estimate)

This is an iterative process. Timeline estimates are rough and will be refined.

### Phase 0: Setup & Planning (Completed)
*   Initial analysis of `heRomod` (R).
*   Creation of `AGENTS.md` (R repo) and initial `PROJECT_PLAN.md`.
*   Setup of proxy repository structure (`hero_python_port/`).
*   Initial stubs for core classes and tests.
*   Creation of `ROADMAP_PYTHON.md`.

### Phase 1: Core Model Engine & Definition API (Est. 4-8 Weeks)

This phase focuses on building the foundational components for defining and running basic cohort-based Markov models.

*   **Task 1.1: Design and Implement Python classes (`Parameters`, `State`, `TransitionMatrix`, `Strategy`)**
    *   **Sub-Task 1.1.1: `Parameters` class:**
        *   Attributes: Internal storage for parameter definitions (mapping name to its fixed value or callable definition), maintain order of definition for sequential evaluation.
        *   Methods:
            *   `__init__(**kwargs)`: Initialize with parameter definitions.
            *   `add_param(name: str, definition: Any)`: Add/update a parameter.
            *   `get_param_definition(name: str) -> Any`: Retrieve the raw definition.
            *   `get_param_value(name: str, cycle: int, context_params: Dict[str, Any]) -> Any`: Evaluate a single parameter. Must handle fixed values and callables. The `context_params` dict will contain already evaluated parameters for the current cycle to resolve inter-dependencies.
            *   `evaluate_all(cycle: int) -> Dict[str, Any]`: Evaluate all parameters in their defined order for a given cycle, correctly handling inter-dependencies by passing the accumulating dictionary of evaluated parameters to `get_param_value`.
            *   `names` (property): Return list of parameter names in order of definition.
        *   *Key Focus:* Robust implementation of `evaluate_all` to correctly handle evaluation order and provide the necessary context for inter-dependent parameters.
    *   **Sub-Task 1.1.2: `State` class:**
        *   Attributes: `name` (str), internal storage for state value definitions (e.g., cost, QALYs; mapping attribute name to fixed value or callable).
        *   Methods:
            *   `__init__(name: str, **value_definitions)`: Initialize state with its name and attribute definitions.
            *   `define_value(attr_name: str, definition: Any)`: Add/update a state attribute.
            *   `get_value(attr_name: str, params_context: Any, cycle: int) -> Any`: Evaluate a single state attribute. `params_context` will typically be the fully evaluated `Parameters` object (or its dictionary form) for the current cycle.
            *   `evaluate_all_values(params_context: Any, cycle: int) -> Dict[str, Any]`: Evaluate all defined attributes for this state.
    *   **Sub-Task 1.1.3: `TransitionMatrix` class:**
        *   **API Design Decision:** Prioritize a method like `set_row_probs(from_state: str, to_state_probs: Dict[str, Union[float, Callable]], complement_on_diagonal: bool = True)` or `complement_key: Optional[str] = None`) for user-friendliness in defining rows, including handling of complement probabilities (where one probability in a row is `1 - sum_of_others`). A lower-level `set_prob(from_state, to_state, prob_def)` can also be available.
        *   Attributes: `state_names` (ordered list defining matrix dimensions and row/column identities), internal storage for probability definitions (flexible enough to store callables or fixed values).
        *   Methods:
            *   `__init__(state_names: List[str])`.
            *   The chosen API methods for setting probabilities (e.g., `set_row_probs`).
            *   `get_matrix(params_context: Any, cycle: int) -> np.ndarray`: Evaluate all probability definitions for the given context and return a fully populated `numpy.ndarray`. This method is CRITICAL and must ensure that each row sums to 1.0, correctly implementing the complement logic.
    *   **Sub-Task 1.1.4: `Strategy` class:**
        *   Attributes: `name` (str), a dictionary or list of `State` objects, a `TransitionMatrix` object, an optional strategy-specific `Parameters` object (which could override or augment global parameters).
        *   Methods:
            *   `__init__(name: str, states: List[State], transition_matrix: TransitionMatrix, strategy_params: Optional[Parameters] = None)`.
            *   `get_state(name: str) -> State`.
            *   Method to combine global and strategy-specific parameters for evaluation within this strategy's context.

*   **Task 1.2: Implement API for dynamic values (lambdas/callables)**
    *   **Sub-Task 1.2.1:** Ensure the evaluation methods in `Parameters`, `State`, and `TransitionMatrix` (i.e., `get_param_value`, `get_value`, `get_matrix`) can correctly execute callable definitions, passing the necessary context (e.g., a dictionary of evaluated parameters for the current cycle, the cycle number).
    *   **Sub-Task 1.2.2:** Rigorously implement the ordered evaluation logic in `Parameters.evaluate_all` so that parameters defined earlier are evaluated first and their values are available for subsequent dependent parameter definitions in the same cycle.

*   **Task 1.3: Implement basic cohort simulation loop**
    *   **Sub-Task 1.3.1:** Define the main simulation function, e.g., `run_simulation(strategy: Strategy, global_parameters: Parameters, num_cycles: int, initial_population: Dict[str, float], discount_rates: Dict[str, float] = None) -> SimulationOutput`.
    *   **Sub-Task 1.3.2:** Implement cycle iteration logic (e.g., for `cycle` in `range(num_cycles)`).
    *   **Sub-Task 1.3.3:** State population tracking: Use a `numpy` array (`current_cohort_distribution`) of length `num_states`. Initialize with `initial_population`.
    *   **Sub-Task 1.3.4:** Transition logic:
        *   Inside the loop, for each cycle:
            *   Resolve combined parameters (global + strategy-specific).
            *   Evaluate the `TransitionMatrix` for the current cycle and parameters: `tm_eval = strategy.transition_matrix.get_matrix(combined_params, cycle)`.
            *   Update cohort distribution: `next_cohort_distribution = current_cohort_distribution @ tm_eval`.
    *   **Sub-Task 1.3.5: Cost/effect accumulation:**
        *   For each state, evaluate its cost/utility attributes for the current cycle and parameters: `cost_s = strategy.get_state(s_name).get_value('cost', combined_params, cycle)`.
        *   Calculate cycle costs/effects: `cycle_total_cost = sum(cost_s * current_cohort_distribution[s_idx] for s in states)`. Store these.
        *   Accumulate discounted total costs/effects over cycles.
    *   **Sub-Task 1.3.6: Discounting:** Implement logic to apply discount rates. Define clearly whether discounting applies to outcomes accrued at the start, mid-point, or end of the cycle.
    *   **Sub-Task 1.3.7:** Define the `SimulationOutput` data structure (e.g., a class or a dictionary of `pandas` DataFrames) to hold time-series of cohort populations per state, aggregated costs/effects per cycle, and total discounted costs/effects.

*   **Task 1.4: Core unit tests (`pytest`)**
    *   **Sub-Task 1.4.1: `test_parameters.py`:** Expand tests for `Parameters.evaluate_all` with various inter-dependencies (e.g., A depends on B, B depends on C). Test error handling for circular dependencies (if detection is implemented), undefined parameters, and incorrect callable signatures.
    *   **Sub-Task 1.4.2: `test_state.py`:** Expand tests for `State.evaluate_all_values`, ensuring correct interaction with different `Parameters` contexts. Test error handling for undefined attributes.
    *   **Sub-Task 1.4.3: `test_transitions.py` (New file):**
        *   Test `TransitionMatrix` instantiation with valid and invalid state lists.
        *   Test the chosen API for setting probabilities (e.g., `set_row_probs`), including fixed values and callables.
        *   Test `TransitionMatrix.get_matrix()` for correct evaluation of probabilities (fixed and callable) given parameters and cycle.
        *   **Critical:** Test the complement probability logic extensively to ensure rows always sum to 1.0 under various definition scenarios (e.g., one probability missing, one explicitly marked as complement).
        *   Test error handling (e.g., row sums not equaling 1 if strict checking is enabled, undefined states in probability definitions).
    *   **Sub-Task 1.4.4: `test_strategy.py` (New file):** Test `Strategy` instantiation, including validation of state and transition matrix alignment. Test retrieval of states. Test parameter overriding/layering if implemented.
    *   **Sub-Task 1.4.5: `test_simulation.py` (New file):**
        *   Create very simple 2-state or 3-state models with fixed, known parameters and transition probabilities.
        *   Test `run_simulation` against manually calculated outcomes for population distribution over a few cycles.
        *   Test cost and effect accumulation (undiscounted and discounted).
        *   Test edge cases: 0 cycles, initial population entirely in one state.

*   **Task 1.5: Refine basic package structure and `requirements.txt`**
    *   **Sub-Task 1.5.1:** Ensure `hero_py/__init__.py` correctly exports all public classes from Phase 1.
    *   **Sub-Task 1.5.2:** Confirm `requirements.txt` (initially `pandas`, `numpy`, `scipy`, `pytest`) is adequate. Add any minor dependencies discovered during Phase 1 implementation (e.g., `typing_extensions` if using newer typing features for older Python 3.8).

### Phase 2: Partitioned Survival Models (PSM) (Est. 3-5 Weeks)
*   Task 2.1: Integrate survival curve fitting/representation.
*   Task 2.2: Implement PSM logic for state proportions.
*   Task 2.3: Integrate PSM definition into `Strategy` or specialized class.
*   Task 2.4: Unit tests for PSM.

### Phase 3: Probabilistic Sensitivity Analysis (PSA) (Est. 4-6 Weeks)
*   Task 3.1: Extend `Parameters` for distributional definitions.
*   Task 3.2: Implement `run_psa` function.
*   Task 3.3: Basic PSA result storage and access.
*   Task 3.4: Unit tests for PSA.

### Phase 4: Deterministic Sensitivity Analysis (DSA) (Est. 3-4 Weeks)
*   Task 4.1: Extend `Parameters` for defining ranges.
*   Task 4.2: Implement `run_dsa` function.
*   Task 4.3: Basic DSA result storage and access.
*   Task 4.4: Unit tests for DSA.

### Phase 5: Initial Results, Summaries & Basic Plotting (Est. 2-4 Weeks)
*   Task 5.1: Implement `summary()` methods for results objects.
*   Task 5.2: Implement basic `plot()` methods (CE plane, tornado).
*   Task 5.3: Accessor functions (`get_counts`, `get_values`).

### Phase 6: Further Analyses (Iterative)
*   Task 6.1: Model Calibration module.
*   Task 6.2: EVPPI/EVPI calculations.
*   Task 6.3: Individual-level simulation (microsimulation).
*   Task 6.4: Value-Based Pricing.
*   Task 6.5: Scenario Analysis.

### Phase 7: Documentation, Examples, and Refinement (Ongoing)
*   Task 7.1: API documentation (Sphinx).
*   Task 7.2: Example scripts/notebooks.
*   Task 7.3: User guides.

### Phase 8: Performance Optimization & Advanced C++ Porting (As needed)
*   Task 8.1: Profiling and optimization.
*   Task 8.2: Porting/wrapping C++ code via `pybind11` if necessary.

## 6. Milestones (High-Level)
*   **M1 (End of Phase 1):** Core model engine runnable for simple cohort models.
*   **M2 (End of Phase 2):** PSM functionality integrated.
*   **M3 (End of Phase 3 & 4):** PSA and DSA capabilities available.
*   **M4 (End of Phase 5):** Basic results, summaries, and plots. Minimally Viable Product.
*   **Subsequent Milestones:** Completion of each module in Phase 6.

## 7. Testing Strategy
*   Unit tests (`pytest`) for all core components.
*   High test coverage (>80-90%).
*   Validation against R `heRomod` outputs where feasible.
*   Integration tests.

## 8. Documentation Strategy
*   Docstrings (Google/NumPy style).
*   User guides and tutorials (Sphinx).
*   API reference (Sphinx).
*   Example scripts and Jupyter notebooks.

## 9. Risks & Mitigation
*   **Complexity of NSE Porting:** (Mitigation: Pythonic alternatives like lambdas, careful API design).
*   **Performance:** (Mitigation: Vectorization, profiling, Numba/pybind11 if needed).
*   **Dependency Differences:** (Mitigation: Careful library selection and testing).
*   **Scope Creep:** (Mitigation: Phased approach, prioritize core features).
*   **Resource Availability:** (Mitigation: Clear roles, realistic timeline).

---
*This `PROJECT_PLAN.md` is a living document and will be updated as the project progresses.*
