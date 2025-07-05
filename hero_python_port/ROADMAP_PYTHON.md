# Roadmap for `hero_py`: A Python Library for Health Economic Modeling

This document outlines the long-term vision and potential future directions for `hero_py`, the Python port of the `heRomod` R package. This roadmap assumes the successful completion of the initial porting phases as detailed in `PROJECT_PLAN.md`.

## Vision for `hero_py`

`hero_py` aims to become a leading open-source Python library for health economic modeling, recognized for its:
*   **Comprehensive Functionality:** Supporting a wide array of modeling techniques (Markov, PSM, DES, microsimulation) and analyses (PSA, DSA, VoI, calibration).
*   **Pythonic API:** Offering an intuitive, user-friendly interface that aligns with Python best practices and integrates well with the PyData ecosystem.
*   **Performance and Scalability:** Providing efficient computation for complex models and large-scale simulations.
*   **Extensibility:** Designing a modular architecture that allows for community contributions and future expansion.
*   **Educational Value:** Serving as a valuable tool for teaching and learning health economic modeling concepts in a Python environment.
*   **Interoperability:** Facilitating easy integration with other Python libraries for data analysis, machine learning, and visualization.

## Post-Initial Porting: Medium-Term Goals (1-3 years after initial functional port)

Once the core functionality from `heRomod` is ported and stabilized (as per `PROJECT_PLAN.md`):

### 1. API Refinement and Pythonization
*   **User Feedback Driven API:** Actively solicit and incorporate user feedback to refine the API.
*   **Enhanced `pandas` Integration:** Seamless input/output with `pandas` DataFrames.
*   **Method Chaining:** Support for fluent model construction.
*   **Configuration Management:** Advanced options (e.g., YAML/JSON config files).

### 2. Advanced Modeling Features
*   **Discrete Event Simulation (DES):** Implement or integrate a DES engine.
*   **Agent-Based Modeling (ABM) Stubs:** Basic ABM capabilities or integration hooks.
*   **Dynamic Transmission Models:** Support for basic infectious disease modeling.
*   **Advanced Calibration Techniques:** Bayesian calibration (e.g., via PyMC, Stan).

### 3. Enhanced Analytics and Visualization
*   **Interactive Plotting:** Integrate `Plotly` or `Bokeh`.
*   **Advanced VoI Analysis:** Expand EVPPI, implement EVSI.
*   **Reporting Automation:** Tools/templates for HE reports (e.g., with `Streamlit` or `Dash`).

### 4. Performance and Scalability
*   **Targeted Optimizations:** Continuous profiling and optimization.
*   **Dask/Ray Integration:** For distributed computing of large-scale simulations.
*   **GPU Acceleration:** Investigate for applicable algorithms.

### 5. Interoperability and Ecosystem
*   **`scikit-learn` Compatibility:** Easy integration with `scikit-learn` pipelines.
*   **Standardized Model Exchange Formats:** Explore import/export capabilities.

## Long-Term Aspirations (3+ years)
*   **Comprehensive Model Validation Suite.**
*   **AI/ML for HE Modeling:** Surrogate modeling, automated parameter estimation, etc.
*   **Cloud Computing Integration.**
*   **Building a Strong Community.**
*   **Educational Hub.**

## Current Development Focus (Update as Phases Complete)
*   **Phase 1 (Core Model Engine & Definition API):** COMPLETED
*   **Phase 2 (Partitioned Survival Models - PSM):** COMPLETED
*   **Phase 3 (Probabilistic Sensitivity Analysis - PSA):** COMPLETED
*   **Phase 4 (Deterministic Sensitivity Analysis - DSA):** COMPLETED
*   **Phase 5 (Initial Results, Summaries & Basic Plotting):** COMPLETED
*   **Phase 6 (Further Analyses):** IN PROGRESS
    *   Model Calibration: COMPLETED.
    *   EVPI & EVPPI (initial regression-based): COMPLETED.
    *   Scenario Analysis (parameter-based): COMPLETED.
    *   Value-Based Pricing (VBP - initial implementation): COMPLETED.
*   **Documentation & Examples (Phase 7):** IN PROGRESS - Initial Sphinx setup, API stubs, getting started tutorial, and detailed examples for all core analyses completed. Topical guides ("Defining Parameters", "Simulation Outputs", "PSA Workflow", "DSA Workflow", "Calibration Workflow", "VoI Guide", "VBP Guide") drafted. API docstring enhancements for key modules applied.
*   **Next Up (Phase 6 Continued):** Focus on full individual-level simulation.
*   **Next Up (Phase 7 Continued):** More topical guides (Scenarios, Plotting), full API doc generation and actual build/review, further docstring refinements for all modules.

## Contribution and Evolution
This roadmap is a living document. Contributions are welcome via GitHub issues for the future Python repository.
---
