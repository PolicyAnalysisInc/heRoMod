<!-- badges: start -->
[![CircleCI build status](https://circleci.com/gh/PolicyAnalysisInc/heRoMod.svg?style=svg)](https://circleci.com/gh/PolicyAnalysisInc/heRoMod)
[![Coverage status](https://codecov.io/gh/PolicyAnalysisInc/heRoMod/branch/master/graph/badge.svg)](https://codecov.io/github/PolicyAnalysisInc/heRoMod?branch=master)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

# heRomod - A Package for Health Economic Modeling

The `heRomod` package provides a comprehensive suite of tools for developing, running, and analyzing health economic models in R. It serves as the calculation engine for the [heRo](https://heroapps.io/) web-based modeling platform and is a fork of the [HEEMOD](https://www.rdocumentation.org/packages/heemod/versions/1.0.1) package.

## Installation

### For Users

To install the latest version of `heRomod` from GitHub, along with its core dependencies:

```R
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true") # Optional: suppress warnings during install

# Install core dependencies if not already present
remotes::install_github("PolicyAnalysisInc/herotools")
remotes::install_github("PolicyAnalysisInc/herosurv")

# Install heRomod
remotes::install_github("PolicyAnalysisInc/heRomod")
```
You may be prompted to update packages; selecting "All" (or equivalent) is usually safe.

### For Developers

If you wish to contribute to `heRomod` or develop it locally:

1.  **Prerequisites:**
    *   Install R (>= 3.5.0). See [CRAN](https://cran.r-project.org/).
    *   For macOS users, consider installing R via `brew install r`. Additional system libraries might be needed for some R package dependencies (e.g., `libgit2`, `harfbuzz`, `fribidi`, etc.).
    *   Install `git`.
2.  **Clone the Repository:**
    ```bash
    git clone https://github.com/PolicyAnalysisInc/heRoMod.git
    cd heRoMod
    ```
3.  **Install Development Dependencies:**
    Open an R session in the cloned directory and run:
    ```R
    # Install devtools and core dependencies
    install.packages(c("devtools", "remotes"))
    remotes::install_github("PolicyAnalysisInc/herotools")
    remotes::install_github("PolicyAnalysisInc/herosurv")

    # Install other development tools (optional, but recommended)
    install.packages(c("usethis", "pkgdown", "styler", "lintr", "covr", "testthat"))

    # Install heRomod dependencies (excluding heRomod itself)
    devtools::install_deps(dependencies = TRUE)
    ```
4.  **Build and Test:**
    ```R
    devtools::install(dependencies = FALSE) # Install local version of heRomod
    devtools::load_all()          # Load package for interactive use
    devtools::test()              # Run tests (can be time-consuming)
    devtools::check()             # Run R CMD check
    ```

## Quick Start

Here's a minimal example of defining and running a simple 2-state Markov model:

```R
library(heRomod)

# Define a strategy with two states and a transition matrix
strategy_1 <- define_strategy(
  transition = define_transition(
    .5, .5,  # From state 1 to state 1, state 1 to state 2
    .1, .9   # From state 2 to state 1, state 2 to state 2
  ),
  state_1 = define_state(
    cost = 543,
    ly = 1
  ),
  state_2 = define_state(
    cost = 432,
    ly = 1
  )
)

# Run the model
model_results <- run_model(
  strategy_1,
  init = c(population_in_state_1 = 100, population_in_state_2 = 0), # Initial distribution
  cycles = 10,      # Number of cycles to run
  cost = cost,      # Name of the cost variable in states
  effect = ly       # Name of the effectiveness variable in states
)

# Print results summary
print(model_results)

# Access detailed results
# model_results$run_model_results
# model_results$run_model_summary
```
For more detailed examples and advanced features, please see the package vignettes.

## Features

*   Support for Markov cohort and partitioned-survival models.
*   Comprehensive tools for defining and operating survival distributions.
*   Probabilistic uncertainty analysis (PSA).
    *   Cost-effectiveness acceptability curves (CEAC).
    *   Expected value of perfect information (EVPI).
    *   Expected value of partially perfect information (EVPPI).
*   Deterministic sensitivity analysis (DSA).
*   Value-based pricing analysis (VBP).

## Contributing

Contributions to `heRomod` are welcome! If you encounter a bug, have a feature request, or would like to contribute code, please:

1.  Check the [issue tracker](https://github.com/PolicyAnalysisInc/heRoMod/issues) to see if your issue or idea has already been discussed.
2.  For bugs or feature requests, please open a new issue.
3.  For code contributions, please fork the repository, make your changes on a new branch, and submit a pull request. Ensure your code adheres to the existing style and includes tests where appropriate.

## Development Team & Contributors

*   **Lead Developer:** [Jordan Amdahl](https://github.com/jrdnmdhl)
*   **Original HEEMOD Authors:** [Kevin Zarca](http://www.urc-eco.fr/Kevin-ZARCA,402), [Antoine Filipović-Pierucci](https://pierucci.org)
*   **Contributors:**
    *   [Matthew Wiener](https://github.com/MattWiener)
    *   [Zdenek Kabat](https://github.com/zkabat)
    *   [Vojtech Filipec](https://github.com/vojtech-filipec)
    *   [Yonatan Carranza Alarcon](https://github.com/salmuz)
    *   [Vince Daniels](https://github.com/daniels4321)
    *   [Fernando Alarid-Escudero](https://github.com/feralaes)

## License

`heRomod` is licensed under the GPL (>= 3). See the [COPYING](COPYING) file for more details.
