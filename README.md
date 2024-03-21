  <!-- badges: start -->

[![CircleCI build status](https://circleci.com/gh/PolicyAnalysisInc/heRoMod.svg?style=svg)](https://circleci.com/gh/PolicyAnalysisInc/heRoMod)
[![Coverage status](https://codecov.io/gh/PolicyAnalysisInc/heRoMod/branch/master/graph/badge.svg)](https://codecov.io/github/PolicyAnalysisInc/heRoMod?branch=master)

  <!-- badges: end -->

# heRomod - A Package for Health Economic Modeling

The heRomod package allows users to develop and run cost-effectiveness models in R and is the calculation engine used in the heRo web-based modeling platform. A fork of the [HEEMOD](https://www.rdocumentation.org/packages/heemod/versions/1.0.1) package.

You can install the latest released version from github with:

## Developing Locally

If you want to run the package locally from source

1. Install R (>= 3.4.0) for your platform.
    1. For Windows users, it is recommended to check Save version number in registry during installation so that the R extension can find the R executable automatically.
    1. For mac users, use brew to install R and dependencies in terminal
        1. `brew install r`
        1. `brew install libgit2 harfbuzz fribidi freetype2 libpng libtiff libjpeg pandoc`
1. Open an R terminal executing `r` in your terminal

    - In some shells, `r` is a built-in command to re-run the last command.
        - You can verify it running `which r`.
        - If the response is `r: shell built-in command` you will need to alias it.
        - First check the path: `whereis r`
        - Then open you `rc` file (`.bashrc`, `.zshrc` or the one you use it) and add:
          `alias r='<your-path-to-r>'`.
        - For example: `alias r='/opt/homebrew/bin/r'`
        - Restart your shell

1. Run the following into your R terminal:

    ```r
    install.packages("remotes")
    install.packages("languageserver")
    install.packages("usethis")
    install.packages("pkgdown")
    install.packages("devtools")
    remotes::install_github("PolicyAnalysisInc/herotools")
    remotes::install_github("PolicyAnalysisInc/herosurv")
    ```

1. Install the R extension for VS Code from the [VS Code Extension Marketplace](https://marketplace.visualstudio.com/items?itemName=reditorsupport.r)

1. Test the build command
    1. `devtools::install(dependencies=F)`
    1. `devtools::build(vignettes=T)`
1. Run the tests (note - this will take significant time, especially running the heRo model)
    1. `devtools::test()`

## Running the heroMod package locally

1. Install the Github package manager and Hero remotes

    ```r
    if(!require(remotes)) {
      install.packages('remotes')
    }
    Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
    remotes::install_github("PolicyAnalysisInc/herotools")
    remotes::install_github("PolicyAnalysisInc/herosurv")
    ```

1. Install heromod (if prompted you can install "All")
    ```r
    remotes::install_github("PolicyAnalysisInc/heRomod")
    ```

## Features

-   Support for Markov cohort and partitioned-survival models.
-   Comprehensive tools for defining and operation of survival distribtions.
-   Probabilistic uncertainty analysis (PSA).
    -   Cost-effectiveness acceptability curves (CEAC).
    -   Expected value of perfect information (EVPI).
    -   Expected value of partially perfect information (EVPPI).
-   Deterministic sensitivity analysis (DSA).
-   Value-based pricing analysis (VBP).

## Devs

-   [Jordan Amdahl](https://github.com/jrdnmdhl)

## Contributors

-   [Kevin Zarca](http://www.urc-eco.fr/Kevin-ZARCA,402) and [Antoine Filipović-Pierucci](https://pierucci.org) (authors of HEEMOD).
-   [Matthew Wiener](https://github.com/MattWiener)
-   [Zdenek Kabat](https://github.com/zkabat)
-   [Vojtech Filipec](https://github.com/vojtech-filipec)
-   [Yonatan Carranza Alarcon](https://github.com/salmuz)
-   [Vince Daniels](https://github.com/daniels4321)
-   [Fernando Alarid](https://github.com/feralaes)
