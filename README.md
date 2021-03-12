  <!-- badges: start -->
  [![CircleCI build status](https://circleci.com/gh/PolicyAnalysisInc/heRoMod.svg?style=svg)](https://circleci.com/gh/PolicyAnalysisInc/heRoMod)
 [![Coverage status](https://codecov.io/gh/PolicyAnalysisInc/heRoMod/branch/master/graph/badge.svg)](https://codecov.io/github/PolicyAnalysisInc/heRoMod?branch=master)
  <!-- badges: end -->

# heRomod - A Package for Health Economic Modeling

The heRomod package allows users to develop and run cost-effectiveness models in R and is the calculation engine used in the heRo web-based modeling platform.  A fork of the HEEMOD package.

You can install the latest released version from github with:

```r
if(!require(remotes)) {
  install.packages('remotes')
}
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
remotes::install_github("PolicyAnalysisInc/heRomod")
```

## Features

  * Support for Markov cohort and partitioned-survival models.
  * Comprehensive tools for defining and operation of survival distribtions.
  * Probabilistic uncertainty analysis (PSA).
    * Cost-effectiveness acceptability curves (CEAC).
    * Expected value of perfect information (EVPI).
    * Expected value of partially perfect information (EVPPI).
  * Deterministic sensitivity analysis (DSA).
  * Value-based pricing analysis (VBP).

## Devs
  * [Jordan Amdahl](https://github.com/jrdnmdhl)

## Contributors
  * [Kevin Zarca](http://www.urc-eco.fr/Kevin-ZARCA,402) and [Antoine Filipović-Pierucci](https://pierucci.org) (authors of HEEMOD).
  * [Matthew Wiener](https://github.com/MattWiener)
  * [Zdenek Kabat](https://github.com/zkabat)
  * [Vojtech Filipec](https://github.com/vojtech-filipec)
  * [Yonatan Carranza Alarcon](https://github.com/salmuz)
  * [Vince Daniels](https://github.com/daniels4321)
  * [Fernando Alarid](https://github.com/feralaes)
  
