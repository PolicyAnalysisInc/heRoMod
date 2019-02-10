library(testthat)
library(heRomod)

## the Sys.setenv fixes a problem running tests
##   with parallel processing; related to
##   https://github.com/hadley/testthat/issues/86 and
##   https://github.com/hadley/testthat/issues/144

Sys.setenv("R_TESTS" = "")

test_check("heRomod")
