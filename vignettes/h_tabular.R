## ---- echo=FALSE, include=FALSE------------------------------------------
library(heRomod)
library(dplyr)
format_na <- function(x, char = " ") {
  x[is.na(x)] <- char
  x
}

## ----echo = FALSE--------------------------------------------------------
heRomod:::read_file(system.file("tabular/thr/REFERENCE.csv", package = "heRomod")) %>% 
  format_na %>% 
  knitr::kable()

## ----echo = FALSE--------------------------------------------------------
heRomod:::read_file(system.file("tabular/thr/THR_states.csv", package = "heRomod")) %>% 
  format_na %>% 
  knitr::kable()

## ----echo = FALSE--------------------------------------------------------
heRomod:::read_file(system.file("tabular/thr/THR_transition_probs.csv", package = "heRomod")) %>% 
  format_na %>% 
  knitr::kable()

## ----echo = FALSE--------------------------------------------------------
heRomod:::read_file(system.file("tabular/thr/THR_parameters.csv", package = "heRomod")) %>% 
  format_na %>% 
  knitr::kable()

## ----echo = FALSE--------------------------------------------------------
heRomod:::read_file(system.file("tabular/thr/THR_options.csv", package = "heRomod")) %>% 
  format_na %>% 
  knitr::kable(row.names = FALSE)

## ------------------------------------------------------------------------
result <- run_model_tabular(
  location = system.file("tabular/thr", package = "heRomod")
)

## ---- fig.width = 6, fig.align='center'----------------------------------
result$model_runs
plot(result$psa,
     type = "ce")
plot(result$dsa,
     result = "cost",
     strategy = "new")
result$demographics

