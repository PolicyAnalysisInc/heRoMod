context("heRo3 compatability layer works")

models_to_test <- tibble::tribble(
  ~name,                  ~filename,
   'Advanced PSM',         'checkimab',
   'Custom PSM',           'custompsm',
   'PSM w/ Curve Fitting', 'fit',
   'Markov w/ Groups',     'groups',
   'Simple Markov',        'markov',
   'Simple PSM',           'psm',
   'TA447 PSM',            'ta447'
)

models_to_test %>%
  rowwise() %>%
  group_split() %>%
  purrr::walk(function(x) {
    do.call(test_model_convert, x)
  })

test_that("Run Analysis works with converted model", {
  model <- jsonlite::read_json(
    system.file(
      "compat",
      paste0('psm', ".json"),
      package="heRomod"
    ),
    simplifyVector = T
  )
  model$analysis <- 'bc'
  expect_error({ expect_res <- do.call(run_analysis, model)}, NA)
})