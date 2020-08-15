#' Helper functions for testing the results of heRo models

#' Test All Results of a Model
test_model_results <- function(name, path, bc, vbp, dsa, scen, psa, export) {
  test_that(paste0(name, ' produces correct results.'), {
    model <- readRDS(system.file("hero", path, "model.rds", package="heRomod"))
    if (bc) test_bc_results(model, name, path)
    if (vbp) test_vbp_results(model, name, path)
    if (dsa) test_dsa_results(model, name, path)
    if (scen) test_scen_results(model, name, path)
    if (psa) test_psa_results(model, name, path)
  })
}

#' Test base case results
test_bc_results <- function(model, name, path) {
  
  # Load saved results
  bc_res <- readRDS(system.file("hero", path, "bc_res.rds", package="heRomod"))
  
  # Run model
  bc_res_test <- do.call(run_hero_bc, model)
  
  # Check that results have not changed
  expect_equal(bc_res$trace, bc_res_test$trace)
  expect_equal(bc_res$outcomes, bc_res_test$outcomes)
  expect_equal(bc_res$costs, bc_res_test$costs)
  expect_equal(bc_res$nmb, bc_res_test$nmb)
  expect_equal(bc_res$ce, bc_res_test$ce)
  expect_equal(bc_res$pairwise_ce, bc_res_test$pairwise_ce)
}

#' Test VBP results
test_vbp_results <- function(model, name, path) {
  
  # Load saved results
  vbp_res <- readRDS(system.file("hero", path, "vbp_res.rds", package="heRomod"))
  
  # Run model
  vbp_res_test <- do.call(run_hero_vbp,model)
  
  # Check that results haven't changed
  expect_equal(vbp_res$eq, vbp_res_test$eq)
}

#' Test DSA Results
test_dsa_results <- function(model, name, path) {
  
  # Setup helper functions to compare equivalent results formats
  convert_dsa_res_format <- function(res) {
    plyr::ldply(res, function(x) mutate(x$data, series = x$series, disc = x$disc, outcome = x$outcome))
  }
  convert_dsa_nmb_res_format <- function(res) {
    select(
      plyr::ldply(res, function(x) mutate(
        x$data, 
        series = x$series,
        health_outcome = x$health_outcome,
        econ_outcome = x$econ_outcome
      )),
      series, health_outcome, econ_outcome, param, base, low, high
    )
  }
  
  # Load previous results
  dsa_res <- readRDS(system.file("hero", path, "dsa_res.rds", package="heRomod"))
  
  # Run model
  bc_res_test <- do.call(run_hero_bc, model)
  dsa_res_test <- do.call(run_hero_dsa, model)
  
  ##########
  # OUTCOMES
  ##########
  
  # Check that result hasn't changed
  expect_equal(
    convert_dsa_res_format(dsa_res$outcomes),
    convert_dsa_res_format(dsa_res_test$outcomes)
  )
  # Check against base case
  bc_outcome_res <-  filter(bc_res_test$outcomes, !grepl(' vs. ', series, fixed = T)) %>%
    group_by(series, outcome, disc) %>%
    summarize(bc_res = sum(value))
  dsa_bc_outcome_res <- convert_dsa_res_format(dsa_res_test$outcomes) %>%
    group_by(series, outcome, disc) %>%
    summarize(dsa_bc_res = base[1])
  combined_outcomes_res <- left_join(bc_outcome_res, dsa_bc_outcome_res, by = c('series', 'outcome', 'disc'))
  expect_equal(combined_outcomes_res$bc_res, combined_outcomes_res$dsa_bc_res)
  
  ##########
  # COSTS
  ##########
  
  # Check that result hasn't changed
  expect_equal(
    convert_dsa_res_format(dsa_res$cost),
    convert_dsa_res_format(dsa_res_test$cost)
  )
  
  # Check against base case
  bc_cost_res <-  filter(bc_res_test$cost, !grepl(' vs. ', series, fixed = T)) %>%
    group_by(series, outcome, disc) %>%
    summarize(bc_res = sum(value))
  dsa_bc_cost_res <- convert_dsa_res_format(dsa_res_test$cost) %>%
    group_by(series, outcome, disc) %>%
    summarize(dsa_bc_res = base[1])
  combined_cost_res <- left_join(bc_cost_res, dsa_bc_cost_res, by = c('series', 'outcome', 'disc'))
  expect_equal(combined_cost_res$bc_res, combined_cost_res$dsa_bc_res)
  
  ##########
  # NMB
  ##########
  
  # Check that result hasn't changed
  expect_equal(
    convert_dsa_nmb_res_format(dsa_res$nmb),
    convert_dsa_nmb_res_format(dsa_res_test$nmb)
  )
  
  # Check that NMB Results Match Base Case
  bc_nmb_cost_res <-  filter(bc_res_test$nmb, type == 'economic') %>%
    group_by(series, outcome) %>%
    summarize(bc_outcome_res = sum(value))
  bc_nmb_outcome_res <-  filter(bc_res_test$nmb, type == 'health') %>%
    group_by(series, outcome) %>%
    summarize(bc_cost_res = sum(value))
  dsa_bc_nmb_res <- convert_dsa_nmb_res_format(dsa_res_test$nmb) %>%
    group_by(series, health_outcome, econ_outcome) %>%
    summarize(dsa_bc_res = base[1])
  
  combined_nmb_res <- tidyr::crossing(
    ref = unique(dsa_bc_nmb_res$series),
    comp = unique(dsa_bc_nmb_res$series),
    health_outcome = unique(dsa_bc_nmb_res$health_outcome),
    econ_outcome = unique(dsa_bc_nmb_res$econ_outcome),
  ) %>%
    filter(ref != comp) %>%
    mutate(series = paste0(ref, ' vs. ', comp)) %>%
    left_join(bc_nmb_cost_res, by = c('series', 'econ_outcome' = 'outcome')) %>%
    left_join(bc_nmb_outcome_res, by = c('series', 'health_outcome' = 'outcome')) %>%
    left_join(transmute(dsa_bc_nmb_res, series, health_outcome, econ_outcome, dsa_ref_res = dsa_bc_res), by = c('ref' = 'series', 'health_outcome', 'econ_outcome')) %>%
    left_join(transmute(dsa_bc_nmb_res, series, health_outcome, econ_outcome, dsa_comp_res = dsa_bc_res), by = c('comp' = 'series', 'health_outcome', 'econ_outcome')) %>%
    mutate(
      bc_res = bc_outcome_res + bc_cost_res,
      dsa_bc_res = dsa_ref_res - dsa_comp_res
    ) 
  
  expect_equal(combined_nmb_res$bc_res, combined_nmb_res$dsa_bc_res)
}

#' Test Scenario Analysis Results
test_scen_results <- function(model, name, path) {
  
  # Load previous results
  scen_res <- readRDS(system.file("hero", path, "scen_res.rds", package="heRomod"))
  
  # Run model
  scen_res_test <- do.call(run_hero_scen, model)
  
  # Check results
  expect_equal(scen_res$outcomes, scen_res_test$outcomes)
  expect_equal(scen_res$cost, scen_res_test$cost)
  expect_equal(scen_res$nmb, scen_res_test$nmb)
}

#' Test PSA Results
test_psa_results <- function(model, name, path) {
  model$psa$n <- 10
  psa_res <- do.call(run_hero_psa,model)
}

#' Test Exporting to Excel
test_export_results <- function(model, name, path) {
  withr::with_dir(new = tempdir(), {
    model$name <- 'test'
    suppressMessages(do.call(export_hero_xlsx, model))
    xl_file <- openxlsx::read.xlsx('test.xlsx')
    file.remove('test.xlsx')
    suppressMessages(do.call(package_hero_model ,model))
    file.remove('test.zip')
  })
}
