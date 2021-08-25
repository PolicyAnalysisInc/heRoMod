#' Helper functions for testing the results of heRo models

#' Test All Results of a Model
test_model_results <- function(name, path, bc, vbp, dsa, twsa, scen, psa, export) {
  test_that(paste0(name, ' produces correct results.'), {
    model <- readRDS(system.file("hero", path, "model.rds", package="heRomod"))
    model$cores <- 1
    if (bc) test_bc_results(model, name, path)
    if (vbp) test_vbp_results(model, name, path)
    if (dsa) test_dsa_results(model, name, path, vbp = F)
    if (dsa && vbp) test_dsa_results(model, name, path, vbp = T)
    if (twsa) test_twsa_results(model, name, path, vbp = F)
    if (twsa && vbp) test_twsa_results(model, name, path, vbp = T)
    if (scen) test_scen_results(model, name, path)
    if (scen && vbp) test_scen_results(model, name, path, vbp = T)
    if (psa) test_psa_results(model, name, path)
    if (export) test_export_results(model, name, path)
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
test_dsa_results <- function(model, name, path, vbp = F) {
  
  if (vbp) {
    model$dsa_settings$run_vbp <- T
  }
  
  # Setup helper functions to compare equivalent results formats
  convert_dsa_res_format <- function(res) {
    plyr::ldply(res, function(x) mutate(x$data, series = x$series, disc = x$disc, outcome = x$outcome)) %>%
      arrange(series, disc, outcome, param)
  }
  convert_dsa_nmb_res_format <- function(res) {
    plyr::ldply(res, function(x) mutate(
      x$data, 
      series = x$series,
      health_outcome = x$health_outcome,
      econ_outcome = x$econ_outcome
    )) %>%
    select(series, health_outcome, econ_outcome, param, base, low, high) %>%
    arrange(series, health_outcome, econ_outcome, param)
  }
  
  # Load previous results
  dsa_res <- readRDS(system.file("hero", path, "dsa_res.rds", package="heRomod"))
  
  # Run model
  dsa_res_test <- do.call(run_hero_dsa, model)
  
  # Load base case and vbp results for comparison
  bc_res <- readRDS(system.file("hero", path, "bc_res.rds", package="heRomod"))
  vbp_res <- readRDS(system.file("hero", path, "vbp_res.rds", package="heRomod"))
  
  ##########
  # OUTCOMES
  ##########
  
  # Check that result hasn't changed
  expect_equal(
    convert_dsa_res_format(dsa_res$outcomes),
    convert_dsa_res_format(dsa_res_test$outcomes)
  )
  # Check against base case
  bc_outcome_res <-  filter(bc_res$outcomes, !grepl(' vs. ', series, fixed = T)) %>%
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
  bc_cost_res <-  filter(bc_res$cost, !grepl(' vs. ', series, fixed = T)) %>%
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
  bc_nmb_cost_res <-  filter(bc_res$nmb, type == 'economic') %>%
    group_by(series, outcome) %>%
    summarize(bc_outcome_res = sum(value))
  bc_nmb_outcome_res <-  filter(bc_res$nmb, type == 'health') %>%
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
  
  ##########
  # VBP
  ##########
  
  # If not running VBP, then expect result to be null
  if (!vbp) {
    expect_equal(dsa_res_test$vbp, NULL)
  } else {
    
    # Check that result hasn't changed
    #expect_equal(dsa_res$vbp, dsa_res_test$vbp)
    
    # Check that against base case VBP
    wtp <- filter(model$hsumm, name == model$vbp$effect)$wtp[1]
    bc_vbp_res <- vbp_res$eq %>%
      mutate(value = a * wtp + b) %>%
      select(strat, value) %>%
      arrange(strat)
    
    dsa_bc_vbp_res <-  purrr::map(
      dsa_res_test$vbp$prices,
      function(x) transmute(x$data[1, ], strat = x$series, value =  base)
    ) %>%
      bind_rows() %>%
      arrange(strat)
    
    expect_equal(bc_vbp_res, dsa_bc_vbp_res)
  }
  
  
}

#' Test TWSA Results
test_twsa_results <- function(model, name, path, vbp = F) {
  
  if (vbp) {
    model$twsa_settings$run_vbp <- T
  } else {
    model$twsa_settings$run_vbp <- F
  }
  
  # Load previous results
  twsa_res <- readRDS(system.file("hero", path, "twsa_res.rds", package="heRomod"))
  
  # Run model
  twsa_res_test <- do.call(run_hero_twsa, model)
  
  convert_twsa_res_format <- function(res, sort_vars = c('id', 'series', 'outcome', 'disc', 'x', 'y')) {
    res %>%
      map(function(x) cbind(x[setdiff(names(x), 'data')], x$data)) %>%
      bind_rows() %>%
      arrange_at(sort_vars)
  }
  
  # Load base case and vbp results for comparison
  bc_res <- readRDS(system.file("hero", path, "bc_res.rds", package="heRomod"))
  vbp_res <- readRDS(system.file("hero", path, "vbp_res.rds", package="heRomod"))
  
  ##########
  # OUTCOMES
  ##########
  
  # Check that result hasn't changed
  expect_equal(twsa_res$outcomes, twsa_res_test$outcomes)
  
  # Check against base case
  bc_outcome_res <-  filter(bc_res$outcomes, !grepl(' vs. ', series, fixed = T)) %>%
    group_by(series, outcome, disc) %>%
    summarize(bc_res = sum(value))
  
  twsa_bc_outcome_res <- convert_twsa_res_format(twsa_res_test$outcomes) %>%
    filter(isBaseCase, id == id[1]) %>%
    group_by(series, outcome, disc) %>%
    summarize(twsa_bc_res = value[1])
  
   combined_outcomes_res <- left_join(bc_outcome_res, twsa_bc_outcome_res, by = c('series', 'outcome', 'disc'))
   
  expect_equal(combined_outcomes_res$bc_res, combined_outcomes_res$twsa_bc_res)
  
  ##########
  # COSTS
  ##########
  
  # Check that result hasn't changed
  expect_equal(twsa_res$cost, twsa_res_test$cost)
  
  # Check against base case
  bc_cost_res <-  filter(bc_res$cost, !grepl(' vs. ', series, fixed = T)) %>%
    group_by(series, outcome, disc) %>%
    summarize(bc_res = sum(value))
  
  twsa_bc_cost_res <- convert_twsa_res_format(twsa_res_test$cost) %>%
    filter(isBaseCase, id == id[1]) %>%
    group_by(series, outcome, disc) %>%
    summarize(twsa_bc_res = value[1])
  
  combined_cost_res <- left_join(bc_cost_res, twsa_bc_cost_res, by = c('series', 'outcome', 'disc'))
  
  expect_equal(combined_cost_res$bc_res, combined_cost_res$twsa_bc_res)
  

  ##########
  # NMB
  ##########
  
  # Check that result hasn't changed
  expect_equal(twsa_res$nmb, twsa_res_test$nmb)
  
  # Check that NMB Results Match Base Case
  bc_nmb_cost_res <-  filter(bc_res$nmb, type == 'economic') %>%
    group_by(series, outcome) %>%
    summarize(bc_outcome_res = sum(value)) %>%
    ungroup()
  bc_nmb_outcome_res <-  filter(bc_res$nmb, type == 'health') %>%
    group_by(series, outcome) %>%
    summarize(bc_cost_res = sum(value)) %>%
    ungroup()
  
  twsa_bc_nmb_res <- convert_twsa_res_format(twsa_res_test$nmb, c('series', 'health_outcome', 'econ_outcome')) %>%
    filter(isBaseCase, id == id[1]) %>%
    group_by(series, health_outcome, econ_outcome) %>%
    summarize(twsa_bc_res = value[1])
  
  combined_nmb_res <- tidyr::crossing(
    ref = unique(twsa_bc_nmb_res$series),
    comp = unique(twsa_bc_nmb_res$series),
    health_outcome = unique(twsa_bc_nmb_res$health_outcome),
    econ_outcome = unique(twsa_bc_nmb_res$econ_outcome),
  ) %>%
    filter(ref != comp) %>%
    mutate(series = paste0(ref, ' vs. ', comp)) %>%
    left_join(bc_nmb_cost_res, by = c('series', 'econ_outcome' = 'outcome')) %>%
    left_join(bc_nmb_outcome_res, by = c('series', 'health_outcome' = 'outcome')) %>%
    left_join(transmute(twsa_bc_nmb_res, series, health_outcome, econ_outcome, twsa_ref_res = twsa_bc_res), by = c('ref' = 'series', 'health_outcome', 'econ_outcome')) %>%
    left_join(transmute(twsa_bc_nmb_res, series, health_outcome, econ_outcome, twsa_comp_res = twsa_bc_res), by = c('comp' = 'series', 'health_outcome', 'econ_outcome')) %>%
    mutate(
      bc_res = bc_outcome_res + bc_cost_res,
      twsa_bc_res = twsa_ref_res - twsa_comp_res
    ) 
  
  expect_equal(combined_nmb_res$bc_res, combined_nmb_res$twsa_bc_res)

  
  ##########
  # VBP
  ##########
  
  # If not running VBP, then expect result to be null
  if (!vbp) {
    expect_equal(twsa_res_test$vbp, NULL)
  } else {
    
    # Check that result hasn't changed
    expect_equal(twsa_res$vbp, twsa_res_test$vbp)
    
    # Check that against base case VBP
    wtp <- filter(model$hsumm, name == model$vbp$effect)$wtp[1]
    bc_vbp_res <- vbp_res$eq %>%
      mutate(value = a * wtp + b) %>%
      select(strat, value) %>%
      arrange(strat)

    twsa_bc_vbp_res <- purrr::map(
      twsa_res_test$vbp$prices,
      function(analysis) transmute(filter(analysis$data, isBaseCase), id = analysis$id, strat =analysis$series, value = value)
    ) %>%
      bind_rows() %>%
      filter(id == id[1]) %>%
      arrange(strat) %>%
      select(strat, value)

    expect_equal(bc_vbp_res, twsa_bc_vbp_res)
  }
  
  
}


#' Test Scenario Analysis Results
test_scen_results <- function(model, name, path, vbp = F) {

  # Load previous results
  if (vbp) {
    model$scenario_settings <- list(run_vbp = T)
    scen_res <- readRDS(system.file("hero", path, "scen_vbp_res.rds", package="heRomod"))
  } else {
    scen_res <- readRDS(system.file("hero", path, "scen_res.rds", package="heRomod"))
  }
  
  convert_scen_res_format <- function(res, sort_vars = c('outcome', 'disc', 'series')) {
    res %>%
      map(function(x) cbind(x[setdiff(names(x), 'data')], x$data, stringsAsFactors = F)) %>%
      bind_rows() %>%
      arrange_at(sort_vars)
  }
  
  # Run model
  scen_res_test <- do.call(run_hero_scen, model)
  
  # Check results
  expect_equal(
    convert_scen_res_format(scen_res$outcomes),
    convert_scen_res_format(scen_res_test$outcomes)
  )
  expect_equal(
    convert_scen_res_format(scen_res$cost),
    convert_scen_res_format(scen_res_test$cost)
  )
  expect_equal(
    convert_scen_res_format(scen_res$nmb, c('health_outcome', 'econ_outcome', 'series')),
    convert_scen_res_format(scen_res_test$nmb, c('health_outcome', 'econ_outcome', 'series'))
  )
  if (vbp) {
    expect_equal(
      scen_res$vbp$referent,
      scen_res_test$vbp$referent
    )
    expect_equal(
      convert_scen_res_format(scen_res$vbp$prices, c('series')),
      convert_scen_res_format(scen_res_test$vbp$prices, c( 'series'))
    )
  }
}

#' Test PSA Results
test_psa_results <- function(model, name, path) {
  model$psa$n <- 5
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
