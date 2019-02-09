context("Running heRo model")




test_that(
  "Example Simple PSM Runs Correctly", {
    example_simple_psm <- readRDS(system.file("hero","example_simple_psm", "model.rds", package="heRomod"))
    
    # Base Case Results
    bc_res <- readRDS(system.file("hero","example_simple_psm", "bc_res.rds", package="heRomod"))
    bc_res_test <- do.call(run_hero_bc, example_simple_psm)
    
    expect_equal(
      bc_res$trace,
      bc_res_test$trace
    )
    
    expect_equal(
      bc_res$outcomes,
      bc_res_test$outcomes
    )
    
    expect_equal(
      bc_res$costs,
      bc_res_test$costs
    )
    
    expect_equal(
      bc_res$nmb,
      bc_res_test$nmb
    )
    
    expect_equal(
      bc_res$ce,
      bc_res_test$ce
    )
    
    vbp_res <- do.call(run_hero_vbp,example_simple_psm)
    dsa_res <- do.call(run_hero_dsa,example_simple_psm)
    psa_res <- do.call(run_hero_psa,example_simple_psm)
    withr::with_dir(new = tempdir(), {
      example_simple_psm$name <- 'test.xlsx'
      do.call(export_hero_xlsx,example_simple_psm)
    })
    
})
