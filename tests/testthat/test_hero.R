context("Running heRo model")

test_that(
  "Example Simple PSM Runs Correctly", {
    model <- readRDS(system.file("hero","example_simple_psm", "model.rds", package="heRomod"))
    
    # Base Case Results
    bc_res <- readRDS(system.file("hero","example_simple_psm", "bc_res.rds", package="heRomod"))
    bc_res_test <- do.call(run_hero_bc, model)
    
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
    
    vbp_res <- readRDS(system.file("hero","example_simple_psm", "vbp_res.rds", package="heRomod"))
    vbp_res_test <- do.call(run_hero_vbp,model)
    
    
    expect_equal(
      vbp_res$eq,
      vbp_res_test$eq
    )
    
    dsa_res <- readRDS(system.file("hero","example_simple_psm", "dsa_res.rds", package="heRomod"))
    dsa_res_test <- do.call(run_hero_dsa,model)
    
    expect_equal(
      dsa_res$main,
      dsa_res_test$main
    )
    
    expect_equal(
      dsa_res$outcomes,
      dsa_res_test$outcomes
    )
    
    expect_equal(
      dsa_res$cost,
      dsa_res_test$cost
    )
    
    expect_equal(
      dsa_res$ce,
      dsa_res_test$ce
    )
    
    expect_equal(
      dsa_res$nmb,
      dsa_res_test$nmb
    )
    
    model$psa$n <- 100
    psa_res <- do.call(run_hero_psa,model)
    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      do.call(export_hero_xlsx,model)
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      do.call(package_hero_model,model)
      file.remove('test.zip')
      
    })
    
})

test_that(
  "PSM Responders Model Runs Correctly", {
    model <- readRDS(system.file("hero","psm_responders", "model.rds", package="heRomod"))
    
    # Base Case Results
    bc_res <- readRDS(system.file("hero","psm_responders", "bc_res.rds", package="heRomod"))
    bc_res_test <- do.call(run_hero_bc, model)
    
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
    
    vbp_res <- readRDS(system.file("hero","psm_responders", "vbp_res.rds", package="heRomod"))
    vbp_res_test <- do.call(run_hero_vbp,model)
    
    
    expect_equal(
      vbp_res$eq,
      vbp_res_test$eq
    )
    
    dsa_res <- readRDS(system.file("hero","psm_responders", "dsa_res.rds", package="heRomod"))
    dsa_res_test <- do.call(run_hero_dsa,model)
    
    expect_equal(
      dsa_res$main,
      dsa_res_test$main
    )
    
    expect_equal(
      dsa_res$outcomes,
      dsa_res_test$outcomes
    )
    
    expect_equal(
      dsa_res$cost,
      dsa_res_test$cost
    )
    
    expect_equal(
      dsa_res$ce,
      dsa_res_test$ce
    )
    
    expect_equal(
      dsa_res$nmb,
      dsa_res_test$nmb
    )
    
    model$psa$n <- 100
    psa_res <- do.call(run_hero_psa,model)
    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      do.call(export_hero_xlsx,model)
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      do.call(package_hero_model,model)
      file.remove('test.zip')
    })
    
  })

test_that(
  "Groups Model Runs Correctly", {
    model <- readRDS(system.file("hero","groups", "model.rds", package="heRomod"))
    
    # Base Case Results
    bc_res <- readRDS(system.file("hero","groups", "bc_res.rds", package="heRomod"))
    bc_res_test <- do.call(run_hero_bc, model)
    
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
    
    vbp_res <- readRDS(system.file("hero","groups", "vbp_res.rds", package="heRomod"))
    vbp_res_test <- do.call(run_hero_vbp,model)
    
    
    expect_equal(
      vbp_res$eq,
      vbp_res_test$eq
    )
    
    dsa_res <- readRDS(system.file("hero","groups", "dsa_res.rds", package="heRomod"))
    dsa_res_test <- do.call(run_hero_dsa,model)
    
    model$psa$n <- 100
    psa_res_test <- do.call(run_hero_psa,model)
    
    expect_equal(
      dsa_res$main,
      dsa_res_test$main
    )
    
    expect_equal(
      dsa_res$outcomes,
      dsa_res_test$outcomes
    )
    
    expect_equal(
      dsa_res$cost,
      dsa_res_test$cost
    )
    
    expect_equal(
      dsa_res$ce,
      dsa_res_test$ce
    )
    
    expect_equal(
      dsa_res$nmb,
      dsa_res_test$nmb
    )

    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      do.call(export_hero_xlsx,model)
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      do.call(package_hero_model,model)
      file.remove('test.zip')
    })
    
  })

test_that(
  "Markov model Runs Correctly", {
    model <- readRDS(system.file("hero","markov_model", "model.rds", package="heRomod"))
    
    # Base Case Results
    bc_res <- readRDS(system.file("hero","markov_model", "bc_res.rds", package="heRomod"))
    bc_res_test <- do.call(run_hero_bc, model)
    
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
    
    vbp_res <- readRDS(system.file("hero","markov_model", "vbp_res.rds", package="heRomod"))
    vbp_res_test <- do.call(run_hero_vbp,model)
    
    
    expect_equal(
      vbp_res$eq,
      vbp_res_test$eq
    )
    
    dsa_res <- readRDS(system.file("hero","markov_model", "dsa_res.rds", package="heRomod"))
    dsa_res_test <- do.call(run_hero_dsa,model)
    
    expect_equal(
      dsa_res$main,
      dsa_res_test$main
    )
    
    expect_equal(
      dsa_res$outcomes,
      dsa_res_test$outcomes
    )
    
    expect_equal(
      dsa_res$cost,
      dsa_res_test$cost
    )
    
    expect_equal(
      dsa_res$ce,
      dsa_res_test$ce
    )
    
    expect_equal(
      dsa_res$nmb,
      dsa_res_test$nmb
    )
    
    model$psa$n <- 100
    psa_res <- do.call(run_hero_psa,model)
    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      do.call(export_hero_xlsx,model)
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      do.call(package_hero_model,model)
      file.remove('test.zip')
    })
    
  })

test_that(
  "Advanced surv model Runs Correctly", {
    model <- readRDS(system.file("hero","advanced_surv_modeling", "model.rds", package="heRomod"))
    
    # Base Case Results
    bc_res <- readRDS(system.file("hero","advanced_surv_modeling", "bc_res.rds", package="heRomod"))
    bc_res_test <- do.call(run_hero_bc, model)
    
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
    
    vbp_res <- readRDS(system.file("hero","advanced_surv_modeling", "vbp_res.rds", package="heRomod"))
    vbp_res_test <- do.call(run_hero_vbp,model)
    
    
    expect_equal(
      vbp_res$eq,
      vbp_res_test$eq
    )
    
    dsa_res <- readRDS(system.file("hero","advanced_surv_modeling", "dsa_res.rds", package="heRomod"))
    dsa_res_test <- do.call(run_hero_dsa,model)
    
    expect_equal(
      dsa_res$main,
      dsa_res_test$main
    )
    
    expect_equal(
      dsa_res$outcomes,
      dsa_res_test$outcomes
    )
    
    expect_equal(
      dsa_res$cost,
      dsa_res_test$cost
    )
    
    expect_equal(
      dsa_res$ce,
      dsa_res_test$ce
    )
    
    expect_equal(
      dsa_res$nmb,
      dsa_res_test$nmb
    )
    
    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      do.call(export_hero_xlsx,model)
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      do.call(package_hero_model,model)
      file.remove('test.zip')
    })
    
  })

test_that(
  "TA 447 model Runs Correctly", {
    model <- readRDS(system.file("hero","ta447", "model.rds", package="heRomod"))
    
    # Base Case Results
    bc_res <- readRDS(system.file("hero","ta447", "bc_res.rds", package="heRomod"))
    bc_res_test <- do.call(run_hero_bc, model)
    
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
    
    vbp_res <- readRDS(system.file("hero","ta447", "vbp_res.rds", package="heRomod"))
    vbp_res_test <- do.call(run_hero_vbp,model)
    
    
    expect_equal(
      vbp_res$eq,
      vbp_res_test$eq
    )
    
    dsa_res <- readRDS(system.file("hero","ta447", "dsa_res.rds", package="heRomod"))
    dsa_res_test <- do.call(run_hero_dsa,model)
    
    expect_equal(
      dsa_res$main,
      dsa_res_test$main
    )
    
    expect_equal(
      dsa_res$outcomes,
      dsa_res_test$outcomes
    )
    
    expect_equal(
      dsa_res$cost,
      dsa_res_test$cost
    )
    
    expect_equal(
      dsa_res$ce,
      dsa_res_test$ce
    )
    
    expect_equal(
      dsa_res$nmb,
      dsa_res_test$nmb
    )
    
    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      do.call(export_hero_xlsx,model)
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      do.call(package_hero_model,model)
      file.remove('test.zip')
    })
    
  })


