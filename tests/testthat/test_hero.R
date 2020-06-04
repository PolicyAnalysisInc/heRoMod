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
    
    expect_equal(
      bc_res$pairwise_ce,
      bc_res_test$pairwise_ce
    )
    
    vbp_res <- readRDS(system.file("hero","example_simple_psm", "vbp_res.rds", package="heRomod"))
    vbp_res_test <- do.call(run_hero_vbp,model)
    
    
    expect_equal(
      vbp_res$eq,
      vbp_res_test$eq
    )
    
    dsa_res <- readRDS(system.file("hero","example_simple_psm", "dsa_res.rds", package="heRomod"))
    dsa_res_test <- do.call(run_hero_dsa, model)


    expect_equal(
      plyr::ldply(dsa_res$outcomes, function(x) mutate(x$data, series = x$series, disc = x$disc, outcome = x$outcome)),
      plyr::ldply(dsa_res_test$outcomes, function(x) mutate(x$data, series = x$series, disc = x$disc, outcome = x$outcome))
    )
    
    
    expect_equal(
      plyr::ldply(dsa_res$cost, function(x) mutate(x$data, series = x$series, disc = x$disc, outcome = x$outcome)),
      plyr::ldply(dsa_res_test$cost, function(x) mutate(x$data, series = x$series, disc = x$disc, outcome = x$outcome))
    )
    
    
    expect_equal(
      plyr::ldply(
        dsa_res$nmb, function(x) mutate(
          x$data,
          series = x$series,
          health_outcome = x$health_outcome,
          econ_outcome = x$econ_outcome
        )
      ),
      plyr::ldply(
        dsa_res_test$nmb, function(x) mutate(
          x$data,
          series = x$series,
          health_outcome = x$health_outcome,
          econ_outcome = x$econ_outcome
        )
      )
    )
    model$psa$n <- 10
    psa_res <- do.call(run_hero_psa,model)
    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      suppressMessages(do.call(export_hero_xlsx,model))
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      suppressMessages(do.call(package_hero_model,model))
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
    
    expect_equal(
      bc_res$pairwise_ce,
      bc_res_test$pairwise_ce
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
      dsa_res$outcomes,
      dsa_res_test$outcomes
    )
    
    expect_equal(
      dsa_res$cost,
      dsa_res_test$cost
    )
    
    expect_equal(
      dsa_res$nmb,
      dsa_res_test$nmb
    )
    model$psa$n <- 10
    psa_res <- do.call(run_hero_psa,model)
    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      suppressMessages(do.call(export_hero_xlsx,model))
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      suppressMessages(do.call(package_hero_model,model))
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
    
    expect_equal(
      bc_res$pairwise_ce,
      bc_res_test$pairwise_ce
    )
    
    vbp_res <- readRDS(system.file("hero","groups", "vbp_res.rds", package="heRomod"))
    vbp_res_test <- do.call(run_hero_vbp,model)
    
    
    expect_equal(
      vbp_res$eq,
      vbp_res_test$eq
    )
    
    dsa_res <- readRDS(system.file("hero","groups", "dsa_res.rds", package="heRomod"))
    dsa_res_test <- do.call(run_hero_dsa,model)
    
    model$psa$n <- 10
    psa_res_test <- do.call(run_hero_psa,model)
    
    expect_equal(
      dsa_res$outcomes,
      dsa_res_test$outcomes
    )
    
    expect_equal(
      dsa_res$cost,
      dsa_res_test$cost
    )

    expect_equal(
      dsa_res$nmb,
      dsa_res_test$nmb
    )

    scen_res <- readRDS(system.file("hero","groups", "scen_res.rds", package="heRomod"))
    scen_res_test <- do.call(run_hero_scen, model)

    expect_equal(
      scen_res$outcomes,
      scen_res_test$outcomes
    )

    expect_equal(
      scen_res$cost,
      scen_res_test$cost
    )

    expect_equal(
      scen_res$nmb,
      scen_res_test$nmb
    )

    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      suppressMessages(do.call(export_hero_xlsx,model))
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      suppressMessages(do.call(package_hero_model,model))
      file.remove('test.zip')
    })
    
  })

test_that(
  "Markov model Runs Correctly", {
    model <- readRDS(system.file("hero","markov_model", "model.rds", package="heRomod"))
    model$evalues <- rbind(model$evalues, tibble(name = 'ae_cost', description = 'Adverse Event Cost', strategy = 'nat', state = 'Model Start', value = '100'))
    model$esumms <- rbind(model$esumms, tibble(name = 'cost_hc', description = 'Healthcare system costs', value = 'ae_cost'))
    
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
    
    expect_equal(
      bc_res$pairwise_ce,
      bc_res_test$pairwise_ce
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
      dsa_res$outcomes,
      dsa_res_test$outcomes
    )
    
    expect_equal(
      dsa_res$cost,
      dsa_res_test$cost
    )
    
    expect_equal(
      dsa_res$nmb,
      dsa_res_test$nmb
    )
    scen_res <- readRDS(system.file("hero","markov_model", "scen_res.rds", package="heRomod"))
    scen_res_test <- do.call(run_hero_scen, model)
    
    expect_equal(
      scen_res$outcomes,
      scen_res_test$outcomes
    )
    
    expect_equal(
      scen_res$cost,
      scen_res_test$cost
    )
    
    expect_equal(
      scen_res$nmb,
      scen_res_test$nmb
    )
    
    model$psa$n <- 10
    psa_res <- do.call(run_hero_psa,model)
    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      suppressMessages(do.call(export_hero_xlsx,model))
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      suppressMessages(do.call(package_hero_model,model))
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
    
    expect_equal(
      bc_res$pairwise_ce,
      bc_res_test$pairwise_ce
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
      dsa_res$outcomes,
      dsa_res_test$outcomes
    )
    
    expect_equal(
      dsa_res$cost,
      dsa_res_test$cost
    )
    
    expect_equal(
      dsa_res$nmb,
      dsa_res_test$nmb
    )
    
    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      suppressMessages(do.call(export_hero_xlsx,model))
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      suppressMessages(do.call(package_hero_model,model))
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
    
    print(tibble::as_tibble(bc_res$costs))
    print(tibble::as_tibble(bc_res_test$costs))
    
    expect_equal(
      bc_res$nmb,
      bc_res_test$nmb
    )
    
    expect_equal(
      bc_res$ce,
      bc_res_test$ce
    )
    
    expect_equal(
      bc_res$pairwise_ce,
      bc_res_test$pairwise_ce
    )
    
    vbp_res <- readRDS(system.file("hero","ta447", "vbp_res.rds", package="heRomod"))
    vbp_res_test <- do.call(run_hero_vbp,model)
    
    
    expect_equal(
      vbp_res$eq,
      vbp_res_test$eq
    )
    
    dsa_res <- readRDS(system.file("hero","ta447", "dsa_res.rds", package="heRomod"))
    dsa_res_test <- do.call(run_hero_dsa,model)
    
    # expect_equal(
    #   dsa_res$outcomes,
    #   dsa_res_test$outcomes
    # )
    
    expect_equal(
      dsa_res$cost,
      dsa_res_test$cost
    )
    
    expect_equal(
      dsa_res$nmb,
      dsa_res_test$nmb
    )
    
    withr::with_dir(new = tempdir(), {
      model$name <- 'test'
      suppressMessages(do.call(export_hero_xlsx,model))
      xl_file <- openxlsx::read.xlsx('test.xlsx')
      file.remove('test.xlsx')
      suppressMessages(do.call(package_hero_model,model))
      file.remove('test.zip')
    })
    
  })


