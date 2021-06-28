context("Expected value of partial perfect information")
logger::log_threshold('ERROR')

test_that(
  "define expected value of partial perfect information", {
    evppi1 <- define_evppi(
      prob,
      cost
    )
    expect_identical(
      length(evppi1$variable),
      c(2L)
    )
    expect_is(
      evppi1$variable,
      "character"
    )
    expect_error(
      define_evppi()
    )
  })

test_that(
  "run expected value of partial perfect information", {
    ### Define first strategy
    mod1 <- define_strategy(
      transition = define_transition(
        .5, .5,
        .1, .9
      ),
      define_state(
        cost = cost_init + age * 5,
        ly = 1
      ),
      define_state(
        cost = cost_init + age,
        ly = 0
      )
    )
    
    ### Define second strategy
    mod2 <- define_strategy(
      transition = define_transition(
        p_trans, C,
        .1, .9
      ),
      define_state(
        cost = 789 * age / 10,
        ly = 1
      ),
      define_state(
        cost = 456 * age / 10,
        ly = 0
      )
      
    )
    
    ### Run model
    res2 <- run_model(
      mod1, mod2,
      parameters = define_parameters(
        age_init = 60,
        cost_init = 1000,
        age = age_init + markov_cycle,
        p_trans = .7
      ),
      init = 1:0,
      cycles = 10,
      cost = cost,
      effect = ly
    )
    
    ### Define PSA
    rsp <- define_psa(
      age_init ~ normal(60, 10),
      cost_init ~ normal(1000, 100),
      p_trans ~ binomial(.7, 100),
      correlation = matrix(c(
        1,  .4, 0,
        .4, 1,  0,
        0,  0,  1
      ), byrow = TRUE, ncol = 3)
    )
    
    ### Run PSA
    set.seed(7783)
    # Could take a while to produce 1000 samples per strategy
    ndt1 <- run_psa(res2, psa = rsp, N = 200)
    
    #### Expected value of partial perfect information (EVPPI) ####
    ### EVPPI on p_trns parameter (non-existing parameter) one WTP
    def_evppi <- define_evppi(
      p_trns
    )
    ## Run EVPPI
    expect_error(
      compute_evppi(x = ndt1, 
                    evppi = def_evppi, 
                    max_wtp = 20000, n = 1)
    )
    
    ### EVPPI on p_trns (non-existing parameter) one WTP
    def_evppi <- define_evppi(
      p_trns
    )
    ## Run EVPPI
    expect_error(
      compute_evppi(x = ndt1, 
                    evppi = def_evppi, 
                    max_wtp = 20000, n = 1)
    )
    
    ### EVPPI on cost_init (existing parameter) and p_trns (non-existing parameter) one WTP
    def_evppi <- define_evppi(
      cost_init, 
      p_trns
    )
    ## Run EVPPI
    expect_error(
      compute_evppi(x = ndt1, 
                    evppi = def_evppi, 
                    max_wtp = 20000, n = 1)
    )
    
    ### EVPPI on p_trans
    def_evppi <- define_evppi(
      p_trans
    )
    ## Run EVPPI
    expect_error(
      compute_evppi(x = ndt1, 
                    evppi = def_evppi, 
                    max_wtp = 20000, n = 0)
    )
    ## Run EVPPI
    expect_error(
      compute_evppi(x = ndt1, 
                    evppi = def_evppi, 
                    max_wtp = 20000, n = -3)
    )
    ## Run EVPPI
    expect_error(
      compute_evppi(x = ndt1, 
                    evppi = def_evppi, 
                    max_wtp = 20000, n = "f")
    )
    
    x <- compute_evppi(x = ndt1, 
                       evppi = def_evppi, 
                       max_wtp = 20000, n = 1,
                       verbose = FALSE)
    
    expect_equal(
      round(as.numeric(x$evppi_res[c(1, 2)])),
      c(20000, 3089)
      )
    
    ### EVPPI on cost_init and p_trans one WTP
    def_evppi <- define_evppi(
      cost_init, 
      p_trans
    )
    
    x <- compute_evppi(x = ndt1, 
                       evppi = def_evppi, 
                       max_wtp = 20000, n = 1,
                       verbose = FALSE)
    
    expect_equal(
      round(as.numeric(x$evppi_res[c(1, 2, 3)])),
      c(20000, 82, 3089)
    )
    
    plot(x)
    suppressMessages(
      plot(x, bw = "TRUE")
    )
    
    ### EVPPI on cost_init and p_trans multiple WTP
    x2 <- compute_evppi(x = ndt1, 
                       evppi = def_evppi, 
                       max_wtp = 50000, n = 50,
                       verbose = FALSE)
    
    plot(x2)
    
    suppressMessages(
      plot(x2, bw = "TRUE")
      )
    
  })
