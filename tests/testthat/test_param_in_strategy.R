context("Parameter influences strategy")
logger::log_threshold('ERROR')

test_that(
  "Parameter influences strategy", {
    param <- define_parameters(
      pA = 10000,
      cA = pA,
      cB = 2000, 
      cC = 100,
      qolDisabled  = .8,  #QoL of failing treatment
      pDieDisease  = 0.02,
      pDieDisabled = 0.03,
      costPerVisit = 5000, 
      cDisabled    = 5000,
      meanNumVisits_C = 2.8,
      meanNumVisits_A = 1,
      meanNumVisits_B = 2,
      pFailA = 0.15,
      pFailB = 0.2,
      pFailC = 0.3,
      dr = .05
    )
    #### Define transition probabilities ####
    ### Base Strategy
    mat_base <- define_transition(
      state_names = c("Disease", "Disabled", "Death"), 
      
      C, pFailC, pDieDisease,
      0, C,      pDieDisabled, 
      0, 0,      1)
    
    ### Medicine strategy
    mat_A <- define_transition(
      state_names = c("Disease", "Disabled", "Death"), 
      
      C, pFailA, pDieDisease,
      0, C,      pDieDisabled, 
      0, 0,      1)
    
    ### Surgery strategy
    mat_B <- define_transition(
      state_names = c("Disease", "Disabled", "Death"), 
      
      C, pFailB, pDieDisease,
      0, C,      pDieDisabled, 
      0, 0,      1)
    
    #### Define state rewards ####
    ## State PreSymptomatic (Pre)
    state_disease <- define_state(
      cost_treat = 4 + (2* by_strategy(
        base = cC, 
        A    = cA, 
        B    = cB)+1), 
      cost_hospit = dispatch_strategy(
        base = meanNumVisits_C * costPerVisit,
        A    = meanNumVisits_A * costPerVisit, 
        B    = meanNumVisits_B * costPerVisit),
      cost_total = discount(cost_treat + cost_hospit, r = dr), 
      qaly = 1)
    
    ## State Symptomatic (Symp)
    state_disabled <- define_state(
      cost_treat = 0, 
      cost_hospit = cDisabled, 
      cost_total = discount(cost_treat + cost_hospit, r = dr), 
      qaly = qolDisabled)
    
    ## State Death (Death)
    state_death <- define_state(
      cost_treat = 0, 
      cost_hospit = 0, 
      cost_total = 0, 
      qaly = 0)
    
    #### Define strategies ####
    ### Base
    strat_base <- define_strategy(
      transition = mat_base, 
      Disease = state_disease, 
      Disabled = state_disabled, 
      Death = state_death)
    
    ### Medicine
    strat_A <- define_strategy(
      transition = mat_A, 
      Disease = state_disease, 
      Disabled = state_disabled, 
      Death = state_death)
    
    ### Surgery
    strat_B <- define_strategy(
      transition = mat_B, 
      Disease = state_disease, 
      Disabled = state_disabled, 
      Death = state_death)
    
    #### Run model ####
    res_mod <- run_model(
      parameters = param, 
      base = strat_base, 
      A    = strat_A, 
      B    = strat_B, 
      cycles = 50, 
      cost = cost_total, 
      effect = qaly, 
      method = "life-table")
    
    #### Parameter influences strategy ####
    ### Parameter influencing strategy "A"
    strategy <- "A"
    parameter <- "pFailA"
    expect_equal(
      param_in_strategy(mod = res_mod, strategy = strategy, parameter = parameter),
      TRUE
    )
    parameter <- "pA"
    expect_equal(
      param_in_strategy(mod = res_mod, strategy = strategy, parameter = parameter),
      TRUE
    )
    parameter <- "cA"
    expect_equal(
      param_in_strategy(mod = res_mod, strategy = strategy, parameter = parameter),
      TRUE
    )
    parameter <- "cB"
    expect_equal(
      param_in_strategy(mod = res_mod, strategy = strategy, parameter = parameter),
      FALSE
    )
    parameter <- "pFailB"
    expect_equal(
      param_in_strategy(mod = res_mod, strategy = strategy, parameter = parameter),
      FALSE
    )
  })