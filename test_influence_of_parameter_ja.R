#################
#### Example ####
#################
#### Parameters ####
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
  cost_treat = 4 + (2* dispatch_strategy(
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

#### Steps:
### 1: Determine if parameter influences a strategy in either the states or starting values
### 2: If it does, run the intervention on two different values and obtaine different C and E
### 3: If it doesn't, don't run intervention and just use base-case analysis value of C and E

### Let's try parameter cA influencing strategy A

## Paramaters
param_list <- res_mod$parameters
# Let's try with parameter cA
param_list$cA <- lazyeval::lazy(.._my_param)
i_params <- heRomod:::interpolate(param_list) # heroMod:::interpolate(params)
i_params$cA

## States
# Extract states
state_list <- res_mod$uneval_strategy_list$A$states
i_state <- heRomod:::interpolate(state_list, more = heRomod:::as_expr_list(i_params)) # heroMod:::as_expr_list(i_params)
i_state
i_state <- lapply(i_state, dispatch_strategy_substitute, strategy = "A")

any(as.logical(lapply(unlist(i_state, recursive=F), function(x) ".._my_param" %in% all.vars(x$expr))))

# Identify existence of ".._my_param" in Disease$cost_treat$expr
all.vars(i_state$Disease$cost_treat$expr)
# CORRECT EXPECTED RESULT: It is indeed there!

## Now let's do it for startegy B, which should not have ".._my_param" in it 
state_list <- res_mod$uneval_strategy_list$B$states
i_state <- heRomod:::interpolate(state_list, more = heRomod:::as_expr_list(i_params)) # heroMod:::as_expr_list(i_params)
i_state <-lapply(i_state, dispatch_strategy_substitute, strategy = "B")

# Identify existence of ".._my_param" in Disease$cost_treat$expr
all.vars(i_state$Disease$cost_treat$expr)
# INCORRECT EXPECTED RESULT: ".._my_param" is still showing up!
# I think it has to do with the fact that we are using "dispatch_strategy" to assign the cost_treat per strategy
# Any suggestions on how to correctly implement this check??

## Transitions
# Extract Transitions
trans_list <- res_mod$uneval_strategy_list$surg$transition
i_trans <- interpolate(trans_list, more = as_expr_list(i_params)) 
i_trans
i_trans <- lapply(i_trans, dispatch_strategy_substitute, strategy = "A")

## Starting values
start_list <- res_mod$uneval_strategy_list$surg$starting_values
i_start <- interpolate(start_list, more = as_expr_list(i_params)) 
i_start
i_start <- lapply(i_start, dispatch_strategy_substitute, strategy = "A")



# This is a modified version of heRomod:::dispatch_strategy_hack
# It goes through each R expression, identifies calls to dispatch_strategy,
# and substitutes only the expression used by the given stratategy
dispatch_strategy_substitute <- function(.dots, strategy) {
  f <- function (x, env) {
    if (is.atomic(x) || is.name(x)) {
      x
    } else if (is.call(x)) {
      if (heRomod:::dispatch_strategy_check(x[[1]], env)) {
        x <- pryr::standardise_call(x)
        stratNames <- names(x)
        x <- x[[which(names(x) == strategy)]]
        f(x)
      } else {
        as.call(lapply(x, f, env = env))
      }
    } else if (is.pairlist(x)) {
      as.pairlist(lapply(x, f, env = env))
    } else {
      stop(sprintf(
        "Don't know how to handle type %s.",
        typeof(x)))
    }
  }
  
  do.call(
    structure,
    c(list(
      .Data = lapply(
        .dots,
        function(x) {
          x$expr <- f(x$expr, env = x$env)
          x
        }
      )),
      attributes(.dots)
    )
  )
}


# .._my_param is present
dispatch_strategy_substitute(i_state$Disease, strategy = "A")

# .._my_param is NOT present
dispatch_strategy_substitute(i_state$Disease, strategy = "B")

