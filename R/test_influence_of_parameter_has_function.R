#### Steps:
### 1: Determine if parameter influences a strategy in either the states or starting values
### 2: If it does, run the intervention on two different values and obtaine different C and E
### 3: If it doesn't, don't run intervention and just use base-case analysis value of C and E

## Paramaters
param_list <- res_mod$parameters
param_list$cost_med <- lazyeval::lazy(.._my_param)
i_params <- interpolate(param_list) # heroMod:::interpolate(params)
i_params$cost_med

## States
# Extract states
state_list <- res_mod$uneval_strategy_list$surg$states
i_state <- interpolate(state_list, more = as_expr_list(i_params)) # heroMod:::as_expr_list(i_params)
i_state

## Transitions
# Extract Transitions
transitions <- get_transition(strat_surg)
i_trans <- interpolate(transitions, more = as_expr_list(i_params)) 
i_trans

trans_list <- res_mod$uneval_strategy_list$surg$transition
i_trans <- interpolate(trans_list, more = as_expr_list(i_params)) 
i_trans

## Starting values
start_list <- res_mod$uneval_strategy_list$surg$starting_values
i_start <- interpolate(start_list, more = as_expr_list(i_params)) 
i_start



has_parameter_strategy(i_state)
has_parameter_strategy(i_trans)
has_parameter_strategy(i_start)


 has_parameter_strategy <- function(x, ...) {
   UseMethod("has_parameter_strategy")
 }
 
 has_parameter_strategy.uneval_matrix <- function(x, ...) {
   unlist(lapply(x, function(y) ".._my_param" %in% all.vars(y$expr)))
 }
 
 has_parameter_strategy.part_surv <- function(x, ...) {
   FALSE
 }
 
 has_parameter_strategy.uneval_state_list <- function(x, ...) {
   state_names <- get_state_names(x)
   s_expand <- unlist(lapply(x, function(y) has_parameter_strategy(y)))
   
   # Figure out state expansion based on state transitions
   # References to .._my_param in state transitions are based
   # on the from state.  If the from state is NA, then use
   # of .._my_param will expand ALL states.
   state_trans <- attr(x, "transitions")
   if(!is.null(state_trans)) {
     st_to_expand <- has_parameter_strategy(state_trans)
     st_from <- lapply(state_trans, function(y) attr(y, "from"))
     st_expand <- st_from[st_to_expand]
     st_from_expanded <- unlist(st_expand)
     if(!is.null(st_from_expanded)){
       if(any(is.na(st_from_expanded))) {
         # Expand all states if from state is NA in a value referencing
         # .._my_param
         s_expand <- rep(T, length(s_expand))
       } else {
         for(i in seq_len(length(state_names))) {
           # Expand states where state transitions from reference
           # .._my_param
           if(state_names[i] %in% st_from_expanded) {
             s_expand[i] <- T
           }
         }
       }
     }
   }
   s_expand
 }
 
 has_parameter_strategy.uneval_state_transition_list <- function(x, ...) {
   unlist(lapply(x, function(y) any(has_parameter_strategy(y))))
 }
 
 has_parameter_strategy.state <- function(x, ...) {
   any(unlist(lapply(x, function(y) ".._my_param" %in% all.vars(y$expr))))
 }
 
 has_parameter_strategy.state_transition <- function(x, ...) {
   any(unlist(lapply(x, function(y) ".._my_param" %in% all.vars(y$expr))))
 } 
 
 has_parameter_strategy.lazy_dots <- function(x, ...) {
   unlist(lapply(x, function(y) any(has_parameter_strategy(y))))
 }
 has_parameter_strategy.state <- function(x, ...) {
   any(unlist(lapply(x, function(y) ".._my_param" %in% all.vars(y$expr))))
 }
 