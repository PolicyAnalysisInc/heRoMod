#' Run Value-Based Price Analysis
#' 
#' @param model An evaluated Markov model.
#' @param vbp An object returned by 
#'   [define_vbp()].
#' @param strategy_vbp A string with strategy for vbp analysis.
#' @param wtp_thresholds A vector with WTP thresholds.
#' @return A `data.frame` with one row per model and 
#'   parameter value.
#' @export
#' 
#' @example inst/examples/example_run_vbp.R
run_vbp <- function(model, vbp, strategy_vbp, wtp_thresholds) {
  
  if (! all(c(".cost", ".effect") %in% names(get_model_results(model)))) {
    stop("No cost and effect defined, value-based pricing analysis unavailable.")
  }
  
  strategy_names <- get_strategy_names(model)
  
  if (! (strategy_vbp %in% strategy_names)) {
    stop("Strategy for VBP not defined")
  }
  
  strategy_comp <- strategy_names[strategy_names!=strategy_vbp]
  
  lambda <- seq(wtp_thresholds[1], wtp_thresholds[2], length.out = 100)
  
  init <- get_uneval_init(model)
  cycles <- get_cycles(model)
  method <- get_method(model)
  
  n_par <- length(vbp$variable)
  pos_par <- cumsum(c(1, rep(c(n_par, n_par+1), n_par)))
  pos_par <- c(pos_par[-length(pos_par)], 3)
  
  list_res <- list()
  e_newdata <- list()
  
  message(sprintf(
    "Running VBP on strategy '%s'...", strategy_vbp
  ))
  
  for (n in strategy_names) { # n <- strategy_names[1]
    tab <- eval_strategy_newdata(
      model,
      strategy = n,
      newdata = vbp$vbp
    )
    
    res <- tab %>% 
      dplyr::mutate_if(
        names(tab) %in% vbp$variable,
        dplyr::funs(to_text_dots),
        name = FALSE
      )
    
    list_res <- c(
      list_res,
      list(res)
    )
    
    e_newdata <- c(
      e_newdata,
      list(unlist(lapply(
        tab$.mod,
        function(x) x$complete_parameters[1, vbp$variable]))[pos_par]))
    
    names(e_newdata)[length(e_newdata)] <- n
  }
  
  for (i in seq_along(strategy_names)) {
    list_res[[i]]$.strategy_names <- strategy_names[i]
  }
  
  res <- 
    dplyr::bind_rows(list_res) %>%
    reshape_long(
      key_col = ".par_names", value_col = ".par_value",
      gather_cols = vbp$variable, na.rm = TRUE) %>% 
    dplyr::rowwise()
  
  res_vbp <- res %>% 
    dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
    dplyr::bind_cols(res %>% dplyr::select_(~ - .mod)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      .par_value_eval = unlist(e_newdata)) %>% 
    dplyr::mutate_(
      .dots = get_ce(model))
  
  m.p_vs_wtp <- matrix(NA, 
                       nrow = length(lambda), 
                       ncol = (1+length(strategy_comp)))
  colnames(m.p_vs_wtp) <- c("WTP", strategy_comp)
  m.p_vs_wtp[, "WTP"]  <- lambda
  
  ### List to indicate if linear approximation is adopted
  lin_approx <- vector("list", length = length(strategy_names))
  names(lin_approx) <- strategy_names
  
  ### Linearization of VBP strategy
  e.P     <- ce_strategy(model, strategy = strategy_vbp)$e.strategy
  lin.params.P <- c_linear(res_vbp, strategy = strategy_vbp)
  beta0.P <- lin.params.P$beta0
  beta1.P <- lin.params.P$beta1
  lin_approx[[strategy_vbp]] <- lin.params.P$lin_approx
  
  ### Linearization of comparison strategies
  for (n in strategy_comp) {
    ### Linearization
    e.comp     <- ce_strategy(model, strategy = n)$e.strategy
    lin.params.comp <- c_linear(res_vbp, strategy = n)
    beta0.comp <- lin.params.comp$beta0
    beta1.comp <- lin.params.comp$beta1
    lin_approx[[n]] <- lin.params.comp$lin_approx
    ### Linear comparison
    m.p_vs_wtp[, n] <- p_comp(e.comp     = e.comp, 
                              e.P        = e.P, 
                              beta0.P    = beta0.P, 
                              beta0.comp = beta0.comp,
                              beta1.P    = beta1.P, 
                              beta1.comp = beta1.comp, 
                              lambda     = lambda)
  }
  
  df.p_vs_wtp <- as.data.frame(m.p_vs_wtp)
  m.p_vs_wtp <- m.p_vs_wtp[, -1]
  index.str.vbp <- max.col(-m.p_vs_wtp)
  
  df_p_vs_wtp.lg <- reshape2::melt(df.p_vs_wtp, 
                                   id.vars = "WTP", 
                                   value.name = "Price",
                                   variable.name = "Comparison")
  vbp_star <- matrixStats::rowMaxs(cbind(0,
                                         m.p_vs_wtp[cbind(1:length(lambda), index.str.vbp)]))
  df_vbp <- data.frame(WTP = lambda,
                       Price = vbp_star)
  
  structure(
    list(
      vbp        = df_vbp,
      p_vs_wtp   = df_p_vs_wtp.lg,
      variable   = vbp$variable,
      lin_approx = lin_approx,
      model      = model
    ),
    class = c("vbp", "list")
  )
}

get_model.vbp <- function(x) {
  x$model
}

c_linear <- function(res_vbp, strategy){
  message(sprintf(
    "Running linearization of cost on strategy '%s'...", strategy
  ))
  C_linear <- res_vbp %>% 
    dplyr::filter(.strategy_names == strategy) %>%
    dplyr::select(.par_value,
                  .cost) %>%
    dplyr::mutate(price = as.numeric(.par_value)) %>%
    dplyr::summarise(beta1 = diff(.cost[1:2])/diff(price[1:2]), # New
                     beta0 = .cost[1]-beta1*price[1])
  ### Linearization test
  lin_test <- res_vbp %>% 
    dplyr::filter(.strategy_names == strategy) %>%
    dplyr::select(.par_value,
                  .cost)
  p_vals <- as.numeric(lin_test$.par_value)
  c_test <- lin_test$.cost
  
  ### Predicted cost using linear function
  c_pred <- C_linear$beta0 + C_linear$beta1*p_vals
  
  ### Compute residual sum of squares (RSS)
  rss <- sum((c_pred - c_test)^2)
  
  ### Detemrine if relantionshipe is linear based on RSS
  if(isTRUE(all.equal(rss, 0))){
    message(sprintf(
      "--Relationship on strategy '%s' is linear", strategy
    ))
    ## No linear approximation is adopted
    lin_approx <- 0
  } else{
    message(sprintf(
      "--Relationship on strategy '%s' is NOT linear, using linear approximation", strategy
    ))
    ## Linear approximation is adopted
    lin_approx <- 1
  }
  
  return(list(beta0 = C_linear$beta0,
              beta1 = C_linear$beta1,
              lin_approx = lin_approx
  )
  )
}

ce_strategy <- function(model, strategy){
  CE_vbp <- get_model_results(model) %>% 
    dplyr::filter(.strategy_names == strategy) %>%
    dplyr::select(.strategy_names,
                  .cost,
                  .effect)
  return(list(
    e.strategy = CE_vbp$.effect, 
    c.strategy = CE_vbp$.cost
  )
  )
}

p_comp <- function(e.P, e.comp, beta0.P, beta0.comp,beta1.P, beta1.comp, lambda){
  p <- lambda*(e.P - e.comp)/(beta1.P - beta1.comp) - (beta0.P - beta0.comp)/(beta1.P - beta1.comp)
  return(p)
}