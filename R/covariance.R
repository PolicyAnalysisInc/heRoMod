#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
#*
#* This program is free software: you can redistribute it and/or modify
#* it under the terms of the GNU General Public License as published by
#* the Free Software Foundation, either version 3 of the License, or
#* (at your option) any later version.
#*
#* This program is distributed in the hope that it will be useful,
#* but WITHOUT ANY WARRANTY; without even the implied warranty of
#* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#* GNU General Public License for more details.
#*
#* You should have received a copy of the GNU General Public License
#* along with this program.  If not, see <http://www.gnu.org/licenses/>.
#**************************************************************************


compute_cov <- function(psa, diff = FALSE, k, k_default = 10, threshold) {
  if (! requireNamespace("mgcv")) {
    stop("'mgcv' package required for covariance analysis.")
  }
  
  if (diff) {
    tab_psa <- psa$psa %>%
      group_by(.index) %>%
      do(compute_icer(
        ., strategy_order = order(get_effect(get_model(psa))),
        threshold = threshold)) %>%
      filter(! is.na(.dref)) %>% 
      ungroup()
  } else {
    tab_psa <- psa$psa
  }
  
  max_k <- tab_psa %>% 
    select(!!!syms(c(psa$resamp_par, ".strategy_names"))) %>% 
    group_by(.strategy_names) %>% 
    summarise_all(list(n_distinct)) %>% 
    summarise_all(min) %>% 
    select(- .strategy_names) %>% 
    unlist()
  
  default_k <- ifelse(
    max_k < k_default,
    max_k, k_default
  )
  
  if (! missing(k)) {
    stopifnot(all(names(k) %in% psa$resamp_par))
    if (any(pb <- default_k[names(k)] < k)) {
      warning(sprintf(
        "Number of distinct values < k: %s.",
        paste(names(k)[pb], collapse = ", ")
      ))
    }
    default_k[names(k)] <- k
  }
  
  if (sum(default_k) >= psa$N) {
    warning(sprintf(
      "Not enough PSA data (%i present, at least %i needed). Consider lowering 'k_default' of running more simulations.",
      psa$N, sum(default_k)
    ))
  }
  
  x_side <- paste(
    sprintf("s(%s, k = %i)", psa$resamp_par, default_k),
    collapse = "+")
  
  form_cost <- stats::as.formula(paste(
    ".cost ~", x_side
  ))
  form_effect <- stats::as.formula(paste(
    ".effect ~", x_side
  ))
  
  if (diff) {
    form_cost <- stats::as.formula(paste(
      ".dcost ~", x_side
    ))
    form_effect <- stats::as.formula(paste(
      ".deffect ~", x_side
    ))
    
    form_nmb <- stats::as.formula(paste(
      ".dnmb ~", x_side
    ))
  }
  
  res <- tab_psa %>% 
    group_by(.strategy_names) %>% 
    do(
      compute_prop_var(mgcv::gam(formula = form_cost, data = .))
    ) %>% 
    mutate(
      .result = "Cost"
    ) %>% 
    bind_rows(
      tab_psa %>% 
        group_by(.strategy_names) %>% 
        do(
          compute_prop_var(mgcv::gam(formula = form_effect, data = .))
        ) %>% 
        mutate(
          .result = "Effect"
        )
    )
  
  if (diff) {
    res <- res %>% 
      bind_rows(
        tab_psa %>% 
          group_by(.strategy_names) %>% 
          do(
            compute_prop_var(mgcv::gam(formula = form_nmb, data = .))
          ) %>% 
          mutate(
            .result = "NMB"
          )
      )
  }
  
  res %>% 
    ungroup() %>% 
    reshape_long(
      key_col = ".par_names",
      value_col = ".prop",
      gather_cols = psa$resamp_par
    )
}

compute_prop_var <- function(mod) {
  n <- attr(mod$terms, "term.labels")
  if (identical(0, stats::var(mod$y))) {
    return(
      rep(0, length(n)) %>% 
        stats::setNames(n) %>% 
        as.list() %>% 
        as.data.frame()
    )
  }
  
  data_trans <- mgcv::predict.gam(mod, type = "terms") %>% 
    cbind(y = mod$y) %>% 
    scale() %>% 
    as.data.frame() %>% 
    stats::setNames(c(n, "y"))
  
  form <- stats::as.formula(paste(
    "y ~", paste(n, collapse = "+")
  ))
  res <- stats::lm(form, data = data_trans)
  
  val <- abs(stats::coef(res)[-1])
  
  if (any(is.na(val))) {
    warning(sprintf(
      "Parameter%s excluded because of perfect collinearity: %s.",
      plur(sum(is.na(val))),
      paste(names(val)[is.na(val)], collapse = ", ")
    ))
    val[is.na(val)] <- 0
  }
  
  tot <- sum(val)
  r2 <- summary(res)$r.squared
  if (r2 < .99) {
    warning(sprintf(
      "Only %.0f%% of variance explained, results may be inaccurate.",
      r2 * 100
    ))
  }
  as.data.frame(as.list(val / tot * r2))
}
