#' Plot Sensitivity Analysis
#' 
#' Plot the results of a sensitivity analysis as a tornado 
#' plot.
#' 
#' Plot type `simple` plots variations of single strategy 
#' values, while `difference` plots incremental values.
#' 
#' @param x A result of [run_vbp()].
#' @param bw Black & white plot for publications?
#' @param ... Additional arguments passed to `plot`.
#'   
#' @return A `ggplot2` object.
#' @export
#' 
plot.vbp <- function(x, 
                     bw = FALSE, ...) {
  res <- ggplot2::ggplot(x$p_vs_wtp, aes(
    x = WTP, 
    y = Price, 
    color = Comparison)) +
    ggplot2::geom_line(linetype = 3) +
    ggplot2::geom_line(data = x$vbp, aes(
      x = WTP, 
      y = Price), 
      linetype = 1,
      color = "black") +
    ggplot2::xlab("WTP Threshold")
  
  if (bw) {
    res <- res +
      ggplot2::scale_color_grey(start = 0.3, end = .8) +
      theme_pub_bw()
  }
  
  res
}

#' @export
print.summary_dsa <- function(x, ...) {
  v <- x$object$variables
  cat(sprintf("A sensitivity analysis on %i parameters.\n\n",
              length(v)))
  cat(paste(c("Parameters:", v), collapse = "\n  -"))
  cat("\n\nSensitivity analysis:\n\n")
  
  rn <- sprintf(
    "%s, %s = %s",
    x$res_comp$.strategy_names,
    x$res_comp$.par_names,
    x$res_comp$.par_value
  )
  
  x <- dplyr::select_(x$res_comp, ~ - .par_names,
                      ~ - .par_value,
                      ~ - .strategy_names)
  x <- pretty_names(x)
  
  res <- as.matrix(x)
  
  rownames(res) <- rn
  print(res, na.print = "-", quote = FALSE)
}

#' @export
print.dsa <- function(x, ...) {
  print(summary(x))
}

get_central_strategy.dsa <- function(x, ...) {
  get_central_strategy(get_model(x))
}

#' @rdname heemod_scale
scale.dsa <- function(x, center = TRUE, scale = TRUE) {
  .bm <- get_central_strategy(x)
  
  res <- x$dsa
  
  if (scale) {
    res <- res %>% 
      dplyr::mutate_(
        .cost = ~ .cost / .n_indiv,
        .effect = ~ .effect / .n_indiv
      )
  }
  
  if (center) {
    res <- res %>% 
      dplyr::group_by_(~ .par_names, ~ .par_value) %>% 
      dplyr::mutate_(
        .cost = ~ .cost - sum(.cost * (.strategy_names == .bm)),
        .effect = ~ .effect - sum(.effect * (.strategy_names == .bm))
      ) %>% 
      dplyr::ungroup()
  }
  res
}

#' @export
summary.dsa <- function(object, ...) {
  res <- object %>% 
    scale(...) %>% 
    dplyr::group_by_(~ .par_names, ~ .par_value)  %>% 
    dplyr::do_(~ compute_icer(
      ., strategy_order = order(get_effect(get_model(object)))
    )) %>% 
    dplyr::ungroup()
  
  structure(
    list(
      res_comp = res,
      object = object
    ),
    class = c("summary_dsa", class(res))
  )
}

digits_at_diff <- function(x, y, addl_digits = 1){
  stopifnot(length(x) == length(y))
  diff <- abs(x - y)
  num_digits <- -floor(log(diff, 10)) + addl_digits
  round_x <- 
    sapply(seq(along = x), 
           function(i){round(x[i], num_digits[i])})
  round_y <- 
    sapply(seq(along = y), 
           function(i){round(y[i], num_digits[i])})
  list(x = round_x, y = round_y, nd = num_digits)
}
