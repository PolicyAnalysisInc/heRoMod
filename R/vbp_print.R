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
  res <- ggplot2::ggplot(x$p_vs_wtp, 
                         aes(x = WTP, y = Price, color = Comparison)) +
    ggplot2::geom_line(linetype = "dotted") +
    ggplot2::geom_line(data = x$vbp, 
                       aes(x = WTP, y = Price), 
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
print.summary_vbp <- function(x, ...) {
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
print.vbp <- function(x, ...) {
  print(summary(x))
}

get_central_strategy.vbp <- function(x, ...) {
  get_central_strategy(get_model(x))
}

