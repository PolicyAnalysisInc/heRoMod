#**************************************************************************
#* 
#* Original work Copyright (C) 2018  Fernando Alarid-Escudero
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

#' Compute Expected Value of partial Perfect Information (EVPPI)
#' 
#' Compute EVPPI using a linear regression metamodeling approach.
#' 
#' @param x Result from [run_psa()].
#' @param evppi An object returned by 
#'   [define_evppi()].
#' @param max_wtp Maximal willingness to pay.
#' @param n Number of WTP thresholds to estimate EVPPI (values above 
#'   10 may take significant time).
#' 
#' @return A `data.frame` with one row per WTP threshold
#' @export
#' 
#' @references 
#' \enumerate{
#' \item Jalal H, Alarid-Escudero F. A Gaussian Approximation Approach for Value of Information Analysis. 
#' Med Decis Making 2018; 38: 174–188. 
#' }
#' 
#' @example inst/examples/example_run_evppi.R
compute_evppi <- function(x, evppi, 
                          max_wtp = 1e5,
                          n = 10) {
  if(!evppi$variable %in% colnames(x$psa)){
    stop(sprintf(
      "Parameter %s is not defined in the PSA", 
      evppi$variable)
    )
  }
  
  res <- export_psa(x)
  
  param   <- as.matrix(res$par[, evppi$variable])
  costs   <- res$c
  effects <- res$e   
  n_strategy <- ncol(costs)
  n_sim <- nrow(costs)
  
  if (n==1){
    wtp_thresholds <- max_wtp
  } else{
    wtp_thresholds <- generate_wtp(max_wtp = max_wtp,
                                   n = n, log_scale = FALSE)
  }
  
  res_evppi <- as.data.frame(matrix(NA, 
                                    nrow = length(wtp_thresholds),
                                    ncol = length(evppi$variable)+1, 
                                    dimnames = list(1:length(wtp_thresholds), c("WTP", evppi$variable))))
  
  res_evppi$WTP <- wtp_thresholds
  
  
  
  for(j in 1:length(evppi$variable)){ # j <- 1
    p <- evppi$variable[j]
    message(sprintf(
      "Computing EVPPI on parameter '%s'...", p
    ))
    
    for (i in 1:length(wtp_thresholds)) {
      wtp <- wtp_thresholds[i]
      # Create NMB
      nmb <- wtp*effects - costs 
      # Find optimal strategy
      d.star <- which.max(colMeans(nmb))
      # Compute the loss matrix
      loss <- nmb - nmb[, d.star]
      
      # Generate linear regresssion metamodel on the each strategie's 
      # opportunity loss as a function of selected parameter
      lmm <- vector("list", n_strategy)
      loss.hat <- matrix(NA, nrow = n_sim, ncol = n_strategy)
      for(strat in 1:n_strategy){
        lmm[[strat]] <- mgcv::gam(loss[, strat] ~ s(param[, j]))
        loss.hat[, strat] <- lmm[[strat]]$fitted
      }
      
      # Compute EVPPI
      res_evppi[i, p] <- mean(matrixStats::rowMaxs(loss.hat))
      
      if(i/10 == round(i/10, 1)){
        cat('\r', paste0(i/length(wtp_thresholds)*100, "% done"))
      }
    }
  }
  
  structure(
    list(
      variables = evppi$variable,
      evppi_res = res_evppi
    ),
    class = "evppi_res"
  )
}

get_evppi.evppi_res <- function(x) {
  x$evppi_res
}
