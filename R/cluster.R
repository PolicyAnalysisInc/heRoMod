#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
#* Modified work Copyright (C) 2017  Matt Wiener
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

reach_cluster <- local({
  heRomod_cluster <- NULL
  
  function(operation = c("get", "set", "close"), value) {
    operation <- match.arg(operation)
    
    switch(
      operation,
      "get" = {
        heRomod_cluster
      },
      "set" = {
        heRomod_cluster <<- value
      },
      "close" = {
        parallel::stopCluster(heRomod_cluster)
        heRomod_cluster <<- NULL
      }
    )
  }
})

get_cluster <- function() {
  reach_cluster(operation = "get")
}

set_cluster <- function(x) {
  reach_cluster(operation = "set", value = x)
}

#' Run `heRomod` on a Cluster
#' 
#' These functions create or delete a cluster for 
#' `heRomod`. When the cluster is created it is 
#' automagically used by `heRomod` functions.
#' 
#' The usual wokflow is to create the cluster with 
#' `use_cluster`, then run functions such as 
#' [run_psa()] that make use of the cluster. To 
#' stop using the cluster run [close_cluster()].
#' 
#' The cluster status is given by `status_cluster`.
#' 
#' A custom cluster can be passed to `use_cluster` with
#' the `cluster` argument. This custom custer needs to 
#' work with [parallel::parLapply()].
#' 
#' @name cluster
#' @param num_cores Number of core.
#' @param cluster A custom cluster. See details.
#' @param close Close existing cluster before defining a new
#'   one?
#' @param verbose Print cluster info.
#'   
#' @return `use_cluster` and `close_cluster` 
#'   return `TRUE` invisibly in case of success. 
#'   `status_cluster` returns `TRUE` if a cluster 
#'   is defined, `FALSE` otherwise.
#'   
#' @export
use_cluster <- function(num_cores, cluster = NULL, close = TRUE) {
  if (status_cluster(verbose = FALSE)) {
    if (close) {
      close_cluster()
    } else {
      stop("A cluster is already defined, use 'close_cluster()' before defining a new cluster.")
    }
  }
  
  if (! is.null(cluster)) {
    set_cluster(cluster)
    message(paste("Using a cluster with", length(cluster), "cores."))
    
  } else {
    if (! requireNamespace("parallel"))
      stop("'parallel' package required to define a cluster.")
    
    if (! is.wholenumber(num_cores))
      stop("'num_cores' is not a whole number.")
    
    on_windows <- grepl("Windows", utils::sessionInfo()$running, 
                        ignore.case = TRUE)
    cluster_type <- 
      ifelse(on_windows, "PSOCK", "FORK")
    
    cl <- parallel::makeCluster(num_cores, type = cluster_type)
    parallel::clusterEvalQ(cl, library(heRomod))
    parallel::clusterEvalQ(cl, library(dplyr))
    
    set_cluster(cl)
    
    message(paste("Using a cluster with", num_cores, "cores."))
  }
  
  invisible(TRUE)
}

#' @rdname cluster
#' @export
status_cluster <- function(verbose = TRUE) {
  sc <- ! is.null(get_cluster())
  
  if (verbose) {
    if (sc) {
      message(sprintf(
        "Running on a %i-cores cluster.",
        length(get_cluster())
      ))
    } else {
      message("No cluster defined.")
    }
  }
  
  invisible(sc)
}

#' @rdname cluster
#' @export
close_cluster <- function() {
  if (! status_cluster(verbose = FALSE)) {
    warning("No cluster defined.")
    return(invisible(FALSE))
  }
  
  catch <- try(reach_cluster(operation = "close"))
  
  if (inherits(catch, "try-error")) {
    stop("Failed to close the cluster.")
  } else {
    message("Cluster closed.")
    invisible(TRUE)
  }
}
