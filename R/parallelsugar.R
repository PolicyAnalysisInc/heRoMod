#**************************************************************************
#* 
#* Original work Copyright (C) 2015  Nathan VanHoudnos
#* 
#* The MIT License (MIT)
#* 
#* Copyright (c) 2015 Nathan VanHoudnos
#* 
#* Permission is hereby granted, free of charge, to any person obtaining a copy
#* of this software and associated documentation files (the "Software"), to deal
#* in the Software without restriction, including without limitation the rights
#* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#* copies of the Software, and to permit persons to whom the Software is
#* furnished to do so, subject to the following conditions:
#*   
#*   The above copyright notice and this permission notice shall be included in all
#* copies or substantial portions of the Software.
#* 
#* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#* SOFTWARE.
#**************************************************************************

#' Define a sockets version of mclapply
#'
#' An implementation of \code{\link[parallel]{mclapply}} using \code{parallel::parLapply}
#'
#' Windows does not support forking. This makes it impossible to use mclapply on Windows to
#' farm out work to additional cores.
#'
#' More words
#'
#'
#' @param ... What you pass to mclapply
#' @return mclapply like list
#' @import parallel
#' @export
mclapply_socket <- function(
    X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
    mc.silent = FALSE, mc.cores = NULL,
    mc.cleanup = TRUE, mc.allow.recursive = TRUE
) {
  ## Create a cluster
  if (is.null(mc.cores)) {
    mc.cores <- min(length(X), detectCores())
  }
  cl <- parallel::makeCluster( mc.cores )
  
  tryCatch( {
    ## Find out the names of the loaded packages
    loaded.package.names <- c(
      ## Base packages
      sessionInfo()$basePkgs,
      ## Additional packages
      names( sessionInfo()$otherPkgs ))
    
    ### Ship it to the clusters
    parallel::clusterExport(cl,
                            'loaded.package.names',
                            envir=environment())
    
    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parallel::parLapply( cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        require(yy , character.only=TRUE)})
    })
    
    
    clusterExport_function(cl, FUN)
    
    ## Run the lapply in parallel, with a special case for the ... arguments
    if( length( list(...) ) == 0 ) {
      return(parallel::parLapply( cl = cl, X=X, fun=FUN) )
    } else {
      return(parallel::parLapply( cl = cl, X=X, fun=FUN, ...) )
    }
  }, finally = {
    ## Stop the cluster
    parallel::stopCluster(cl)
  })
}


#' Overwrite the serial version of mclapply on Windows only
#'
#' @param empty it takes nothing
#' @return mclapply like list
#' @export
mclapply <- switch( Sys.info()[['sysname']],
                    Windows = {mclapply_socket},
                    Linux   = {parallel::mclapply},
                    Darwin  = {parallel::mclapply})
#
# ## end mclapply.hack.R


clusterExport_function <- function(cl, FUN ) {
  
  ## We want the enclosing environment, not the calling environment
  ## (I had tried parent.frame, which was not what we needed)
  ##
  ## Written by Hadley Wickham, off the top of his head, when I asked him
  ##   for help at one of his Advanced R workshops.
  env <- environment(FUN)
  while(!identical(env, globalenv())) {
    env <- parent.env(env)
    parallel::clusterExport(cl, ls(all.names=TRUE, envir = env), envir = env)
  }
  parallel::clusterExport(cl, ls(all.names=TRUE, envir = env), envir = env)
  ## // End Hadley Wickham
}