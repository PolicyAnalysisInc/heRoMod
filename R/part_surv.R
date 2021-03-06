#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
#* Modified work Copyright (C) 2017  Matt Wiener
#* Modified work Copyright (C) 2017  Jordan Amdahl
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

allowed_fit_distributions <- c("exp", "weibull", "lnorm", "llogis", 
                               "gamma", "gompertz", "gengamma")

#' Define Partitioned Survival
#' 
#' Define a partitioned survival model with progression-free
#' survival and overall survival.
#' 
#' @param pfs,os Either results from 
#'   [flexsurv::flexsurvreg()] or 
#'   [define_survival()].
#' @param state_names named character vector, length 3 or 4.
#'   State names for progression-free state, progression, 
#'   (optionally terminal) and death respectively. Elements 
#'   should be named `"progression_free"`, 
#'   `"progression"`, (optionally `"terminal"`), 
#'   and `"death"`. See examples.
#' @param terminal_state Should a terminal state be 
#'   included? Only used when state names are not provided.
#' @param cycle_length The value of a Markov cycle in
#'   absolute time units.
#'   
#' @return A `part_surv` object.
#' @export
#' 
#' @examples
#' dist_pfs <- define_survival("exp", rate = 1)
#' dist_os <- define_survival("exp", rate = .5)
#' 
#' define_part_surv(
#'   pfs = dist_pfs,
#'   os = dist_os,
#'   state_names = c(
#'     progression_free = "A",
#'     progression = "B",
#'     terminal = "C",
#'     death = "D"
#'   )
#' )
#' # identical to:
#' define_part_surv(
#'   pfs = dist_pfs,
#'   os = dist_os,
#'   terminal_state = TRUE
#' )
#' 
define_part_surv <- function(pfs, os, state_names,
                             terminal_state = FALSE,
                             cycle_length = 1) {
  
  if (missing(state_names)) {
    message("No named state -> generating names.")
    
    if (terminal_state) {
      state_names <- LETTERS[seq_len(4)]
      names(state_names) <- c(
        "progression_free",
        "progression",
        "terminal",
        "death"
      )
    } else {
      state_names <- LETTERS[seq_len(3)]
      names(state_names) <- c(
        "progression_free",
        "progression",
        "death"
      )
    }
  }
  
  if (is.null(names(state_names))) {
    if (terminal_state) {
      warning("Argument 'terminal_state' ignored when state names are given.")
    }
    message("Trying to guess PFS model from state names...")
    state_names <- guess_part_surv_state_names(state_names)
  }
  
  define_part_surv_(
    pfs = lazyeval::lazy_(substitute(pfs), env = parent.frame()),
    os = lazyeval::lazy_(substitute(os), env = parent.frame()),
    state_names = state_names,
    cycle_length = cycle_length)
}

#' @export
#' @rdname define_part_surv
define_part_surv_ <- function(pfs, os, state_names,
                              cycle_length = 1) {
  
  if (is.null(names(state_names))) {
    state_names <- guess_part_surv_state_names(state_names)
  }
  
  stopifnot(
    inherits(pfs, "lazy"),
    inherits(os, "lazy"),
    
    length(state_names) %in% 3:4,
    ! is.null(names(state_names)),
    all(names(state_names) %in% c(
      "progression_free",
      "progression",
      "terminal",
      "death"
    )),
    ! any(duplicated(names(state_names))),
    length(cycle_length) %in% 1:2,
    all(cycle_length > 0)
  )
  
  if (length(cycle_length) == 1) {
    cycle_length <- rep(cycle_length, 2)
  }
  
  res <- list(
    pfs = pfs,
    os = os,
    state_names = state_names,
    cycle_length = cycle_length
  )
  
  structure(
    res,
    class = "part_surv"
  )
}



#' Convert saved fits to partitioned survival objects
#'
#' @param surv_inputs a list of matrices of `flexsurvreg` objects,
#'  for example the first element of the output of `survival_from_data`.
#' @param state_names names of states of the model
#'
#' @details  surv_inputs is a tibble with columns
#'   type (PFS or OS, not case sensitive), treatment, 
#'   set_name (for data subsets),
#'   dist (for survival distribution assumptions),
#'   fit (for the fitted survival object) and set_def
#'   (how the subset of data was defined, just to keep it around)


#' @export
define_part_surv_custom <- function(...) {
  .dots <- lazyeval::lazy_dots(...)
  define_part_surv_custom_(.dots = .dots)
}

#' @export
define_part_surv_custom_ <- function(.dots) {
  
  if (length(.dots)){
    check_names(names(.dots))
  }
  structure(.dots,
            class = c("part_surv_custom", class(.dots)))
  
}



#' @return a tibble of partitioned survival objects, similar to the
#'   original tibble of survival fits, with all the columns
#'   except type and fit, and a new column part_surv.
#' @export
#'
part_survs_from_surv_inputs <- function(surv_inputs, state_names) {
  
  surv_inputs %>%
    group_by(treatment, set_name, dist, set_def) %>%
    do(tibble(
      part_surv = list(make_part_surv_from_small_tibble(
        ., state_names = state_names))))
}

get_state_names.part_surv <- function(x) {
  x$state_names
}

eval_transition.part_surv <- function(x, parameters, expand, state_groups = NULL) {
  
  time_ <- c(0, parameters$markov_cycle)
  
  pfs_dist <- lazyeval::lazy_eval(
    x$pfs, 
    data = slice(parameters, 1)
  )
  
  pfs_surv <- compute_surv(
    pfs_dist,
    time = time_,
    cycle_length = x$cycle_length[1],
    type = "surv"
  )
  
  os_dist <- lazyeval::lazy_eval(
    x$os, 
    data = slice(parameters, 1)
  )
  
  os_surv <- compute_surv(
    os_dist,
    time = time_,
    cycle_length = x$cycle_length[2],
    type = "surv"
  )
  
  structure(
    list(
      pfs_surv = pfs_surv,
      os_surv = os_surv,
      state_names = x$state_names
    ),
    class = "eval_part_surv")
}

eval_transition.part_surv_custom <- function(x, parameters, expand, state_groups = NULL) {
  
  parameters$C <- -pi
  
  trace <- safe_eval(parameters, x, .vartype = "transition") %>%
    select(!!!names(x))
  
  posC <- trace == -pi
  
  if (! all(rowSums(posC) <= 1)) {
    stop("Only one 'C' is allowed per cycle.", call. = F)
  }
  
  valC <-  trace %>%
    mutate_all(list(~ifelse(. == -pi, 0, .))) %>%
    rowSums() %>%
    {1- .}
  
  trace <- mutate_all(trace, list(~ifelse(. == -pi, valC, .)))
  
  structure(
    list(
      trace = trace,
      state_names = names(x)
    ),
    class = "eval_part_surv_custom")
}

compute_counts.eval_part_surv <- function(x, init,
                                          inflow) {
  
  stopifnot(
    length(x$state_names) %in% 3:4,
    all(names(x$state_names) %in% c(
      "progression_free",
      "progression",
      "terminal",
      "death"
    )),
    ! any(duplicated(names(x$state_names))),
    length(init) == length(x$state_names),
    all(init[-1] == 0)
  )
  
  pfs_surv <- pmin(x$pfs_surv, x$os_surv)
  
  res <- tibble::tibble(
    progression_free = pfs_surv,
    progression      = x$os_surv - pfs_surv, 
    death            = 1 - x$os_surv
  )
  
  if (length(x$state_names) == 4) {
    res$terminal <- diff(c(0, res$death))
    res$death <- c(0, res$death[-nrow(res)])
  }
  
  if (any(res < 0)) {
    neg_cycles <- which(res < 0, arr.ind = TRUE)[, 1]
    stop("Negative counts in partitioned survival model, at cycle",
         plur(length(neg_cycles)),
         ": ",
         paste(neg_cycles, collapse = ", "))
  }
  
  res <- res * sum(init)
  
  names(res) <- x$state_names[names(res)]
  res <- res[x$state_names]
  
  n_state <- length(x$state_names)
  n_cycle <- nrow(res)
  
  trans_counts <- array(
    rep(0, n_state * n_state * (n_cycle - 1)),
    dim = c(n_state, n_state, (n_cycle - 1))
  )
  
  trans_counts[1, 2, ] <- pfs_surv[-n_cycle] - pfs_surv[-1]
  trans_counts[2, 3, ] <- x$os_surv[-n_cycle] - x$os_surv[-1]
  
  structure(
    res,
    class = c("cycle_counts", class(res)),
    transitions = trans_counts
  )
}



compute_counts.eval_part_surv_custom <- function(x, init,
                                                 inflow) {
  
  res <- x$trace
  if (any(res < 0)) {
    neg_cycles <- which(res < 0, arr.ind = TRUE)[, 1]
    stop("Negative counts in partitioned survival model, at cycle",
         plur(length(neg_cycles)),
         ": ",
         paste(neg_cycles, collapse = ", "))
  }
  
  if (all.equal(rowSums(res), rep(1,nrow(res))) != TRUE) {
    err_cycles <- which(any(rowSums(res) != 1))
    stop("Counts do not equal to 1, at cycle",
         plur(length(err_cycles)),
         ": ",
         paste(err_cycles, collapse = ", "))
  }
  
  res <- rbind(
    init,
    res * sum(init)
  )
  
  colnames(res) <- x$state_names
  
  structure(
    res,
    class = c("cycle_counts", class(res))
  )
}

guess_part_surv_state_names <- function(state_names) {
  death_state <- c(
    grep("death", state_names, ignore.case = TRUE),
    grep("dead", state_names, ignore.case = TRUE)
  )
  progfree_state <- grep("free", state_names, ignore.case = TRUE)
  progressive_state <- setdiff(
    grep("progress", state_names, ignore.case = TRUE),
    progfree_state)
  terminal_state <- grep("terminal", state_names, ignore.case = TRUE)
  
  if (length(death_state) != 1) {
    stop("State name representing death must contain ",
         "'death' or 'dead' (case insensitive).")
  }
  
  if (length(progfree_state) != 1) {
    stop("Progression free state (only) must have 'free' in its name.")
  }
  
  if (length(progressive_state) != 1) {
    stop("Progression state must have 'progress' ",
         "but not 'free', in its name.")
  }
  
  if (length(state_names) == 3) {
    names(state_names) <- c(
      "progression_free",
      "progression",
      "death")[c(
        progfree_state,
        progressive_state,
        death_state)]
    
  } else if (length(state_names) == 4) {
    if (length(terminal_state) == 0) {
      stop(
        "If there are 4 states, a state must be called 'terminal' ",
        "(not case sensitive)."
      )
    }
    
    names(state_names) <- c(
      "progression_free",
      "progression",
      "terminal",
      "death")[c(
        progfree_state,
        progressive_state,
        terminal_state,
        death_state)]
    
  } else {
    stop("There must be 3 or 4 states.")
  }
  
  message(sprintf(
    "Successfully guessed PFS from state names:\n%s",
    paste(paste0(
      "  ", names(state_names), " = ", state_names),
      collapse = "\n")
  ))
  
  state_names
}