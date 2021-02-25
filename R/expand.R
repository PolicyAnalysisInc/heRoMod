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
state_time_vars <- c("state_time", "state_day", "state_week", "state_month", "state_year")

has_state_time <- function(x, ...) {
  UseMethod("has_state_time")
}

#' @export
has_state_time.uneval_matrix <- function(x, ...) {
  unlist(lapply(x, function(y) any(state_time_vars %in% all.vars(y$expr))))
}

#' @export
has_state_time.part_surv <- function(x, ...) {
  FALSE
}

#' @export
has_state_time.part_surv_custom <- function(x, ...) {
  FALSE
}

#' @export
has_state_time.uneval_state_list <- function(x, ...) {
  state_names <- get_state_names(x)
  s_expand <- unlist(lapply(x, function(y) has_state_time(y)))
  
  # Figure out state expansion based on state transitions
  # References to state_time in state transitions are based
  # on the from state.  If the from state is NA, then use
  # of state_time will expand ALL states.
  state_trans <- attr(x, "transitions")
  if(!is.null(state_trans)) {
    st_to_expand <- has_state_time(state_trans)
    st_from <- lapply(state_trans, function(y) attr(y, "from"))
    st_expand <- st_from[st_to_expand]
    st_from_expanded <- unlist(st_expand)
    if(!is.null(st_from_expanded)){
      if(any(is.na(st_from_expanded))) {
        # Expand all states if from state is NA in a value referencing
        # state_time
        s_expand <- rep(T, length(s_expand))
      } else {
        for(i in seq_len(length(state_names))) {
          # Expand states where state transitions from reference
          # state_time
          if(state_names[i] %in% st_from_expanded) {
            s_expand[i] <- T
          }
        }
      }
    }
  }
  s_expand
}

#' @export
has_state_time.uneval_state_transition_list <- function(x, ...) {
  unlist(lapply(x, function(y) any(has_state_time(y))))
}

#' @export
has_state_time.state <- function(x, ...) {
  any(unlist(lapply(x, function(y) any(state_time_vars %in% all.vars(y$expr)))))
}

#' @export
has_state_time.state_transition <- function(x, ...) {
  any(unlist(lapply(x, function(y) any(state_time_vars %in% all.vars(y$expr)))))
}

#' Convert Lazy Dots to Expression List
#' 
#' This function is used by [interpolate()].
#'
#' @param .dots A lazy dots object.
#'
#' @return A list of expression.
#' @keywords internal
as_expr_list <- function(.dots) {
  setNames(
    lapply(.dots, function(x) x$expr),
    names(.dots)
  )
}

#' Interpolate Lazy Dots
#' 
#' Sequentially interpolates lazy dots, optionally using 
#' external references.
#' 
#' The interpolation is sequential: the second dot is 
#' interpolated using the first, the third using the 
#' interpolated first two, and so on.
#' 
#' @param x A parameter, transition matrix or state list
#'   object.
#' @param more A list of expressions.
#' @param ... Addition parameters passed to methods.
#'   
#' @return An interpolated lazy dots object.
#' @keywords internal
interpolate <- function(x, ...) {
  UseMethod("interpolate")
}

#' @export
#' @rdname interpolate
interpolate.default <- function(x, more = NULL, ...) {
  
  res <- NULL
  
  for (i in seq_along(x)) {
    to_interp <- x[[i]]
    for_interp <- c(more, as_expr_list(res))
    funs <- all.funs(to_interp$expr)
    
    if (any(pb <- funs %in% names(for_interp))) {
      stop(sprintf(
        "Some parameters are named like a function, this is incompatible with the use of 'state_time': %s.",
        paste(funs[pb], collapse = ", ")
      ))
    }
    
    new <- setNames(list(lazyeval::interp(
      to_interp,
      .values = for_interp
    )
    ), names(x)[i])
    res <- c(res, new)
    
  }
  lazyeval::as.lazy_dots(res)
}


#' @export
#' @rdname interpolate
interpolate.uneval_matrix <- function(x, ...) {
  res <- interpolate.default(x, ...)
  define_transition_(res, get_state_names(x))
}

#' @export
#' @rdname interpolate
interpolate.state <- function(x, ...) {
  res <- interpolate.default(x, ...)
  define_state_(res)
}

#' @export
#' @rdname interpolate
interpolate.state_transition <- function(x, ...) {
  from <- attr(x, "from")
  to <- attr(x, "to")
  res <- interpolate.default(x, ...)
  define_state_transition_(from = from, to = to, res)
}

#' @export
#' @rdname interpolate
interpolate.part_surv <- function(x, ...) {
  x
}

#' @export
#' @rdname interpolate
interpolate.part_surv_custom <- function(x, ...) {
  x
}

#' @export
#' @rdname interpolate
interpolate.uneval_state_list <- function(x, ...) {
  for (i in seq_along(x)) {
    x[[i]] <- interpolate(x[[i]], ...)
  }
  state_trans <- attr(x, "transitions")
  if(!is.null(state_trans)) {
    attr(x, "transitions") <- interpolate(state_trans)
  }
  x
}

#' @export
#' @rdname interpolate
interpolate.uneval_state_transition_list <- function(x, ...) {
  for (i in seq_along(x)) {
    x[[i]] <- interpolate(x[[i]], ...)
  }
  x
}

all.funs <- function(expr) {
  with_funs <- table(all.names(expr))
  without_funs <- table(all.names(expr, functions = FALSE))
  
  with_funs[names(without_funs)] <-
    with_funs[names(without_funs)] -
    without_funs
  names(with_funs)[with_funs > 0]
}

complete_stl <- function(scl, state_names,
                         strategy_names, cycles, state_groups = NULL) {
  uni <- FALSE
  
  
  if(is.null(scl)) {
    scl <- cycles + 1
  } else {
    # Handle limiting of state time using state groups if specified
    if (!is.null(state_groups)) {
        
      scl_table <- tibble(
        name = names(scl),
        limit = unname(scl)
      ) %>%
        full_join(state_groups, by = c('name')) %>%
        group_by(group) %>%
        mutate(limit = ifelse(all(is.na(limit)), NA, max(limit, na.rm = T))) %>%
        ungroup() %>%
        filter(!is.na(limit))
      
      if (nrow(scl_table) == 0) {
        scl <- cycles + 1
      }
      
      scl <- set_names(
        as.numeric(scl_table$limit),
        scl_table$name
      )
    }
  }
  
  if (is.numeric(scl) && length(scl) == 1 && is.null(names(scl))) {
    uni <- TRUE
    stopifnot(
      #scl <= (cycles + 1),
      scl > 0,
      ! is.na(scl),
      is.wholenumber(scl)
    )
    cycles <- scl
  }
  
  res <- lapply(
    strategy_names,
    function(x) rep(cycles, length(state_names)) %>% 
      setNames(state_names)
  ) %>% 
    setNames(strategy_names)
  
  if (is.null(scl) || uni) {
    return(res)
  }
  
  check_scl <- function(scl, cycles) {
    if (is.null(names(scl))) {
      stop("'state_time_limit' must be named.")
    }
    if (any(duplicated(names(scl)))) {
      stop("'state_time_limit' names must be unique.")
    }
    if (any(pb <- ! names(scl) %in% state_names)) {
      stop(sprintf(
        "Some 'state_time_limit' names are not state names: %s.",
        paste(names(scl)[pb], collapse = ", ")
      ))
    }
    
    stopifnot(
      ! is.na(scl),
      scl > 0,
      #scl <= cycles + 1,
      is.wholenumber(scl)
    )
  }
  
  if (is.numeric(scl)) {
    check_scl(scl, cycles)
    for (i in seq_along(res)) {
      res[[i]][names(scl)] <- scl
    }
    return(res)
  }
  
  if (is.list(scl)) {
    if (any(pb <- ! names(scl) %in% strategy_names)) {
      stop(sprintf(
        "Some 'state_limit_cycle' names are not model names: %s.",
        paste(names(scl)[pb], collapse = ", ")
      ))
    }
    for (n in names(scl)) {
      check_scl(scl[[n]], cycles)
      
      res[[n]][names(scl[[n]])] <- scl[[n]]
    }
    return(res)
  }
  
  stop("'Incorrect 'state_time_limit' type.")
}

check_state_groups <- function(state_groups, state_names) {
  
  if (is.null(state_groups)) return()
  
  # Check that its a data frame and has right columns
  is_df <- "data.frame" %in% class(state_groups)
  has_right_cols <- c('name', 'group', 'share') %in% colnames(state_groups)
  
  if (!all(is_df, has_right_cols)) {
    stop(error_codes$state_group_wrong_type, call. = F)
  }
  
  # Check the types for each column
  name_right_type <- class(state_groups$name) == 'character'
  if (!name_right_type) {
    stop(
      glue(error_codes$state_group_wrong_col_type, col = 'name', type = 'character'),
      call. = F
    )
  }
  
  group_right_type <- class(state_groups$group) == 'character'
  if (!group_right_type) {
    stop(
      glue(error_codes$state_group_wrong_col_type, col = 'group', type = 'character'),
      call. = F
    )
  }
  
  share_right_type <- class(state_groups$share) %in% c('integer', 'numeric', 'logical')
  if (!share_right_type) {
    stop(
      glue(error_codes$state_group_wrong_col_type, col = 'share', type = 'logical, integer, or numeric'),
      call. = F
    )
  }
  
  # Check that all state names exist
  correct_state_names <- state_groups$name %in% state_names
  if (!all(correct_state_names)) {
    incorrect_names <- paste0('"', state_groups$name[!correct_state_names], '"', collapse = ', ')
    stop(
      glue(error_codes$state_group_bad_names, names = incorrect_names),
      call. = F
    )
  }
  
}
