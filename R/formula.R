#' @export
sort_formulas <- function(x, extra_vars = NULL) {
  
  
  # Deal with extra vars if given
  if (is.null(extra_vars)) {
    extras <- list()
  } else {
    extras <- set_names(extra_vars$formula, extra_vars$name)
  }
  
  # Extract variable names
  par_names <- x$name
  
  # Extract the variable names referenced in each variable's
  # formula
  var_list <-  purrr::map(x$formula, function(y) {
    vars <- y$direct_deps
    vars[vars %in% par_names]
  }) %>%
    set_names(x$name)
  
  # Define the lists of ordered and unordered variables
  ordered <- c()
  unordered <- var_list
  
  # While we still have variables in the unordered list...
  while (length(unordered) > 0) {
    
    # Define a vector which will hold the indices of each
    # variable to be moved to the ordered list
    to_remove <- c()
    
    # Loop through each unordered variable
    for (i in seq_len(length(unordered))) {
      
      # If all the variables its formula references are in the ordered
      # list then it can be added to ordered list.
      if (all(unordered[[i]] %in% ordered)) {
        
        # Append it to the list of ordered variables
        ordered <- c(ordered, names(unordered)[i])
        
        # Get current variable
        current_var <-  names(unordered)[i]
        
        # Make a named list of all variables
        all_vars <- c(
          set_names(x$formula, x$name),
          extras,
          c('strategy', 'group')
        )
        
        # Assemble first-order dependencies
        fo_deps <- unique(x$formula[[which(x$name == current_var)]]$direct_deps)
        
        # Add second+ order dependencies to variable
        x$formula[[which(x$name == current_var)]]$deps <- x$formula %>%
          set_names(x$name) %>%
          c(extras) %>%
          .[fo_deps] %>%
          lapply(function(y) y$depends) %>%
          discard(is.null) %>%
          flatten_chr(.) %>%
          union(fo_deps)
        
        # and mark it  for removal
        to_remove <- c(to_remove, i)
      }
    }
    
    if (length(to_remove) == 0) {
      # If we didn't find anything to move to the ordered list,
      # throw a circular reference error
      err_txt <- err_name_string(names(unordered))
      stop('Circular reference detected: ', err_txt)
    } else {
      # Otherwise, remove from the unordered list the variables
      # that were appended to the ordered list
      unordered <- unordered[-to_remove]
    }
  }
  
  # Return variables in sorted order
  res <- as_tibble(x[order(factor(x$name, levels = ordered)), ])
  class(res) <- c('hero_formulas', class(res))
  res
}


#' @export
define_formulas <- function(tbl) {
  tbl <- select(as_tibble(tbl), name, description, formula, source)
  formula_text_list <- as.list(tbl$formula)
  formula_list <- map(formula_text_list, define_hero_formula)
  tbl$formula <- formula_list
  class(tbl) <- c('hero_formulas', class(tbl))
  tbl
}

#' @export
print.hero_formulas <- function(x, ...) {
  x$formula <- map_chr(x$formula, as.character)
  class(x) <- c("tbl_df", "tbl", "data.frame")
  print(x)
}

#' @export
define_hero_formula <- function(text) {
  expr <-  parse(text = text)
  structure(
    list(text = text, expr = expr, direct_deps = all.vars(expr), deps = NA),
    class = 'hero_formula'
  )
}

#' @export
print.hero_formula <- function(x, ...) {
  cat(x$text)
}

#' @export
as.character.hero_formula <- function(x, ...) {
  x$text
}


#' @export
evaluate <- function(x, env, ...) {
  UseMethod('evaluate', x)
}

#' @export
evaluate.hero_formula <- function(x, env = parent.frame(), ...) {
  eval(x$expr, env)
}

#' @export
evaluate.hero_formulas <- function(x, env = parent.frame(), ...) {
  n_formulas <- nrow(x)
  for(i in seq_len(n_formulas)) {
    value <- evaluate(x$formula[[i]], env = env)
    name <- x$name[i]
    assign(name, value, env)
  }
  env
}