sort_hero_formula_list <- function(l) {
  
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
define_hero_formula <- function(text) {
  expr <-  parse(text = text)
  structure(
    list(text = text, expr = expr, direct_deps = all.vars(expr)),
    class = 'hero_formula'
  )
}

#' @export
print.hero_formula <- function(x, ...) {
  cat(x$text)
}

#' @export
format.hero_formula <- function(x, ...) {
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