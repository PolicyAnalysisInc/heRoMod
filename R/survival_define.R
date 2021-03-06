#**************************************************************************
#* 
#* Original work Copyright (C) 2017  Jordan Amdahl
#* Modified work Copyright (C) 2017  Antoine Pierucci
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


#' Define a Survival Distribution
#' 
#' Define a parametric survival distribution.
#' 
#' @param distribution A parametric survival distribution.
#' @param ... Additional distribution parameters (see 
#'   respective distribution help pages).
#'   
#' @return A `surv_dist` object.
#' @export
#' 
#' @examples
#' 
#' define_survival(distribution = "exp", rate = .5)
#' define_survival(distribution = "gompertz", rate = .5, shape = 1)
#' 
define_survival <- function(distribution = c("exp", "weibull",
                                             "weibullPH",
                                             "lnorm", "llogis",
                                             "gamma", "gompertz",
                                             "gengamma",
                                             "gengamma.orig",
                                             "genf", "genf.orig"),
                            ...) {
  
  distribution <- match.arg(distribution)
  
  list_arg <- list(...)
  
  if (distribution %in% c("exp", "weibull",
                          "llogis", "lnorm", "gamma")) {
    env_f <- asNamespace("stats")
  } else {
    if (! requireNamespace("flexsurv")) {
      stop("'flexsurv' package required.")
    }
    env_f <- asNamespace("flexsurv")
  }
  
  pf <- get(paste0("p", distribution),
            envir = env_f)
  
  names_fun <- setdiff(names(list_arg), "distribution")
  names_par <- setdiff(names(formals(pf)), "q")
  
  correct_names <- names_fun %in% names_par
  
  if (! all(correct_names)) {
    stop(sprintf(
      "Incorrect argument%s: %s.",
      plur(sum(! correct_names)),
      paste(names_fun[! correct_names], collapse = ", ")))
  }
  
  structure(
    list(
      distribution = distribution,
      ...
    ),
    class = c("surv_object", "surv_dist")
  )
}




#' Define a Parametric Mixture or Non-Mixture Cure Distribution
#' 
#' Define a parametric cure survival model.
#' 
#' @param distribution A parametric survival distribution.
#' @param theta The model cure fraction.
#' @param ... Additional distribution parameters (see 
#'   respective distribution help pages).
#' @param mixture a logical determining whether a mixture
#'   or non-mixture is being defined.
#'   
#' @return A `surv_dist_cure` object.
#' @export
#' 
#' @examples
#' 
#' define_survival_cure(distribution = "exp", theta = 0.34, rate = .5)
#' define_survival_cure(distribution = "weibull", theta = 0.5, shape = 1.5, scale = 34.43)
#' 
define_survival_cure <- function(distribution = c("exp", "weibull",
                                             "weibullPH",
                                             "lnorm", "llogis",
                                             "gamma", "gompertz",
                                             "gengamma",
                                             "gengamma.orig",
                                             "genf", "genf.orig"),
                            theta,
                            ...,
                            mixture = T) {
  
  distribution <- match.arg(distribution)
  
  list_arg <- list(...)
  
  if (distribution %in% c("exp", "weibull", "lnorm", "gamma")) {
    env_f <- asNamespace("stats")
  } else {
    if (! requireNamespace("flexsurv")) {
      stop("'flexsurv' package required.")
    }
    env_f <- asNamespace("flexsurv")
  }
  
  pf <- get(paste0("p", distribution),
            envir = env_f)
  
  names_fun <- setdiff(names(list_arg), "distribution")
  names_par <- setdiff(names(formals(pf)), "q")
  
  correct_names <- names_fun %in% names_par
  
  if (! all(correct_names)) {
    stop(sprintf(
      "Incorrect argument%s: %s.",
      plur(sum(! correct_names)),
      paste(names_fun[! correct_names], collapse = ", ")))
  }
  
  structure(
    list(
      distribution = distribution,
      mixture = mixture,
      theta = theta,
      ...
    ),
    class = c("surv_object", "surv_dist_cure")
  )
}

#' Define a Restricted Cubic Spline Survival Distribution
#' 
#' Define a restricted cubic spline parametric survival
#' distribution.
#' 
#' @param scale "hazard", "odds", or "normal", as described
#'   in flexsurvspline. With the default of no knots in
#'   addition to the boundaries, these models reduce to the
#'   Weibull, log-logistic and log-normal respectively. The
#'   scale must be common to all times.
#' @param ... Additional distribution parameters (see 
#'   respective distribution help pages).
#'   
#' @return A \code{surv_dist} object.
#'   
#' @examples
#' 
#' define_spline_survival(
#'   scale = "hazard", 
#'   gamma1 = -18.3122,
#'   gamma2 = 2.7511,
#'   gamma3 = 0.2292,
#'   knots1 = 4.276666,
#'   knots2 = 6.470800,
#'   knots3 = 7.806289
#'  )
# define_spline_survival(
#   scale = "odds",
#   gamma1 = -18.5809,
#   gamma2 = 2.7973,
#   gamma3 = 0.2035,
#   knots1 = 4.276666,
#   knots2 = 6.470800,
#   knots3 = 7.806289
# )
#' 
#' @export
define_spline_survival <- function(scale = c("hazard", "odds", 
                                             "normal"),
                                   ...) {
  
  scale <- match.arg(scale)
  list_arg <- lapply(list(...), unique)
  n_param <- length(list_arg)
  
  stopifnot(
    all(unlist(lapply(list_arg, length)) == 1),
    n_param >= 4,
    n_param %% 2 == 0
  )
  
  
  if (! requireNamespace("flexsurv")) {
    stop("'flexsurv' package required.")
  }
  
  pf <- flexsurv::unroll.function(
    flexsurv::psurvspline,
    gamma = seq_len(n_param/2),
    knots = seq_len(n_param/2)
  )
  
  names_fun <- setdiff(names(list_arg), "scale")
  names_par <- setdiff(names(formals(pf)), "q")
  
  correct_names <- names_fun %in% names_par
  
  if (! all(correct_names)) {
    stop(sprintf(
      "Incorrect argument%s: %s.",
      plur(sum(! correct_names)),
      paste(names_fun[! correct_names], collapse = ", ")))
  }
  
  structure(
    list(
      distribution = "survspline",
      scale = scale,
      ...
    ),
    class = c("surv_object", "surv_dist_spline")
  )
}

#' Define a survival distribution based on a function
#'
#' @param f a function that generates a vector of survival probabilities 
#'
#' @return a `surv_function` object, which can be used with [compute_surv()].
#' @export
define_surv_function <- function(f, ...) {
  
  structure(
    list(
      func = f,
      arguments = list(...)
    ),
    class = c("surv_object", "surv_function")
  )
  
}

#' Define a survival distribution based on explicit survival probabilities
#'
#' @param x a data frame with columns `time` and `survival` 
#'
#' @return a `surv_table` object, which can be used with [compute_surv()].
#' @export
#'
#' @examples
#'  x <- data.frame(time = c(0, 1, 5, 10), survival = c(1, 0.9, 0.7, 0.5))
#'  define_surv_table(x)
#'  
define_surv_table <- function(x){
  UseMethod("define_surv_table")
}

#' @rdname define_surv_table
#' @export
define_surv_table.data.frame <- function(x){
  required_names <- c("time", "survival")
  names_present <- required_names %in% names(x)
  if(any(!names_present)){
    stop("missing column",
         plur(sum(!names_present)),
         " in surv_table object: ",
         paste(required_names[!names_present], collapse = ", ")
    )
  }
  x$time <- as.numeric(x$time)
  x <- x[order(x$time),]
  dup_time <- duplicated(x$time)
  if(any(dup_time))
    stop("any time can appear only once in explicit survival data. ",
         "Duplicated time",
         plur(sum(dup_time)),
         ": ",
         paste(x$time[dup_time], collapse = ", ")
    )
  
  if(x$time[1] != 0 | x$survival[1] != 1)
    stop("surv_table data must start with time 0 and survival 1")
  
  increasing_survival <- diff(x$survival) > 0
  if(any(increasing_survival)){
    problem_times <- matrix(x$time[which(increasing_survival) + c(0,1)],
                            ncol = 2, byrow = TRUE)
    stop("survival cannot increase over time; see times:\n",
         paste("(", 
               problem_times[,1],
               ", ",
               problem_times[,2],
               ")",
               sep = "", collapse = ", ")
    )
  }
  class(x) <- c("surv_table", "surv_object", "data.frame")
  x
}
#' @rdname define_surv_table
#' @export
define_surv_table.character <- function(x){
  define_surv_table(read_file(x))
}

#' Define a survival distribution based on explicit survival probabilities
#'
#' @param x a data frame with columns `age`, `male`, and `female`, where age is
#' the starting age in each age band, male is the conditional probability of death
#' at each age for men, and female is the conditional probability of death at each
#' for women.
#' @param start_age The starting age of the population.
#' @param percent_male The percent of population that is male.
#' @param output_unit The time unit resulting survival distribution will be defined
#' in terms of. Valid options are `c("days", "weeks", "months", "years")`. Defaults to `"years"`.
#' @param age_col Optional argument to change name of the `age` column accepted by the
#' first argument.
#' @param male_col Optional argument to change name of the `male` column accepted by the
#' first argument.
#' @param female_col Optional argument to change name of the `female` column accepted by the
#' first argument.

#' @return a `surv_lifetable` object, which can be used with [compute_surv()].
#' @export
#'
#' @examples
#'  x <- data.frame(age = c(0, 1, 2, 3), male = c(0.011, 0.005, 0.003, 0.002), female = c(0.010, 0.005, 0.004, 0.002))
#'  define_surv_lifetable(x, 1, 0.45)
#'  
define_surv_lifetable <- function(x, start_age, percent_male, output_unit = "years", age_col = "age", male_col = "male", female_col = "female"){
  UseMethod("define_surv_lifetable", x)
}

#' @rdname define_surv_lifetable
#' @export
define_surv_lifetable.data.frame <- function(x, start_age, percent_male, output_unit = "years", age_col = "age", male_col = "male", female_col = "female") {
  required_names <- c(age_col, male_col, female_col)
  names_present <- required_names %in% names(x)
  start_age <- start_age[1]
  percent_male <- percent_male[1]
  output_unit <- output_unit[1]
  male_col <- male_col[1]
  age_col <- age_col[1]
  female_col <- female_col[1]
  if(any(!names_present)){
    stop("missing column",
         plur(sum(!names_present)),
         " in surv_lifetable object: ",
         paste(required_names[!names_present], collapse = ", ")
    )
  }
  x[[age_col]] <- as.numeric(x[[age_col]])
  x <- x[order(x[[age_col]]),]
  dup_time <- duplicated(x[[age_col]])
  if(any(dup_time))
    stop("any age can appear only once in life table data. ",
         "Duplicated age",
         plur(sum(dup_time)),
         ": ",
         paste(x[[age_col]][dup_time], collapse = ", ")
    )
  
  agediffs <- diff(x[[age_col]])
  agediff <- agediffs[1]
  if(!all(agediffs == agediff)) {
    stop("Life table must use constant age bands.")
  }
  class(x) <- c("surv_lifetable", "surv_object", "data.frame")
  
  first_index <- tail(which(x[[age_col]] <= start_age), 1)
  first_used_age <- x[[age_col]][first_index]
  indices_to_use <- seq_len(nrow(x)) >= first_index
  cut_points <- x[indices_to_use, ][[age_col]] - start_age
  cut_points[1] <- 0
  
  lambdas_male <- (-log(1 - x[[male_col]]) / agediff)[indices_to_use]
  lambdas_female <- (-log(1 - x[[female_col]]) / agediff)[indices_to_use]
  
  dpy <- get_dpy()

  func_male <- function(time) msm::ppexp(time,  rate = lambdas_male, t = cut_points, lower.tail = F)
  func_female <- function(time) msm::ppexp(time,  rate = lambdas_female, t = cut_points, lower.tail = F)
  the_surv_func <- function(time) {
    converted_time <- time * time_in_days(output_unit, dpy) / dpy
    func_male(converted_time) * percent_male[1] + func_female(converted_time) * (1 - percent_male[1])
  }
  
  attr(x, "start_age") <- start_age[1]
  attr(x, "percent_male") <- percent_male[1]
  attr(x, "surv_func") <- the_surv_func

  x
}
