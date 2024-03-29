% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_model_define.R
\name{run_model}
\alias{run_model}
\alias{run_model_}
\title{Run Markov Model}
\usage{
run_model(
  ...,
  parameters = define_parameters(),
  init = c(1000L, rep(0L, get_state_number(get_states(list(...)[[1]])) - 1)),
  cycles = 1,
  method = "life-table",
  cost = NULL,
  effect = NULL,
  state_time_limit = NULL,
  central_strategy = NULL,
  inflow = rep(0L, get_state_number(get_states(list(...)[[1]]))),
  aux_params = NULL,
  parallel = F,
  cores = 1,
  disc_method = "start",
  create_progress_reporter = create_null_prog_reporter,
  progress_reporter = create_progress_reporter(),
  state_groups = NULL,
  individual_level = F
)

run_model_(
  uneval_strategy_list,
  parameters,
  init,
  cycles,
  method,
  cost,
  effect,
  state_time_limit,
  central_strategy,
  inflow,
  aux_params = NULL,
  parallel = F,
  cores = 1,
  disc_method = "start",
  create_progress_reporter = create_null_prog_reporter,
  progress_reporter = create_progress_reporter(),
  state_groups = NULL,
  individual_level = F
)
}
\arguments{
\item{...}{One or more \code{uneval_model} object.}

\item{parameters}{Optional. An object generated by
\code{\link[=define_parameters]{define_parameters()}}.}

\item{init}{numeric vector or result of \code{\link[=define_init]{define_init()}},
same length as number of states. Number of individuals
in each state at the beginning.}

\item{cycles}{positive integer. Number of Markov Cycles
to compute.}

\item{method}{Counting method.}

\item{cost}{Names or expression to compute cost on the
cost-effectiveness plane.}

\item{effect}{Names or expression to compute effect on
the cost-effectiveness plane.}

\item{state_time_limit}{Optional expansion limit for
\code{state_time}, see details.}

\item{central_strategy}{character. The name of the
strategy at the center of the cost-effectiveness plane,
for readability.}

\item{inflow}{numeric vector or result of
\code{\link[=define_inflow]{define_inflow()}}, similar to \code{init}. Number of new
individuals in each state per cycle.}

\item{aux_params}{Optional. an object generated by \code{\link[=define_parameters]{define_parameters()}}
representing object parameters.}

\item{parallel}{Optional. Whether to use parallel evaluation of strategies.}

\item{uneval_strategy_list}{List of models, only used by
\code{\link[=run_model_]{run_model_()}} to avoid using \code{...}.}
}
\value{
A list of evaluated models with computed values.
}
\description{
Runs one or more strategy. When more than one strategy is
provided, all strategies should have the same states and
state value names.
}
\details{
In order to compute comparisons strategies must be
similar (same states and state value names). Thus
strategies can only differ through transition matrix cell
values and values attached to states (but not state value
names).

The initial number of individuals in each state and the
number of cycle will be the same for all strategies

\code{state_time_limit} can be specified in 3 different ways:
\enumerate{
\item As a single value: the limit is applied to all states
in all strategies. 2. As a named vector (where names are
state names): the limits are applied to the given state
names, for all strategies. 3. As a named list of named
vectors: the limits are applied to the given state names
for the given strategies.
}
}
\examples{
# running a single model

mod1 <-
  define_strategy(
    transition = define_transition(
      .5, .5,
      .1, .9
    ),
    define_state(
      cost = 543,
      ly = 1
    ),
    define_state(
      cost = 432,
      ly = 1
    )
  )


res <- run_model(
  mod1,
  init = c(100, 0),
  cycles = 2,
  cost = cost,
  effect = ly
)

# running several models
mod2 <-
  define_strategy(
    transition = define_transition(
      .5, .5,
      .1, .9
    ),
    define_state(
      cost = 789,
      ly = 1
    ),
    define_state(
      cost = 456,
      ly = 1
    )
    
  )


res2 <- run_model(
  mod1, mod2,
  init = c(100, 0),
  cycles = 10,
  cost = cost,
  effect = ly
)
}
