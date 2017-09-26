# param <- define_parameters(
#   p1 = .5,
#   p2 = .2,
#   r = .05
# )
# mod1 <- define_strategy(
#   transition = define_transition(
#     C, p1,
#     p2, C
#   ),
#   define_state(
#     cost = discount(543, r),
#     ly = 1
#   ),
#   define_state(
#     cost = discount(432, r),
#     ly = .5
#   )
# )
# 
# mod2 <- define_strategy(
#   transition = define_transition(
#     C, p1,
#     p2, C
#   ),
#   define_state(
#     cost = 789,
#     ly = 1
#   ),
#   define_state(
#     cost = 456,
#     ly = .8
#   )
# )
# 
# res2 <- run_model(
#   mod1, mod2,
#   parameters = param,
#   init = c(100, 0),
#   cycles = 10,
#   cost = cost,
#   effect = ly
# )
# 
# ds <- define_dsa(
#   p1, .1, .9,
#   p2, .1, .3,
#   r, .05, .1
# )
# print(ds)
# 
# x <- run_dsa(res2, ds)
# 
# plot(x, value = "cost")
# 
# # can be specified as a function of other parameters
# 
# 
# ds2 <- define_dsa(
#   p2, p1 - .1, p1 + .1
# )
# 
# run_dsa(res2, ds2)

#### Define parameters ####
par_mod <- define_parameters(
  age_base = 20, 
  age_cycle = model_time + age_base)

par_mod <- modify(
  par_mod, 
  sex_indiv = "MLE", # MLE => male in the WHO database 
  p_death_all = get_who_mr(
    age = age_cycle, 
    sex = sex_indiv, 
    country = "GBR", 
    local = TRUE))

par_mod <- modify(
  par_mod, 
  p_death_disease = compute_surv(
    fit_death_disease,
    time = state_time, 
    km_limit = 5))

tab_surv <- structure(list(time = c(0.4, 8.7, 7, 5.1, 9.2, 1, 0.5, 3.3, 1.8, 3, 6.7, 3.7, 1.1, 5.9, 5.1, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10), status = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)), .Names = c("time", "status"), row.names = c(NA, -25L), class = "data.frame")

fit_death_disease <- flexsurv::flexsurvreg(
  survival::Surv(time, status) ~ 1, 
  dist = "weibull", 
  data = tab_surv)

par_mod <- modify(
  par_mod, 
  p_death_symp = combine_probs(
    p_death_all, 
    p_death_disease))

par_mod <- modify(
  par_mod, 
  p_disease_base = 0.25, 
  med_effect = 0.5, 
  p_disease_med = p_disease_base * med_effect)

par_mod <- modify(
  par_mod, 
  shape = 1.5, # We will see later why we need
  scale = 5, # to define these 2 parameters here. 
  p_disease_surg = define_survival(
    distribution = "weibull",
    shape = shape,
    scale = scale) %>% compute_surv(time = state_time))

par_mod <- modify(
  par_mod, 
  cost_surg = 20000, 
  cost_surg_cycle = ifelse(state_time == 1, cost_surg, 0))

par_mod <- modify(
  par_mod, 
  cost_hospit_start = 11000, 
  cost_hospit_end = 9000, 
  n_years = 9,
  cost_hospit_cycle = ifelse(
    state_time < n_years, 
    cost_hospit_start, 
    cost_hospit_end))

par_mod <- modify(
  par_mod, 
  p_cured = 0.001,
  cost_med = 5000, 
  dr = 0.05,
  qaly_disease = 0.5)

#### Define transition probabilities ####
### Base Strategy
mat_base <- define_transition(
  state_names = c("pre", "symp", "death"), 
  
  C,       p_disease_base, p_death_all,
  p_cured, C,              p_death_symp, 
  0,       0,              1)

### Medicine strategy
mat_med <- define_transition(
  state_names = c("pre", "symp", "death"), 
  
  C,       p_disease_med, p_death_all,
  p_cured, C,             p_death_symp,
  0,       0,             1)

### Surgery strategy
mat_surg <- define_transition(
  state_names = c("pre", "symp", "death"), 
  
  C,       p_disease_surg, p_death_all,
  p_cured, C,              p_death_symp,
  0,       0,              1)


#### Define state rewards ####
## State PreSymptomatic (Pre)
state_pre <- define_state(
  cost_treat = dispatch_strategy(
    base = 0, # no treatment => no treatment cost 
    med = cost_med, 
    surg = cost_surg_cycle), 
  cost_hospit = 0, # good health => no hospital expenses 
  cost_total = discount(cost_treat + cost_hospit, r = dr), 
  qaly = 1)

## State Symptomatic (Symp)
state_symp <- define_state(
  cost_treat = 0, 
  cost_hospit = cost_hospit_cycle, 
  cost_total = discount(cost_treat + cost_hospit, r = dr), 
  qaly = qaly_disease)

## State Death (Death)
state_death <- define_state(
  cost_treat = 0, 
  cost_hospit = 0, 
  cost_total = 0, 
  qaly = 0)

#### Define strategies ####
### Base
strat_base <- define_strategy(
  transition = mat_base, 
  pre = state_pre, 
  symp = state_symp, 
  death = state_death)

### Medicine
strat_med <- define_strategy(
  transition = mat_med, 
  pre = state_pre, 
  symp = state_symp, 
  death = state_death)

### Surgery
strat_surg <- define_strategy(
  transition = mat_surg, 
  pre = state_pre, 
  symp = state_symp, 
  death = state_death)

#### Run model ####
res_mod <- run_model(
  parameters = par_mod, 
  base = strat_base, 
  med = strat_med, 
  surg = strat_surg, 
  cycles = 10, 
  cost = cost_total, 
  effect = qaly, 
  method = "life-table")

#### Value-Based Pricing (VBP) analysis ####
## VBP on cost_med
def_vbp <- define_vbp(
  cost_surg, 0, 1000
)

### Run VBP 
res_vbp <- run_vbp(model = res_mod, 
                   vbp = def_vbp,
                   strategy_vbp = "med",
                   wtp_thresholds = c(0, 10000))
plot(res_vbp)
plot(res_vbp, bw = T)
