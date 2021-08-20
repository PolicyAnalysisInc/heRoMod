context("Running heRo model")

models_to_test <- tibble::tribble(
  ~name,                        ~path,                    ~bc, ~vbp, ~dsa, ~scen, ~psa, ~export, ~twsa,
  'Simple PSM',                 'example_simple_psm',     T,   T,    T,    F,     T,    T,       F,
  'Custom PSM',                 'psm_responders',         T,   F,    F,    F,     F,    T,       F,
  'Groups Model',               'groups',                 T,   T,    T,    T,     T,    T,       T,
  'Simple Markov Model',        'markov_model',           T,   T,    T,    T,     T,    T,       T,
  'Advanced Survival Modeling', 'advanced_surv_modeling', T,   T,    F,    F,     F,    F,       F,
  'TA447 Replication',          'ta447',                  T,   F,    F,    F,     F,    F,       F,
  'Shared State-Time',          'shared_st',              T,   F,    F,    F,     F,    F,       F,
  'Sparse Matrix',              'sparse',                 T,   F,    F,    F,     F,    F,       F
)

models_to_test %>%
  rowwise() %>%
  group_split() %>%
  purrr::walk(function(x) {
    do.call(test_model_results, x)
  })
