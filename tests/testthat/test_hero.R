context("Running heRo model")

models_to_test <- tibble::tribble(
  ~name,                        ~path,                    ~bc, ~vbp, ~dsa, ~scen, ~psa, ~export,
  'Simple PSM',                 'example_simple_psm',     T,   T,    T,    F,     T,    T,
  'Custom PSM',                 'psm_responders',         T,   F,    T,    F,     T,    T,
  'Groups Model',               'groups',                 T,   T,    T,    T,     T,    T,
  'Simple Markov Model',        'markov_model',           T,   T,    T,    T,     T,    T,
  'Advanced Survival Modeling', 'advanced_surv_modeling', T,   T,    T,    F,     F,    T,
  'TA447 Replication',          'ta447',                  T,   F,    T,    F,     F,    T,
  'Shared State-Time',          'shared_st',              T,   F,    T,    F,     F,    F,
  'Sparse Matrix',              'sparse',                 T,   F,    F,    F,     F,    F,
)

models_to_test %>%
  rowwise() %>%
  group_split() %>%
  purrr::walk(function(x) {
    do.call(test_model_results, x)
  })
