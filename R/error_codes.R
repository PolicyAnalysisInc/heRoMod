error_codes <- list(
  vbp_not_linear = 'Error in VBP analysis, cost was not a linear function of price. This typically means you have selected the wrong parameter.',
  vbp_affects_outcomes = 'Error in VBP analysis, price parameter should not affect outcomes. This typically means you have selected the wrong parameter.',
  duplicate_param_in_scenario = 'Error in scenario "{scenario_name}", parameter "{parameter_name}" is used more than once.',
  scenario_wrong_datatype = 'Error in scenario analysis, scenarios be provided as a data.frame object.',
  scenario_null = 'Error in scenario analysis, no scenarios were defined.',
  scenario_missing_name = 'Error in scenario analysis, scenario was missing name.',
  scenario_missing_param_name = 'Error in scenario "{scenario_name}", missing parameter name.',
  scenario_missing_value = 'Error in scenario "{scenario_name}", parameter "{parameter_name}" is missing value.',
  syntax_error = 'Syntax error in {context}.',
  state_group_wrong_col_type = 'Error in state group specification. Column "{col}" must be of type {type}.',
  state_group_wrong_type = 'Error in state group specification, must be provided as data.frame with columns for "name", "state_group", and "share".',
  state_group_bad_names = 'Error in state group specification, invalid states referenced: {names}.',
  patch_model_bad_key = 'Error in patch_model, key "{key}" already exists on model.',
  zero_initial_prob = 'Initial state probabilities must be defined.'
)