error_codes <- list(
  vbp_not_linear = 'Error in VBP analysis, cost was not a linear function of price. This typically means you have selected the wrong parameter.',
  vbp_affects_outcomes = 'Error in VBP analysis, price parameter should not affect outcomes. This typically means you have selected the wrong parameter.',
  duplicate_param_in_scenario = 'Error in scenario "{scenario_name}", parameter "{parameter_name}" is used more than once.',
  scenario_wrong_datatype = 'Error in scenario analysis, scenarios be provided as a data.frame object.',
  scenario_null = 'Error in scenario analysis, no scenarios were defined.',
  scenario_missing_name = 'Error in scenario analysis, scenario was missing name.',
  scenario_missing_param_name = 'Error in scenario "{scenario_name}", missing parameter name.',
  scenario_missing_value = 'Error in scenario "{scenario_name}", parameter "{parameter_name}" is missing value.'
)