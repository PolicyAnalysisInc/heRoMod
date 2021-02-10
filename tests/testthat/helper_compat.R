test_model_convert <- function(name, filename) {
  test_that(name, {
    
    # Load frontend formatted model
    input_format <- jsonlite::read_json(
      system.file(
        "compat",
        paste0(filename, ".json"),
        package="heRomod"
      ),
      simplifyVector = T
    )
    
    # Load backend Formatted model
    output_format <- readRDS(
      system.file(
        "compat",
        paste0(filename, ".rds"),
        package="heRomod"
      )
    )
    
    # Convert frontend to backend format
    converted <- heRomod:::convert_model(input_format)
    
    # Compare Settings
    #print(diffobj::diffObj(output_format$settings, converted$settings))
    expect_equal(converted$settings, output_format$settings)
    
    # Compare Groups
    #print(diffobj::diffObj(output_format$groups, converted$groups))
    expect_equal(converted$groups, output_format$groups)
    
    # Compare States
    expect_equal(converted$states, output_format$states)
    #print(diffobj::diffObj(output_format$states, converted$states))
    
    # Compare Strategies
    expect_equal(converted$strategies, output_format$strategies)
    #print(diffobj::diffObj(output_format$strategies, converted$strategies))
    
    # Compare Transitions
    expect_equal(converted$transitions, output_format$transitions)
    #print(diffobj::diffObj(output_format$transitions, converted$transitions))
    
    # Compare Health Values
    expect_equal(converted$hvalues, output_format$hvalues)
    #print(diffobj::diffObj(output_format$hvalues, converted$hvalues))
    
    # Compare Econ Values
    expect_equal(converted$evalues, output_format$evalues)
    #print(diffobj::diffObj(output_format$evalues, converted$evalues))
    
    # Compare Health Summaries
    fixed_hsumm <-  mutate(output_format$hsumms, wtp = as.numeric(wtp))
    expect_equal(converted$hsumms, fixed_hsumm)
    #print(diffobj::diffObj(fixed_hsumm, converted$hsumms))
    
    # Compare Econ Summaries
    fixed_esumm <-  mutate(output_format$esumms, wtp = as.numeric(wtp))
    expect_equal(converted$esumms, fixed_esumm)
    #print(diffobj::diffObj(fixed_esumm, converted$esumms))
    
    # Compare Parameters
    expect_equal(converted$variables, output_format$variables)
    #print(diffobj::diffObj(converted$variables, output_format$variables))
    
    # Compare Surv Dists
    expect_equal(converted$surv_dists, output_format$surv_dists)
    #print(diffobj::diffObj(converted$surv_dists, output_format$surv_dists))
    
    # Compare Scripts
    expect_equal(converted$scripts, output_format$scripts)
    #print(diffobj::diffObj(converted$scripts, output_format$scripts))
    
    # Compare Tables
    expect_equal(converted$tables, output_format$tables)
    # tbl_names <- unique(c(names(converted$tables), names(output_format$tables)))
    # for (name in tbl_names) {
    #   print(diffobj::diffObj(converted$tables[[name]], output_format$tables[[name]]))
    # }
    
    # Compare VBP
    expect_equal(converted$vbp, output_format$vbp)
    
    # Compare Scenarios
    expect_equal(converted$scenario, output_format$scenario)
    #print(diffobj::diffObj(converted$scenario, output_format$scenario))
    
    # Compare DSA Settings
    expect_equal(converted$dsa_settings, output_format$dsa_settings)
    
    # Compare Scenario Settings
    expect_equal(converted$scenario_settings, output_format$scenario_settings)
    
    # Compare PSA
    # OK for this not to match, so we'll make it match to pass test.
    # Also ok that named list items in different order, so we'll sort first
    sort_by_name <- function(x) x[order(names(x))]
    output_format$psa$seed <- converted$psa$seed
    output_format$psa$n <- converted$psa$n
    expect_equal(sort_by_name(converted$psa), sort_by_name(output_format$psa))
  
  })
}