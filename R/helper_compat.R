test_model_convert <- function(name, filename) {
  test_that(name, function() {
    input_format <- jsonlite::read_json(system.file("compat", paste0(filename, ".json"), package="heRomod"))
    output_format <- readRDS(system.file("compat", paste0(filename, ".rds"), package="heRomod"))
    converted <- heRomod:::convert_model(heRo3_format)
    expect_equal(converted$settings, output_format$settings)
  })
}