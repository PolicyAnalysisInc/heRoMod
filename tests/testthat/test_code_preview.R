
read_file_to_string <- function(path) {
  readChar(path, file.info(path)$size)
}


test_that("Code preivew generates appropriate HTML when a heading is included", {
  withr::with_dir(new = tempdir(), {
    run_markdown(
      text = "## # Foo \n mean(c(1,2,3,4,5))",
      data = list(),
      name = 'thetitle'
    )
    html <- read_file_to_string('thetitle.html')
    
    # Title tag is present and properly closed
    title_count <- stringr::str_count(
      html,
      stringr::fixed("<title>thetitle.knit</title>")
    )
    expect_equal(title_count, 1)
    
    # Heading is present exactly once
    heading_count <- stringr::str_count(
      html,
      stringr::fixed("<h1>Foo</h1>")
    )
    expect_equal(heading_count, 1)
    
    # Code is present exactly once
    code_count <- stringr::str_count(
      html,
      stringr::fixed("mean(c(1,2,3,4,5))")
    )
    expect_equal(code_count, 1)
    
    # Result is present exactly once
    res_count <- stringr::str_count(
      html,
      stringr::fixed("[1] 3")
    )
    expect_equal(res_count, 1)
  })
})

test_that("Code preivew generates appropriate HTML when a heading is not included", {
  withr::with_dir(new = tempdir(), {
    run_markdown(
      text = "mean(c(1,2,3,4,5))",
      data = list(),
      name = 'thetitle'
    )
    html <- read_file_to_string('thetitle.html')
    
    # Title tag is present and properly closed
    title_count <- stringr::str_count(
      html,
      stringr::fixed("<title>thetitle.knit</title>")
    )
    expect_equal(title_count, 1)
    
    # No heading is present
    heading_count <- stringr::str_count(
      html,
      stringr::fixed("<h1>")
    )
    expect_equal(heading_count, 0)
    
    # Code is present exactly once
    code_count <- stringr::str_count(
      html,
      stringr::fixed("mean(c(1,2,3,4,5))")
    )
    expect_equal(code_count, 1)
    
    # Result is present exactly once
    res_count <- stringr::str_count(
      html,
      stringr::fixed("[1] 3")
    )
    expect_equal(res_count, 1)
  })
})