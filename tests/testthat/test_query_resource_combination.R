#test you get expected error if resource combination is not found ing gtr_combinations------------------
testthat::test_that(
  "unknown resource combination gives error",
  expect_error(
    gtR::query_resource_combination("organisation", "product", "961756BF-E31F-4A13-836F-0A09BA02385C"),
    "'resource' and 'output' combination must be in the list of options as per 'gtr_combinations'",
    fixed = T
  )
)

#test you get expected error if choosing a size outside range------------------------
testthat::test_that(
  "size outside range gives error",
  expect_error(
    gtR::query_resource_combination("organisation", "project", "961756BF-E31F-4A13-836F-0A09BA02385C", size = 101),
    "'size' must be an integer >= 10 and <= 100",
    fixed = T
  )
)

#test that you get a data frame or list as expected-----------------------------

#that you get a dataframe, assuming no API errors
testthat::expect_is(gtR::query_resource_combination("organisation", "project", "961756BF-E31F-4A13-836F-0A09BA02385C"), "data.frame")

#that you get a list assuming no API errors
testthat::expect_is(gtR::query_resource_combination("organisation", "project", "961756BF-E31F-4A13-836F-0A09BA02385C", df_only = F), "list")

#that picking a non-logical df_only throws an error-----------------------

testthat::test_that(
  "size outside range gives error",
  expect_error(
    gtR::query_resource_combination("organisation", "project", "961756BF-E31F-4A13-836F-0A09BA02385C", df_only = "lol"),
    "'df_only` must be logical (TRUE or FALSE)",
    fixed = T
  )
)
