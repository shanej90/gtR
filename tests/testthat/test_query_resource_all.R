#test you get expected error if choosing a size outside range------------------------
testthat::test_that(
  "size outside range gives error",
  expect_error(
    gtR::query_resource_all("organisation", size = 101),
    "'size' must be an integer >= 10 and <= 100",
    fixed = T
  )
)

#test that you get a data frame or list as expected-----------------------------

#that you get a dataframe, assuming no API errors
testthat::expect_s3_class(gtR::query_resource_all("organisation"), "data.frame")

#that you get a list assuming no API errors
testthat::expect_s3_class(gtR::query_resource_all("organisation", df_only = F), "list")

#tyhat picking a non-logical df_only throws an error-----------------------

testthat::test_that(
  "size outside range gives error",
  expect_error(
    gtR::query_resource_all("organisation", df_only = "lol"),
    "'df_only` must be logical (TRUE or FALSE)",
    fixed = T
  )
)

#that choosing a resource outside the range gives the expected error message---------------
testthat::test_that(
  "size outside range gives error",
  expect_error(
    gtR::query_resource_all("lol"),
    "'resource' must be in the list of resources as per `gtr_endpoints`, excluding `base`, `outcomes`, or `configs`",
    fixed = T
  )
)

