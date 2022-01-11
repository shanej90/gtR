#you've 'kind of' run tests in some vignettes anyway and we know some query combinations are working.
#This is to test an outstanding type

#test you get results for query where both output and resource are in endpoints[1:7]

#that you get a dataframe, assuming no API errors
testthat::expect_s3_class(
  get_resources(
    "organisation",
    "project",
    "961756BF-E31F-4A13-836F-0A09BA02385C",
    size = 10,
    page_nums = 1
    ),
  "data.frame"
  )
