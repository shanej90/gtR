#' Retreive configurations for each data type in the GtR API.
#'
#' Allows youto retrieve details of the data configuration for each type of entity (eg, project, organisation) in the GtR API. This information could be useful in helping you structure queries.
#'
#' @param resource Specify the type of data for which you want to retrieve the configuration. Names must be as per `gtr_endpoints`.
#' @return A list holding two dataframes - one ('parameters') covering query parameters and another ('fields') holding details about the fields you return.
#' @export

get_configs <- function(
  resource
) {

  #error handling------------------------------------------

  #make sure '.type' is of an acceptable format
  if(!resource %in% names(unlist(gtR::gtr_endpoints)) | resource %in% c("base", "configs")) stop("'resource' must be in the list of resources as per `gtr_endpoints`, except `base` or `configs`")

  #define the url---------------------------------------
  .url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[['configs']]}{gtR::gtr_endpoints[[resource]]}")

  #run query------------------------------------

  #result
   result <- httr::GET(
     url = .url,
     timeout = httr::timeout(15)
   )

  # #check whether you got an error and send message if not
   if(result$status_code >= 400) {
     err_msg = httr::http_status(result)
     stop(err_msg)
   }

  # #turn your results into a dataframe------------------------

  # #text format
   result_text <- httr::content(result, "text")

  # #convert from JSON
   converted <- jsonlite::fromJSON(result_text)

   #list version
   list("parameters" = converted[["parameters"]][["parameter"]], "fields" = converted[["fields"]][["field"]])

}
