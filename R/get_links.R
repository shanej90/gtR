#' Function to gather data about linked entities for a specified resource.
#'
#' Sends a query to return all records for your chosen resource, with the option to query a combination of resources.
#' @param resource The name of the resource you would like to query/return, as per `gtr_endpoints`.
#' @param resource_id The ID of the resource.
#' @return A dataframe holding details of any entities linked to the one you specified.
#' @export

get_links <- function(
  resource,
  resource_id
) {

  resource <- "organisation"
  resource_id <- "961756BF-E31F-4A13-836F-0A09BA02385C"

  #error handling---------------------------------------

  #check resource is correct
  if(!resource %in% names(unlist(gtR::gtr_endpoints)) | resource %in% c("base", "configs", "outcomes")) stop("'resource' must be in the list of resources as per `gtr_endpoints`, excluding `base`, `outcomes`, or `configs`")

  #make sure a resource id is specified
  if(missing(resource_id)) stop("`resource_id` must be supplied. Other functions in this package may help you find the right ID.")

  #run query-=-------------------------------------

  #construct url
  query_url <- paste0(gtR::gtr_endpoints[['base']], gtR::gtr_endpoints[[resource]], "/", resource_id)

  #make request
  prelim_result <- httr::GET(url = query_url, timeout = httr::timeout(15))

  #message if query fails
  if(prelim_result$status_code >= 400) {
    err_msg = httr::http_status(prelim_result)
    stop(err_msg)
  }

  #result text
  result_text <- httr::content(prelim_result, "text")

  #dataframe
  full_df <- jsonlite::fromJSON(result_text) |> rbind() |> data.frame()

  #extract links-----------------------------------
  final <- full_df |>
    dplyr::select(links) |>
    tidyr::unnest(links) |>
    tidyr::unnest_wider(links) |>
    tidyr::unnest(dplyr::everything()) |>
    #add additional columns
    dplyr::mutate(
      parent_id = resource_id,
      entity_id = stringr::word(href, -1, sep = "/")
    )

}
