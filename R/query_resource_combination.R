#' Function to streamline searching for resource records without limiting to records associated with another specific resource. Eg, query ALL projects, not just those associated with the University of Exeter.
#'
#' Sends a query to return all records for your chosen resource. Note that the API returns a maximum of 100 results at a time and this you may need to specify a page or search to get the specific results you want.
#' @param resource The name of the resource you would like to query, as per `gtr_endpoints`.
#' @param output The name of the secondary resource you would like to query
#' @param id The id used to link the resources
#' @param size The number of results you would like to return (max 100).
#' @param page_num The page number of results you would like to see.
#' @param search_term Term you want to check against search_fields. Search works on a 'contains' basis.
#' @param search_fields Fields you want to check search_term against. Enter as character vector. Note fields must be referred to by their code, and some fields may be searched by default. Use `get_configs()` for details of codes and default search fields.
#' @param df_only Choose whether you only want a dataframe of the 'core' results (T) or you would like additional metadata returned with the query (F).
#' @return A dataframe holding the results for your chosen query, or a list holding the dataframe plus corresponding metadata..
#' @export

query_resource_combination <- function(
  resource,
  output,
  id,
  size = 20,
  page_num = 1,
  search_term,
  search_fields,
  df_only = T
) {

  #error handling---------------------------------------

  #check resource combination is correct
  if(!paste(resource, output) %in% names(unlist(gtR::gtr_combinations))) stop("'resource' and 'output' combination must be in the list of options as per 'gtr_combinations'")

  #check resource is correct
  if(!resource %in% names(unlist(gtR::gtr_endpoints)) | resource %in% c("base", "configs", "outcomes") |!output %in% names(unlist(gtR::gtr_endpoints)) | output %in% c("base", "configs", "outcomes")) stop("'resource' must be in the list of resources as per `gtr_endpoints`, excluding `base`, `outcomes`, or `configs`")

  #make sure size is between 1 and 100
  if(size < 10 | size > 100 | size != round(size)) stop("'size' must be an integer >= 10 and <= 100")

  #df_only is logical
  if(!is.logical(df_only)) stop("'df_only` must be logical (TRUE or FALSE)")

  #url---------------------------------------

  #check against whether the resource is an outcome type, which necessitates a different URL format
  if(resource %in% names(gtR::gtr_endpoints[1:7])) {

    #added in ID break to link resources
    #figure out how to retrieve ID without pasting in complete...
    .url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[[resource]]}/{id}{gtR::gtr_endpoints[[output]]}")

  } else {

    .url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[['outcomes']]}{gtR::gtr_endpoints[[resource]]}")

  }

  #query-------------------------------------------------------

  #query settings
  if(missing(search_fields) & missing(search_term)) {

    query_settings <- list(
      s = size,
      p = page_num
    )

  } else if(missing(search_fields)) { #if searching a default field

    query_settings <- list(
      q = search_term,
      s = size,
      p = page_num
    )

  } else { # if searching fields not included in default

    #set up search fields
    s_fields <- paste0(search_fields, collapse = "&")

    query_settings <- list(
      q = search_term,
      f = s_fields,
      s = size,
      p = page_num
    )

  }

  #get result
  result <- httr::GET(
    url = .url,
    query = query_settings,
    timeout = httr::timeout(15)
  )

  #display error message if required
  if(result$status_code >= 400) {
    err_msg = httr::http_status(result)
    stop(err_msg)
  }

  #turn your results into a df-------------------------------

  #text format
  result_text <- httr::content(result, "text")

  #final result
  if(df_only == F) {

    jsonlite::fromJSON(result_text)[[resource]]

  } else {

    jsonlite::fromJSON(result_text)

  }

}

