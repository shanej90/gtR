#' Function to streamline searching for resource records either in isolation or records associated with another resource.
#'
#' Sends a query to return all records for your chosen resource, with the option to query a combination of resources.
#' @param resource The name of the resource you would like to query/return, as per `gtr_endpoints`.
#' @param output The name of the resource you would like to return (linked to a specific resource_id), as per `gtr_endpoints`.
#' @param resource_id The id used to link the resources
#' @param page_nums The numbers of results you would like to see, leaving blank will return all pages bound together. Enter as numeric vector.
#' @param size The number of results you would like to return (min 10, max 100).
#' @param search_term Term you want to check against search_fields. Search works on a 'contains' basis. Note search_term applies to the `resource`, NOT the `output`.
#' @param search_fields Fields you want to check search_term against. Enter as character vector. Note fields must be referred to by their code, and some fields may be searched by default. Use `get_configs()` for details of codes and default search fields. Note `search_fields` relates to the resource, NOT the output.
#' @return A dataframe holding the results for your chosen query, or a list holding the dataframe plus corresponding metadata..
#' @export

get_resources <- function(
  resource,
  output,
  resource_id,
  page_nums,
  size = 20,
  search_term,
  search_fields
) {

  #hack to avoid errors about missing output definition
  output_hack <- if(missing(output)) {"blah"} else {output}

  #error handling---------------------------------------

  #check resource is correct
  if(!resource %in% names(unlist(gtR::gtr_endpoints)) | resource %in% c("base", "configs", "outcomes")) stop("'resource' must be in the list of resources as per `gtr_endpoints`, excluding `base`, `outcomes`, or `configs`")

  #check resource combination is correct
  if(!missing(output) & !missing(resource_id) & !paste(resource, output_hack) %in% unlist(gtR::gtr_combinations)) stop("'resource' and 'output' combination must be in the list of options as per 'gtr_combinations'")

  #check resource ID has been provided if returning a combination of resources
  if(!missing(output) & missing(resource_id)) stop("'resource_id' must be provided to return a combination of resources as per 'gtr_combinations")

  #make sure size is between 1 and 100
  if(size < 10 | size > 100 | size != round(size)) stop("'size' must be an integer >= 10 and <= 100")

  #url---------------------------------------

  #check against whether the resource is an outcome type, which necessitates a different URL format
  if(
    !missing(output) &
    !missing(resource_id) &
    resource %in% names(gtR::gtr_endpoints)[1:7] &
    output_hack %in% names(gtR::gtr_endpoints)[1:7]
    ) {

    #added in ID break to link resources
    #figure out how to retrieve ID without pasting in complete...
    .url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[[resource]]}/{resource_id}{gtR::gtr_endpoints[[output]]}")

  } else if (!missing(output) & !missing(resource_id)) {

    .url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[[resource]]}/{resource_id}{gtR::gtr_endpoints[['outcomes']]}{gtR::gtr_endpoints[[output]]}")

  } else if (missing(output) & missing(resource_id) & resource %in% names(gtR::gtr_endpoints[1:7])) {

    .url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[[resource]]}")

  } else {

    .url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[['outcomes']]}{gtR::gtr_endpoints[[resource]]}")

  }

  #query-------------------------------------------------------

  #query settings - you're leaving page number as 1 here as you're only doing this to ascertain total number of pages
  if(missing(search_fields) & missing(search_term)) {

    query_settings <- list(
      s = size,
      p = 1
    )

  } else if(missing(search_fields)) { #if searching a default field

    query_settings <- list(
      q = search_term,
      s = size,
      p = 1
    )

  } else { # if searching fields not included in default

    #set up search fields
    s_fields <- paste0(search_fields, collapse = "&")

    query_settings <- list(
      q = search_term,
      f = s_fields,
      s = size,
      p = 1
    )

  }

  #get result
  prelim_result <- httr::GET(
    url = .url,
    query = query_settings,
    timeout = httr::timeout(15)
  )

  #display error message if required
  if(prelim_result$status_code >= 400) {
    err_msg = httr::http_status(prelim_result)
    stop(err_msg)
  }

  #run prelimary resi;ts tp get page numbers-------------------------------

  #text format
  result_text <- httr::content(prelim_result, "text")

  #return list
  return_list <- jsonlite::fromJSON(result_text)

  #extract total page numbers
  if(missing(page_nums)) { #if you want to return all pages

    page_numbers <- c(1:return_list$totalPages)

  } else {

    page_numbers <- page_nums

  }


  #loop return for all page numbers--------------------------
  loop_results <- function(
    .page = page_numbers,
    .size = size,
    .search_term = search_term,
    .search_fields = search_fields
    ) {

    #sleep for 5 seconds to avoid potential throttling
    Sys.sleep(5)

    #set up search fields to run for each page
    if(missing(.search_fields) & missing(.search_term)) {

      loop_query_settings <- list(
        s = .size,
        p = .page
      )

    } else if(missing(.search_fields)) { #if searching a default field

      loop_query_settings <- list(
        q = .search_term,
        s = .size,
        p = .page
      )

    } else { # if searching fields not included in default

      #set up search fields
      .s_fields <- paste0(.search_fields, collapse = "&")

      loop_query_settings <- list(
        q = .search_term,
        f = .s_fields,
        s = .size,
        p = .page
      )

    }

    #get result
    result <- httr::GET(
      url = .url,
      query = loop_query_settings,
      timeout = httr::timeout(15)
    )

    #display error message if required
    if(result$status_code >= 400) {
      err_msg = httr::http_status(result)
      stop(err_msg)
    }

    #turn your results into a df-------------------------------

    #get json result
    loop_result_text <- httr::content(prelim_result, "text")

    #convert to dataframe
    if(output_hack == "blah") {

      jsonlite::fromJSON(loop_result_text)[[resource]]

    } else {

      jsonlite::fromJSON(loop_result_text)[[output]]

    }

  }

  #apply the function over all pages
  all_results <- lapply(page_numbers, loop_results) |>
    #bind together all pages
    dplyr::bind_rows()

  #final result
  return(all_results)

}
