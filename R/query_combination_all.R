#' Function to streamline searching for resource records either in isolation or records associated with another resource.
#'
#' Sends a query to return all records for your chosen resource, with the option to query a combination of resources.
#' @param resource The name of the resource you would liek to query, as per `gtr_endpoints`.
#' @param output The name of the resource you would like to return, as per `gtr_endpoints`.
#' @param resource_id The id used to link the resources
#' @param page_num The page number of results you would like to see, leaving blank will return all pages bound together.
#' @param size The number of results you would like to return (max 100).
#' @param search_term Term you want to check against search_fields. Search works on a 'contains' basis.
#' @param search_fields Fields you want to check search_term against. Enter as character vector. Note fields must be referred to by their code, and some fields may be searched by default. Use `get_configs()` for details of codes and default search fields.
#' @param df_only Choose whether you only want a dataframe of the 'core' results (T) or you would like additional metadata returned with the query (F).
#' @return A dataframe holding the results for your chosen query, or a list holding the dataframe plus corresponding metadata..
#' @export

query_combination_all <- function(
  resource,
  output = NULL,
  resource_id = NULL,
  page_num = NULL,
  size = 20,
  search_term,
  search_fields,
  df_only = T
) {

  #error handling---------------------------------------

  #check resource is correct
  if(!resource %in% names(unlist(gtR::gtr_endpoints)) | resource %in% c("base", "configs", "outcomes")) stop("'resource' must be in the list of resources as per `gtr_endpoints`, excluding `base`, `outcomes`, or `configs`")

  #check resource combination is correct
  if(!is.null(output) & !is.null(resource_id) & !paste(resource, output) %in% names(unlist(gtR::gtr_combinations))) stop("'resource' and 'output' combination must be in the list of options as per 'gtr_combinations'")

  #check resource ID has been provided if returning a combination of resources
  if(!is.null(output) & is.null(resource_id)) stop("'resource_id' must be provided to return a combination of resources as per 'gtr_combinations")

  #make sure size is between 1 and 100
  if(size < 10 | size > 100 | size != round(size)) stop("'size' must be an integer >= 10 and <= 100")

  #df_only is logical
  if(!is.logical(df_only)) stop("'df_only` must be logical (TRUE or FALSE)")

  #url---------------------------------------

  #check against whether the resource is an outcome type, which necessitates a different URL format
  if(!is.null(output) & !is.null(resource_id) & resource %in% names(gtR::gtr_endpoints[1:7])) {

    #added in ID break to link resources
    #figure out how to retrieve ID without pasting in complete...
    .url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[[resource]]}/{resource_id}{gtR::gtr_endpoints[[output]]}")

  } else if (!is.null(output) & !is.null(resource_id)) {

    .url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[['outcomes']]}/{resource_id}{gtR::gtr_endpoints[[resource]]}")

  } else if (is.null(output) & is.null(resource_id) & resource %in% names(gtR::gtr_endpoints[1:7])) {

    .url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[[resource]]}")

  } else {

    .url <- glue::glue("{gtR::gtr_endpoints[['base']]}{gtR::gtr_endpoints[['outcomes']]}{gtR::gtr_endpoints[[resource]]}")

  }

  #query-------------------------------------------------------

  #query settings
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
    url = url,
    query = query_settings,
    timeout = httr::timeout(15)
  )

  #display error message if required
  if(prelim_result$status_code >= 400) {
    err_msg = httr::http_status(prelim_result)
    stop(err_msg)
  }

  #turn your results into a df-------------------------------

  #text format
  result_text <- httr::content(prelim_result, "text")

  #return list
  return_list <- jsonlite::fromJSON(result_text)

  #extract total page numbers
  if(is.null(page_num)) { #if you want to return all pages

    page_numbers <- (1:return_list$totalPages)

  } else { #if you want to specift which pages to return

    page_numbers <- page_num

  }


  #loop return for all page numbers--------------------------
  loop_results <- function(.page = page_numbers,
                           .size = size,
                           .search_term = search_term,
                           .search_fields = search_fields,
                           df_only = T) {

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

    #text format
    loop_result_text <- httr::content(prelim_result, "text")

    #final result
    if(df_only == T) {

      jsonlite::fromJSON(result_text)[[resource]]

    } else {

      jsonlite::fromJSON(result_text)

    }

  }

  #apply the function over all pages
  all_results <- for (p in page_numbers) {

    lapply(page_numbers,
           loop_results) |>
      #bind together all pages
      dplyr::bind_rows()
  }

  return(all_results)

}
