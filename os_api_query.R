# required packages: un-comment to install.
# install.packages("tidyverse", "httr", "jsonlite")
library(tidyverse) ; library(httr) ; library(jsonlite)

# Documentation on endpoints: https://api.os.uk/search/places/v1/

OS_PLACES_FIND_PATH <- "https://api.os.uk/search/places/v1/find"
OS_PLACES_UPRN_PATH <- "https://api.os.uk/search/places/v1/uprn"

# Query the Ordnance Survey address finder API

# Parameters: building        - number or name,
#             postcode        - postcode with or without spaces
#             api_key         - OS API license key
#             failed_response - optional failure message for unmatched responses
#             stop_on_error   - optional, STOP when error is encountered.
#                               FALSE (default) gracefully returns in text.
#                               TRUE - throws STOP up to calling program.
# Returns: string containing top address match.

get_address_raw <-
    get_address <- function(building,
                            postcode,
                            api_key,
                            failed_response="",
                            stop_on_error=FALSE,
                            rate_limit_delay = 1) {
        tryCatch({
            Sys.sleep(rate_limit_delay)

            # build search and request strings & execute
            building = casefold(building, upper = TRUE)
            postcode = gsub(" ", "", casefold(postcode, upper = TRUE))
            search_string <- paste(building, postcode)

            request <- GET(url = OS_PLACES_FIND_PATH,
                           query = list(query = search_string,
                                        maxresults = 1,
                                        key = api_key
                           )
            )

            # STOP if server response status is not 200.
            if(request$status_code != 200)
                stop(paste("Expected 200 Status response from API, received:",
                           request$status), call. = FALSE)

            response <- content(request, as = "text", encoding = "UTF-8")

            # Build a data frame containing the response
            req <- fromJSON(response)

            if(req$header$totalresults == 0)
                return(failed_response)

            df <- req$results$DPA
            df <- add_missing_columns(df)

            df <- select(df, ADDRESS, BUILDING_NUMBER, BUILDING_NAME,
                         SUB_BUILDING_NAME, POSTCODE)

            return(df$ADDRESS)

        }, error = function(e) {
            if(stop_on_error){
                stop(e)  # Throw error to calling program.
            } else {
                return(paste(e))  # return caught error as a string.
            }
        })
    }


get_address <- function(building,
                        postcode,
                        api_key,
                        failed_response="",
                        stop_on_error=FALSE,
                        rate_limit_delay = 1) {
  tryCatch({
    Sys.sleep(rate_limit_delay)

    # build search and request strings & execute
    building = casefold(building, upper = TRUE)
    postcode = gsub(" ", "", casefold(postcode, upper = TRUE))
    search_string <- paste(building, postcode)

    request <- GET(url = OS_PLACES_FIND_PATH,
                   query = list(query = search_string,
                                maxresults = 1,
                                key = api_key
                   )
    )

    # STOP if server response status is not 200.
    if(request$status_code != 200)
      stop(paste("Expected 200 Status response from API, received:",
                 request$status), call. = FALSE)

    response <- content(request, as = "text", encoding = "UTF-8")

    # Build a data frame containing the response
    req <- fromJSON(response)

    if(req$header$totalresults == 0)
      return(failed_response)

    df <- req$results$DPA
    df <- add_missing_columns(df)

    df <- select(df, ADDRESS, BUILDING_NUMBER, BUILDING_NAME,
                 SUB_BUILDING_NAME, POSTCODE)

    stripped_postcode = gsub(" ", "", df$POSTCODE)

    # check building number match
    if (!is.na(df$BUILDING_NUMBER)) {
      if (str_detect(building, df$BUILDING_NUMBER) &&
          postcode == stripped_postcode) {
        return(df$ADDRESS)
      }
    }

    # check building name match
    if (!is.na(df$BUILDING_NAME)) {
      building_response <-c(df$BUILDING_NAME, df$SUB_BUILDING_NAME)
      if ((any(str_detect(building_response, building) %in% TRUE) ||
           any(str_detect(building, building_response) %in% TRUE)) &&
          postcode == stripped_postcode) {
        return(df$ADDRESS)
      }
    }
    return(failed_response)

  }, error = function(e) {
    if(stop_on_error){
      stop(e)  # Throw error to calling program.
    } else {
      return(paste(e))  # return caught error as a string.
    }
  })
}

add_missing_columns <- function(data) {
  if (!"BUILDING_NUMBER" %in% colnames(data)) {data$BUILDING_NUMBER = NA}
  if (!"BUILDING_NAME" %in% colnames(data)) {data$BUILDING_NAME = NA}
  if (!"SUB_BUILDING_NAME" %in% colnames(data)) {data$SUB_BUILDING_NAME = NA}
  return(data)
}

get_address_from_uprn = function(uprn_string,
                                 api_key,
                                 failed_response="",
                                 stop_on_error=FALSE,
                                 rate_limit_delay = 1){
    tryCatch({
        Sys.sleep(rate_limit_delay)
        request <- GET(url = OS_PLACES_UPRN_PATH,
                       query = list(uprn = uprn_string,
                                    key = api_key
                       )
        )

        # STOP if server response status is not 200.
        if(request$status_code != 200)
            stop(paste("Expected 200 Status response from API, received:",
                       request$status), call. = FALSE)

        response <- content(request, as = "text", encoding = "UTF-8")

        # Build a data frame containing the response
        req <- fromJSON(response)

        if(req$header$totalresults == 0)
            return(failed_response)

        df <- req$results$DPA
        return(df$ADDRESS)
    }, error = function(e) {
        if(stop_on_error){
            stop(e)  # Throw error to calling program.
        } else {
            return(paste(e))  # return caught error as a string.
        }
    })
}
