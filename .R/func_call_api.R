library(tidyverse)
library(httr2)
library(jsonlite)
func_call_api <- function(){
  
}

my_url <- "https://api.usaspending.gov/api/v2/search/spending_by_award/"
req <- request(my_url)
req

get_data <- function(req = req, page = 1){
  request_data <- list(
    "subawards" = "false",
    "limit" = 100,  # the max for a page
    "page" = page,
    "filters" = list(
      "award_type_codes" = c("A", "B", "C", "D"),
      "agencies" = list(list(
        "type" = "awarding",
        "tier" = "toptier",
        "name" = "Agency for International Development"
      ))
    ), # end of Filters list
    "fields" = c(
      "Award ID",
      "Recipient Name",
      "Start Date",
      "End Date",
      "Award Amount",
      "Awarding Agency",
      "Awarding Sub Agency",
      "Contract Award Type",
      "Award Type",
      "Funding Agency",
      "Funding Sub Agency"
    )
  )
  # use the req to execute the body where req_body_json will encode 
  # the named list into a valid JSON string
  req |>
    req_body_json(data = request_data) |>
    req_perform() |> 
    resp_body_json() -> tempp
  
  # process the return object results to get a data frame
  tempp$results |> 
    tibble(data = _) |> 
    unnest_wider(data)
}

get_data(req, 1) -> test

map(c(1:10), \(page) get_data(req, page)) |> 
  list_rbind() ->
  my_df




GET_all_pages <- function( PATH, QUERY ) {
  # Create empty list
  pages <- list()
  # Create initial API url
  url <- modify_url("https://api.usaspending.gov"
                    , path = PATH
                    , query = QUERY
  )
  # Get API url
  raw.resp <- GET(url)
  if (http_type(raw.resp) != "application/json") {
    stop("API did not return json. Check 'status code = 200'"
         , call. = FALSE)
  }
  this.char.resp <- rawToChar( raw.resp$content) # convert from raw to char
  # convert JSON object into R object
  this.clean.resp <- fromJSON(this.char.resp
                              , flatten = TRUE
  ) 
  # Set initial page number
  page_number <- 1
  
  # conditional element selection
  # if results page is does not have a next page
  # return a data frame for these results
  if( this.clean.resp$page_metadata$has_next_page == FALSE ){
    return( this.clean.resp$results)
  }
  # while loop with boolean condition
  # if the results page contains a next page
  # call the next page and bind the results to a data frame
  # return the data frame with all the page results
  while( this.clean.resp$page_metadata$has_next_page == TRUE ) {
    # identify current page url
    current.page.url <- this.clean.resp$page_metadata$current
    # subsitute "&page=XX" with "&page='page_number'"
    next.page.url <- gsub( pattern = "&page=[[:digit:]]+"
                           , replacement = paste0( "&page=", page_number)
                           , x = current.page.url
    )
    # Get new API url
    raw.resp <- GET( url = next.page.url )
    # Convert raw vector to character vector
    this.char.resp <- rawToChar( raw.resp$content )
    # Convert JSON object into R object
    this.clean.resp <- fromJSON( this.char.resp
                                 , flatten = TRUE
    )
    # For every page number (1, 2, 3...), insert that page's "results" inside the list
    pages[[ page_number ]] <- this.clean.resp$results
    # Add to the page number and restart the loop
    page_number <- page_number + 1
  }
  # once all the pages have been collected,
  data_api_data <- rbind_pages(pages) # rbind.pages() is deprecated
  # return what we've collected
  return( data_api_data )
  
  
  # Turn API errors into R errors
  if (http_error( raw.resp )) {
    stop(
      sprintf(
        "USASpending.gov API request failed [%s]\n%s\n<%s>", 
        status_code( raw.resp),
        this.clean.resp$message,
        this.clean.resp$documentation_url
      ),
      call. = FALSE
    )
  }
  # add some structure stuff 
  structure(
    list(
      content = this.clean.resp
      , path = PATH
      , response = raw.resp
    )
    , class = "usa_spending_api"
  )
} # end of function
