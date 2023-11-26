library(tidyverse)
library(httr2)
library(jsonlite)
func_call_api <- function(){
  
}

award_url <- "https://api.usaspending.gov/api/v2/search/spending_by_award/"
award_req <- request(award_url)

get_data <- function(req = award_req){
  ##creates empty list to store all pages 
  pages <- list()
  page_number <- 1
  
  request_data <- list(
    "subawards" = "false",
    "limit" = 100,  # the max for a page
    "page" = page_number,
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
      "Total Outlays",
      "Description",
      "recipient_id",
      "Place of Performance Country Code",
      "Base Obligation Date"
    )
  )
  # use the req to execute the body where req_body_json will encode 
  # the named list into a valid JSON string
  req |>
    req_body_json(data = request_data) |>
    req_perform() |> 
    resp_body_json() -> response
  
  ##progress bar
  progress_bar = txtProgressBar(min=0, max=500, style = 1, char="=")

  while(response$page_metadata$hasNext == TRUE) {
    pages[[page_number]] <- response$results
    page_number <- page_number + 1
    req |> 
      req_body_json(data = request_data) |>
      req_perform() |> 
      resp_body_json() -> response
    setTxtProgressBar(progress_bar, value = page_number)
  }
  
  if(response$page_metadata$hasNext == TRUE){
    pages[[page_number]] <- response$results
  }
  close(progress_bar)
  all_pages <- rbind_pages(pages)
  return(all_pages)
}

get_data() |> 
  tibble(data = _) |> 
  unnest_wider(data) -> test2





