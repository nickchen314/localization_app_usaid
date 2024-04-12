library(tidyverse)
func_clean_data <- function(df) {
  country_codes <- readRDS("data-raw/country_codes.rds") 
  left_join(df, country_codes, by = join_by("Place of Performance Country"== "Country Code")) %>% 
    mutate(Country = case_when(
      `Place of Performance Country` == "FSM" ~ "MICRONESIA",
      `Place of Performance Country` == "PLZ" ~ "PALAU",
      is.na(`Place of Performance Country`) ~ "NO LISTED COUNTRY",
      .default = Country
    )) %>%
    mutate(`Fiscal Year` = as.numeric(str_replace_all(`Fiscal Year`, "^FY",  ""))) %>%
    mutate(`Action Signed Date` = lubridate::ymd(`Action Signed Date`)) %>%
    select(-c(Modification, `Procurement Type`, `Award Type`, `Reported UEI`, `Place of Performance Country`)) %>%
    rename(`USAID Local Metric` = `Local Metric`) %>%
    mutate(`USAID Local Metric` = case_when(
      `USAID Local Metric` == "Local Funding" ~ "Local",
      .default = `USAID Local Metric`)) %>%
    mutate(`Activity Name` = case_when(
      `Activity Name` == "USAID redacted this field in accordance with the exceptions outlined in the Foreign Aid Transparency and Accountability Act of 2016." ~ "Redacted by USAID",
      is.na(`Activity Name`) ~ "Not Listed",
      .default = `Activity Name`
    ))
}

                       
