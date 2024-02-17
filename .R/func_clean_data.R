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
    select(-c(Modification, `Procurement Type`, `Award Type`, `Reported UEI`, `Place of Performance Country`))
}


