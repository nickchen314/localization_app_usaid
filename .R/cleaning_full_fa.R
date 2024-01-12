library(readr)
library(tidyverse)
fa_data %>%
  select(`Country Name`, `Country Code`, `Region Name`, `Income Group Name`, 
         `US Category Name`, `Activity Name`, `Funding Agency Acronym`, `Fiscal Year`, `Current Dollar Amount`, `Constant Dollar Amount`, `Transaction Type Name`, `Activity Project Number`) %>%
  filter(`Funding Agency Acronym` == "USAID", `Transaction Type Name` == "Disbursements") -> filtered_fa
saveRDS(filtered_fa, "./data-raw/condensed_fa.rds")

