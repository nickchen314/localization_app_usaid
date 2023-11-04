library(shiny)
library(tidyverse)

##Business Logic

#loads data
contracts_prime <- read_csv("./data-raw/Contracts_PrimeAwardSummaries_2023-10-01_H04M40S14_1.csv")
grants_prime <- read_csv("./data-raw/Assistance_PrimeAwardSummaries_2023-10-09_H17M29S35_1.csv")

##selects needed columns for analysis then joins df(can wrap in function later)
contracts_prime %>%
  select(contract_award_unique_key, award_id_piid, total_obligated_amount, 
                 total_outlayed_amount, period_of_performance_start_date, 
                 period_of_performance_current_end_date, recipient_name, 
                 recipient_uei, recipient_parent_name, prime_award_base_transaction_description,  
                 recipient_parent_uei, recipient_country_code, awarding_office_code, awarding_office_name,
                  primary_place_of_performance_country_code, award_base_action_date_fiscal_year) %>%
  mutate(is.grant = F) -> conts_filtered

grants_prime %>%
  select(assistance_award_unique_key, award_id_fain, total_obligated_amount, 
         total_outlayed_amount, period_of_performance_start_date, 
         period_of_performance_current_end_date, recipient_name, prime_award_base_transaction_description,  
         recipient_uei, recipient_parent_name, recipient_country_code, 
         recipient_parent_uei, awarding_office_code, awarding_office_name,
         primary_place_of_performance_country_code, award_base_action_date_fiscal_year) %>% 
  mutate(is.grant = T) -> grants_filtered

grants_filtered %>% ##renames grants data to work with contracts data functions
  rename(
    contract_award_unique_key = assistance_award_unique_key,
    award_id_piid = award_id_fain
  ) -> grants_filtered
joined_df <- rbind(grants_filtered, conts_filtered) %>%
  mutate(is.local = case_when(primary_place_of_performance_country_code == recipient_country_code ~ T,
                              is.na(primary_place_of_performance_country_code) | 
                                is.na(recipient_country_code) ~ NA,
                                primary_place_of_performance_country_code != recipient_country_code ~ F))

##some EDA
# count of na's 
sum(is.na(joined_df$is.local)) # 3751
joined_df %>%
  filter(is.na(is.local)) %>%
  view() ##can potentially append region based on USAID mission name
sum(is.na(joined_df$total_obligated_amount)) # 0

#^use API function once minimal viable product complete

#Clean data to remove any duplicates/input errors if needed (negative obligations, duplicate awards)

#Checks if recipient location and primary place of performance are the same


# Define UI for application that shows data
ui <- fluidPage(

)

server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
