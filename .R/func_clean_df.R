library(tidyverse)
func_clean_df <- function(grants_prime, contracts_prime){
  #needs error checking
  ##selects needed columns for analysis then joins df
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
  
  ##cleans country codes
  code_names <- readxl::read_excel("./data-raw/gg-updated-country-and-state-lists.xlsx") %>% rename("country_code" = "Country Code") %>%
    mutate(country_code = str_squish(country_code))
  joined_df2 <- left_join(joined_df, code_names, by = join_by("primary_place_of_performance_country_code" == "country_code")) %>%
    rename("primary_place_of_performance_country_name" = "Country")
  
  ##cleans non-code names in recipient_country_code section
  joined_df3 <- left_join(joined_df2, code_names, by = join_by("recipient_country_code" == "Country")) %>%
    mutate(recipient_country_code = case_when(
      !is.na(country_code) ~ country_code,
      .default = recipient_country_code)) %>%
    select(-country_code) %>%
    left_join(code_names, by = join_by("recipient_country_code" == "country_code")) %>%
    rename("recipient_country_name" = "Country")
  joined_df3
}



