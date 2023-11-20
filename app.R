library(shiny)
library(tidyverse)
library(ggplot2)
library(bslib)
library(scales)
library(DT)
library(thematic)
library(shinyWidgets)
library(plotly)
thematic_shiny(font = "auto")

##Business Logic

#loads data
contracts_prime <- read_csv("./data-raw/Contracts_PrimeAwardSummaries_2023-10-01_H04M40S14_1.csv")
grants_prime <- read_csv("./data-raw/Assistance_PrimeAwardSummaries_2023-10-09_H17M29S35_1.csv")
#calls function to join data
source("./.R/func_clean_df.R")
joined_df <- func_clean_df(grants_prime, contracts_prime)

##need to fully clean country names
##need to join df with recipients df 

##defines min and max FY and award thresholds
year_min <- min(joined_df$award_base_action_date_fiscal_year)
year_max <- max(joined_df$award_base_action_date_fiscal_year)
award_min <- min(joined_df$total_obligated_amount)
award_max <- max(joined_df$total_obligated_amount)

# Define UI for application that shows data
ui <- fluidPage(
  theme = bs_theme(
    version = 5, bootswatch = "darkly",
    bg = "#0b3d91", # a custom blue
    fg = "white",
    base_font = "Source Sans Pro"
  ),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("ppp_country1", "Search a Country",
                  choices = c(unique(joined_df$primary_place_of_performance_country_name)),
                  options = list(placeholder = "select a country name")),
      sliderTextInput(
        inputId = "year1",
        label = "Year Select",
        choices = c(year_min:year_max),
        selected =c(year_min, year_max),
        grid = FALSE, dragRange = FALSE),
      searchInput(
        inputId = "awardthreshold", 
        label = "Enter Minimum Award Value($)", 
        placeholder = "Enter Integer", 
        btnSearch = icon("search"), 
        btnReset = icon("remove"),
        resetValue = 0, 
        width = "100%"
      ),
      pickerInput("award_type1", "What type(s) of awards?",
                  choices = c("Grant", "Contract"),
                  selected = c("Grant", "Contract"),
                  multiple = TRUE, 
                  options = pickerOptions(
                    actionsBox = TRUE, 
                    size = 10
                  )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("a"),
        tabPanel("b"),
        tabPanel("c")
      )
    )
  )
)

server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
