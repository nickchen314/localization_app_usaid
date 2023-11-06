library(shiny)
library(tidyverse)
library(bslib)

##Business Logic

#loads data
contracts_prime <- read_csv("./data-raw/Contracts_PrimeAwardSummaries_2023-10-01_H04M40S14_1.csv")
grants_prime <- read_csv("./data-raw/Assistance_PrimeAwardSummaries_2023-10-09_H17M29S35_1.csv")
#calls function to join data
source("./.R/func_clean_df.R")
joined_df <- func_clean_df(grants_prime, contracts_prime)



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
      sliderInput("number", "select a number", 0, 100, 40),
      selectInput("countryvar", "Select a Country Code",
                  choices = c(unique(joined_df$recipient_country_code))),
      checkboxGroupInput("years", "Select Award Years:", 
                         choices = unique(joined_df$award_base_action_date_fiscal_year), 
                         selected = 2022, inline = TRUE),
      selectInput("grantvar", "Select a Type", choices = c("Grant", "Local")),
      numericInput("awardvar", "Awarded Money:", 
                   min = 0, max = max(joined_df$total_obligated_amount), 
                   value = 0)
      
      
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("a",plotOutput("hist")),
        tabPanel("b"),
        tabPanel("c")
      )
    )
  )
)

server <- function(input, output) {

  output$hist <- renderPlot({
    
    ggplot(data = joined_df, mapping = aes(x = recipient_country_code, 
                                           y = total_obligated_amount, 
                                           fill = is.local)) +
      geom_histogram()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
