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
  theme = bslib::bs_theme(version = 5, bootswatch = "materia"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("ppp_country1", "Search a Country",
                  choices = c(unique(joined_df$primary_place_of_performance_country_name)),
                  multiple = TRUE,
                  options = pickerOptions(liveSearch = TRUE, 
                                          actionsBox = TRUE,
                                          size = 10)),
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
        tabPanel("a",
          mainPanel(
            plotlyOutput("barplot")
                 )),
        tabPanel("b"),
        tabPanel(
          "c",
          mainPanel(
            dataTableOutput("full_data")
          )) #tabpanel 1
        
      )
    )
  )
)


server <- function(input, output) {
    output$barplot <- renderPlotly({
    plot1 <- ggplot(data = joined_df) +
       geom_bar(aes(x = award_base_action_date_fiscal_year, fill = is.local)) +
       labs(title = str_c(input$ppp_country1, ": Count of Projections by Localization Status")) +
       xlab("Fiscal Year") +
       ylab("Count of Projects") + 
       scale_x_continuous(breaks=seq(year_min,year_max,2)) +
       theme(axis.title = element_text(face="bold"), 
             title = element_text(face="bold"),
             axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      facet_wrap(~recipient_country_name) 
     ggplotly(plot1) %>%
       layout(height = 400, width = 900)})
  
  output$full_data <- DT::renderDT(
    {
      joined_df
    },
    filter = "top",
    options = list(pageLength = 20, autoWidth = TRUE)
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
