library(shiny)
library(tidyverse)
library(ggplot2)
library(bslib)
library(scales)
library(DT)
library(thematic)
library(shinyWidgets)
library(plotly)
library(scales)
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)

thematic_shiny(font = "auto")

## Business Logic

world <- ne_countries(scale = "medium", returnclass = "sf")



# loads data
contracts_prime <- readRDS("./data-raw/contracts.rds")
grants_prime <- readRDS("./data-raw/grants.rds")
# calls function to join data
source("./.R/func_clean_df.R")
joined_df <- func_clean_df(grants_prime, contracts_prime)


# defines min and max FY and award thresholds
year_min <- min(joined_df$award_base_action_date_fiscal_year)
year_max <- max(joined_df$award_base_action_date_fiscal_year)
award_min <- min(joined_df$total_obligated_amount)
award_max <- max(joined_df$total_obligated_amount)

# defines color scheme
local_colors <- setNames(
  c(
    "#F8766D",
    "#7CAE00",
    "gray"
  ),
  c(
    "non-local",
    "local",
    "incomplete location information"
  )
)

# Define UI for application that shows data
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "materia"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("ppp_country1", "Search a Country",
        choices = c(sort(unique(joined_df$primary_place_of_performance_country_name))),
        multiple = TRUE,
        selected = c(sort(unique(joined_df$primary_place_of_performance_country_name)))[1],
        options = pickerOptions(
          liveSearch = TRUE,
          actionsBox = TRUE,
          size = 10,
          selectedTextFormat = "count > 5"
        )
      ),
      sliderTextInput(
        inputId = "year1",
        label = "Year Select",
        choices = c(year_min:year_max),
        selected = c(year_min, year_max),
        grid = FALSE, dragRange = FALSE
      ),
      searchInput(
        inputId = "awardthreshold",
        value = 0,
        label = "Enter Minimum Award Value($)",
        placeholder = "Enter Integer",
        btnSearch = icon("search"),
        btnReset = icon("remove"),
        resetValue = award_min,
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
      ),
      checkboxInput("facet", "Aggregate Countries?"),
      downloadButton('downFile',label = "Download Table")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Introduction",
          h2("Data Overview"),
          p("This Shiny app provides an analysis of USAID-funded projects. It includes data on contracts and grants awarded over various fiscal years, detailing the obligated amounts and project localization."),
          p("The datasets utilized in this app are derived from two primary sources:"),
          tags$ul(
            tags$li("Contracts Data: Contains detailed information about contracts awarded by USAID."),
            tags$li("Grants Data: Contains detailed information about grants awarded by USAID.")
          ),
          h2("Using the App"),
          p("To interact with the app, follow these steps:"),
          tags$ol(
            tags$li("Select the desired country or countries from the 'Search a Country' dropdown."),
            tags$li("Use the 'Year Select' slider to choose the range of fiscal years."),
            tags$li("Enter a minimum value for project awards in the 'Enter Minimum Award Value($)' field."),
            tags$li("Choose the type(s) of awards to display using the 'What type(s) of awards?' selector."),
            tags$li("Check the 'Aggregate Countries?' box if you want to combine data from selected countries."),
            tags$li("Click 'Download Table' to save the currently displayed data to a CSV file.")
          ),
          p("Navigate through the tabs to view different data visualizations and tables. Interactive charts and maps are available for a dynamic exploration experience.")
        ),
        tabPanel(
          "Count of Projects",
          mainPanel(
            plotlyOutput("barplot_counts"),
            plotlyOutput("barplot_countsprop")
          )
        ),
        tabPanel(
          "Dollar Value Projects",
          mainPanel(
            plotlyOutput("barplot_value"),
            plotlyOutput("barplot_valueprop")
          )
        ),
        tabPanel(
          "Data Table",
          mainPanel(
            dataTableOutput("full_data")
          )
        ), #tab panel 3
        tabPanel(
          "Map",
          mainPanel(
            plotlyOutput("map")
          )
        ) #tab panel 3
      ) ## tabset panel
    ) ## main panel
  ) ## sidebar layout
) ## fluid page


server <- function(input, output) {
  ##reactive df
  filtered_df <- reactive({
    joined_df %>%
      mutate(is.local = case_when(
        is.local == TRUE ~ "local",
        is.local == FALSE ~ "non-local",
        is.na(is.local) ~ "incomplete location information"
      )) %>%
      mutate(is.grant = case_when(
        is.grant == TRUE ~ "Grant",
        is.grant == FALSE ~ "Contract"
      )) %>%
      filter(primary_place_of_performance_country_name %in% req(input$ppp_country1)) %>%
      filter(award_base_action_date_fiscal_year %in% (input$year1[1]:input$year1[2])) %>%
      filter(is.grant %in% input$award_type1) %>%
      filter(as.double(total_obligated_amount) >= as.double(req(input$awardthreshold)))
  })

  ##### GRAPHS
  ## project counts
  output$barplot_counts <- renderPlotly({
    # Create the plot using the filtered data
    plot1 <- ggplot(data = filtered_df()) +
      geom_bar(aes(x = award_base_action_date_fiscal_year, fill = is.local)) +
      labs(title = "Count of USAID Projects by Localization Status") +
      xlab("Fiscal Year") +
      ylab("Count of Projects") +
      scale_x_continuous(breaks = seq(year_min, year_max, 1)) +
      theme(
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))
      ) +
      scale_fill_manual(values = local_colors)

    if (input$facet == FALSE) {
      plot1 <- plot1 +
        facet_wrap(~primary_place_of_performance_country_name, scales = "free_y", ncol = 2)
    }

    ggplotly(plot1) %>%
      layout(
        height = 400, width = 900,
        annotations = list(
          x = 1, y = -.21, text = "Accountability Research Center",
          showarrow = F, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        )
      )
  })

  ## proportion of project count
  output$barplot_countsprop <- renderPlotly({
    # Create the plot using the filtered data
    plot1 <- ggplot(data = filtered_df()) +
      geom_bar(aes(x = award_base_action_date_fiscal_year, fill = is.local), position = "fill") +
      labs(title = "Proportion of Projects by Localization Status") +
      xlab("Fiscal Year") +
      ylab("Proportion of Projects") +
      scale_x_continuous(breaks = seq(year_min, year_max, 1)) +
      theme(
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))
      ) +
      scale_fill_manual(values = local_colors)

    if (input$facet == FALSE) {
      plot1 <- plot1 +
        facet_wrap(~primary_place_of_performance_country_name, scales = "free_y", ncol = 2)
    }

    ggplotly(plot1) %>%
      layout(
        height = 400, width = 900,
        annotations = list(
          x = 1, y = -.21, text = "Accountability Research Center",
          showarrow = FALSE, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        )
      )
  })
  
  ##obs df
  obs_df <- reactive({
    if (input$facet == FALSE) {
      df1 <- filtered_df() %>%
        group_by(primary_place_of_performance_country_name, award_base_action_date_fiscal_year, is.local) %>%
        summarize(total_amount = sum(total_obligated_amount))
    } 
    
    if (input$facet == TRUE) {
      df1 <- filtered_df() %>%
        group_by(award_base_action_date_fiscal_year, is.local) %>%
        summarize(total_amount = sum(total_obligated_amount))
    } 
    df1
  })

  ## project obligations
  output$barplot_value <- renderPlotly({
    # Create the plot using the filtered data
    plot1 <- ggplot(data = obs_df()) +
      geom_bar(aes(
        x = award_base_action_date_fiscal_year,
        y = total_amount,
        fill = is.local
      ), stat = "identity") +
      labs(title = "Obligations of USAID Projects by Localization Status") +
      xlab("Fiscal Year") +
      ylab("Value of Projects ($)") +
      scale_x_continuous(breaks = seq(year_min, year_max, 1)) +
      theme(
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))
      ) +
      scale_fill_manual(values = local_colors) +
      scale_y_continuous(labels = scales::dollar_format(scale = .000001, suffix = "M"))

    if (input$facet == FALSE) {
      plot1 <- plot1 +
        facet_wrap(~primary_place_of_performance_country_name, scales = "free_y", ncol = 2)
    }
    ggplotly(plot1) %>%
      layout(
        height = 400, width = 900,
        annotations = list(
          x = 1, y = -.21, text = "Accountability Research Center",
          showarrow = F, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        )
      )
  })
  
  ## proportion of obligation
  output$barplot_valueprop <- renderPlotly({
    # Create the plot using the filtered data
    plot1 <- ggplot(obs_df()) +
      geom_bar(
        aes(
          x = award_base_action_date_fiscal_year,
          y = total_amount, fill = is.local
        ),
        stat = "identity",
        position = "fill"
      ) +
      labs(title = "Proportion of Project Obligations by Localization Status") +
      xlab("Fiscal Year") +
      ylab("Proportion of Project Obligations") +
      scale_x_continuous(breaks = seq(year_min, year_max, 1)) +
      theme(
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))
      ) +
      scale_fill_manual(values = local_colors)

    if (input$facet == F) {
      plot1 <- plot1 +
        facet_wrap(~primary_place_of_performance_country_name, scales = "free_y", ncol = 2)
    }
    ggplotly(plot1) %>%
      layout(
        height = 400, width = 900,
        annotations = list(
          x = 1, y = -.21, text = "Accountability Research Center",
          showarrow = F, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        )
      )
  })
  
  ##full datatable
  output$full_data <- DT::renderDT(
    {
      filtered_df()
    },
    filter = "top",
    options = list(pageLength = 20, autoWidth = TRUE)
  )
  

  
  ## file download for download button
  output$downFile <- downloadHandler(
    filename = "usaid_localization_data.csv",
    content = function(file) {
      write.csv(filtered_df(), file, row.names = FALSE)
    }
  )
  
  ##mapping feature
  output$map <- renderPlotly({
    df1 <- filtered_df() %>%
      group_by(recipient_country_code) %>%
      summarize(tot_ob = sum(total_obligated_amount)) 
    IQR <- quantile(df1$tot_ob, .75) - quantile(df1$tot_ob, .25)
    map_dat <- right_join(df1, world, by = join_by("recipient_country_code" == "adm0_a3_us"))
    ggplot(data = map_dat) + 
      geom_sf(aes(fill=tot_ob, geometry = geometry)) +
      scale_fill_gradient(limits=c(
        0, 
        quantile(df1$tot_ob, .75) + 1.5 * IQR), 
        low="blue", high="red", oob=squish, label=comma) +
      labs(title = "Total Obligations to Selected Country Filled by Recipient Country") +
      theme_void() -> plot1
    ggplotly(plot1)%>%
      layout(
        height = 500, width = 1000,
        annotations = list(
          x = 1, y = -.21, text = "Accountability Research Center",
          showarrow = F, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
