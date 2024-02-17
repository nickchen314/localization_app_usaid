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

thematic_shiny(font = "auto")

## Business Logic

# loads data
source("./.R/func_clean_data.R")
usaid_source <- readRDS("./data-raw/usaid_data.rds") 
usaid_df <- func_clean_data(usaid_source)
usaid_percents <- readRDS("./data-raw/loc_percents.rds")


# defines min and max FY and award thresholds
year_min <- min(usaid_df$`Fiscal Year`)
year_max <- max(usaid_df$`Fiscal Year`)
award_min <- min(usaid_df$`Obligated Amount`)
award_max <- max(usaid_df$`Obligated Amount`)

# defines color scheme
local_colors <- setNames(
  c(
    "#F8766D",
    "#7CAE00"
  ),
  c(
    "Not Local",
    "Local Funding"
  )
)

# Define UI for application that shows data
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "materia"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("ppp_country1", "Search a Country",
        choices = c(sort(unique(usaid_df$Country))),
        multiple = TRUE,
        selected = c(sort(unique(usaid_df$Country)))[1],
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
      checkboxInput("facet", "Aggregate Countries?"),
      downloadButton('downFile',label = "Download Table"),
      downloadButton('downFile2',label = "Download Grouped Table")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Introduction",
          source("./.R/intro_html.R", local = TRUE)$value
        ),
        tabPanel(
          "Dollar Value Projects",
          mainPanel(
            plotlyOutput("barplot_value"),
            plotlyOutput("barplot_valueprop")
          )
        ),
        tabPanel(
          "Count of Projects",
          mainPanel(
            plotlyOutput("barplot_counts"),
            plotlyOutput("barplot_countsprop")
          )
        ),
        tabPanel(
          "Data Table",
          mainPanel(
            dataTableOutput("full_data")
          )
        ), #tab panel 3
        tabPanel(
          "Grouped Data Table",
          mainPanel(
            dataTableOutput("full_data_grouped")
          )
        )
      ) ## tabset panel
    ) ## main panel
  ) ## sidebar layout
) ## fluid page


server <- function(input, output) {
  ##reactive df
  filtered_df <- reactive({
    usaid_df %>%
      filter(`Country` %in% req(input$ppp_country1)) %>%
      filter(`Fiscal Year` %in% (input$year1[1]:input$year1[2])) %>%
      filter(as.double(`Obligated Amount`) >= 
               as.double(req(input$awardthreshold))) %>%
      arrange(desc(`Obligated Amount`)) -> df
    if (nrow(df) == 0) {
      stop("Zero Detected Awards")
    }
    df
  })
  
  grouped_df <- reactive({
    filtered_df() %>%
      group_by(Country, `Fiscal Year`, Award) %>%
      summarize(`Local Metric` = names(which.max(table(`Local Metric`))),
                `Total Obligations` = sum(`Obligated Amount`),
                `Reported Recipient Name` = 
                  names(which.max(table(`Reported Recipient Name`)))) %>%
      arrange(desc(`Total Obligations`)) -> df
    if (nrow(df) == 0) {
      stop("Zero Detected Awards")
    }
    df
  })
  

  ##### GRAPHS
  ## project counts
  output$barplot_counts <- renderPlotly({
    # Create the plot using the filtered data
    plot1 <- ggplot(data = grouped_df()) +
      geom_bar(aes(x = `Fiscal Year`, fill = `Local Metric`)) +
      labs(title = "Count of USAID Projects by Localization Status", fill = "Local Metric") +
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
        facet_wrap(~`Country`, scales = "free_y", ncol = 2)
    }

    ggplotly(plot1, height = 400, width = 900) %>%
      layout(
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
    plot1 <- ggplot(data = grouped_df() %>%
                      group_by(Country,
                               `Fiscal Year`, 
                               `Local Metric`) %>%
                      summarize(count = n()) %>%
                      mutate(prop = count / sum(count))) +
      geom_bar(aes(x = `Fiscal Year`, 
                   y = prop, 
                   fill = `Local Metric`,
                   text = paste("Percentage: ", scales::percent(prop))), 
               position = "fill", stat = "identity") +
      labs(title = "Proportion of Projects by Localization Status", fill = "Local Metric") +
      xlab("Fiscal Year") +
      ylab("Proportion of Projects") +
      scale_x_continuous(breaks = seq(year_min, year_max, 1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
      theme(
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))
      ) +
      scale_fill_manual(values = local_colors)

    if (input$facet == FALSE) {
      plot1 <- plot1 +
        facet_wrap(~`Country`, scales = "free_y", ncol = 2)
    }

    ggplotly(plot1, 
             tooltip = c("Local Metric", "Fiscal Year", "text"),
             height = 400, width = 900) %>%
      layout(
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
        group_by(`Country`, `Fiscal Year`, `Local Metric`) %>%
        summarize(total_amount = sum(`Obligated Amount`))
    } 
    
    if (input$facet == TRUE) {
      df1 <- filtered_df() %>%
        group_by(`Fiscal Year`, `Local Metric`) %>%
        summarize(total_amount = sum(`Obligated Amount`))
    } 
    df1
  })

  ## project obligations
  output$barplot_value <- renderPlotly({
    # Create the plot using the filtered data
    plot1 <- ggplot(data = obs_df() %>%
                      mutate(tot_obs = scales::dollar_format()(total_amount))) +
      geom_bar(aes(
        x = `Fiscal Year`,
        y = total_amount,
        fill = `Local Metric`,
        text = paste("Gift($):", tot_obs)
      ), stat = "identity") +
      labs(title = "Obligations of USAID Projects by Localization Status", fill = "Local Metric") +
      xlab("Fiscal Year") +
      ylab("Value of Projects ($)") +
      scale_x_continuous(breaks = seq(year_min, year_max, 1)) +
      theme(
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))
      ) +
      scale_fill_manual(values = local_colors) +
      scale_y_continuous(labels = scales::dollar_format(scale = .000001, suffix = "M"))

    if (input$facet == FALSE) {
      plot1 <- plot1 +
        facet_wrap(~`Country`, scales = "free_y", ncol = 2)
    }
    ggplotly(plot1, 
             tooltip = c("Local Metric", "Fiscal Year", "text"),
             height = 400, width = 900) %>%
      layout(
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
    plot1 <- ggplot(obs_df() %>% 
                      mutate(prop = total_amount / sum(total_amount))) +
      geom_bar(
        aes(
          x = `Fiscal Year`,
          y = prop, 
          fill = `Local Metric`, 
          text = paste("Percentage: ", scales::percent(prop))
        ),
        stat = "identity",
        position = "fill"
      ) +
      labs(title = "Proportion of Project Obligations by Localization Status", fill = "Local Metric") +
      xlab("Fiscal Year") +
      ylab("Proportion of Project Obligations") +
      scale_x_continuous(breaks = seq(year_min, year_max, 1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
      theme(
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))
      ) +
      scale_fill_manual(values = local_colors)

    if (input$facet == F) {
      plot1 <- plot1 +
        facet_wrap(~`Country`, scales = "free_y", ncol = 2)
    }
    ggplotly(plot1, 
             tooltip = c("Local Metric", "Fiscal Year", "text"),
             height = 400, width = 900) %>%
      layout(
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
      datatable(filtered_df(), filter = "top") %>% formatCurrency("Obligated Amount", digits = 0)
    },
    options = list(pageLength = 20, autoWidth = TRUE)
  )
  
  ##grouped table
  output$full_data_grouped<- DT::renderDT(
    {
      datatable(grouped_df(), filter = "top") %>% formatCurrency("Total Obligations", digits = 0)
    },
    options = list(pageLength = 20, autoWidth = TRUE)
  )

  
  ## file download for download button
  output$downFile <- downloadHandler(
    filename = "usaid_localization_data.csv",
    content = function(file) {
      write.csv(filtered_df(), file, row.names = FALSE)
    }
  )
  
  output$downFile2 <- downloadHandler(
    filename = "usaid_localization_data_grouped.csv",
    content = function(file) {
      write.csv(grouped_df(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
