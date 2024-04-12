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

usaid_df <- readRDS("./data-raw/usaid_df_final.rds")
usaid_percents <- readRDS("./data-raw/loc_percents1.rds")


# defines min and max FY and award thresholds
year_min <- min(usaid_df$`Fiscal Year`)
year_max <- max(usaid_df$`Fiscal Year`)
award_min <- min(usaid_df$`Obligated Amount`)
award_max <- max(usaid_df$`Obligated Amount`)

# defines color scheme
local_colors <- setNames(
  c(
    "#ecb546",
    "#2875a2"
  ),
  c(
    "Not Local",
    "Local"
  )
)

# defines css for hiding select all in country selection tab
my_css <- "
.bs-select-all {
  display: none;
}
.bs-deselect-all {
  width: 100%;
}
"

# function for flipping legends in plotly js
reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}


# Define UI for application that shows data
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "materia"),
  title = "ARC USAID Localization App",
  fluidRow(
    column(8, tags$a(
      href = "https://accountabilityresearch.org/",
      img(height = 150, width = 300, src = "ARC_logo.jpg")))
  ),
  titlePanel(
    h1("USAID Localization Dashboard: FY2020-2022", align = "center"),
    ),
  HTML("<span  
       style=\" 
       width: 1000px;  
       display: inline-block;
       margin:auto; display:table; 
       border:1px solid black;
       text-align: center\"> 
       “Localization” in this app refers to <a href = \"https://www.usaid.gov/sites/default/files/2023-04/Key-Performance-Indicators-Direct-AA-Funding-Localization.pdf\"  target=\"_blank\" > direct local funding</a> and follows USAID's determination of contractors as 'local' or 'non-local'. </span>"),  ## subheader description
  tabsetPanel(
    footer = source("./.R/footer_html.R", local = TRUE)$value,
    tabPanel(strong("USAID Source Data"),
    sidebarLayout(
      sidebarPanel(
        tags$head(tags$style(HTML(my_css))),
        width = 3,
        pickerInput("ppp_country1", "Select Country(s)",
          choices = c(sort(unique(usaid_df$Country))),
          multiple = TRUE,
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
        downloadButton('downFile',label = "Download Source Data"),
        downloadButton('downFile5',label = "Project Data Table"),
        downloadButton('downFile2',label = "Project Data by FY"),
        downloadButton('downFile3',label = "Recipient Data Table")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            strong("Introduction"),
            source("./.R/intro_html.R", local = TRUE)$value
          ),
          tabPanel(
            strong("Funding Obligations by Country"),
            mainPanel(
              plotlyOutput("barplot_value"),
              plotlyOutput("barplot_valueprop")
            )
          ),
          tabPanel(
            strong("Project Count by Country"),
            mainPanel(
              plotlyOutput("barplot_counts"),
              plotlyOutput("barplot_countsprop")
            )
          ),
          tabPanel(
            strong("Top 10 Projects"),
            mainPanel(
              plotlyOutput("top10_projects")
            )
          ),
          tabPanel(
            strong("Project Data"),
            mainPanel(
              dataTableOutput("full_data_grouped_aggregated")
            )
          ), #tab panel 3
          tabPanel(
            strong("Project Data Separated by Fiscal Year"),
            mainPanel(
              dataTableOutput("full_data_grouped")
            )
          ),
          tabPanel(
            strong("Recipient Data"),
            mainPanel(
              dataTableOutput("full_data_grouped_recipient")
            )
          )
        ) ## tabset panel
      ) ## main panel
    ) ## sidebar layout
    ), ## first tab panel
    tabPanel(strong("USAID Self-Reported Localization Shares"),
      sidebarLayout(
        sidebarPanel(
          width = 3, 
          pickerInput("country_1", "Search a USAID Operating Unit",
                      choices = c(sort(unique(usaid_percents$`USAID Operating Unit`))),
                      multiple = TRUE,
                      options = pickerOptions(
                        liveSearch = TRUE,
                        actionsBox = TRUE,
                        size = 10,
                        selectedTextFormat = "count > 5"
                      )
          ),
          pickerInput("year_2", "Select FY",
                      choices = c(2021:2022),
                      multiple = TRUE,
                      selected = c(2021:2022),
                      options = pickerOptions(
                        actionsBox = TRUE,
                        size = 10,
                        selectedTextFormat = "count > 5"
                      )
          ),
          downloadButton('downFile4',
                         label = "Download USAID Self-Reported Localization Percentages")
        ),
      mainPanel(
        tabsetPanel(
          tabPanel(strong("Localization Bar Graph"),
            plotlyOutput("barplot_report") 
        )
        )
      ) # sidebar layout
      )
  ) ## panel 2
  ) ## top-level tabset panel
  ) ## fluid page
server <- function(input, output) {
  ## reactive df
  filtered_df <- reactive({
    usaid_df %>%
      filter(`Country` %in% req(input$ppp_country1)) %>%
      filter(`Fiscal Year` %in% (input$year1[1]:input$year1[2])) %>%
      filter(as.double(`Obligated Amount`) >= 
               as.double(req(input$awardthreshold))) %>%
      arrange(desc(`Obligated Amount`)) -> df
    if (nrow(df) == 0) {
      stop(safeError("Zero Detected Awards"))
    }
    df
  })
  grouped_df <- reactive({
    filtered_df() %>%
      group_by(Country, `Fiscal Year`, Award) %>%
      summarize(`USAID Local Metric` = names(which.max(table(`USAID Local Metric`))),
                `Total Obligations` = sum(`Obligated Amount`),
                `Reported Recipient Name` = names(which.max(table(`Reported Recipient Name`))),
                `Activity Name` = names(which.max(table(`Activity Name`)))) %>%
      arrange(desc(`Total Obligations`)) %>%
      rename(`Activity ID` = Award) %>%
      select(1, 6, 7, 4, 2, 5, 3)-> df
    if (nrow(df) == 0) {
      stop(safeError("Zero Detected Awards"))
    }
    df
  })
  ## grouped by recipient dataframe
  grouped_recipient_df <- reactive({
    grouped_df() %>%
      group_by(Country, `Reported Recipient Name`, `Activity ID`) %>%
      summarize(`USAID Local Metric` = names(which.max(table(`USAID Local Metric`))),
                                             `Total Obligations` = sum(`Total Obligations`)) %>%
      ungroup() %>%
      group_by(Country, `Reported Recipient Name`) %>%
      summarize(`USAID Local Metric` = names(which.max(table(`USAID Local Metric`))),
                `Total Obligations` = sum(`Total Obligations`),
                `Count Projects` = n()) %>%
      arrange(desc(`Total Obligations`)) %>%
      rename(`Total Obligations for Selected Years` = `Total Obligations`,
             `Total Project Count for Selected Years` = `Count Projects`)-> df
    if (nrow(df) == 0) {
      stop(safeError("Zero Detected Awards"))
    }
    df
  })
  ## grouped by recipient aggregated across years dataframe
  grouped_df_aggregated <- reactive({
    filtered_df() %>%
      group_by(Country, Award) %>%
      summarize(`USAID Local Metric` = names(which.max(table(`USAID Local Metric`))),
                `Total Obligations` = sum(`Obligated Amount`),
                `Reported Recipient Name` = names(which.max(table(`Reported Recipient Name`))),
                `Activity Name` = names(which.max(table(`Activity Name`)))) %>%
      arrange(desc(`Total Obligations`)) %>%
      rename(`Activity ID` = Award) %>%
      select(1, 5, 6, 3, 4, 2)-> df
    if (nrow(df) == 0) {
      stop(safeError("Zero Detected Awards"))
    }
    df
  })

  ### GRAPHS
  ## project counts
  output$barplot_counts <- renderPlotly({
    # Create the plot using the filtered data
    plot1 <- ggplot(data = grouped_df()) +
      geom_bar(aes(x = `Fiscal Year`, fill = `USAID Local Metric`)) +
      labs(title = "Count of USAID Projects by Localization Status", fill = "USAID Local Metric") +
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
          x = 1, y = -.21, text = "Source: USAID FY2020-2022 Localization Source Data",
          showarrow = F, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        ),
        title = list(text = paste0('<b>Count of USAID Projects by Localization Status</b>',
                                          '<br>',
                                          '<sup>',
                                          'Hover Over Bars for Exact Values','</sup>')),
        margin = list(t = 90)
      )
  })
  ## proportion of project count
  output$barplot_countsprop <- renderPlotly({
    # Create the plot using the filtered data
    

    if (input$facet == FALSE) {
      plot1 <- ggplot(data = grouped_df() %>%
                        group_by(Country,
                                 `Fiscal Year`, 
                                 `USAID Local Metric`) %>%
                        summarize(count = n()) %>%
                        mutate(prop = count / sum(count))) +
        geom_bar(aes(x = `Fiscal Year`, 
                     y = prop, 
                     fill = `USAID Local Metric`,
                     text = paste("Percentage: ", scales::percent(prop))), 
                 position = "fill", stat = "identity") +
        labs(title = "Proportion of Projects by Localization Status", fill = "USAID Local Metric") +
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
      
      plot1 <- plot1 +
        facet_wrap(~`Country`, scales = "free_y", ncol = 2)
    }
    if (input$facet == TRUE) {
      plot1 <- ggplot(data = grouped_df() %>%
                        group_by(`Fiscal Year`, 
                                 `USAID Local Metric`) %>%
                        summarize(count = n()) %>%
                        mutate(prop = count / sum(count))) +
        geom_bar(aes(x = `Fiscal Year`, 
                     y = prop, 
                     fill = `USAID Local Metric`,
                     text = paste("Percentage: ", scales::percent(prop))), 
                 position = "fill", stat = "identity") +
        labs(title = "Proportion of Projects by Localization Status", fill = "USAID Local Metric") +
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
    }
    ggplotly(plot1, 
             tooltip = c("USAID Local Metric", "Fiscal Year", "text"),
             height = 400, width = 900) %>%
      layout(
        annotations = list(
          x = 1, y = -.21, text = "Source: USAID FY2020-2022 Localization Source Data",
          showarrow = FALSE, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        ),
        title = list(text = paste0('<b>Proportion of Projects by Localization Status</b>',
                                   '<br>',
                                   '<sup>',
                                   'Hover Over Bars for Exact Values','</sup>')),
        margin = list(t = 90)
      )
  })
  ##top 10 projects bar plot
  output$top10_projects <- renderPlotly({
    plot1 <- ggplot(data = grouped_df_aggregated() %>%
                      slice_max(order_by = `Total Obligations`, n = 10)) +
      geom_col(aes(x = fct_reorder(.f = as.factor(`Activity ID`),
                                   .x = `Total Obligations`,
                                   .fun = max), 
                   y = `Total Obligations`, 
                   fill = `USAID Local Metric`,
                   text = paste0("Total Obligations($):", scales::dollar(`Total Obligations`), 
                                  "<br>Reported Recipient Name:", `Reported Recipient Name`,
                                  "<br>Project Name:", `Activity Name`,
                                  "<br>Project Name:", `Activity ID`))) +
      scale_y_continuous(labels = scales::dollar_format(scale = .000001, suffix = "M")) +
      labs(title = , fill = "USAID Local Metric") +
      ylab("Project Obligations($)") +
      scale_fill_manual(values = local_colors) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      coord_flip()
    
    if (input$facet == FALSE) {
      plot1 <- plot1 + facet_wrap(~`Country`, scales = "free_y", ncol = 2)
    } 
    
    ggplotly(plot1, 
             tooltip = c("Activity Name", "USAID Local Metric", "text"),
             height = 500, width = 900) %>%
      layout(
        annotations = list(
          x = 1, y = -.21, text = "Source: USAID FY2020-2022 Localization Source Data",
          showarrow = FALSE, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        ),
        title = list(text = paste0('<b> Top 10 Projects by Total Obligations ($)</b>',
                                   '<br>',
                                   '<sup>',
                                   'Hover Over Bars for Project Name and ID','</sup>')),
        margin = list(t = 90)
      )
    })
  ## obligations dataframe for project obligation graphs
  obs_df <- reactive({
    if (input$facet == FALSE) {
      df1 <- filtered_df() %>%
        group_by(`Country`, `Fiscal Year`, `USAID Local Metric`) %>%
        summarize(total_amount = sum(`Obligated Amount`))
    } 
    
    if (input$facet == TRUE) {
      df1 <- filtered_df() %>%
        group_by(`Fiscal Year`, `USAID Local Metric`) %>%
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
        fill = `USAID Local Metric`,
        text = paste("Total Obligations($):", tot_obs)
      ), stat = "identity") +
      labs(title = "USAID Funding Obligations by Localization Status", fill = "USAID Local Metric") +
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
             tooltip = c("USAID Local Metric", "Fiscal Year", "text"),
             height = 400, width = 900) %>%
      layout(
        annotations = list(
          x = 1, y = -.21, text = "Source: USAID FY2020-2022 Localization Source Data",
          showarrow = F, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        ),
        title = list(text = paste0('<b>USAID Funding Obligations by Localization Status</b>',
                                   '<br>',
                                   '<sup>',
                                   'Hover Over Bars for Exact Values','</sup>')),
        margin = list(t = 90)
      )
  })
  ## proportion of obligations
  output$barplot_valueprop <- renderPlotly({
    # Create the plot using the filtered data
    plot1 <- ggplot(obs_df() %>% 
                      mutate(prop = total_amount / sum(total_amount))) +
      geom_bar(
        aes(
          x = `Fiscal Year`,
          y = prop, 
          fill = `USAID Local Metric`, 
          text = paste("Localization Percentage: ", scales::percent(prop))
        ),
        stat = "identity"
      ) +
      labs(title = "Proportion of USAID Funding Obligations by Localization Status", fill = "USAID Local Metric") +
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
             tooltip = c("USAID Local Metric", "Fiscal Year", "text"),
             height = 400, width = 900) %>%
      layout(
        annotations = list(
          x = 1, y = -.21, text = "Source: USAID FY2020-2022 Localization Source Data",
          showarrow = F, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 10, color = "black")
        ),
        title = list(text = paste0('<b>Proportion of USAID Funding Obligations by Localization Status</b>',
                                   '<br>',
                                   '<sup>',
                                   'Hover Over Bars for Exact Values','</sup>')),
        margin = list(t = 90)
      )
  })
  ## Datatable outputs
  output$full_data <- DT::renderDT(
    {
      datatable(filtered_df(), filter = "top") %>% formatCurrency("Obligated Amount", digits = 0)
    },
    options = list(pageLength = 20, autoWidth = TRUE)
  )
  
  ##grouped awards separated by year table
  output$full_data_grouped_aggregated<- DT::renderDT(
    {
      datatable(grouped_df_aggregated(), filter = "top") %>% formatCurrency("Total Obligations", digits = 0)
    },
    options = list(pageLength = 20, autoWidth = TRUE)
  )
  ##grouped awards table (not separated by year)
  output$full_data_grouped<- DT::renderDT(
    {
      datatable(grouped_df(), filter = "top") %>% formatCurrency("Total Obligations", digits = 0)
    },
    options = list(pageLength = 20, autoWidth = TRUE)
  )
  ## grouped recipient table
  output$full_data_grouped_recipient<- DT::renderDT(
    {
      datatable(grouped_recipient_df(), filter = "top") %>% formatCurrency("Total Obligations for Selected Years", digits = 0)
    },
    options = list(pageLength = 20, 
                   autoWidth = TRUE
) 
  )
  ## file download for download button
  output$downFile <- downloadHandler(
    filename = "usaid_localization_data_source.csv",
    content = function(file) {
      write.csv(usaid_df, file, row.names = FALSE)
    }
  )
  
  output$downFile2 <- downloadHandler(
    filename = "usaid_localization_data_grouped_byfy.csv",
    content = function(file) {
      write.csv(grouped_df(), file, row.names = FALSE)
    }
  )
  
  output$downFile5 <- downloadHandler(
    filename = "usaid_localization_data_grouped.csv",
    content = function(file) {
      write.csv(grouped_df_aggregated(), file, row.names = FALSE)
    }
  )
  
  output$downFile3 <- downloadHandler(
    filename = "usaid_localization_data_grouped_recipient.csv",
    content = function(file) {
      write.csv(grouped_recipient_df(), file, row.names = FALSE)
    }
  )
  output$downFile4 <- downloadHandler(
    filename = "usaid_localization_self_reported_figures.csv",
    content = function(file) {
      write.csv(usaid_percents, file, row.names = FALSE)
    }
  )
  
  ## USAID Report Percentages
  percent_df <- reactive({
    usaid_percents %>% 
      filter(`USAID Operating Unit` %in% req(input$country_1))
  })
  
  output$barplot_report <- renderPlotly({
    # Create the plot using the filtered data
    plot1 <- ggplot(percent_df()) +
      geom_bar(mapping = aes(x = as.factor(`Fiscal_Year`), 
                             y = prop, 
                             fill = fct_rev(`USAID Local Metric`),
                             text = paste("Percentage: ", scales::percent(prop))), 
               stat = "identity") +
      labs(title = "USAID Reported Localization Figures by Operating Unit", fill = "USAID Local Metric") +
      xlab("Fiscal Year") +
      ylab("Reported Localization Percentage") +
      scale_x_discrete(limits = rev) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L), n.breaks = 10) +
      theme(
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold")
      ) +
      scale_fill_manual(values = local_colors) +
      facet_wrap(~`USAID Operating Unit`) +
      coord_flip()
    ggplotly(plot1, 
             tooltip = c("USAID Local Metric", "Fiscal Year", "text"),
             height = 400, width = 900) %>%
      layout(
        annotations = list(
          x = 1, y = -.18, text = "Source: USAID FY 2022 Localization <br> Progress Report",
          showarrow = F, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 8, color = "black")
        ),
        title = list(text = paste0('<b>Reported Localization Percentage</b>',
                                   '<br>',
                                   '<sup>',
                                   'Hover Over Bars for Exact Values. Negative Localization Percentages Dropped for visual clarity.','</sup>')),
        margin = list(t = 90)
      ) %>%
    reverse_legend_labels()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
