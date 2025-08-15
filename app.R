library(shiny)
library(CausalImpact)
library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)

ui <- fluidPage(
  tags$head(
    tags$script(HTML("$(function () { $('[data-toggle=\"tooltip\"]').tooltip(); })")),
    tags$style(HTML("
      /* Modern Statistical Calculator Styling */
      body {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
        background-color: #f8f9fa;
        color: #333;
        line-height: 1.6;
      }
      
      .navbar-brand {
        font-weight: 600;
        font-size: 1.4rem;
      }
      
      /* Clean card design */
      .analysis-card {
        background: white;
        border: none;
        border-radius: 12px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
        padding: 24px;
        margin-bottom: 24px;
        transition: box-shadow 0.2s ease;
      }
      
      .analysis-card:hover {
        box-shadow: 0 4px 20px rgba(0,0,0,0.12);
      }
      
      .card-header {
        font-size: 1.1rem;
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 16px;
        padding-bottom: 8px;
        border-bottom: 2px solid #e9ecef;
      }
      
      /* Input styling */
      .form-group label {
        font-weight: 500;
        color: #495057;
        margin-bottom: 6px;
      }
      
      .form-control {
        border: 1.5px solid #e9ecef;
        border-radius: 8px;
        padding: 10px 12px;
        transition: border-color 0.2s ease, box-shadow 0.2s ease;
      }
      
      .form-control:focus {
        border-color: #007bff;
        box-shadow: 0 0 0 0.2rem rgba(0,123,255,0.15);
      }
      
      /* Button styling */
      .btn-analyze {
        background: linear-gradient(135deg, #007bff 0%, #0056b3 100%);
        border: none;
        padding: 12px 32px;
        font-weight: 600;
        font-size: 1rem;
        border-radius: 8px;
        color: white;
        transition: all 0.2s ease;
        width: 100%;
        margin-top: 16px;
      }
      
      .btn-analyze:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(0,123,255,0.3);
        color: white;
      }
      
      /* Results styling */
      .results-container {
        background: #f8f9fa;
        border-left: 4px solid #007bff;
        padding: 20px;
        border-radius: 0 8px 8px 0;
        margin: 16px 0;
      }
      
      .summary-text {
        font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
        font-size: 0.9rem;
        line-height: 1.5;
        white-space: pre-wrap;
        background: white;
        padding: 16px;
        border-radius: 6px;
        border: 1px solid #e9ecef;
        margin: 0;
      }
      
      /* Tab styling */
      .nav-tabs {
        border-bottom: 2px solid #e9ecef;
      }
      
      .nav-tabs .nav-link {
        color: #6c757d;
        font-weight: 500;
        border: none;
        padding: 12px 20px;
        border-radius: 8px 8px 0 0;
        margin-right: 4px;
      }
      
      .nav-tabs .nav-link.active {
        color: #007bff;
        background-color: white;
        border-bottom: 2px solid #007bff;
      }
      
      /* Data preview table */
      .table {
        margin-bottom: 0;
      }
      
      .table th {
        background-color: #f8f9fa;
        font-weight: 600;
        border-top: none;
        color: #495057;
      }
      
      /* Instructions */
      .instructions {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
      }
      
      .instructions h4 {
        color: #2c3e50;
        margin-bottom: 12px;
        font-weight: 600;
      }
      
      .step {
        margin: 8px 0;
        padding-left: 20px;
        position: relative;
      }
      
      .step::before {
        content: '→';
        position: absolute;
        left: 0;
        color: #007bff;
        font-weight: bold;
      }
      
      /* Responsive plot container */
      .plot-container {
        background: white;
        border-radius: 8px;
        padding: 16px;
        margin: 16px 0;
      }
      
      /* Sidebar styling */
      .well {
        background: white;
        border: none;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        padding: 20px;
      }
    "))
  ),
  
  navbarPage(
    title = "Causal Impact Analysis",
    theme = NULL,
    
    tabPanel("Setup",
             fluidRow(
               column(4,
                      div(class = "well",
                          h4("Data & Configuration", class = "card-header"),
                          fileInput("file", "Upload CSV File", 
                                    accept = ".csv",
                                    buttonLabel = "Browse...",
                                    placeholder = "No file selected"),
                          uiOutput("var_select"),
                          
                          h5("Analysis Periods", style = "margin-top: 20px; font-weight: 600; color: #495057;"),
                          dateInput("pre_start", "Pre-Period Start"),
                          dateInput("pre_end", "Pre-Period End"),
                          dateInput("post_start", "Post-Period Start"),
                          dateInput("post_end", "Post-Period End"),
                          
                          actionButton("run", "Run Analysis", class = "btn-analyze")
                      )
               ),
               column(8,
                      div(class = "analysis-card",
                          div(class = "card-header", "Getting Started"),
                          div(class = "instructions",
                              h4("How to Use This Tool"),
                              div(class = "step", "Upload your time series data as a CSV file"),
                              div(class = "step", "Select the response variable (metric to analyze)"),
                              div(class = "step", "Choose predictor variables (optional control variables)"),
                              div(class = "step", "Specify the date column and format"),
                              div(class = "step", "Adjust the pre/post intervention periods"),
                              div(class = "step", "Run analysis and view results in the Results tab")
                          )
                      )
               )
             )
    ),
    
    tabPanel("Results",
             div(class = "analysis-card",
                 div(class = "card-header", "Natural Language Summary"),
                 div(class = "results-container",
                     uiOutput("impactReport")
                 )
             ),
             
             div(class = "analysis-card",
                 div(class = "card-header", "Causal Impact Visualization"),
                 div(class = "plot-container",
                     plotOutput("impactPlot", width = "100%", height = "500px")
                 )
             ),
             
             div(class = "analysis-card",
                 div(class = "card-header", "Statistical Summary"),
                 div(class = "results-container",
                     uiOutput("summary")
                 )
             )
    ),
    
    tabPanel("Data Preview",
             div(class = "analysis-card",
                 div(class = "card-header", "Uploaded Data Preview"),
                 div(style = "max-height: 500px; overflow-y: auto; overflow-x: auto; border: 1px solid #dee2e6; border-radius: 6px;",
                     tableOutput("dataPreview")
                 )
             )
    )
  )
)

server <- function(input, output, session) {
  # Combined data processing reactive
  processed_data <- reactive({
    req(input$file, input$date_col, input$date_format, input$response)
    
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    
    # Parse dates
    dates <- if (input$date_format == "auto") {
      parse_date_time(df[[input$date_col]], orders = c("ymd", "mdy", "dmy"))
    } else {
      as.Date(df[[input$date_col]], format = input$date_format)
    }
    
    df[[input$date_col]] <- as.Date(dates)
    
    # Select and clean columns
    cols <- c(input$date_col, input$response, input$predictors)
    df_clean <- df[complete.cases(df[, cols]), cols, drop = FALSE]
    df_clean[order(df_clean[[input$date_col]]), ]
  }) %>% bindCache(input$file, input$date_col, input$date_format, 
                   input$response, input$predictors)
  
  # Dynamic variable selection
  output$var_select <- renderUI({
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    choices <- names(df)
    
    tagList(
      selectInput("response", "Response Variable", 
                  choices = choices,
                  selected = character(0)),
      selectInput("predictors", "Predictor Variables (Optional)", 
                  choices = choices, 
                  multiple = TRUE,
                  selected = character(0)),
      selectInput("date_col", "Date Column", 
                  choices = choices,
                  selected = character(0)),
      selectInput("date_format", "Date Format", 
                  choices = c(
                    "Auto Detect" = "auto",
                    "YYYY-MM-DD" = "%Y-%m-%d",
                    "MM/DD/YYYY" = "%m/%d/%Y",
                    "DD/MM/YYYY" = "%d/%m/%Y",
                    "MM/DD/YY" = "%m/%d/%y"
                  ),
                  selected = "auto")
    )
  })
  
  # Data preview
  output$dataPreview <- renderTable({
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    df  # Show all data instead of just head()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Auto-set date ranges
  observe({
    req(processed_data())
    dates <- sort(processed_data()[[input$date_col]])
    mid_point <- dates[floor(length(dates) * 0.6)]
    
    updateDateInput(session, "pre_start", value = min(dates))
    updateDateInput(session, "pre_end", value = mid_point - 1)
    updateDateInput(session, "post_start", value = mid_point)
    updateDateInput(session, "post_end", value = max(dates))
  })
  
  # Analysis results
  results <- eventReactive(input$run, {
    req(processed_data(), input$pre_start, input$pre_end, input$post_start, input$post_end)
    
    df_clean <- processed_data()
    model_data <- df_clean[, c(input$response, input$predictors), drop = FALSE]
    model_data[] <- lapply(model_data, as.numeric)
    
    ts_data <- zoo(model_data, df_clean[[input$date_col]])
    pre_period <- as.Date(c(input$pre_start, input$pre_end))
    post_period <- as.Date(c(input$post_start, input$post_end))
    
    CausalImpact(ts_data, pre_period, post_period)
  })
  
  # Output: Natural language report
  output$impactReport <- renderUI({
    req(results())
    report <- capture.output(summary(results(), "report"))
    HTML(paste0('<pre class="summary-text">', paste(report, collapse = "\n"), '</pre>'))
  })
  
  # Output: Plot
  output$impactPlot <- renderPlot({
    req(results())
    plot(results())
  }, res = 96)
  
  # Output: Technical summary
  output$summary <- renderUI({
    req(results())
    summ <- capture.output(summary(results()))
    HTML(paste0('<pre class="summary-text">', paste(summ, collapse = "\n"), '</pre>'))
  })
}

shinyApp(ui, server)