library(shiny)
library(CausalImpact)
library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .card {
        background-color: #ffffff;
        border: 1px solid #ddd;
        border-radius: 10px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        padding: 20px;
        margin-bottom: 20px;
      }

      .card pre {
        white-space: pre-wrap;
        font-family: 'Courier New', monospace;
        font-size: 14px;
        margin: 0;
      }

      .card-title {
        font-weight: bold;
        font-size: 18px;
        margin-bottom: 10px;
      }
    "))
  ),
  
  titlePanel("Causal Impact Analysis"),
  
  tabsetPanel(
    tabPanel("Setup",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload CSV", accept = ".csv"),
                 uiOutput("var_select"),
                 dateInput("pre_start", "Pre-Period Start"),
                 dateInput("pre_end", "Pre-Period End"),
                 dateInput("post_start", "Post-Period Start"),
                 dateInput("post_end", "Post-Period End"),
                 actionButton("run", "Run Analysis")
               ),
               mainPanel(
                 div(class = "card",
                     div(class = "card-title", "Instructions"),
                     "After uploading your data and selecting settings, go to the Results tab to view your analysis.")
               )
             )
    ),
    
    tabPanel("Results",
             div(class = "card",
                 div(class = "card-title", "Impact Summary (Natural Language)"),
                 uiOutput("impactReport")
             ),
             div(class = "card",
                 div(class = "card-title", "Causal Impact Plot"),
                 plotOutput("impactPlot", width = "100%")
             ),
             div(class = "card",
                 div(class = "card-title", "Technical Summary"),
                 uiOutput("summary")
             )
    ),
    
    tabPanel("Preview Data",
             div(class = "card",
                 div(class = "card-title", "Raw Data Preview"),
                 tableOutput("dataPreview")
             )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$var_select <- renderUI({
    req(data())
    df <- data()
    tagList(
      selectInput("response", "Response Variable", names(df)),
      selectInput("predictors", "Predictor(s)", names(df), multiple = TRUE),
      selectInput("date_col", "Date Column", names(df)),
      selectInput("date_format", "Date Format", choices = c(
        "Year-Month-Day (2024-12-17)" = "%Y-%m-%d",
        "Month/Day/Year (12/17/2024)" = "%m/%d/%Y",
        "Day/Month/Year (17/12/2024)" = "%d/%m/%Y",
        "Month/Day/2-digit Year (9/25/24)" = "%m/%d/%y",
        "Auto Detect (Try common formats)" = "auto"
      ))
    )
  })
  
  output$dataPreview <- renderTable({
    req(data())
    head(data(), 20)
  })
  
  observe({
    req(data(), input$date_col, input$date_format)
    df <- data()
    
    dates <- tryCatch({
      if (input$date_format == "auto") {
        parse_date_time(df[[input$date_col]], orders = c("ymd", "mdy", "dmy", "ymd HMS", "mdy HMS", "dmy HMS"))
      } else {
        as.Date(df[[input$date_col]], format = input$date_format)
      }
    }, error = function(e) {
      showNotification("Date parsing failed. Check your date column and format.", type = "error")
      return(NULL)
    })
    
    if (is.null(dates)) return()
    dates <- sort(na.omit(dates))
    if (length(dates) < 10) return()
    
    start_date <- min(dates)
    end_date <- max(dates)
    mid_point <- start_date + floor(as.numeric(difftime(end_date, start_date)) * 0.6)
    
    updateDateInput(session, "pre_start", value = start_date, min = start_date, max = end_date)
    updateDateInput(session, "pre_end", value = mid_point - 1, min = start_date, max = end_date)
    updateDateInput(session, "post_start", value = mid_point, min = start_date, max = end_date)
    updateDateInput(session, "post_end", value = end_date, min = start_date, max = end_date)
  })
  
  observeEvent(input$run, {
    req(data(), input$response, input$predictors, input$date_col, input$pre_start, input$pre_end, input$post_start, input$post_end)
    df <- data()
    
    # Parse date
    dates <- tryCatch({
      if (input$date_format == "auto") {
        parse_date_time(df[[input$date_col]], orders = c("ymd", "mdy", "dmy", "ymd HMS", "mdy HMS", "dmy HMS"))
      } else {
        as.Date(df[[input$date_col]], format = input$date_format)
      }
    }, error = function(e) {
      showNotification("Date conversion failed. Check your date format.", type = "error")
      return(NULL)
    })
    
    if (is.null(dates)) return()
    df[[input$date_col]] <- as.Date(dates)
    
    # Clean dataset
    all_cols <- c(input$date_col, input$response, input$predictors)
    df_clean <- df[complete.cases(df[, all_cols]), ]
    df_clean <- df_clean[order(df_clean[[input$date_col]]), ]
    
    if (input$pre_end >= input$post_start) {
      showNotification("Pre-period must end before post-period starts.", type = "error")
      return()
    }
    
    # Prepare model input
    df_model <- df_clean[, c(input$response, input$predictors)]
    df_model[] <- lapply(df_model, function(x) suppressWarnings(as.numeric(as.character(x))))
    df_dates <- df_clean[[input$date_col]]
    df_xts <- zoo(df_model, df_dates)
    
    pre_period <- as.Date(c(input$pre_start, input$pre_end))
    post_period <- as.Date(c(input$post_start, input$post_end))
    
    # Run Causal Impact
    impact <- tryCatch({
      CausalImpact(df_xts, pre_period, post_period)
    }, error = function(e) {
      showNotification(paste("CausalImpact failed:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(impact)) return()
    
    # 📝 Natural-language summary with wrapping
    output$impactReport <- renderUI({
      report <- capture.output(summary(impact, "report"))
      HTML(paste("<pre style='white-space: pre-wrap;'>", paste(report, collapse = "\n"), "</pre>"))
    })
    
    # 📊 Plot
    output$impactPlot <- renderPlot({ plot(impact) })
    
    # 📋 Technical summary with wrapping
    output$summary <- renderUI({
      summ <- capture.output(summary(impact))
      HTML(paste("<pre style='white-space: pre-wrap;'>", paste(summ, collapse = "\n"), "</pre>"))
    })
  })
}

shinyApp(ui, server)