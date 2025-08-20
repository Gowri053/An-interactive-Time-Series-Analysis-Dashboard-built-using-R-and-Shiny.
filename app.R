library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(forecast)
library(tseries)
library(readxl)
library(gridExtra)
library(plotly)
library(rmarkdown)
library(rugarch)  # Added for GARCH modeling

# Define UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = span(tagList(icon("chart-line"), "Time Series Analysis"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload"),
               fileInput("file", "Upload CSV or XLSX", accept = c(".csv", ".xlsx"))
      ),
      menuItem("Analysis Settings", tabName = "settings", icon = icon("sliders-h"),
               numericInput("frequency", "Set Frequency (12 = Monthly, 4 = Quarterly)", value = 12),
               numericInput("horizon", "Forecast Horizon (h)", value = 12, min = 1),
               actionBttn("analyze", "Analyze Data", style = "gradient", color = "primary")
      ),
      hr(),
      h5("Developed by TEAM 4", align = "center"),
      br()
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".box { border-radius: 10px; box-shadow: 2px 2px 12px #aaa; }"))),
    tabsetPanel(
      tabPanel("Summary", verbatimTextOutput("summary") %>% withSpinner(color = "#007BFF")),
      tabPanel("Plot", plotlyOutput("timeSeriesPlot") %>% withSpinner(color = "#007BFF")),
      tabPanel("Decomposition", plotOutput("decompositionPlot") %>% withSpinner(color = "#007BFF")),
      tabPanel("Seasonality Check", verbatimTextOutput("seasonalityCheck") %>% withSpinner(color = "#007BFF")),
      tabPanel("Order of Differencing", verbatimTextOutput("orderDiff") %>% withSpinner(color = "#007BFF")),
      tabPanel("Stationarity Check", plotOutput("acf_pacf") %>% withSpinner(color = "#007BFF"), verbatimTextOutput("adf_test") %>% withSpinner(color = "#007BFF")),
      tabPanel("Post-Stationarity ACF & PACF", plotOutput("acf_pacf_stationary") %>% withSpinner(color = "#007BFF")),
      tabPanel("All Fitted Models", verbatimTextOutput("allModels") %>% withSpinner(color = "#007BFF")),
      tabPanel("Best Model Selection", verbatimTextOutput("modelSelection") %>% withSpinner(color = "#007BFF"), verbatimTextOutput("modelAccuracy") %>% withSpinner(color = "#007BFF")),
      
      tabPanel("Residual Analysis",
               plotOutput("residualPlot") %>% withSpinner(color = "#007BFF"),
               verbatimTextOutput("residualSummary") %>% withSpinner(color = "#007BFF")),
      
      tabPanel("Volatility Modeling (GARCH)", 
               verbatimTextOutput("garchSummary") %>% withSpinner(color = "#007BFF"),
               plotOutput("garchPlot") %>% withSpinner(color = "#007BFF")),
      
      tabPanel("Smoothing Techniques",
               selectInput("smoothing_method", "Choose a Smoothing Technique:",
                           choices = c("Simple Exponential Smoothing" = "ses",
                                       "Holt’s Linear Trend Method" = "holt",
                                       "Holt-Winters Seasonal Method" = "hw")),
               checkboxInput("show_smooth_summary", "Show Model Summary", value = TRUE),
               plotOutput("smoothingPlot") %>% withSpinner(color = "#007BFF"),
               verbatimTextOutput("smoothingSummary") %>% withSpinner(color = "#007BFF")),
      
      tabPanel("Forecast", 
               plotOutput("forecastPlot") %>% withSpinner(color = "#007BFF"), 
               verbatimTextOutput("forecastValues") %>% withSpinner(color = "#007BFF")),
      
      tabPanel("Download Report", 
               downloadButton("downloadReport", "Download HTML Report", class = "btn-primary"))
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- if (ext == "csv") read.csv(input$file$datapath) else read_excel(input$file$datapath)
    df[[1]] <- as.Date(df[[1]], format = "%Y-%m-%d")
    ts(df[[2]], frequency = input$frequency)
  })
  
  output$summary <- renderPrint({
    ts_data <- data()
    cat("Basic Statistics:\n")
    print(summary(ts_data))
    cat("\nLength of Time Series:", length(ts_data), "\n")
    cat("\nFirst Few Observations:\n")
    print(head(ts_data))
  })
  
  output$timeSeriesPlot <- renderPlotly({
    req(input$file)
    ts_data <- data()
    p <- ggplot(data.frame(Time = seq_along(ts_data), Value = as.numeric(ts_data)), aes(x = Time, y = Value)) +
      geom_line(color = "blue") +
      ggtitle("Time Series Data") +
      xlab("Time") +
      ylab("Value") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$decompositionPlot <- renderPlot({
    ts_data <- data()
    decomp <- stl(ts_data, s.window = "periodic")
    autoplot(decomp) + ggtitle("Time Series Decomposition")
  })
  
  output$seasonalityCheck <- renderPrint({
    ts_data <- data()
    test <- nsdiffs(ts_data)
    cat("Time Series", ifelse(test > 0, "has", "does not have"), "a seasonal component\n")
    if (test > 0) {
      cat("Conclusion: The time series exhibits seasonality and requires seasonal differencing.\n")
    } else {
      cat("Conclusion: The time series does not exhibit seasonality.\n")
    }
  })
  
  output$orderDiff <- renderPrint({
    ts_data <- data()
    d <- ndiffs(ts_data)
    cat("Optimal differencing order:", d, "\n")
    s_d <- nsdiffs(ts_data)
    if (s_d > 0) {
      cat("Optimal seasonal differencing order:", s_d, "\n")
    } else {
      cat("No seasonal component detected, seasonal differencing is not required.\n")
    }
  })
  
  output$acf_pacf <- renderPlot({
    ts_data <- data()
    grid.arrange(ggAcf(ts_data), ggPacf(ts_data), ncol = 2)
  })
  
  output$adf_test <- renderPrint({
    ts_data <- data()
    test_result <- adf.test(ts_data)
    print(test_result)
    cat("Conclusion:", ifelse(test_result$p.value < 0.05, "The series is stationary.", "The series is non-stationary and requires differencing."), "\n")
  })
  
  output$acf_pacf_stationary <- renderPlot({
    ts_data <- diff(data(), differences = ndiffs(data()))
    grid.arrange(ggAcf(ts_data), ggPacf(ts_data), ncol = 2)
  })
  
  is_seasonal <- reactive({
    nsdiffs(data()) > 0
  })
  
  output$allModels <- renderPrint({
    ts_data <- diff(data(), differences = ndiffs(data()))
    
    if (is_seasonal()) {
      best_sarima <- auto.arima(ts_data, seasonal = TRUE)
      models <- list("Seasonal ARIMA (SARIMA)" = best_sarima)
    } else {
      best_arima <- auto.arima(ts_data, seasonal = FALSE)
      p <- best_arima$arma[1]  
      q <- best_arima$arma[2]  
      
      ar_model <- Arima(ts_data, order = c(p, ndiffs(ts_data), 0))  
      ma_model <- Arima(ts_data, order = c(0, ndiffs(ts_data), q))  
      arma_model <- Arima(ts_data, order = c(p, ndiffs(ts_data), q))  
      
      models <- list(
        "Auto ARIMA" = best_arima,
        "AR Model (p, d, 0)" = ar_model,
        "MA Model (0, d, q)" = ma_model,
        "ARMA Model (p, d, q)" = arma_model
      )
    }
    
    cat("All Fitted Models:\n")
    print(models)
  })
  
  output$modelSelection <- renderPrint({
    ts_data <- diff(data(), differences = ndiffs(data()))
    
    if (is_seasonal()) {
      best_model <- auto.arima(ts_data, seasonal = TRUE)
      best_model_name <- "Seasonal ARIMA (SARIMA)"
    } else {
      best_arima <- auto.arima(ts_data, seasonal = FALSE)
      p <- best_arima$arma[1]  
      q <- best_arima$arma[2]  
      
      ar_model <- Arima(ts_data, order = c(p, ndiffs(ts_data), 0))
      ma_model <- Arima(ts_data, order = c(0, ndiffs(ts_data), q))
      arma_model <- Arima(ts_data, order = c(p, ndiffs(ts_data), q))
      
      models <- list(
        "Auto ARIMA" = best_arima,
        "AR Model (p, d, 0)" = ar_model,
        "MA Model (0, d, q)" = ma_model,
        "ARMA Model (p, d, q)" = arma_model
      )
      
      best_model_name <- names(which.min(sapply(models, AIC)))
      best_model <- models[[best_model_name]]
    }
    
    cat("Best Model Selected Based on AIC:", best_model_name, "\n")
    print(best_model)
  })
  
  output$residualPlot <- renderPlot({
    ts_data <- data()
    model <- auto.arima(ts_data)
    checkresiduals(model)
  })
  
  output$residualSummary <- renderPrint({
    model <- auto.arima(diff(data(), differences = ndiffs(data())))
    residuals <- residuals(model)
    cat("Residuals Summary:\n")
    print(summary(residuals))
    cat("\nAssumptions:\n")
    cat(ifelse(shapiro.test(residuals)$p.value > 0.05, "Residuals follow normal distribution.", "Residuals do not follow normal distribution."), "\n")
    cat(ifelse(Box.test(residuals, type = "Ljung-Box")$p.value > 0.05, "No autocorrelation among residuals.", "Autocorrelation detected in residuals."))
  })
  
  output$forecastPlot <- renderPlot({
    model <- auto.arima(diff(data(), differences = ndiffs(data())))
    forecast_data <- forecast(model, h = input$horizon)
    autoplot(forecast_data) + ggtitle("Forecasted Values")
  })
  
  output$forecastValues <- renderPrint({
    model <- auto.arima(diff(data(), differences = ndiffs(data())))
    forecast_data <- forecast(model, h = input$horizon)
    print(forecast_data)
  })
  
  output$garchSummary <- renderPrint({
    req(input$file)
    ts_data <- data()
    
    tryCatch({
      returns <- na.omit(diff(log(ts_data)))
      
      if (length(returns) < 30) {
        stop("Not enough data points for GARCH modeling (need at least 30).")
      }
      
      spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
        distribution.model = "norm"
      )
      
      fit <- ugarchfit(spec, returns, solver = "hybrid")
      show(fit)
    }, error = function(e) {
      cat("GARCH fitting failed:\n", e$message)
    })
  })
  
  output$garchPlot <- renderPlot({
    req(input$file)
    ts_data <- data()
    
    tryCatch({
      returns <- na.omit(diff(log(ts_data)))
      
      if (length(returns) < 30) {
        plot.new()
        text(0.5, 0.5, "Not enough data for GARCH modeling.", cex = 1.2)
        return()
      }
      
      spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
        distribution.model = "norm"
      )
      
      fit <- ugarchfit(spec, returns, solver = "hybrid")
      plot(fit, which = "all")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error in GARCH plot:\n", e$message), cex = 1.2)
    })
  })
  
  output$smoothingPlot <- renderPlot({
    req(input$file)
    ts_data <- data()
    fitted_model <- switch(input$smoothing_method,
                           "ses" = ses(ts_data),
                           "holt" = holt(ts_data),
                           "hw" = hw(ts_data, seasonal = "additive"))
    
    autoplot(fitted_model) +
      ggtitle(paste("Smoothing -", input$smoothing_method)) +
      theme_minimal()
  })
  
  output$smoothingSummary <- renderPrint({
    req(input$show_smooth_summary)
    ts_data <- data()
    fitted_model <- switch(input$smoothing_method,
                           "ses" = ses(ts_data),
                           "holt" = holt(ts_data),
                           "hw" = hw(ts_data, seasonal = "additive"))
    summary(fitted_model)
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("Time_Series_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        data = data(),
        horizon = input$horizon,
        frequency = input$frequency
      )
      
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Run the Shiny App
shinyApp(ui, server)
