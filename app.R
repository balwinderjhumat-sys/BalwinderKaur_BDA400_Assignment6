
# ============================================================
# BDA400 - ASSIGNMENT 6
# TECHNICAL ANALYSIS USING R - VISUALIZATION PHASE
# Student Name: Balwinder Kaur
# Stock: Apple Inc. (AAPL)
# ============================================================


# SECTION 1: LOAD PACKAGES

library(shiny)
library(quantmod)
library(ggplot2)
library(TTR)
library(dplyr)
library(scales)


# SECTION 2: DATA COLLECTION

stock_symbol <- "AAPL"

start_date <- Sys.Date() - 365
end_date <- Sys.Date()


fetch_stock_data <- function(symbol, start, end, timeframe) {
  
  stock <- getSymbols(
    symbol,
    src = "yahoo",
    from = as.Date(start),
    to = as.Date(end) + 1,
    auto.assign = FALSE
  )
  
  # Select time frame
  
  if (timeframe == "Weekly") {
    stock <- to.weekly(stock, indexAt = "lastof")
  }
  
  if (timeframe == "Monthly") {
    stock <- to.monthly(stock, indexAt = "lastof")
  }
  
  data <- data.frame(
    Date = as.Date(index(stock)),
    Open = as.numeric(Op(stock)),
    High = as.numeric(Hi(stock)),
    Low = as.numeric(Lo(stock)),
    Close = as.numeric(Cl(stock)),
    Adjusted = as.numeric(Ad(stock)),
    Volume = as.numeric(Vo(stock))
  )
  
  return(data)
}


# SECTION 3: TECHNICAL INDICATORS


# SECTION 3: TECHNICAL INDICATORS
# Clean missing values before calculating indicators

calculate_indicators <- function(data, short_period, long_period) {
  
  # Remove missing or invalid stock prices
  data <- data %>%
    filter(
      complete.cases(.),
      is.finite(Adjusted),
      Adjusted > 0
    ) %>%
    arrange(Date)
  
  # Check that sufficient data is available
  if (nrow(data) < max(long_period, 30)) {
    stop("Not enough valid stock data. Please select a longer date range.")
  }
  
  # Calculate Moving Averages
  data$Short_MA <- as.numeric(
    TTR::SMA(data$Adjusted, n = short_period)
  )
  
  data$Long_MA <- as.numeric(
    TTR::SMA(data$Adjusted, n = long_period)
  )
  
  # Calculate RSI
  data$RSI <- as.numeric(
    TTR::RSI(data$Adjusted, n = 14)
  )
  
  # Calculate MACD
  macd_values <- TTR::MACD(
    data$Adjusted,
    nFast = 12,
    nSlow = 26,
    nSig = 9
  )
  
  data$MACD <- as.numeric(macd_values[, 1])
  data$MACD_Signal <- as.numeric(macd_values[, 2])
  data$MACD_Histogram <- data$MACD - data$MACD_Signal
  
  # Trading rules: Moving Average Crossover
  previous_short <- dplyr::lag(data$Short_MA)
  previous_long <- dplyr::lag(data$Long_MA)
  
  buy_signal <- (
    !is.na(previous_short) &
      !is.na(previous_long) &
      !is.na(data$Short_MA) &
      !is.na(data$Long_MA) &
      previous_short <= previous_long &
      data$Short_MA > data$Long_MA
  )
  
  sell_signal <- (
    !is.na(previous_short) &
      !is.na(previous_long) &
      !is.na(data$Short_MA) &
      !is.na(data$Long_MA) &
      previous_short >= previous_long &
      data$Short_MA < data$Long_MA
  )
  
  data$Signal <- ifelse(
    buy_signal,
    "Buy",
    ifelse(sell_signal, "Sell", "Hold")
  )
  
  return(data)
}

  
  # SECTION 4: TRADING RULES
  
  previous_short <- dplyr::lag(data$Short_MA)
  previous_long <- dplyr::lag(data$Long_MA)
  
  buy_signal <-
    !is.na(previous_short) &
    !is.na(previous_long) &
    !is.na(data$Short_MA) &
    !is.na(data$Long_MA) &
    previous_short <= previous_long &
    data$Short_MA > data$Long_MA
  
  sell_signal <-
    !is.na(previous_short) &
    !is.na(previous_long) &
    !is.na(data$Short_MA) &
    !is.na(data$Long_MA) &
    previous_short >= previous_long &
    data$Short_MA < data$Long_MA
  
  data$Signal <- ifelse(
    buy_signal,
    "Buy",
    ifelse(sell_signal, "Sell", "Hold")
  )
  
  return(data)
}


# SECTION 5: USER INTERFACE

ui <- fluidPage(
  
  titlePanel(
    "Balwinder Kaur - Apple Stock Portfolio Dashboard"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Stock Selection"),
      
      textInput(
        "symbol",
        "Enter Stock Symbol:",
        value = "AAPL"
      ),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = Sys.Date() - 365,
        end = Sys.Date(),
        max = Sys.Date()
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c("Line", "Area", "Candlestick")
      ),
      
      hr(),
      
      h4("Technical Indicators"),
      
      checkboxGroupInput(
        "indicators",
        "Choose Indicators:",
        choices = c(
          "Moving Averages",
          "RSI",
          "MACD"
        ),
        selected = c(
          "Moving Averages",
          "RSI",
          "MACD"
        )
      ),
      
      numericInput(
        "short_ma",
        "Short Moving Average:",
        value = 20,
        min = 2,
        max = 200
      ),
      
      numericInput(
        "long_ma",
        "Long Moving Average:",
        value = 50,
        min = 3,
        max = 300
      ),
      
      actionButton(
        "refresh",
        "Refresh Stock Data",
        class = "btn-primary"
      )
    ),
    
    
    # MAIN DASHBOARD
    
    mainPanel(
      
      h3("Stock Market Analysis"),
      
      textOutput("stock_status"),
      
      uiOutput("summary"),
      
      h4("Stock Price Chart"),
      
      plotOutput(
        "stock_chart",
        height = "450px"
      ),
      
      h4("Relative Strength Index"),
      
      plotOutput(
        "rsi_chart",
        height = "250px"
      ),
      
      h4("MACD Indicator"),
      
      plotOutput(
        "macd_chart",
        height = "250px"
      ),
      
      h4("Buy and Sell Trading Signals"),
      
      tableOutput("signal_table")
    )
  )
)


# SECTION 6: SERVER COMPONENT

server <- function(input, output, session) {
  
  stock_data <- eventReactive(input$refresh, {
    
    req(input$symbol)
    req(input$date_range)
    
    validate(
      need(
        input$short_ma < input$long_ma,
        "Short MA must be smaller than Long MA."
      ),
      
      need(
        input$date_range[1] <= input$date_range[2],
        "Please select a valid date range."
      )
    )
    
    tryCatch({
      
      fetch_stock_data(
        input$symbol,
        input$date_range[1],
        input$date_range[2],
        input$time_frame
      )
      
    }, error = function(e) {
      
      showNotification(
        paste("Data Error:", conditionMessage(e)),
        type = "error",
        duration = 10
      )
      
      NULL
    })
    
  }, ignoreInit = FALSE)
  
  
  analyzed_data <- reactive({
    
    req(stock_data())
    
    validate(
      need(
        input$short_ma < input$long_ma,
        "Short MA must be smaller than Long MA."
      )
    )
    
    calculate_indicators(
      stock_data(),
      input$short_ma,
      input$long_ma
    )
  })
  
  
  # SECTION 7: STOCK SUMMARY
  
  output$stock_status <- renderText({
    
    req(analyzed_data())
    
    paste(
      "Stock:", toupper(input$symbol),
      "| Records:", nrow(analyzed_data()),
      "| Time Frame:", input$time_frame
    )
  })
  
  
  output$summary <- renderUI({
    
    data <- analyzed_data()
    
    latest <- data[nrow(data), ]
    
    fluidRow(
      
      column(
        4,
        wellPanel(
          strong("Latest Adjusted Price"),
          h3(dollar(latest$Adjusted))
        )
      ),
      
      column(
        4,
        wellPanel(
          strong("RSI Value"),
          h3(round(latest$RSI, 2))
        )
      ),
      
      column(
        4,
        wellPanel(
          strong("Trading Signal"),
          h3(latest$Signal)
        )
      )
    )
  })
  
  
  # SECTION 8: STOCK PRICE CHART
  
  output$stock_chart <- renderPlot({
    
    data <- analyzed_data()
    
    p <- ggplot(data, aes(x = Date))
    
    if (input$chart_type == "Line") {
      
      p <- p +
        geom_line(
          aes(y = Adjusted, color = "Stock Price"),
          linewidth = 0.8
        )
      
    } else if (input$chart_type == "Area") {
      
      p <- p +
        geom_area(
          aes(y = Adjusted),
          fill = "skyblue",
          alpha = 0.5
        ) +
        geom_line(
          aes(y = Adjusted),
          color = "blue"
        )
      
    } else {
      
      candle_data <- data %>%
        mutate(
          Direction = ifelse(
            Close >= Open,
            "Up",
            "Down"
          )
        )
      
      p <- p +
        geom_segment(
          aes(
            x = Date,
            xend = Date,
            y = Low,
            yend = High
          ),
          color = "black"
        ) +
        geom_rect(
          data = candle_data,
          aes(
            xmin = Date - 0.35,
            xmax = Date + 0.35,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Direction
          ),
          inherit.aes = FALSE
        ) +
        scale_fill_manual(
          values = c(
            "Up" = "darkgreen",
            "Down" = "red"
          )
        )
    }
    
    
    # ADD MOVING AVERAGES
    
    if ("Moving Averages" %in% input$indicators) {
      
      p <- p +
        
        geom_line(
          aes(
            y = Short_MA,
            color = "Short MA"
          ),
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        
        geom_line(
          aes(
            y = Long_MA,
            color = "Long MA"
          ),
          linewidth = 0.8,
          na.rm = TRUE
        )
    }
    
    
    # ADD BUY AND SELL ANNOTATIONS
    
    signals <- data %>%
      filter(Signal %in% c("Buy", "Sell"))
    
    if (nrow(signals) > 0) {
      
      p <- p +
        
        geom_point(
          data = signals,
          aes(
            y = Adjusted,
            color = Signal,
            shape = Signal
          ),
          size = 3
        ) +
        
        geom_text(
          data = signals,
          aes(
            y = Adjusted,
            label = Signal
          ),
          vjust = -1,
          size = 3
        )
    }
    
    
    p +
      
      scale_y_continuous(
        labels = dollar
      ) +
      
      labs(
        title = paste(
          toupper(input$symbol),
          "Stock Price Analysis"
        ),
        x = "Date",
        y = "Price (USD)",
        color = "Chart Indicators",
        shape = "Trading Signal"
      ) +
      
      theme_minimal() +
      
      theme(
        legend.position = "bottom"
      )
  })
  
  
  # SECTION 9: RSI CHART
  
  output$rsi_chart <- renderPlot({
    
    req("RSI" %in% input$indicators)
    
    data <- analyzed_data()
    
    ggplot(
      data,
      aes(x = Date, y = RSI)
    ) +
      
      geom_line(
        color = "purple",
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      
      geom_hline(
        yintercept = 70,
        color = "red",
        linetype = "dashed"
      ) +
      
      geom_hline(
        yintercept = 30,
        color = "green",
        linetype = "dashed"
      ) +
      
      scale_y_continuous(
        limits = c(0, 100)
      ) +
      
      labs(
        title = "Relative Strength Index (RSI)",
        x = "Date",
        y = "RSI Value"
      ) +
      
      theme_minimal()
  })
  
  
  # SECTION 10: MACD CHART
  
  output$macd_chart <- renderPlot({
    
    req("MACD" %in% input$indicators)
    
    data <- analyzed_data()
    
    ggplot(
      data,
      aes(x = Date)
    ) +
      
      geom_col(
        aes(y = MACD_Histogram),
        fill = "grey"
      ) +
      
      geom_line(
        aes(y = MACD),
        color = "blue",
        linewidth = 0.8
      ) +
      
      geom_line(
        aes(y = MACD_Signal),
        color = "orange",
        linewidth = 0.8
      ) +
      
      labs(
        title = "MACD Technical Indicator",
        x = "Date",
        y = "MACD Value"
      ) +
      
      theme_minimal()
  })
  
  
  # SECTION 11: TRADING SIGNAL TABLE
  
  output$signal_table <- renderTable({
    
    data <- analyzed_data()
    
    signals <- data %>%
      
      filter(
        Signal %in% c("Buy", "Sell")
      ) %>%
      
      select(
        Date,
        Adjusted,
        Short_MA,
        Long_MA,
        RSI,
        Signal
      ) %>%
      
      arrange(desc(Date)) %>%
      
      head(10) %>%
      
      mutate(
        Adjusted = round(Adjusted, 2),
        Short_MA = round(Short_MA, 2),
        Long_MA = round(Long_MA, 2),
        RSI = round(RSI, 2)
      )
    
    if (nrow(signals) == 0) {
      return(data.frame(
        Message = "No Buy or Sell crossover signals found."
      ))
    }
    
    signals
    
  }, striped = TRUE, bordered = TRUE)
  
}


# SECTION 12: RUN SHINY APPLICATION

shinyApp(
  ui = ui,
  server = server
)
