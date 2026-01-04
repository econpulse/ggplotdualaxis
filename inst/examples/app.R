library(shiny)
library(ggplot2)
library(dplyr)
library(quantmod)
library(bslib) # Modern theme
# library(ggplotdualaxis) # Use this if package is installed
# If running mainly from dev project, one might use:
 pkgload::load_all("../..") # Adjust path if needed or just install the package first

# Default Tickers
DEFAULT_TICKERS <- c("^NDX", "EURUSD=X", "BTC-USD", "CHF=X", "GC=F", "CL=F")

ui <- page_sidebar(
  title = "Universal Dual Axis Plotter",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    selectInput("ticker1", "Primary Axis (Left)", choices = DEFAULT_TICKERS, selected = "^NDX"),
    selectInput("ticker2", "Secondary Axis (Right)", choices = DEFAULT_TICKERS, selected = "EURUSD=X"),
    dateRangeInput("dates", "Date Range", start = Sys.Date() - 365, end = Sys.Date()),
    numericInput("n_breaks", "Approx. Tick Count", value = 6, min = 3, max = 20),
    checkboxInput("invert", "Invert Right Axis", value = FALSE),
    actionButton("refresh", "Force Refresh Data", class = "btn-primary")
  ),
  
  card(
    card_header("Dual Axis Visualization"),
    plotOutput("mainPlot", height = "600px")
  ),
  card(
    card_header("Data Preview"),
    tableOutput("headTable")
  )
)

server <- function(input, output, session) {
  
  # --- Data Fetching ---
  # Reactive that fetches ALL selected tickers at once to keep it simple
  stock_data <- reactive({
    req(input$ticker1, input$ticker2, input$dates)
    input$refresh # Dependencies
    
    tickers <- unique(c(input$ticker1, input$ticker2))
    s_date <- input$dates[1]
    e_date <- input$dates[2]
    
    # Progress indicator
    withProgress(message = "Fetching Data...", value = 0, {
      
      all_res <- list()
      for(tk in tickers) {
        incProgress(1/length(tickers), detail = tk)
        tryCatch({
          tmp <- getSymbols(tk, src = "yahoo", from = s_date, to = e_date, auto.assign = FALSE, warnings = FALSE)
          # Clean and convert to Tibble
          # Use Ad() for Adjusted Close, fallback to Cl() if not found
          val_col <- tryCatch(Ad(tmp), error = function(e) Cl(tmp))
          
          df_tk <- data.frame(
            date = index(tmp),
            ticker = tk,
            value = as.numeric(val_col)
          )
          all_res[[tk]] <- df_tk
        }, error = function(e) {
          showNotification(paste("Error fetching", tk, ":", e$message), type = "error")
        })
      }
      
      bind_rows(all_res)
    })
  })
  
  # --- Transformation ---
  # Prepare the data for plotting
  plot_data <- reactive({
    req(stock_data())
    df <- stock_data()
    
    # Ensure both tickers exist in data
    if (!all(c(input$ticker1, input$ticker2) %in% unique(df$ticker))) {
      return(NULL)
    }
    
    transform_dual_axis(
      df,
      primary_var   = input$ticker1,
      secondary_var = input$ticker2,
      param_col     = "ticker",
      value_col     = "value",
      n_breaks      = input$n_breaks,
      invert_right  = input$invert
    )
  })
  
  # --- Plotting ---
  output$mainPlot <- renderPlot({
    df_trans <- plot_data()
    req(df_trans)
    
    # Base Plot
    p <- ggplot(df_trans, aes(x = date, y = value, color = ticker)) +
      geom_line(linewidth = 1) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      ) +
      labs(x = NULL, color = NULL)
    
    # Apply the Dual Axis Scale
    # We pass the data to `scale_y_dual_axis`? No, we pass attributes.
    # The scale function needs access to the attributes of the data plotted.
    # Standard ggplot doesn't easily pass data attributes to scale functions automatically 
    # UNLESS we pass the data explicitly to the scale helper or the helper knows where to look.
    # 
    # My helper `scale_y_dual_axis(data, ...)` requires the data object.
    
    p + scale_y_dual_axis(
      df_trans, 
      name_pri = input$ticker1,
      name_sec = input$ticker2
    )
  })
  
  output$headTable <- renderTable({
    req(plot_data())
    head(plot_data(), 10)
  })
}

shinyApp(ui, server)
