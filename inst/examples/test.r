library(quantmod)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

# ------------------------------------------------------------
ticker <-   c("^NDX", "BTC-USD", "CHF=X", "EURUSD=X")

df_long <- 
  map_df(ticker, function(i) {
    temp <- 
      getSymbols(
        i,
        src = "yahoo",
        from = "2025-01-01",
        to   = "2025-12-31",
        auto.assign = F,
        warnings = FALSE
      ) 
    temp[,4] |> data.frame() |> mutate(date = index(temp), ticker = i) |> rename(value = 1) |> tibble() 
  }) |> select(ticker, date, value)


build_dual_axis_universal_long <- function(
    df,
    y_left,
    y_right,
    value_col  = "value",
    ticker_col = "ticker",
    invert_right = FALSE,
    n_breaks = 6,
    center_right_override = NULL
) {
  
  # --- Safety checks (bewusst minimal) ---
  stopifnot(
    value_col  %in% names(df),
    ticker_col %in% names(df)
  )
  
  # --- Split data ---
  df <- df |> filter(ticker %in% c(y_left, y_right))
  df_left  <- df[df[[ticker_col]] %in% y_left,  , drop = FALSE]
  df_right <- df[df[[ticker_col]] %in% y_right, , drop = FALSE]
  
  if (nrow(df_left) == 0 || nrow(df_right) == 0) {
    stop("y_left or y_right does not match any rows in the data.")
  }
  
  y_left_vals  <- df_left[[value_col]]
  y_right_vals <- df_right[[value_col]]
  
  # --------------------------------------------------
  # Reuse the scalar logic (identisch zur Universal-Version)
  # --------------------------------------------------
  
  left_breaks <- pretty(y_left_vals, n = n_breaks)
  n <- length(left_breaks)
  
  span_left <- diff(range(left_breaks))
  
  center_right <- if (!is.null(center_right_override)) {
    center_right_override
  } else {
    median(y_right_vals, na.rm = TRUE)
  }
  
  span_right_data <- diff(range(y_right_vals, na.rm = TRUE))
  
  nice_step <- function(x) {
    if (!is.finite(x) || x <= 0) return(1)
    pow <- 10^floor(log10(x))
    m <- x / pow
    base <- if (m <= 1) 1 else if (m <= 2) 2 else if (m <= 5) 5 else 10
    base * pow
  }
  
  min_step <- span_right_data / (n - 1)
  step <- nice_step(min_step)
  
  k <- floor((n - 1) / 2)
  right_labels <- seq(
    center_right - k * step,
    by = step,
    length.out = n
  )
  
  span_right_labels <- diff(range(right_labels))
  a <- span_left / span_right_labels
  
  a_max <- span_left / span_right_data
  if (a > a_max) {
    factor <- a / a_max
    step2 <- nice_step(step * factor)
    right_labels <- seq(
      center_right - k * step2,
      by = step2,
      length.out = n
    )
    span_right_labels <- diff(range(right_labels))
    a <- span_left / span_right_labels
  }
  
  mid_left  <- mean(left_breaks)
  mid_right <- mean(right_labels)
  
  # --- Transformation ---
  if (!invert_right) {
    forward <- function(x) mid_left + a * (x - mid_right)
  } else {
    forward <- function(x) mid_left - a * (x - mid_right)
    right_labels <- rev(right_labels)
  }
  
  # --------------------------------------------------
  # Apply transformation ONLY to y_right rows
  # --------------------------------------------------
  df_out <- df
  idx_right <- df_out[[ticker_col]] %in% y_right
  
  df_out[[value_col]][idx_right] <- forward(df_out[[value_col]][idx_right])
  
  # --------------------------------------------------
  # Attach metadata as attributes (ggplot-friendly)
  # --------------------------------------------------
  attr(df_out, "dual_axis") <- list(
    left_breaks  = left_breaks,
    right_labels = right_labels,
    invert_right = invert_right
  )
  
  df_out
}

df_scaled <- build_dual_axis_universal_long(
  df = df_long,
  y_left  = c("EURUSD=X"),
  y_right = c( "CHF=X"),
  invert_right = FALSE,
  n_breaks= 4, 
  center_right_override = 0.8
)

ax <- attr(df_scaled, "dual_axis")

ggplot(df_scaled, aes(x = date, y = value, color = ticker)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(
    name   = "Nasdaq 100",
    breaks = ax$left_breaks,
    sec.axis = sec_axis(
      transform = ~ .,
      breaks    = ax$left_breaks,
      labels    = scales::label_number(accuracy = 0.01)(ax$right_labels),
      name      = "EUR / USD"
    )
  ) +
  theme_minimal()  + theme(panel.grid.minor.y = element_blank(), 
                           panel.grid.major.x = element_blank(),
                           panel.grid.minor.x = element_blank(),
                           legend.position = "bottom"
                           )
