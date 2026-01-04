library(dplyr)
library(ggplot2)
library(scales)

#' @return A secondary-axis-ready ggplot object.
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'   # library(ggplotdualaxis)
#'   df <- data.frame(
#'     date = rep(seq(1, 10), 2),
#'     value = c(seq(1, 10), seq(100, 1000)),
#'     ticker = rep(c("A", "B"), each = 10)
#'   )
#'   ggplot_dual_axis(df, primary_var = "A", secondary_var = "B") + geom_line()
#' }
#' 
#' @export
ggplot_dual_axis <- function(data, mapping = aes(), 
                             primary_var, secondary_var,
                             group_col = "ticker",
                             value_col = NULL,
                             name_pri = primary_var, name_sec = secondary_var,
                             ...) {
  
  # Extract value column from mapping if not specified
  if (is.null(value_col)) {
    # Try to extract 'y' aesthetic
    if (!is.null(mapping$y)) {
      # Use rlang or simple deparse if available, typically rlang::as_label
      value_col <- rlang::as_label(mapping$y)
    } else {
      value_col <- "value" # fallback
    }
  }

  # 1. Transform Data
  df_trans <- transform_dual_axis(
    df = data,
    primary_var = primary_var,
    secondary_var = secondary_var,
    param_col = group_col,
    value_col = value_col,
    ... # e.g. n_breaks, invert_right
  )
  
  # 2. Init Plot with transformed data
  p <- ggplot(data = df_trans, mapping = mapping)
  
  # 3. Add Scale (configured using the transformed data attributes)
  p <- p + scale_y_dual_axis(df_trans, name_pri = name_pri, name_sec = name_sec)
  
  return(p)
}

#' Transform Data for Dual Axis Plotting
#'
#' Scales a secondary variable to match the range of a primary variable,
#' aligning grid lines if possible.
#'
#' @param df Dataframe in long format.
#' @param param_col Name of the column containing variable names (e.g. "ticker").
#' @param value_col Name of the column containing values.
#' @param primary_var Name of the primary variable (Left Axis).
#' @param secondary_var Name of the secondary variable (Right Axis).
#' @param n_breaks Number of desired breaks for the primary axis.
#' @param center_right_override Optional override for the right axis center (median).
#' @param invert_right Boolean, whether to invert the right axis.
#'
#' @return A dataframe with transformed values for the secondary variable,
#'   along with a "dual_axis" attribute containing transformation parameters.
#' @export
transform_dual_axis <- function(
    df,
    primary_var,
    secondary_var,
    param_col = "ticker",
    value_col = "value",
    n_breaks = 6,
    center_right_override = NULL,
    invert_right = FALSE
) {
  
  # --- 1. Basic Checks ---
  if (!all(c(param_col, value_col) %in% names(df))) {
    stop("Specified columns not found in dataframe.")
  }
  
  # Filter data
  df_sub <- df[df[[param_col]] %in% c(primary_var, secondary_var), , drop = FALSE]
  
  # Split vectors
  vals_pri <- df_sub[[value_col]][df_sub[[param_col]] == primary_var]
  vals_sec <- df_sub[[value_col]][df_sub[[param_col]] == secondary_var]
  
  if (length(vals_pri) == 0) stop("No data for primary variable: ", primary_var)
  if (length(vals_sec) == 0) stop("No data for secondary variable: ", secondary_var)
  
  # --- 2. Calculate Axis Parameters ---
  
  # Primary Axis Breaks (The Driver)
  breaks_pri <- pretty(vals_pri, n = n_breaks)
  n <- length(breaks_pri)
  span_pri <- diff(range(breaks_pri))
  mid_pri  <- mean(breaks_pri)
  
  # Secondary Axis Range & Center
  center_sec <- if (!is.null(center_right_override)) center_right_override else median(vals_sec, na.rm = TRUE)
  span_sec_data <- diff(range(vals_sec, na.rm = TRUE))
  if (span_sec_data == 0) span_sec_data <- 1 # Prevent division by zero
  
  # Helper for nice steps (1, 2, 5, 10...)
  nice_step <- function(x) {
    if (!is.finite(x) || x <= 0) return(1)
    pow <- 10^floor(log10(x))
    m <- x / pow
    base <- if (m <= 1) 1 else if (m <= 2) 2 else if (m <= 5) 5 else 10
    base * pow
  }
  
  # Calculate Initial Secondary Step to match tick count (n)
  min_step_sec <- span_sec_data / (n - 1)
  step_sec <- nice_step(min_step_sec)
  
  # Generate Candidate Labels centered around the median/center
  # Snap the center to the nearest step to ensure "pretty" labels (e.g. 100, 110 vs 103.4, 113.4)
  center_sec <- round(center_sec / step_sec) * step_sec
  
  k <- floor((n - 1) / 2)
  labels_sec <- seq(center_sec - k * step_sec, by = step_sec, length.out = n)
  
  # Re-scale if the generated labels are too tight (zoomed in too much)
  span_sec_labels <- diff(range(labels_sec))
  # Ratio of Primary Span to Secondary Label Span
  scaling_factor <- span_pri / span_sec_labels
  
  # Limit the zoom. If primary span covers too little of the secondary data relative to labels, expand secondary step.
  # (Heuristic: Ensure we cover enough of the secondary data range)
  ratio_max <- span_pri / span_sec_data
  if (scaling_factor > ratio_max) {
    adj_factor <- scaling_factor / ratio_max
    step_sec_2 <- nice_step(step_sec * adj_factor)
    labels_sec <- seq(center_sec - k * step_sec_2, by = step_sec_2, length.out = n)
    span_sec_labels <- diff(range(labels_sec))
    scaling_factor <- span_pri / span_sec_labels
  }
  
  mid_sec <- mean(labels_sec)
  
  # --- 3. Define Transformation Functions ---
  # To plot Secondary on Primary Scale:
  # y_plot = mid_pri + scaling_factor * (y_raw - mid_sec)
  
  if (invert_right) {
    trans_forward <- function(y) mid_pri - scaling_factor * (y - mid_sec)
    trans_reverse <- function(y) mid_sec - (y - mid_pri) / scaling_factor # maps back
    labels_final  <- rev(labels_sec)
  } else {
    trans_forward <- function(y) mid_pri + scaling_factor * (y - mid_sec)
    trans_reverse <- function(y) mid_sec + (y - mid_pri) / scaling_factor
    labels_final  <- labels_sec
  }
  
  # --- 4. Apply Transformation ---
  df_out <- df_sub
  is_sec <- df_out[[param_col]] == secondary_var
  df_out[[value_col]][is_sec] <- trans_forward(df_out[[value_col]][is_sec])
  
  # --- 5. Return with Attributes ---
  attr(df_out, "dual_axis") <- list(
    primary_breaks   = breaks_pri,
    secondary_labels = labels_final,
    sec_name         = secondary_var,
    trans_forward    = trans_forward,
    trans_reverse    = trans_reverse # Not strictly needed for sec_axis breaks but useful
  )
  
  return(df_out)
}


#' @param labels Label formatter for primary axis (default waiver()).
#' @param label_sec Label formatter for secondary axis (default scales::label_number(accuracy = 0.01)).
#' @export
scale_y_dual_axis <- function(data, name_pri = waiver(), name_sec = waiver(), 
                              labels = waiver(), 
                              label_sec = scales::label_number(accuracy = 0.01),
                              ...) {
  
  params <- attr(data, "dual_axis")
  if (is.null(params)) {
    warning("No dual_axis attributes found on data. Using standard scale.")
    return(scale_y_continuous(name = name_pri, labels = labels, ...))
  }
  
  if (inherits(name_sec, "waiver")) name_sec <- params$sec_name
  
  scale_y_continuous(
    name = name_pri,
    breaks = params$primary_breaks, 
    labels = labels,
    
    sec.axis = sec_axis(
      transform = ~ ., # Identity transform
      breaks = params$primary_breaks, 
      labels = function(x) {
        # Transform back to original values
        vals <- params$trans_reverse(x)
        
        # Apply formatter if provided
        if (is.function(label_sec)) {
          return(label_sec(vals))
        }
        
        # Fallback if label_sec is not a function (though default is)
        # Check alignment just in case
        if (length(x) == length(params$secondary_labels) && 
            all(abs(x - params$primary_breaks) < 1e-8)) {
           return(scales::label_number(accuracy = 0.01)(params$secondary_labels))
        }
        return(scales::label_number(accuracy = 0.01)(vals))
      },
      name = name_sec
    ),
    ...
  )
}

#' Wrapper for Dual Axis Plot
#' 
#' Combines transformation and plot initialization.
#'
#' @param data Long format dataframe.
#' @param mapping Aesthetic mapping (must include x, y, color/group).
#' @param group_col Name of column distinguishing series (default "ticker").
#' @param primary_var Value in group_col for Left Axis.
#' @param secondary_var Value in group_col for Right Axis.
#' @param value_col Name of y-value column (optional, tries to detect from mapping).
#' @param name_pri Title for left axis.
#' @param name_sec Title for right axis.
#' @param labels Label formatter for primary axis.
#' @param label_sec Label formatter for secondary axis.
#' @param ... Additional arguments for transform_dual_axis (n_breaks, invert_right).
#' 
#' @return A secondary-axis-ready ggplot object.
#' @export
ggplot_dual_axis <- function(data, mapping = aes(), 
                             primary_var, secondary_var,
                             group_col = "ticker",
                             value_col = NULL,
                             name_pri = primary_var, name_sec = secondary_var,
                             labels = waiver(),
                             label_sec = scales::label_number(accuracy = 0.01),
                             ...) {
  
  # Extract value column from mapping if not specified
  if (is.null(value_col)) {
    # Try to extract 'y' aesthetic
    if (!is.null(mapping$y)) {
      # Use rlang or simple deparse if available, typically rlang::as_label
      value_col <- rlang::as_label(mapping$y)
    } else {
      value_col <- "value" # fallback
    }
  }

  # 1. Transform Data
  df_trans <- transform_dual_axis(
    df = data,
    primary_var = primary_var,
    secondary_var = secondary_var,
    param_col = group_col,
    value_col = value_col,
    ... # e.g. n_breaks, invert_right
  )
  
  # 2. Init Plot with transformed data
  p <- ggplot(data = df_trans, mapping = mapping)
  
  # 3. Add Scale (configured using the transformed data attributes)
  p <- p + scale_y_dual_axis(df_trans, name_pri = name_pri, name_sec = name_sec, 
                             labels = labels, label_sec = label_sec)
  
  return(p)
}
