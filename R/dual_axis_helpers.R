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
#' @param axis_align Alignment constraint: "center" (default), "min", "max", or "zero".
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
    invert_right = FALSE,
    axis_align = c("center", "min", "max", "zero")
) {
  
  axis_align <- match.arg(axis_align)

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
  # span_pri <- diff(range(breaks_pri)) # Unused
  step_pri <- if(n > 1) diff(breaks_pri)[1] else 1
  
  # Secondary Axis Range
  span_sec_data <- diff(range(vals_sec, na.rm = TRUE))
  if (span_sec_data == 0) span_sec_data <- 1 # Prevent division by zero
  
  # Helper for nice steps
  nice_step <- function(x) {
    if (!is.finite(x) || x <= 0) return(1)
    pow <- 10^floor(log10(x))
    m <- x / pow
    base <- if (m <= 1) 1 else if (m <= 2) 2 else if (m <= 5) 5 else 10
    base * pow
  }
  
  # Calculate Target Secondary Step
  # We want roughly 'n' ticks covering the data range
  min_step_sec <- span_sec_data / (n - 1)
  step_sec <- nice_step(min_step_sec)
  
  # Determine Pivot Points (Anchor) based on alignment
  # pivot_pri and pivot_sec will be forced to align
  
  if (axis_align == "center") {
    pivot_pri <- mean(breaks_pri)
    raw_pivot_sec <- if (!is.null(center_right_override)) center_right_override else median(vals_sec, na.rm = TRUE)
  } else if (axis_align == "min") {
    pivot_pri <- min(breaks_pri)
    raw_pivot_sec <- min(vals_sec, na.rm = TRUE)
  } else if (axis_align == "max") {
    pivot_pri <- max(breaks_pri)
    raw_pivot_sec <- max(vals_sec, na.rm = TRUE)
  } else if (axis_align == "zero") {
    pivot_pri <- 0
    raw_pivot_sec <- 0
  }
  
  # Snap Pivot Secondary to Grid
  # We want pivot_sec to be a multiple of step_sec.
  pivot_sec <- round(raw_pivot_sec / step_sec) * step_sec
  
  # Calculate Scaling Factor based on Step Ratio
  # We want step_sec (secondary) to map to step_pri (primary)
  scaling_factor <- step_pri / step_sec
  
  # --- Zoom Optimization (Optional) ---
  # If the heuristic scaling produces wildly bad fit (e.g. data is much larger/smaller than grid),
  # standard dual axis logic sometimes iterates. 
  # However, with strict alignment (Pivot + Step), we are strictly constrained.
  # We stick to the simple scaling derived from step matching.
  
  # --- 3. Define Transformation Functions ---
  # y_plot - pivot_pri = scaling * (y_raw - pivot_sec)
  # y_plot = pivot_pri + scaling * (y_raw - pivot_sec)
  
  if (invert_right) {
    # If inverted, step_pri corresponds to -step_sec? 
    # Or we flip the transformation relative to pivot.
    # pivot_pri aligns with pivot_sec.
    # increasing y_sec should decrease y_plot.
    trans_forward <- function(y) pivot_pri - scaling_factor * (y - pivot_sec)
    trans_reverse <- function(y) pivot_sec - (y - pivot_pri) / scaling_factor 
  } else {
    trans_forward <- function(y) pivot_pri + scaling_factor * (y - pivot_sec)
    trans_reverse <- function(y) pivot_sec + (y - pivot_pri) / scaling_factor
  }
  
  # --- 4. Generate Secondary Labels matching Primary Breaks ---
  # For each break on primary axis, what is the value on secondary?
  labels_final <- trans_reverse(breaks_pri)
  
  # --- 5. Apply Transformation ---
  df_out <- df_sub
  is_sec <- df_out[[param_col]] == secondary_var
  df_out[[value_col]][is_sec] <- trans_forward(df_out[[value_col]][is_sec])
  
  # --- 6. Return with Attributes ---
  attr(df_out, "dual_axis") <- list(
    primary_breaks   = breaks_pri,
    secondary_labels = labels_final,
    sec_name         = secondary_var,
    trans_forward    = trans_forward,
    trans_reverse    = trans_reverse
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
#' @param axis_align Alignment constraint: "center" (default), "min", "max", or "zero".
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
                             axis_align = c("center", "min", "max", "zero"),
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
    axis_align = axis_align,
    ... # e.g. n_breaks, invert_right
  )
  
  # 2. Init Plot with transformed data
  p <- ggplot(data = df_trans, mapping = mapping)
  
  # 3. Add Scale (configured using the transformed data attributes)
  p <- p + scale_y_dual_axis(df_trans, name_pri = name_pri, name_sec = name_sec, 
                             labels = labels, label_sec = label_sec)
  
  return(p)
}
