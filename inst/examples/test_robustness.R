source("dual_axis_helpers.R")

# --- Helper to check "niceness" ---
is_nice <- function(x) {
  # A number is "nice" for a plot label if it typically has few decimal places relative to its magnitude,
  # or is a multiple of 1, 2, 5, 10, 0.1, 0.2, 0.5 etc.
  # We check if it matches the "nice_step" logic roughly.
  
  abs_x <- abs(x)
  if (all(abs_x == 0)) return(TRUE)
  
  # Check if steps between labels are nice
  steps <- diff(x)
  if (length(unique(round(steps, 10))) > 1) {
    cat("  [FAIL] Steps are not uniform!\n")
    return(FALSE)
  }
  
  step <- steps[1]
  
  # Check if step is 1*10^k, 2*10^k, 5*10^k
  pow <- 10^floor(log10(step))
  m <- step / pow
  is_nice_step <- abs(m - round(m)) < 1e-9 && round(m) %in% c(1, 2, 4, 5, 10) # 4 sometimes appears in 'pretty'
  
  if (!is_nice_step) {
    cat(sprintf("  [WARN] Step %f is not strictly 1,2,5-based.\n", step))
  }
  
  return(TRUE)
}

test_scenario <- function(name, vals_pri, vals_sec, n_breaks = 5) {
  cat(paste0("\n=== Test Scenario: ", name, " ===\n"))
  
  df <- data.frame(
    ticker = c(rep("A", length(vals_pri)), rep("B", length(vals_sec))),
    value = c(vals_pri, vals_sec),
    date = Sys.Date() # dummy
  )
  
  res <- tryCatch({
    transform_dual_axis(
      df, "A", "B", n_breaks = n_breaks
    )
  }, error = function(e) {
    cat("  [ERROR] Function threw error:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(res)) return()
  
  params <- attr(res, "dual_axis")
  
  cat("  Primary Breaks:  ", paste(params$primary_breaks, collapse = ", "), "\n")
  cat("  Secondary Labels:", paste(params$secondary_labels, collapse = ", "), "\n")
  
  # Check Alignment Count
  if (length(params$primary_breaks) != length(params$secondary_labels)) {
    cat("  [FAIL] Mismatch in number of breaks/labels!\n")
  } else {
    cat("  [PASS] Counts match.\n")
  }
  
  # Check Niceness
  if (is_nice(params$secondary_labels)) {
    cat("  [PASS] Secondary labels appear uniformly stepped.\n")
  }
  
  # Check Data Coverage
  # Ensure the secondary data is actually visible (not squeezed into a tiny line or blown out)
  # Transformed secondary values should ideally span a good chunk of the primary range.
  
  # Transform one test point
  min_sec <- min(vals_sec, na.rm=T)
  max_sec <- max(vals_sec, na.rm=T)
  t_min <- params$trans_forward(min_sec)
  t_max <- params$trans_forward(max_sec)
  
  pri_range <- range(params$primary_breaks)
  cat(sprintf("  Primary Range: [%.2f, %.2f]\n", pri_range[1], pri_range[2]))
  cat(sprintf("  Transformed Sec Range: [%.2f, %.2f]\n", t_min, t_max))
  
  if (t_max < pri_range[1] || t_min > pri_range[2]) {
    cat("  [WARN] Secondary data completely outside primary break range (Visual Clipping?)\n")
  } else {
    cat("  [PASS] Data overlaps with grid.\n")
  }
}

# --- Usage Cases ---

# 1. Standard Case (Hundreds vs Thousands)
test_scenario("Standard Indices", 
              vals_pri = seq(100, 200, length.out=100) + rnorm(100), 
              vals_sec = seq(10000, 12000, length.out=100) + rnorm(100)*10)

# 2. Small Decimal Differences (FX vs Index)
test_scenario("FX Rate (Small Variance)", 
              vals_pri = seq(10000, 15000, length.out=50), 
              vals_sec = seq(1.05, 1.15, length.out=50))

# 3. Negative Numbers
test_scenario("Negative vs Positive", 
              vals_pri = seq(-10, 10, length.out=50), 
              vals_sec = seq(100, 200, length.out=50))

# 4. Large Offset
test_scenario("Large Offset", 
              vals_pri = seq(0, 10, length.out=50), 
              vals_sec = seq(100000, 100010, length.out=50))

