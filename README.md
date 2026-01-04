# ggplotdualaxis

Visualizing two variables with different scales on a single plot in `ggplot2` is notoriously difficult because `ggplot2` fundamentally believes in a single coordinate system. `ggplotdualaxis` provides a rigorous "two-step" wrapper to make this standard practice robust and easy.

## Features

-   **Automatic Scaling**: Calculates the optimal linear transformation to map the secondary variable onto the primary axis.
-   **Pretty Breaks**: Ensures that text labels on the secondary axis are "nice" round numbers (multiples of 1, 2, 5, etc.) that align perfectly with the primary axis grid lines.
-   **Wrapper Function**: A single `ggplot_dual_axis()` call replaces complicated manual data manipulation.

## Installation

You can install the package directly from GitHub:
```r
devtools::install_github("econpulse/ggplotdualaxis")
```

## Usage

### The Easy Way (`ggplot_dual_axis`)

The package provides a wrapper that behaves like `ggplot()` but handles the dual-axis logic for you.

```r
library(ggplotdualaxis)
library(ggplot2)

# Sample Data
df <- data.frame(
  date = rep(seq.Date(from = Sys.Date(), by = "day", length.out = 30), 2),
  ticker = c(rep("Stock A", 30), rep("Exchange Rate", 30)),
  value = c(
    rnorm(30, mean = 100, sd = 10),    # Range ~ 80-120
    rnorm(30, mean = 1.05, sd = 0.05)  # Range ~ 0.9-1.2
  )
)

# Plot
ggplot_dual_axis(
  data = df, 
  mapping = aes(x = date, y = value, color = ticker),
  primary_var = "Stock A",
  secondary_var = "Exchange Rate"
) +
geom_line(linewidth = 1) +
theme_minimal()
```

### The Manual Way (Under the Hood)

If you need full control, you can use the underlying `transform_dual_axis` and `scale_y_dual_axis` functions separately.

```r
# 1. Transform the secondary data to match the primary range
df_trans <- transform_dual_axis(df, "Stock A", "Exchange Rate")

# 2. Plot using standard ggplot, but adding our custom scale
ggplot(df_trans, aes(x = date, y = value, color = ticker)) +
  geom_line() +
  scale_y_dual_axis(df_trans, name_pri = "Stock Price", name_sec = "FX Rate")
```

## How It Works

1.  **Grid Alignment**: The function first determines "pretty" breaks for the primary axis (using `base::pretty`).
2.  **Steps Calculation**: It calculates a step size for the secondary axis that aligns with the primary steps.
3.  **Linear Transformation**: It computes $y_{new} = m \cdot y_{old} + c$ such that the "nice" secondary values align with the grid lines of the primary axis.
4.  **Inverse Labeling**: Finally, `scale_y_dual_axis` sets up a `sec_axis` with the inverse transformation to display the correct original values as labels.
