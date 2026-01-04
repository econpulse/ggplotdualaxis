# library(ggplotdualaxis) # if installed
source("R/dual_axis_helpers.R") # for local dev usage
library(ggplot2)
library(scales)

# Dummy Data with percentages and dollars
df <- data.frame(
  ticker = rep(c("Rate", "Amount"), each=20),
  date = rep(seq(1, 20), 2),
  value = c(seq(0.01, 0.05, length.out=20), seq(1000, 5000, length.out=20))
)

# Test: Primary as Percent (0.01 -> 1%), Secondary as Dollar ($1,000)
p <- ggplot_dual_axis(
  data = df,
  mapping = aes(x = date, y = value, color = ticker),
  primary_var = "Rate",
  secondary_var = "Amount",
  labels = scales::label_percent(accuracy = 0.1),
  label_sec = scales::label_dollar()
) + 
geom_line() +
ggtitle("Test: Rate (%) vs Amount ($)")

ggsave("test_labels.png", p, width=6, height=4)
cat("Label test completed. Check test_labels.png for % and $ symbols.\n")
