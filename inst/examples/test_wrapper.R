# library(ggplotdualaxis) # if installed
source("../../R/dual_axis_helpers.R") # for local dev usage
# library(ggplot2)

# Dummy Data
df <- data.frame(
  ticker = rep(c("A", "B"), each=20),
  date = rep(seq(1, 20), 2),
  value = c(seq(0, 10, length.out=20), seq(100, 200, length.out=20))
)

# New Wrapper Usage
p <- ggplot_dual_axis(
  data = df,
  mapping = aes(x = date, y = value, color = ticker),
  primary_var = "A",
  secondary_var = "B",
  group_col = "ticker"
) + 
geom_line() +
ggtitle("Wrapper Function Test")

ggsave("test_wrapper.png", p, width=6, height=4)
cat("Wrapper test completed successfully.\n")
