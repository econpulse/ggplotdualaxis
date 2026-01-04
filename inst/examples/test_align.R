# library(ggplotdualaxis)
source("R/dual_axis_helpers.R")
library(ggplot2)
library(dplyr)
library(patchwork)

# Data: Primary is high (1000s), Secondary is small (0-100)
# Designed so that 'center' alignment might accidentally align 1000 with 0
df <- data.frame(
  date = seq(1, 20),
  val_pri = seq(1000, 1020, length.out=20), # Range 1000-1020. Breaks likely 1000, 1005...
  val_sec = seq(50, 70, length.out=20)      # Range 50-70.
) %>% 
  tidyr::pivot_longer(c(val_pri, val_sec), names_to="ticker", values_to="value")

# Function to plot with alignment
plot_align <- function(align) {
  ggplot_dual_axis(
    df, 
    aes(x=date, y=value, color=ticker), 
    "val_pri", "val_sec",
    axis_align = align,
    n_breaks = 5
  ) + 
  geom_line() + 
  ggtitle(paste("Align:", align)) + 
  theme_bw()
}

p1 <- plot_align("center")
p2 <- plot_align("min")
p3 <- plot_align("max")
p4 <- plot_align("zero") # Will look weird for this data as 0 is far away

p_all <- (p1 + p2) / (p3 + p4)

ggsave("test_align.png", p_all, width=10, height=8)
