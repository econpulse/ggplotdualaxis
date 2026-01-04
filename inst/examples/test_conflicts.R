source("dual_axis_helpers.R")
library(ggplot2)
library(patchwork) # to layout plots if installed, else printable sequentially

# Dummy Data
df <- data.frame(
  ticker = rep(c("A", "B"), each=20),
  date = rep(seq(1, 20), 2),
  value = c(seq(0, 10, length.out=20), seq(100, 200, length.out=20))
)

df_trans <- transform_dual_axis(df, "A", "B", n_breaks=5)

# Base Plot
p_base <- ggplot(df_trans, aes(x=date, y=value, color=ticker)) + geom_line()

# 1. Control (Dual Axis)
p1 <- p_base + scale_y_dual_axis(df_trans, name_pri="Primary", name_sec="Secondary") + ggtitle("Control")

# 2. Conflict: Adding scale_y_continuous AFTER
# Expectation: Right axis disappears, breaks reset to default or manual
p2 <- p_base + 
  scale_y_dual_axis(df_trans, name_pri="Primary", name_sec="Secondary") + 
  scale_y_continuous(name = "Overwritten") + 
  ggtitle("Conflict: scale_y_continuous added after")

# 3. Zoom: coord_cartesian
# Expectation: Alignment holds, but ticks are fixed (might disappear if zoomed too tight)
p3 <- p_base + 
  scale_y_dual_axis(df_trans, name_pri="Primary", name_sec="Secondary") + 
  coord_cartesian(ylim = c(4, 6)) + 
  ggtitle("Zoom: coord_cartesian(ylim=c(4,6))")

# Save to check visually
ggsave("test_conflict_1.png", p1, width=6, height=4)
ggsave("test_conflict_2.png", p2, width=6, height=4)
ggsave("test_conflict_3.png", p3, width=6, height=4)

cat("Plots saved. Check artifacts.\n")
