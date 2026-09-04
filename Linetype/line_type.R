library(ggplot2)

# 1. 构造3种比较适合绘图的线条

line_plot <- ggplot() +
  geom_hline(yintercept = 5, linetype = "twodash") +
  geom_hline(yintercept = 3, linetype = "44") +
  geom_hline(yintercept = 1, linetype = 'longdash')

ggsave(plot = line_plot, filename = 'Linetype/output/line_example.svg')
