# Load pacakge ======================
library(tidyverse)

library(rio)

library(sf)

library(rnaturalearth)

library(patchwork)

library(data.table)

# Figure 1 ======================

## Figure 1a ======================

# Top = ((10 + (-5))/2 = 2.5)

T_low <- -5
T_upp <- 10
T_op <- 2.5

# 【关键修改】：使用 by = 0.1 (或者更小)，确保包含顶点 2.5
fig1a_data <- tibble(
  tem = seq(-10, 20, by = 0.1),

  # 你的原始公式逻辑是完全正确的，这就是标准的三角形模型公式
  chill = case_when(
    # 上升段：(当前温度 - 下限) / (最适温度 - 下限)
    tem >= T_low & tem < T_op ~ (tem - T_low) / (T_op - T_low),

    # 下降段：(上限 - 当前温度) / (上限 - 最适温度)
    tem >= T_op & tem <= T_upp ~ (T_upp - tem) / (T_upp - T_op),

    # 无效区间
    TRUE ~ 0
  )
)

fig1a_p <- ggplot(fig1a_data, aes(x = tem, y = chill)) +
  geom_line(color = "#ff0000", linewidth = 1) +
  geom_vline(xintercept = T_upp, linewidth = 1.25) + # 标出顶点位置
  scale_x_continuous(breaks = seq(-10, 20, 5)) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, by = 0.5)) +
  labs(
    x = "Temperature (℃)",
    y = "Chill accumulation rate (CU·h-1)"
  )


## Figure 1b ======================

library(ggplot2)

# 1. 准备数据
# 定义红线的起点和终点坐标
fig1b_data <- data.frame(
  x = c(1, 9),
  y = c(8, 4)
)

# 2. 绘图
fig1b_p <- ggplot() +
  # 添加红线
  geom_line(
    data = fig1b_data,
    aes(x = x, y = y),
    color = "red",
    linewidth = 1
  ) +

  # 添加文本标注

  # 左下方文字（使用 atop 换行）
  annotate(
    "text",
    x = 1.5,
    y = 4,
    label = "atop(Low~T[upp], selected~against)",
    parse = TRUE,
    size = 8,
    hjust = 0
  ) +

  # 右上方文字（使用 atop 换行）
  annotate(
    "text",
    x = 4.5,
    y = 8,
    label = "atop(High~T[upp], selected~against)",
    parse = TRUE,
    size = 8,
    hjust = 0
  ) +

  # 设置坐标轴标签 (使用 expression 渲染下标)
  labs(x = "Latitude (°N)", y = 'Tupp (℃)') +

  # 设定坐标轴范围，留出边距
  xlim(0, 10) +
  ylim(0, 10)

# 显示图形

## Figure 1c ======================

# 单图层直接读取
fig1c_quhua <- read_sf(
  "quhua_simple/Climate_quhua_simple.gpkg"
)

# 将 Qu1 转为因子并指定顺序（和 Python 端一样的逻辑）
fig1c_seqs <- c(
  "高原气候区",
  "北温带",
  "中温带",
  "南温带",
  "北亚热带",
  "中亚热带",
  "南亚热带",
  "热带"
)

fig1c_color <- c(
  "高原气候区" = '#620606',
  "北温带" = '#2680B8',
  "中温带" = '#64BCD0',
  "南温带" = '#9BCDC1',
  "北亚热带" = '#E8A289',
  "中亚热带" = '#DE614F',
  "南亚热带" = '#D62626',
  "热带" = '#B2605C'
)

fig1c_label <- c(
  "高原气候区" = 'Arid',
  "北温带" = 'North temperate',
  "中温带" = 'Mid-temperate',
  "南温带" = 'South temperate',
  "北亚热带" = 'North subtropical',
  "中亚热带" = 'Mid-subtropical',
  "南亚热带" = 'South subtropical',
  "热带" = 'Tropical'
)

world <- ne_countries(scale = "medium", returnclass = "sf")

# 2. ggplot2 绘图
fig1c_map <- ggplot() +
  # 第一层：灰色世界陆地背景（fill填色，color=NA去掉边界线）
  geom_sf(data = world, fill = "grey90", color = NA) +
  # 第二层：你的气候带数据
  geom_sf(
    data = fig1c_quhua,
    aes(fill = 气候带),
    linewidth = 0.3,
    color = NA
  ) +
  # 手动指定颜色和图例顺序
  scale_fill_manual(
    values = fig1c_color,
    breaks = fig1c_seqs,
    labels = fig1c_label
  )


grid_info <- rio::import(
  'figure 1-18 points.xlsx'
)

# 1. 先按 Excel 原始的经纬度（WGS84）创建空间点
grid_pts1 <- st_as_sf(
  grid_info,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# 2. 再转换到 fig1c_quhua.data 的 Albers 投影坐标系
grid_pts <- st_transform(grid_pts1, st_crs(fig1c_quhua))


fig1c_p <- fig1c_map +
  geom_sf(data = grid_pts) +
  scale_y_continuous(limits = c(15, 55)) +
  scale_x_continuous(limits = c(110, 140), breaks = seq(110, 130, by = 10))


## Figure 1d ======================

# 1. 准备数据：定义红线的起点和终点
fig1d_data <- data.frame(
  x = c(2, 8), # Tupp 的范围
  y = c(3, 7) # RAFUmax 的范围
)

# 2. 绘图
fig1d_p <- ggplot() +
  # 添加红线
  geom_line(
    data = fig1d_data,
    aes(x = x, y = y),
    color = "red",
    linewidth = 1
  ) +

  # 添加文本标注

  # 顶部标题文字
  annotate(
    "text",
    x = 5,
    y = 9,
    label = "Prediction for high latitudes",
    size = 8,
    hjust = 0.5
  ) +

  # 设置坐标轴标签 (使用 expression 渲染下标)
  labs(
    x = 'Tupp (℃)',
    y = 'RAFUmax (GDD)'
  ) +

  # 设定坐标轴范围
  xlim(0, 10) +
  ylim(0, 10)

# 显示图形

## Figure 1e ======================

# 1. 准备数据：定义红线的起点和终点
fig1e_data <- data.frame(
  x = c(2, 8), # Tupp 的范围
  y = c(7, 3) # RAFUmax 的范围
)

# 2. 绘图
fig1e_p <- ggplot() +
  # 添加红线
  geom_line(
    data = fig1e_data,
    aes(x = x, y = y),
    color = "red",
    linewidth = 1
  ) +

  # 添加文本标注

  # 顶部标题文字
  annotate(
    "text",
    x = 5,
    y = 9,
    label = "Prediction for low latitudes",
    size = 8,
    hjust = 0.5
  ) +

  # 设置坐标轴标签 (使用 expression 渲染下标)
  labs(
    x = 'Tupp (℃)',
    y = 'RAFUmax (GDD)'
  ) +

  # 设定坐标轴范围
  xlim(0, 10) +
  ylim(0, 10)

# 显示图形

# Figure 1 combine ======================

fig1_layout <- "
  AB
  CD
  CE
"

fig1_p <- fig1a_p +
  fig1b_p +
  fig1c_p +
  fig1d_p +
  fig1e_p +
  plot_layout(
    design = fig1_layout,
    width = c(2, 1)
  ) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &
  theme_classic(base_size = 28) &
  theme(
    axis.text = element_text(color = '#000000'),
    plot.tag.position = c(0.07, 0.95),
    plot.tag.location = 'panel',
    plot.tag = element_text(face = 'bold')
  )

fig1_p[[2]] <- fig1_p[[2]] +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


# 后续子图主题修改逻辑不变...
fig1_p[[3]] <- fig1_p[[3]] +
  theme_bw(base_size = 36) +
  theme(
    legend.title = element_blank(),
    legend.key.spacing.y = unit(5, 'mm'),
    legend.position = c(0.7, 0.25),
    legend.text = element_text(size = 20)
  )

fig1_p[[4]] <- fig1_p[[4]] +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank()
  )

fig1_p[[5]] <- fig1_p[[5]] +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# ggsave(
#   plot = fig1_p,
#   width = 21,
#   height = 18,
#   filename = 'report/fig1.svg'
# )

# Figure 1 trend ==============

fig1_trend.files <- c(
  'figure 1f and 1g.xlsx',
  'figure 1h and 1i.xlsx',
  'figure 1j and 1k.xlsx',
  'figure 1l and 1m.xlsx'
)

fig1_trend.import <- function(file) {
  OriDat <- rio::import(file, setclass = 'data.table')

  MeltDat <- melt(
    OriDat,
    id.vars = c('Zone', 'Tupp'),
    variable.factor = FALSE,
    variable.name = 'Points',
    value.name = 'RAFU'
  )

  return(MeltDat)
}

Grid_color <- c(
  "Grid 1" = '#144f5d',
  "Grid 2" = '#1f7284',
  "Grid 3" = '#2d92a9',
  "Grid 4" = '#43aec7',
  "Grid 5" = '#6ebdcf',
  "Grid 6" = '#8bbfc9',
  "Grid 7" = '#a8d0d6',
  "Grid 8" = '#2a5e52',
  "Grid 9" = '#3c7769',
  "Grid 10" = '#4f9582',
  "Grid 11" = '#6ab39e',
  "Grid 12" = '#8ed0bb',
  "Grid 13" = '#993f1e',
  "Grid 14" = '#c45a33',
  "Grid 15" = '#de624f',
  "Grid 16" = '#e88a72',
  "Grid 17" = '#f0ad99',
  "Grid 18" = '#f7d0c3'
)

Zone_color <- c(
  "Mid-temperate" = '#64bcd0',
  "South temperate" = '#9BCDC1',
  "North subtropical" = '#E8A289',
  "Mid-subtropical" = '#DE614F'
)

fig1_trend.data <- purrr::map_dfr(
  .x = fig1_trend.files,
  .f = fig1_trend.import
)

fig1_trend.all <- function(plot_Zone) {
  fig1_pdata <- fig1_trend.data |>
    filter(Zone %in% plot_Zone, Points != 'Average')

  fig1.grid_p <- ggplot(fig1_pdata, aes(x = Tupp, y = RAFU)) +
    geom_point(aes(color = Points)) +
    geom_line(aes(color = Points)) +
    scale_color_manual(values = Grid_color) +
    scale_y_continuous(
      limits = c(0, 165),
      name = 'RAFUmax (GDD)',
      breaks = seq(0, 150, by = 75)
    ) +
    ggtitle(plot_Zone)

  return(fig1.grid_p)
}

fig1_trend.ave <- function(plot_Zone) {
  fig1.ave_pdata <- fig1_trend.data |>
    filter(Zone %in% plot_Zone, Points == 'Average')

  fig1.ave_p <- ggplot(fig1.ave_pdata, aes(x = Tupp, y = RAFU)) +
    geom_point(aes(color = Zone)) +
    geom_line(aes(color = Zone)) +
    scale_color_manual(values = Zone_color) +
    scale_y_continuous(
      limits = c(0, 165),
      name = '',
      breaks = seq(0, 150, by = 75)
    ) +
    ggtitle(plot_Zone)

  return(fig1.ave_p)
}

fig1_trend.p.list1 <- map(.x = names(Zone_color), .f = fig1_trend.all)

fig1_trend.p.list2 <- map(.x = names(Zone_color), .f = fig1_trend.ave)

fig1_trend.p.list1[[4]] <- fig1_trend.p.list1[[4]] +
  scale_y_continuous(
    limits = c(0, 405),
    breaks = seq(0, 400, by = 200),
    name = 'RAFUmax (GDD)'
  )

fig1_trend.p.list2[[4]] <- fig1_trend.p.list2[[4]] +
  scale_y_continuous(
    limits = c(0, 405),
    name = '',
    breaks = seq(0, 400, by = 200)
  )

Fig1.trend_p <- (wrap_plots(
  fig1_trend.p.list1,
  ncol = 1,
  axis_titles = 'collect_x',
  guides = 'collect'
) |
  wrap_plots(
    fig1_trend.p.list2,
    ncol = 1,
    axis_titles = 'collect',
    guides = 'collect'
  )) &
  theme_classic(base_size = 28) &
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0.5)
  )

# ggsave(
#   plot = Fig1.trend_p,
#   width = 15.6,
#   height = 19.6,
#   filename = 'report/fig1 trend.svg'
# )

# Figure 2 ==============

fig2_data <- rio::import(
  'figure 2.xlsx'
)

colnames(fig2_data) <- c('Grid', 'Latitude', 'Mini_RAFUmax', 'Tupp')

grid_zone.dict <- grid_info |> select(Points, Zone) |> deframe()

fig2_pdata <- fig2_data |>
  mutate(Tupp_p = Tupp * 10) |>
  mutate(Grid = paste('Grid', Grid), Zone = grid_zone.dict[Grid])

# x threshold

fig2_bound <- rio::import(
  'quhua_simple/Climate_quhua_bounds_110E.csv'
)

fig2_bound.pdata <- fig2_bound |>
  filter(气候带 %in% c('中温带', '南温带', '北亚热带', '中亚热带'))

setDT(fig2_bound.pdata)

fig2_p <- ggplot(fig2_pdata) +
  geom_line(aes(Latitude, Mini_RAFUmax), color = '#4F80BD') +
  geom_point(aes(Latitude, Mini_RAFUmax), color = '#4F80BD') +
  geom_line(aes(Latitude, Tupp_p), color = '#C0504D') +
  geom_point(aes(Latitude, Tupp_p), color = '#C0504D') +
  scale_y_continuous(
    limits = c(0, 200),
    sec.axis = sec_axis(transform = ~ . / 10, name = 'Tupp* (℃)')
  ) +
  scale_x_continuous(limits = c(20, 50)) +
  ggthemes::theme_few(base_size = 28) +
  theme(axis.title.y.right = element_text(angle = 90))

# ggsave(
#   plot = fig2_p,
#   width = 10.139,
#   height = 6.806,
#   filename = 'report/fig2.svg'
# )

# Supp Figure 1 ===============

supp.fig1_low <- tibble(
  tem = seq(-10, 25, by = 0.1),
  chill = case_when(tem < (-5) ~ 0, tem > 2 ~ 0, between(tem, -5, 2) ~ 1),
  Type = 'Low Tupp'
)

supp.fig1_high <- tibble(
  tem = seq(-10, 25, by = 0.1),
  chill = case_when(tem < (-6) ~ 0, tem > 20 ~ 0, between(tem, -6, 20) ~ 1),
  Type = 'High Tupp'
)

supp.fig1_data <- rbind(supp.fig1_low, supp.fig1_high)

suppfig1_p <- ggplot(supp.fig1_data, aes(tem, chill)) +
  geom_line(aes(color = Type, linetype = Type)) +
  scale_linetype_manual(values = c('Low Tupp' = 1, 'High Tupp' = 8)) +
  geom_hline(yintercept = 0.5) +
  scale_color_manual(
    values = c('Low Tupp' = '#4F80BD', 'High Tupp' = '#C0504D')
  ) +
  labs(x = 'Temperature (℃)', y = 'Chilling accumulation rate (CU·h-1)') +
  theme_classic(base_size = 28) +
  theme(
    axis.text = element_text(color = 'black'),
    legend.position = 'top',
    legend.title = element_blank()
  )

# ggsave(
#   plot = suppfig1_p,
#   width = 9.6,
#   height = 7.2,
#   filename = 'report/suppfig1.svg'
# )

# Supp Figure 2 ===============

supp.fig2_CHcrit <- rio::import(
  'supplementary figure S2a andS2c.xlsx'
)

supp.fig2_tEDR <- rio::import(
  'supplementary figure S2b andS2d.xlsx'
)

supp.fig2a <- ggplot(data = supp.fig2_CHcrit, aes(x = Tupp, y = CHcrit)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 1000), name = 'CHcrit (CU)')

supp.fig2b <- ggplot(data = supp.fig2_tEDR, aes(x = Tupp, y = CHcrit)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 1000), name = 'CHcrit (CU)')

supp.fig2c <- ggplot(data = supp.fig2_CHcrit, aes(x = Tupp)) +
  geom_line(aes(y = tearly), color = '#156082') +
  geom_point(aes(y = tearly), color = '#156082') +
  geom_line(aes(y = tlate), color = '#E97132') +
  geom_point(aes(y = tlate), color = '#E97132') +
  scale_y_continuous(
    limits = c(0, 180),
    breaks = seq(0, 180, by = 30),
    name = 'tEDR'
  )

supp.fig2d <- ggplot(data = supp.fig2_tEDR, aes(x = Tupp)) +
  geom_line(aes(y = tearly), color = '#156082') +
  geom_point(aes(y = tearly), color = '#156082') +
  geom_line(aes(y = tlate), color = '#E97132') +
  geom_point(aes(y = tlate), color = '#E97132') +
  scale_y_continuous(
    limits = c(0, 180),
    breaks = seq(0, 180, by = 30),
    name = 'tEDR'
  )

supp.fig2_p <- supp.fig2a +
  supp.fig2b +
  supp.fig2c +
  supp.fig2d +
  plot_layout(tag_level = 'keep', axis_titles = 'collect') +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &
  scale_x_continuous(limits = c(0, 20), name = 'Tupp (℃)') &
  theme_classic(base_size = 28) &
  theme(
    axis.text = element_text(color = '#000000'),
    plot.tag.position = c(0.07, 0.95),
    plot.tag.location = 'panel',
    plot.tag = element_text(face = 'bold')
  )

# ggsave(
#   plot = supp.fig2_p,
#   width = 13.6,
#   height = 9.6,
#   filename = 'report/suppfig2.svg'
# )

# Supp Figure 3 ===============

supp.fig3_data <- rio::import(
  "supplementary figure S3.csv",
  setclass = 'data.table'
)

supp.fig3_table <- melt(
  supp.fig3_data,
  id.vars = c('Grid', 'Latitude_N'),
  variable.name = 'Type',
  value.name = 'value'
)

supp.fig3_table[,
  Type := factor(
    Type,
    labels = c(
      "Tupp_star_maximum" = "Maximum",
      "Tupp_star_p95" = "95th percent",
      "Tupp_star_p90" = "90th percent",
      "Tupp_star_mean" = "Mean"
    )
  ),
]

supp.fig3_p <- ggplot(supp.fig3_table, aes(x = Latitude_N, value)) +
  geom_point(aes(color = Type, shape = Type, size = Type)) +
  geom_line(aes(color = Type)) +
  ylab('Tupp* (℃)') +
  scale_x_continuous(
    limits = c(23, 50),
    name = 'Latitude (°N)',
    breaks = seq(25, 50, by = 5)
  ) +
  theme_classic(base_size = 28) +
  scale_color_manual(
    values = c(
      'Maximum' = '#C0504D',
      "95th percent" = '#6E96C8',
      "90th percent" = '#8BA264',
      "Mean" = "#7759A4"
    )
  ) +
  scale_size_manual(
    values = c(
      'Maximum' = 4.5,
      "95th percent" = 6,
      "90th percent" = 7.5,
      "Mean" = 6.8
    )
  ) +
  scale_shape_manual(
    values = c(
      'Maximum' = 16,
      "95th percent" = 1,
      "90th percent" = 2,
      "Mean" = 0
    )
  ) +
  theme(
    axis.title.y.right = element_text(angle = 90),
    legend.position = c(0.7, 0.65),
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.background = element_blank(),
    legend.key.spacing.y = unit(6, 'mm')
  )

# ggsave(
#   plot = supp.fig3_p,
#   width = 13.6,
#   height = 8.4,
#   filename = 'report/supp fig3.svg'
# )

# Supp Figure 4 ===============

supp.fig4_data <- rio::import(
  "supplementary figure S4.xlsx",
  setclass = 'data.table'
)

supp.fig4a <- supp.fig4_data |>
  select(
    Grid,
    Latitude,
    min = 'occu min',
    mean = 'occu mean',
    max = 'occu max'
  ) |>
  pivot_longer(
    cols = c(min, mean, max),
    names_to = "type",
    values_to = "value"
  )

supp.fig4b <- supp.fig4_data |>
  select(
    Grid,
    Latitude,
    min = 'seve min',
    mean = 'seve mean',
    max = 'seve max'
  ) |>
  pivot_longer(
    cols = c(min, mean, max),
    names_to = "type",
    values_to = "value"
  )

supp.fig4a_p <- ggplot(data = supp.fig4a, aes(x = Latitude, y = value)) +
  geom_point(aes(color = type)) +
  geom_line(aes(color = type)) +
  scale_y_continuous(name = 'Number of frost days', limits = c(0, 250))

supp.fig4b_p <- ggplot(data = supp.fig4b, aes(x = Latitude, y = value)) +
  geom_point(aes(color = type)) +
  geom_line(aes(color = type)) +
  scale_y_continuous(name = 'Lowest temperature (℃)', limits = c(-50, 20))

supp.fig4_p <- supp.fig4a_p +
  supp.fig4b_p +
  plot_layout(
    tag_level = 'keep',
    axis_titles = 'collect_x',
    guides = 'collect'
  ) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &
  scale_x_continuous(limits = c(20, 50), name = 'Latitude (°N)') &
  scale_color_manual(
    values = c(
      'min' = '#0571B0', # 深蓝 - 代表极端低温
      'mean' = '#FDB863', # 橙黄 - 代表平均水平
      'max' = '#CA0020'
    ), # 深红 - 代表极端高温
    name = ''
  ) &
  theme_classic(base_size = 28) &
  theme(
    axis.text = element_text(color = '#000000'),
    plot.tag.position = c(0.07, 0.95),
    plot.tag.location = 'panel',
    plot.tag = element_text(face = 'bold'),
    legend.position = 'top'
  )

ggsave(
  plot = supp.fig4_p,
  width = 15.6,
  height = 7.2,
  filename = 'report/suppfig4.svg'
)

# Supp Figure 5 ===============

## Supp Figure 5是一张示意图，基于pdf 导入inkscape，inkscape修改后，导出为png

# Supp Figure 6 ===============

# Top = ((10 + (-5))/2 = 2.5)

# 【关键修改】：使用 by = 0.1 (或者更小)，确保包含顶点 2.5
supp.fig6_func <- function(Tlow, Tupp, Top, chillNo) {
  chill_curve <- tibble(
    tem = seq(-10, 25, by = 0.1),

    # 你的原始公式逻辑是完全正确的，这就是标准的三角形模型公式
    chill = case_when(
      # 上升段：(当前温度 - 下限) / (最适温度 - 下限)
      tem >= Tlow & tem < Top ~ (tem - Tlow) / (Top - Tlow),

      # 下降段：(上限 - 当前温度) / (上限 - 最适温度)
      tem >= Top & tem <= Tupp ~ (Tupp - tem) / (Tupp - Top),

      # 无效区间
      TRUE ~ 0
    ),

    Type = chillNo
  )

  chill_curve_p <- ggplot(chill_curve, aes(x = tem, y = chill)) +
    geom_line(color = "#ff0000", linewidth = 1) +
    geom_vline(xintercept = Tupp, linewidth = 1.25) + # 标出顶点位置
    scale_x_continuous(
      limits = c(-10, 25),
      breaks = seq(-10, 20, by = 10)
    ) +
    scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, by = 0.5)) +
    labs(
      x = "Temperature (℃)",
      y = "Chill accumulation rate (CU·h-1)"
    )

  return(chill_curve_p)
}

supp.fig6_table <- tibble(
  Tlow = -5,
  Tupp = c(2, 10, 20),
  Top = (Tupp + Tlow) / 2,
  chillNo = c('Chill1', 'Chill2', 'Chill3')
)

supp.fig6_No <- c('Chill1', 'Chill2', 'Chill3')

supp.fig6_list <- pmap(.l = supp.fig4_table, .f = supp.fig4_func) |>
  set_names(supp.fig4_No)

supp.fig6_p <- wrap_plots(supp.fig4_list, ncol = 1, axis_titles = 'collect') +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &
  theme_classic(base_size = 28) &
  theme(
    axis.text = element_text(color = '#000000'),
    plot.tag.position = c(0.07, 0.95),
    plot.tag.location = 'panel',
    plot.tag = element_text(face = 'bold')
  )

fig1_add <- supp.fig6_list[[2]] +
  theme_classic(base_size = 28) +
  theme(axis.text = element_text(color = 'black'))

# ggsave(
#   plot = fig1_add,
#   width = 7.6,
#   height = 5.6,
#   filename = 'report/fig1 add.svg'
# )

# ggsave(
#   plot = supp.fig6_p,
#   width = 9.6,
#   height = 15.6,
#   filename = 'report/suppfig6.svg'
# )

# Supp Figure 7 ===============

## Supp Figure 7a ===============

supp.fig7a_data <- rio::import(
  'supplementary figure s7a.xlsx'
)

colnames(supp.fig7a_data) <- c('Time', 'Mini_CU', 'Max_CU')

setDT(supp.fig7a_data)

supp.fig7a_CHcrit <- supp.fig7a_data[180, Mini_CU / 2]

supp.fig7a_late <- supp.fig7a_data[Mini_CU > supp.fig7a_CHcrit, min(Time)]

supp.fig7a_early <- supp.fig7a_data[Max_CU > supp.fig7a_CHcrit, min(Time)]

supp.fig7a_p <- ggplot(supp.fig7a_data, aes(x = Time)) +
  geom_line(aes(y = Max_CU), color = '#d13933') +
  annotate(
    "text",
    x = 150,
    y = 450,
    label = "526.5 CU",
    size = 8,
    color = '#d13933'
  ) +
  geom_line(aes(y = Mini_CU), color = '#1e90ffff') +
  annotate(
    "text",
    x = 150,
    y = 165,
    label = "211.1 CU",
    size = 8,
    color = '#1e90ffff'
  ) +
  geom_hline(yintercept = supp.fig7a_CHcrit, linetype = 6) +
  annotate(
    "text",
    x = 150,
    y = 85,
    label = "CHcrit = 105.5 CU",
    size = 8,
    color = '#000000'
  ) +
  geom_vline(xintercept = c(supp.fig7a_early, supp.fig7a_late)) +
  scale_x_continuous(
    limits = c(0, 180),
    breaks = seq(0, 180, by = 60),
    name = 'Time since 1 September (days)'
  ) +
  scale_y_continuous(limits = c(0, 600), name = 'Accumulated CU') +
  theme_classic(base_size = 28) +
  theme(axis.text = element_text(color = '#000000'))


## Supp Figure 7b ===============

supp.fig7b_data <- rio::import(
  'supplementary figure S7b.xlsx'
)

supp.fig7b_p <- ggplot(supp.fig7b_data, aes(x = doy)) +
  geom_line(aes(y = tearly_CU), color = '#d13933') +
  annotate(
    "text",
    x = 150,
    y = 270,
    label = "1969-1970",
    size = 8,
    color = '#d13933'
  ) +
  geom_line(aes(y = tlate_CU), color = '#1e90ffff') +
  annotate(
    "text",
    x = 150,
    y = 165,
    label = "1959-1960",
    size = 8,
    color = '#1e90ffff'
  ) +
  geom_hline(yintercept = supp.fig7a_CHcrit, linetype = 6) +
  annotate(
    "text",
    x = 150,
    y = 85,
    label = "CHcrit = 105.5 CU",
    size = 8,
    color = '#000000'
  ) +
  geom_vline(xintercept = c(supp.fig7a_early, supp.fig7a_late)) +
  scale_x_continuous(
    limits = c(0, 180),
    breaks = seq(0, 180, by = 60),
    name = 'Time since 1 September (days)'
  ) +
  scale_y_continuous(limits = c(0, 400), name = 'Accumulated CU') +
  theme_classic(base_size = 28) +
  theme(axis.text = element_text(color = '#010101'))

# Supp Figure 7 combine ===============

supp.fig7_p <- supp.fig7a_p /
  supp.fig7b_p +
  plot_layout(axis_titles = 'collect_x') +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(
    plot.tag.location = 'panel',
    plot.tag.position = "topleft",
    plot.tag = element_text(color = '#000000', face = 'bold', size = 36)
  )

ggsave(
  plot = supp.fig7_p,
  width = 10.8,
  height = 15.6,
  filename = 'report/supp.fig7.svg'
)
