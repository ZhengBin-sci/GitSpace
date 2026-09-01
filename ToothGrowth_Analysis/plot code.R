# 研究主要会关心分组研究的结果，cld输出功能很强大，可以直接用于绘图

# 以同一个dose.factor，比较不同supp 为例

library(tidyverse)
library(emmeans)
library(multcomp)
library(multcompView)

# 导入数据 ==================

df <- read.csv("tooth.csv")

str(df) #检查数据结构

df.analysis <- df |> mutate(dose.factor = as.factor(dose))

# 方差分析 ==================

df.aov <- aov(len ~ supp * dose.factor, data = df.analysis)

# emmeans分组比较 2 ==================

df.emm_group <- emmeans(df.aov, ~ dose.factor | supp) # 注意符号差异

# 显著性字母 ======================

cld_group <- cld(df.emm_group, Letters = letters)

# 绘图 ======================

# 将 cld 对象转换为数据框
cld_df <- as.data.frame(cld_group)

ggplot(cld_df, aes(x = supp, y = emmean, fill = dose.factor)) +
  # 绘制柱子
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +

  # 绘制 95% 置信区间误差线
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +

  # 【核心】把 cld 生成的显著性字母标在柱子上方
  geom_text(
    aes(y = upper.CL, label = .group),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4
  ) +

  # 图表美化
  labs(
    title = "不同喂养方式下的剂量效应",
    subtitle = "分组比较与显著性字母 (CLD)",
    x = "剂量 (mg)",
    y = "牙齿长度估计边际均值",
    fill = "喂养方式"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))
