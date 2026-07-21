# 构建测试数据集 (原始数据为Toothgrowth)

# df <- ToothGrowth

# write.csv(df,file = "tooth.csv")

# 正式分析 ==================

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

summary(df.aov)

# emmeans整体比较 ==================

df.emm_all <- emmeans(df.aov, ~ supp * dose.factor)

df.emm_all

# emmeans分组比较 1 ==================

df.emm_group1 <- emmeans(df.aov, ~ supp | dose.factor) # 注意符号差异
#同一个dose.factor，比较不同supp

df.emm_group1

## 显著性字母 ======================

cld_group1 <- cld(df.emm_group1, Letters = letters)

cld_group1

# emmeans分组比较 2 ==================

df.emm_group2 <- emmeans(df.aov, ~ dose.factor | supp) # 注意符号差异
#同一个supp，比较不同dose

df.emm_group2

## 显著性字母 ======================

cld_group2 <- cld(df.emm_group2, Letters = letters)

cld_group2
