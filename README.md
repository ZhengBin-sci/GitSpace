# ToothGrowth 数据分析与显著性标记工作流

## 项目简介
这是我建立的第一个 GitHub 公共仓库。本项目以经典的 `ToothGrowth` 数据集为例，记录了一套基于 `emmeans` 和 `multcomp` 的分组比较与可视化流程。

## 为什么选择这个工作流？
在早期的数据分析中，我习惯使用 `agricolae` 包来处理方差分析后的多重比较。但在处理多组交叉设计（如本例中的 3x2 设计）时，输出的显著性字母往往比较混乱，难以直接用于绘图。为了解决这个问题，我不得不转向 `ggpubr` 手动添加星号。

近期尝试了 **`emmeans` (Estimated Marginal Means)** 结合 **`multcomp`** 的工作流，发现它能非常流畅地实现分组内的简单效应分析，并直接输出整洁的显著性字母（CLD）。这套方法逻辑清晰，非常适合实际研究需求。

## 拓展思考
除了基础的方差分析，`emmeans` 在处理混合效应模型（如 `lme4::lmer`）时也表现出色，这在生态学等涉及随机效应的研究中非常实用。

在使用过程中我也产生了一个疑问：**`emmeans` 输出的置信区间，是否与基础模型中 `confint(..., method="wald")` 的结果一致？**
- 目前看来 `emmeans` 提供了更灵活的自由度调整选项（如 Kenward-Roger 或 Satterthwaite 近似），这在小样本或复杂设计中可能比标准的 Wald 区间更准确。
- 这一点值得后续通过模拟数据进一步验证。

## 运行环境
- **R Version**: 4.5.3
- **主要依赖包**:
  ```r
  library(tidyverse)    # 数据处理与绘图
  library(emmeans)      # 边际均值估算
  library(multcomp)     # 多重比较与 CLD 生成
  library(multcompView) # 辅助可视化
  ```

## 文件说明
- `analysis code.R`: 包含数据清洗、方差分析及 emmeans 计算的核心代码。
- `plot code.R`: 基于 cld 结果绘制带显著性字母柱状图的脚本。
- `tooth.csv`: 示例数据文件。

## 致谢
本项目的分析流依托于以下优秀开源 R 包的支持，特此向开发者社区致以诚挚谢意：
- **emmeans**: Russell V. Lenth 及其贡献者团队。该包为复杂模型的边际均值估算提供了极其稳健的实现。
- **multcomp & multcompView**: Torsten Hothorn, Frank Bretz 等开发者。正是这些工具让多重比较结果的可视化变得如此直观。
- **tidyverse**: Hadley Wickham 及 RStudio 团队。感谢你们构建了如此优雅的数据科学生态系统。
- **R Core Team**: 感谢 R 语言核心团队维护了这个强大的统计计算环境。
