# 1. 加载所需包 =====================
library(data.table) # 高效数据处理
library(purrr) # 函数式编程（map_dfr）
library(tidyverse) # 数据操作与可视化（含 dplyr, tibble, readr 等）
library(readr) # 读取数据（实际未直接使用，但依赖）
library(matrixStats) # 矩阵统计（rowMins, rowMaxs, rowCumsums）
library(clinUtils)

# 2. 读取原始温度数据 =====================
tem_data <- read.table(
  'C:/Users/james/Desktop/RAFU/dat/gridtemp-9.dat',
  header = FALSE, # 文件无列名
  fill = TRUE, # 允许行长度不一致（处理某些行缺失）
  comment.char = "", # 不将任何字符视为注释
  strip.white = TRUE # 去除字段首尾空格
)
# 数据结构：每两行为一组，第一行包含日期（V1-V3）和 0-11 时的温度（V4-V15），
# 第二行包含 12-23 时的温度（V1-V12），且 V13-V15 可能缺失。

# 3. 数据清洗与重塑 =====================
library(dplyr) # 显式加载（tidyverse 已包含，但为确保）

clean_tem <- tem_data %>%
  mutate(
    # 从奇数行（第一行）提取 0-11 时温度
    H00 = V4,
    H01 = V5,
    H02 = V6,
    H03 = V7,
    H04 = V8,
    H05 = V9,
    H06 = V10,
    H07 = V11,
    H08 = V12,
    H09 = V13,
    H10 = V14,
    H11 = V15,

    # 从下一行（偶数行）提取 12-23 时温度（使用 lead 将下一行的 V1-V12 上移）
    H12 = lead(V1),
    H13 = lead(V2),
    H14 = lead(V3),
    H15 = lead(V4),
    H16 = lead(V5),
    H17 = lead(V6),
    H18 = lead(V7),
    H19 = lead(V8),
    H20 = lead(V9),
    H21 = lead(V10),
    H22 = lead(V11),
    H23 = lead(V12)
  ) %>%

  # 只保留奇数行（因为偶数行的数据已经被上移到前一行）
  filter(row_number() %% 2 != 0) %>%

  # 选择最终列：日期（年、月、日）和 24 个时刻的温度
  select(
    Year = V1,
    Month = V2,
    Day = V3,
    H00:H23
  )

# 转换为 data.table 以便后续高效操作
setDT(clean_tem)

# 生成“冷季年”字段：1-2月属于前一年（冷季从上年秋季开始）
clean_tem[, chill_year := ifelse(Month %in% 1:2, Year - 1, Year)]

# 生成标准日期（Date 类）
clean_tem[, Date := paste(Year, Month, Day) |> ymd()]

# 转换为 data.table 以便后续高效操作
setDT(clean_tem)

# 生成“冷季年”字段：1-2月属于前一年（冷季从上年秋季开始）
clean_tem[, chill_year := ifelse(Month %in% 1:2, Year - 1, Year)]

# 生成标准日期（Date 类）
clean_tem[, Date := paste(Year, Month, Day) |> ymd()]

clean_tem <- clean_tem[Month %in% c(1:2, 9:12) & chill_year %in% 1951:2022]

# 2. 按 chill_year 和日期排序（确保时间顺序）
setorder(clean_tem, chill_year, Date)

# 3. 每个冷季年取前 180 天（从9月1日开始算，对应 doy 1~180）
tem_slice.grid9 <- clean_tem[, .SD[1:180], by = .(chill_year)]

# 4. 生成 doy（年内日序，1~180）
tem_slice.grid9[, doy := 1:180, by = .(chill_year)]

tem_slice.grid9$H00

tem_slice.ori <- tem_slice[Points %in% 'Grid09']

(tem_slice.grid9[['H00']] - tem_slice.ori[['H00']]) |> sum()

(tem_slice.grid9[['H01']] - tem_slice.ori[['H01']]) |> sum()

# 生成 H00 到 H23 的列名
cols <- sprintf("H%02d", 0:23)

# 批量计算并返回一个包含24个结果的向量
results_vector <- sapply(cols, function(col) {
  sum(tem_slice.grid9[[col]] - tem_slice.ori[[col]])
})

# 查看结果
print(results_vector)
