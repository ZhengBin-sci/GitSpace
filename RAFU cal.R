# 1. 加载所需包 =====================
library(data.table) # 高效数据处理
library(purrr) # 函数式编程（map_dfr）
library(tidyverse) # 数据操作与可视化（含 dplyr, tibble, readr 等）
library(readr) # 读取数据（实际未直接使用，但依赖）
library(matrixStats) # 矩阵统计（rowMins, rowMaxs, rowCumsums）
library(clinUtils)

# 2. 读取原始温度数据 =====================
tem_data <- rio::import(
  'C:/Users/james/Desktop/RAFU/18_points_Grid_hourly_tem.csv'
)
# 数据结构：每两行为一组，第一行包含日期（V1-V3）和 0-11 时的温度（V4-V15），
# 第二行包含 12-23 时的温度（V1-V12），且 V13-V15 可能缺失。

# 3. 数据清洗与重塑 =====================
library(dplyr) # 显式加载（tidyverse 已包含，但为确保）

# 转换为 data.table 以便后续高效操作
setDT(tem_data)

# 生成“冷季年”字段：1-2月属于前一年（冷季从上年秋季开始）
tem_data[, chill_year := ifelse(Month %in% 1:2, Year - 1, Year)]

# 生成标准日期（Date 类）
tem_data[, Date := paste(Year, Month, Day) |> ymd()]

clean_tem <- tem_data[Month %in% c(1:2, 9:12) & chill_year %in% 1951:2022]

# 2. 按 chill_year 和日期排序（确保时间顺序）
setorder(clean_tem, chill_year, Date)

# 3. 每个冷季年取前 180 天（从9月1日开始算，对应 doy 1~180）
tem_slice <- clean_tem[, .SD[1:180], by = .(Points, chill_year)]

# 4. 生成 doy（年内日序，1~180）
tem_slice[, doy := 1:180, by = .(Points, chill_year)]

chill.cal <- function(tem, T_low, T_upp, T_op) {
  chill_unit <- case_when(
    # 上升段：(当前温度 - 下限) / (最适温度 - 下限)
    tem >= T_low & tem < T_op ~ (tem - T_low) / (T_op - T_low),

    # 下降段：(上限 - 当前温度) / (上限 - 最适温度)
    tem >= T_op & tem <= T_upp ~ (T_upp - tem) / (T_upp - T_op),

    # 无效区间
    TRUE ~ 0
  )

  return(chill_unit)
}

# 4. 核心冷量计算函数 =====================
chill_func <- function(OriClim, Useyear, Usethre) {
  # 1. 筛选冷季数据（9-12月及1-2月），并排除边界年份（1950, 2023）
  chill.tem <- OriClim[chill_year %in% Useyear]
  # 5. 移除不再需要的日期列（保留 chill_year, doy 及温度列）
  # 顺便把 grid_no 也保留下来，避免后面找不到
  chill.tem[, c("Date", "Year", "Month", "Day") := NULL]

  # 定义需要操作的 24 小时温度列
  temp_cols <- paste0("H", sprintf("%02d", 0:23))

  # 6. 计算每日温度极值和平均值（使用 data.table 的 set 或 rowMeans）
  chill.tem[,
    `:=`(
      max_temp = do.call(pmax, c(.SD, list(na.rm = TRUE))),
      mean_temp = rowMeans(.SD, na.rm = TRUE),
      min_temp = do.call(pmin, c(.SD, list(na.rm = TRUE)))
    ),
    .SDcols = temp_cols
  ]

  # 7. 冷量判断：温度在 [-5, thre] 之间记为 1（有效冷量），否则为 0
  # 使用 lapply 批量替换原列

  Tlow <- -5
  Tupp <- Usethre
  Top <- (Tlow + Tupp) / 2

  chill.tem[,
    (temp_cols) := lapply(
      .SD,
      chill.cal,
      T_upp = Tupp,
      T_low = Tlow,
      T_op = Top
    ),
    .SDcols = temp_cols
  ]

  # 8. 计算每日总有效冷量（当日24小时求和）
  chill.tem[, chill_day := rowSums(.SD, na.rm = TRUE), .SDcols = temp_cols]

  # 9. 计算逐日累积冷量（按年份分组，对每日冷量进行累加）
  # 这才是真正的“逐日累积”，而不是小时内的累积！
  chill.tem[, chill_cumsum := cumsum(chill_day), by = .(Points, chill_year)]

  # 10. 组装最终结果（只提取需要的列）
  chill.res <- chill.tem[, .(
    Points,
    Latitude,
    Longitude,
    chill_year,
    doy,
    min_temp,
    mean_temp,
    max_temp,
    thre = Usethre, # 假设 thre 是一个外部变量，这里会循环填充
    chill_day, # 当日有效冷量
    chill_cumsum # 逐日累积冷量
  )]

  return(chill.res)
}


# 5. 对多个阈值批量计算 =====================
# 使用 map_dfr 遍历阈值 2,4,6,...,20，调用 chill_func，并合并结果

chill.cal_table <- crossing(Useyear = 1951:2022, Usethre = seq(2, 20, by = 2))

chill.cal_res <- pmap_dfr(
  .l = chill.cal_table,
  .f = chill_func,
  OriClim = tem_slice
)

# 转换为 data.table
setDT(chill.cal_res)

# 6. 宽表转换：各阈值作为列 =====================

Points_info <- chill.cal_res[,
  .(
    Longitude = unique(Longitude),
    Latitude = unique(Latitude)
  ),
  by = list(Points)
]

chill.cal_dcast <- dcast(
  data = chill.cal_res,
  formula = Points + chill_year + doy + min_temp + mean_temp + max_temp ~ thre,
  value.var = 'chill_cumsum'
)

# 7. 计算每个阈值的冷量关键统计 =====================
# 每个 doy 下的累积冷量最小值和最大值（跨年份）
chill.crit_ori <- chill.cal_res[,
  list(chill.min = min(chill_cumsum), chill.max = max(chill_cumsum)),
  by = list(Points, doy, thre)
]

# 计算临界冷量 CH.crit = 第180天最小累积冷量的一半
chill.crit_res <- chill.crit_ori[,
  list(CH.crit = chill.min[180] / 2),
  by = list(Points, thre)
]

# 语法格式：X[Y, on = .(连接键), 赋值 := 目标列]
chill.cal_res[chill.crit_res, CHcrit := i.CH.crit, on = .(Points, thre)]

# 8. 筛选达到临界冷量的最早日期 (tdoy) =====================
chill.cal_filter <- chill.cal_res[chill_cumsum >= CHcrit]

# 每个年份和阈值下，首次达到临界冷量的 doy
chill.tdoy_ori <- chill.cal_filter[,
  list(tdoy = min(doy)),
  by = list(Points, chill_year, thre)
]

chill.tdoy_dcast <- dcast(
  data = chill.tdoy_ori,
  formula = Points + chill_year ~ thre,
  value.var = 'tdoy'
)

# 按阈值汇总早/晚的 tdoy（跨年份最小和最大）
chill.tdoy_res <- chill.tdoy_ori[,
  list(tdoy.early = min(tdoy), tdoy.late = max(tdoy)),
  by = list(Points, thre)
]

# 9. 计算 GDD (生长度日) =====================
# 对筛选后的数据，当平均温度 >5℃ 时，GDD = 平均温度 - 5，否则为 0

# 语法格式：X[Y, on = .(连接键), 赋值 := 目标列]
chill.cal_res[chill.tdoy_ori, tdoy := i.tdoy, on = .(Points, chill_year, thre)]

rafu.cal_filter <- chill.cal_res[doy > tdoy & CHcrit != 0]

rafu.cal_filter[,
  GDD := ifelse(mean_temp > 5, yes = mean_temp - 5, 0),
]

# doy 1:122 对应 9月1日 ~ 12月31日（前122天）
# 按 chill_year 和 thre 汇总 GDD 总和（即 RAFU）
rafu.cal_res <- rafu.cal_filter[
  doy <= 122,
  list(rafu = sum(GDD)),
  by = list(Points, chill_year, thre)
]

rafu.cal_dcast <- dcast(
  data = rafu.cal_res,
  formula = Points + chill_year ~ thre,
  value.var = 'rafu'
)

# 按阈值汇总 RAFU 的最小、平均、最大值
rafu.sum_res <- rafu.cal_res[,
  list(
    rafu.min = min(rafu),
    rafu.mean = mean(rafu),
    rafu.max = max(rafu)
  ),
  by = list(Points, thre)
]

rafu.sum_dcast <- dcast(
  data = rafu.sum_res,
  formula = thre ~ Points,
  value.var = 'rafu.max'
)

# 假设 rafu.sum_dcast 是你的宽表数据
# 提取除 'thre' 以外的所有列名，并按自然数字顺序排序
points_cols <- setdiff(names(rafu.sum_dcast), "thre")
points_cols <- points_cols[gtools::mixedorder(points_cols)]

# 直接在原对象上重排序：将 "thre" 放在最前面，后面跟着排好序的 Grid 列
setcolorder(rafu.sum_dcast, c("thre", points_cols))
