import matplotlib.pyplot as plt
import numpy as np  # noqa: F401
import xarray as xr
from python_space.land_mask import (  # 假设你的类保存在 land_mask.py 中
    GeoPlotter,
    LandMaskProcessor,
)

# 1. 读取 ERA5 真实数据
file_path = "/user_data/lizhijun/ERA5_NH/2m_temperature_daily_mean/2008.nc"
ds = xr.open_dataset(file_path)

# 【调试】打印一下数据集的信息，看看变量名和维度叫什么
# print("数据集结构:", ds)

# 2. 提取温度数据并转换为摄氏度 (ERA5 默认是开尔文 K)
# 注意：ERA5 的变量名通常是 't2m'，如果是其他名字请自行替换
temp_raw = ds['t2m'] - 273.15  

# 3. 处理 ERA5 的维度命名 (你的类要求维度必须是 'lat' 和 'lon')
# 如果 ERA5 的维度叫 'latitude' 和 'longitude'，我们需要重命名
if 'latitude' in temp_raw.dims:
    temp_raw = temp_raw.rename({'latitude': 'lat', 'longitude': 'lon'})

# 将 0~360 转换为 -180~180，并重新排序
temp_raw = temp_raw.assign_coords(lon=(temp_raw.lon + 180) % 360 - 180)
temp_raw = temp_raw.sortby('lon')

# 4. 提取某一天的数据 (例如 2008-01-01) 避免数据太大导致内存爆炸

# 检查是否有数据
temp_data = temp_raw.sel(valid_time='2008-08-05').where(
    (temp_raw.lat >= 0) & (temp_raw.lat <= 90) & 
    (temp_raw.lon >= 100) & (temp_raw.lon <= 180), 
    drop=True
)

# 5. 初始化你的陆地掩膜处理器
# 这里根据设置精度，从ERA5 数据降采样
processor = LandMaskProcessor(
    lat_range=(0, 90), 
    lon_range=(100, 180), 
    grid_resolution=1.0, 
    mask_resolution='50m'
)

# 6. 处理并导出为 CSV
output_csv = "output/era5_2m_temp_land.csv"
df = processor.process_and_export(
    temp_data, 
    var_name="temperature"
)

# 7. 使用你的绘图类画出来看看效果
plotter = GeoPlotter()
fig, ax = plotter.plot_land_data(
    data_source=df, 
    value_col='temperature', 
    cmap='coolwarm',  # 温度数据用冷暖色图比较直观
    s=2,              # ERA5 数据点多，散点调小一点
    alpha=0.8,
    padding=5
)
