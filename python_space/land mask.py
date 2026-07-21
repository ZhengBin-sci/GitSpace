
import numpy as np
import xarray as xr
from python_space.land_mask import LandMaskProcessor  # 确保文件名是小写

# 1. 实例化
processor = LandMaskProcessor(
    lon_range=(0, 180), 
    lat_range=(0, 90), 
    grid_resolution=0.5,        
    mask_resolution='50m'       
)

# 2. 模拟或读取你的真实数据
# 注意：这里模拟的数据必须覆盖 processor 设定的范围
lon = np.arange(0, 180, 0.1)
lat = np.arange(0, 90, 0.1)
# 模拟温度数据
temp_data = xr.DataArray(
    np.random.rand(len(lat), len(lon)), 
    dims=["lat", "lon"], 
    coords={"lat": lat, "lon": lon}
)

# 3. 一键处理并获取 DataFrame
df_land = processor.process_and_export(
    data_array=temp_data, 
    var_name="temperature", 
    output_csv=r"D:\GitSpace\python_space\output\land_temp_NE.csv"
)

# 4. 查看结果前几行
print(df_land.head())