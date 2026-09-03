
import numpy as np
import xarray as xr
from python_space.land_mask import LandMaskProcessor  # 确保文件名是小写

# 定义分辨率
resolution = 1.0

# 1. 初始化处理器
processor = LandMaskProcessor(
    lon_range=(0, 180), 
    lat_range=(0, 90), 
    grid_resolution=resolution, 
    mask_resolution='50m'
)

# 2. 【关键】根据最新的分辨率，重新生成坐标和数据
lat = processor.lat_coords
lon = processor.lon_coords

base_random = np.random.rand(len(lat), len(lon))
lat_gradient = np.linspace(0, 1, len(lat)).reshape(-1, 1)
temp_data_values = base_random + lat_gradient

temp_data = xr.DataArray(
    temp_data_values, 
    dims=["lat", "lon"], 
    coords={"lat": lat, "lon": lon}
)

"/user_data/lizhijun/ERA5_NH/2m_temperature_daily_mean/2008.nc"

# 3. 处理并导出
df = processor.process_and_export(
    temp_data, 
    var_name="temperature", 
    output_csv=f"/user_data/zhengjinbin/MyGit/GitSpace/python_space/output/land_temp_{resolution}_NE.csv"
)
