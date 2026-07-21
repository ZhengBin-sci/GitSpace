import numpy as np
import pandas as pd
import regionmask


class LandMaskProcessor:
    """
    陆地掩膜处理器：支持自定义网格精度、生成陆地掩膜、提取陆地数据及获取经纬度范围。
    """
    def __init__(self, lon_range, lat_range, grid_resolution=0.25, mask_resolution='50m'):
        self.lon_range = lon_range
        self.lat_range = lat_range
        self.grid_resolution = grid_resolution
        self.mask_resolution = mask_resolution
        
        # 1. 初始化自然地球陆地掩膜
        res_map = {
            '110m': regionmask.defined_regions.natural_earth_v5_1_2.land_110,
            '50m': regionmask.defined_regions.natural_earth_v5_1_2.land_50,
            '10m': regionmask.defined_regions.natural_earth_v5_1_2.land_10
        }
        
        if mask_resolution not in res_map:
            raise ValueError(f"不支持的掩膜分辨率: {mask_resolution}，请选择 {list(res_map.keys())}")
            
        self.mask_land = res_map[mask_resolution]
        
        # 2. 创建基础网格 (用于后续匹配)
        self.lon_coords = np.arange(lon_range[0], lon_range[1] + grid_resolution/2, grid_resolution)
        self.lat_coords = np.arange(lat_range[0], lat_range[1] + grid_resolution/2, grid_resolution)
        
        print(f"[初始化完成] 范围: {lon_range}x{lat_range}, 网格精度: {grid_resolution}°, 掩膜: {mask_resolution}")

    def get_bounds(self):
        """返回当前的经纬度范围"""
        return {
            "lon_range": self.lon_range,
            "lat_range": self.lat_range,
            "grid_res": self.grid_resolution
        }

    def process_and_export(self, data_array, var_name="value", output_csv=None):
        """
        核心方法：应用掩膜并导出为 DataFrame
        
        Parameters:
            data_array (xr.DataArray): 输入的数据变量 (维度必须包含 lat, lon)
            var_name (str): 输出 CSV 中数值列的名称
            output_csv (str, optional): 如果提供路径，则保存为 CSV
            
        Returns:
            pd.DataFrame: 仅包含陆地格点的 DataFrame (列: lon, lat, value)
        """
        # 1. 确保数据坐标与处理器一致 (防止精度误差导致不匹配)
        # 这里我们假设传入的 data_array 已经是我们要处理的范围
        # 如果需要重采样，可以在这里加 .interp()
        
        # 2. 生成陆地掩膜 (针对当前数据的网格)
        mask = self.mask_land.mask(data_array)
        
        # 3. 应用掩膜：海洋区域变为 NaN
        land_only_data = data_array.where(mask.notnull())
        
        # 4. 转换为 DataFrame 并清洗
        # stack 会将多维数组压平，dropna 会自动剔除海洋的 NaN 值
        df = land_only_data.to_dataframe(name=var_name).dropna()
        
        # 5. 重置索引，将 lat/lon 从索引变为普通列
        df = df.reset_index()
        
        # 6. 调整列顺序，让 lon, lat 排在前面，看起来更直观
        cols = [c for c in df.columns if c in ['lon', 'lat']] + [var_name]
        df = df[cols]
        
        print(f"[处理完成] 原始格点数: {data_array.size}, 陆地格点数: {len(df)}")
        
        # 7. 可选：导出 CSV
        if output_csv:
            df.to_csv(output_csv, index=False, encoding='utf-8-sig')
            print(f"[已保存] 文件路径: {output_csv}")
            
        return df

import geodatasets
import geopandas as gpd
import matplotlib.pyplot as plt
from shapely.geometry import Point


class GeoPlotter:
    """
    地理空间绘图器：支持自动计算边界、自定义外扩范围、无标题清爽绘图。
    """
    def __init__(self):
        # 0. 解决中文显示和警告问题
        plt.rcParams['font.sans-serif'] = ['SimHei', 'Microsoft YaHei', 'Arial Unicode MS']
        plt.rcParams['axes.unicode_minus'] = False
        
        # 1. 缓存底图数据 (避免每次绘图都重新读取文件，提升性能)
        self.world = gpd.read_file(geodatasets.get_path('naturalearth.land'))

    def plot_land_data(self, data_source, value_col='temperature', padding=2, 
                       s=5, alpha=0.6, cmap='RdYlBu_r', figsize=(10, 8)):
        """
        绘制陆地数据散点图
        
        Parameters:
            data_source (str or pd.DataFrame): CSV文件路径 或 Pandas DataFrame
            value_col (str): 需要映射颜色的数值列名
            padding (float): 边界向外扩充的度数，默认 2度
            s (int): 散点大小
            alpha (float): 散点透明度
            cmap (str): 颜色映射
            figsize (tuple): 画布大小
        Returns:
            tuple: (fig, ax) 对象，方便外部进一步自定义
        """
        # --- 1. 处理输入数据 ---
        if isinstance(data_source, str):
            df = pd.read_csv(data_source)
        elif isinstance(data_source, pd.DataFrame):
            df = data_source
        else:
            raise ValueError("data_source 必须是 CSV 路径字符串 或 pandas.DataFrame")  # noqa: TRY004

        # --- 2. 转换为 GeoDataFrame ---
        geometry = [Point(xy) for xy in zip(df['lon'], df['lat'])]
        gdf = gpd.GeoDataFrame(df, geometry=geometry, crs="EPSG:4326")

        # --- 3. 绘图 ---
        fig, ax = plt.subplots(figsize=figsize)

        # A. 绘制世界陆地轮廓作为背景
        self.world.plot(ax=ax, color='lightgray', edgecolor='white', linewidth=0.5, zorder=0)

        # B. 绘制数据散点
        gdf.plot(
            ax=ax, 
            column=value_col, 
            cmap=cmap, 
            s=s, 
            alpha=alpha, 
            legend=True, 
            zorder=2,
            rasterized=True,  # 【核心】将散点渲染为栅格图像，大幅降低渲染负载
            legend_kwds={
                'label': value_col, 
                'orientation': "vertical",
                'shrink': 0.8,  # 顺便优化了图例的缩放比例
                'pad': 0.02     # 调整图例与图表的间距
            }
        )

        # C. 【核心】自动计算范围并外扩
        minx, miny, maxx, maxy = gdf.total_bounds
        ax.set_xlim(minx - padding, maxx + padding)
        ax.set_ylim(miny - padding, maxy + padding)

        # D. 设置坐标轴标签 (默认不加标题)
        ax.set_xlabel("经度 (Longitude)")
        ax.set_ylabel("纬度 (Latitude)")

        plt.tight_layout()
        return fig, ax