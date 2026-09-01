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

    def process_and_export(self, data_array, var_name="value",agg_method = 'mean', output_csv=None):
        """
        核心方法：应用掩膜并导出为 DataFrame
        """
        # 【核心新增】如果传入数据的分辨率比设定的 grid_resolution 更精细，则进行降采样
        # 计算需要粗化的倍数 (假设数据是均匀网格)
        if len(data_array.lat) > 1:
            data_res = abs(data_array.lat[1].item() - data_array.lat[0].item())
            if data_res < self.grid_resolution:
                factor = round(self.grid_resolution / data_res)
                if factor > 1:
                    print(f"[重采样] 检测到原始数据分辨率为 {data_res}°，正在降采样至 {self.grid_resolution}°...")
                    # 使用 coarsen 进行平均降采样，边界用 trim 截断
                    coarsen_obj = data_array.coarsen(lat=factor, lon=factor, boundary='trim')
            
            # 动态获取聚合方法，如果方法不存在则默认回退到 mean
            agg_func = getattr(coarsen_obj, agg_method, None)
            if agg_func is None:
                print(f"[警告] 不支持的聚合方法 '{agg_method}'，已自动回退为 'mean'")
                agg_func = coarsen_obj.mean
                
            data_array = agg_func()
        
        # 1. 生成陆地掩膜 (针对当前数据的网格)
        mask = self.mask_land.mask(data_array)
        
        # 2. 应用掩膜：海洋区域变为 NaN
        land_only_data = data_array.where(mask.notnull())
        
        # 3. 转换为 DataFrame 并清洗
        df = land_only_data.to_dataframe(name=var_name).dropna()
        df = df.reset_index()
        
        # 4. 调整列顺序
        cols = [c for c in df.columns if c in ['lon', 'lat']] + [var_name]
        df = df[cols]
        
        print(f"[处理完成] 原始格点数: {data_array.size}, 陆地格点数: {len(df)}")
        
        # 5. 可选：导出 CSV
        if output_csv:
            # 【核心新增】自动创建不存在的目录
            import os
            dir_name = os.path.dirname(output_csv)
            if dir_name and not os.path.exists(dir_name):
                os.makedirs(dir_name)
                
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
        plt.rcParams['font.sans-serif'] = [
                                            'SimHei',              # Windows 黑体
                                            'Microsoft YaHei',     # Windows 微软雅黑
                                            'Arial Unicode MS',    # macOS 常见中文字体
                                            'WenQuanYi Micro Hei', # Linux 常用：文泉驿微米黑
                                            'WenQuanYi Zen Hei',   # Linux 常用：文泉驿正黑
                                            'Noto Sans CJK SC'     # Linux 常用：思源黑体 (简体中文)
                                        ]
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