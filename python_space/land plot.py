import matplotlib.pyplot as plt  # noqa: F401
from python_space.land_mask import GeoPlotter

# 1. 实例化绘图器
plotter = GeoPlotter()

# 2. 传入 CSV 路径，一键出图
# padding=2 表示在数据边界的基础上，上下左右各向外扩充 2度
fig, ax = plotter.plot_land_data(
    data_source="/user_data/zhengjinbin/MyGit/GitSpace/python_space/output/land_temp_NE.csv", 
    value_col="temperature", 
    padding=5,s = 2.5
)

# 3. (可选) 如果你偶尔还是想加个标题，或者保存图片，可以在外部操作
# ax.set_title("我想临时加个标题", fontsize=14)
# 导出完美的 SVG（无多余 DPI 参数）
fig.savefig("/user_data/zhengjinbin/MyGit/GitSpace/python_space/output/my_map.svg", format="svg", bbox_inches='tight')
