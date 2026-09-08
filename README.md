# 项目概述与文件索引

本项目包含了大美洲生物大迁徙（GABI）宏生态与宏演化模拟的完整代码文档。文件划分为 **Configuration**（环境构建与配置管理）、**Analysis**（模拟输出日志处理与演化统
计）以及 **Figures**（论文图表绘制与假设检验）三大模块。

### 1. Configuration 模块
- [[1] 1.raw_env_info.r](#config-01)
- [[2] 2.env_mean_sd.r](#config-02)
- [[3] 3.fig.env_mean_sd.r](#config-03)
- [[4] 4.extract_values.r](#config-04)
- [[5] 5.env_2_hexagon.r](#config-05)
- [[6] 6.iucn_niche_breadth.r](#config-06)
- [[7] 7.create_virtual_species.r](#config-07)
- [[8] 8.example.r](#config-08)
- [[9] 9.remove.unfinished.r](#config-09)
- [[10] 10.create.null.env.r](#config-10)


### 2. Analysis 模块
- [[1] compare.3vs4.r](#analysis-01)
- [[2] distanct.2.another.continent.r](#analysis-02)
- [[3] generate.tree.4.0.r](#analysis-03)
- [[4] get.distribution.n.isthmus.r](#analysis-04)
- [[5] get_nb.r](#analysis-05)
- [[6] get_range.r](#analysis-06)
- [[7] get_range_burn_in.r](#analysis-07)
- [[8] get_stat.3.x.r](#analysis-08)
- [[9] get_stat.4.0.r](#analysis-09)
- [[10] lat.gradient.r](#analysis-10)
- [[11] lat.low.high.NULL.r](#analysis-11)
- [[12] lat.low.high.r](#analysis-12)
- [[13] merge.stat.3.x.NULL.r](#analysis-13)
- [[14] merge.stat.3.x.r](#analysis-14)
- [[15] N.Speciation.Extinction.r](#analysis-15)
- [[16] N.species.By.Type.with.bridge.merge.isthmus.r](#analysis-16)
- [[17] N.species.By.Type.with.bridge.r](#analysis-17)
- [[18] NB.Prec.r](#analysis-18)
- [[19] netcdf.info.r](#analysis-19)
- [[20] packages.used.r](#analysis-20)
- [[21] richness.r](#analysis-21)
- [[22] species.type.with.bridge.merge.isthmus.r](#analysis-22)
- [[23] species.type.with.bridge.NULL.r](#analysis-23)
- [[24] species.type.with.bridge.r](#analysis-24)

### 3. Figures 模块
- [[1] bootstrapping.analysis.r](#fig-01)
- [[2] bridge.usage.r](#fig-02)
- [[3] climate.curve.r](#fig-03)
- [[4] common.r](#fig-04)
- [[5] Immigrant.Propotion.Threshold.r](#fig-05)
- [[6] Invader.per.r](#fig-06)
- [[7] Lat.Gradient.NULL.r](#fig-07)
- [[8] Lat.Gradient.r](#fig-08)
- [[9] Lat.Low.High.r](#fig-09)
- [[10] lat.null.analysis.r](#fig-10)
- [[11] N.Seed.Dispersal.r](#fig-11)
- [[12] N.Speciation.Extinction.Dispersal.distance.5.r](#fig-12)
- [[13] N.Speciation.Extinction.Dispersal.merge.isthmus.r](#fig-13)
- [[14] N.Speciation.Extinction.Dispersal.no.outliers.r](#fig-14)
- [[15] N.Speciation.Extinction.Dispersal.NULL.r](#fig-15)
- [[16] N.Speciation.Extinction.Dispersal.r](#fig-16)
- [[17] N.Speciation.Extinction.Dispersal.random.seeds.r](#fig-17)
- [[18] N.Species.Biome.r](#fig-18)
- [[19] N.Species.Dispersal.by.seed.end.NULL.r](#fig-19)
- [[20] N.Species.Dispersal.by.seed.end.r](#fig-20)
- [[21] NB.Definition.r](#fig-21)
- [[22] net.diversity.null.r](#fig-22)
- [[23] net.diversity.r](#fig-23)
- [[24] Null.Model.Movies.r](#fig-24)
- [[25] number.check.r](#fig-25)
- [[26] outlier.contribution.r](#fig-26)
- [[27] packages.usage.r](#fig-27)
- [[28] prec_cutoff.r](#fig-28)
- [[29] Range.r](#fig-29)
- [[30] Richness.r](#fig-30)
- [[31] Richness.Stat.r](#fig-31)
- [[32] sample.seed.by.nb.5.degree.r](#fig-32)
- [[33] sample.seed.by.nb.no.outliers.r](#fig-33)
- [[34] sample.seed.by.nb.r](#fig-34)
- [[35] sample.seed.by.nb.test.r](#fig-35)
- [[36] sample.seed.random.r](#fig-36)
- [[37] Seed.Bootstrap.R](#fig-37)
- [[38] Seed.Overview.r](#fig-38)
- [[39] Seed.Status.r](#fig-39)
- [[40] Simulation.Mask.r](#fig-40)
- [[41] why.seeds.dispersed.r](#fig-41)

---

# 模块一：Configuration 脚本库

<a id="config-01"></a>
## [1] 1.raw_env_info.r

```r
#' @file 1.raw_env_info.r
#' @title 提取古气候 NetCDF 全局及变量元数据
#' @description 扫描古气候 NetCDF 序列，提取全局属性、变量属性（维度、单位）和年代信息，导出为统一元数据字典。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ncdf4}: NetCDF 属性读取
#'     \item \code{data.table}: 汇总表构建
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{NetCDF 气候图层 \code{.nc}, \code{name_year.csv}}
#'     \item{输出数据}{\code{meta_table.csv}, \code{meta_table.rda}}
#'   }
```

<a id="config-02"></a>
## [2] 2.env_mean_sd.r

```r
#' @file 2.env_mean_sd.r
#' @title 计算古气候变量的时序均值与离散度统计
#' @description 遍历古气候 NetCDF 序列各变量，逐层计算全域均值、标准差与极值，建立长时间尺度气候统计集。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ncdf4}: 变量矩阵提取
#'     \item \code{data.table}: 数据格式规整
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{meta_table.rda}, 原始 NetCDF 文件}
#'     \item{输出数据}{\code{env_fullstat.rda}}
#'   }
```

<a id="config-03"></a>
## [3] 3.fig.env_mean_sd.r

```r
#' @file 3.fig.env_mean_sd.r
#' @title 绘制长时间序列气候变量均值演变曲线
#' @description 计算关键气候因子在各年代的全域均值与时间步跨度，绘制多变量时序折线图。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 统计排序与时序差分
#'     \item \code{ggplot2}: 多面板时间序列可视化
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{env_fullstat.rda}}
#'     \item{输出数据}{控制台或输出图形设备的时序曲线}
#'   }
```

<a id="config-04"></a>
## [4] 4.extract_values.r

```r
#' @file 4.extract_values.r
#' @title 古气候栅格旋转、缺失值插补与重采样
#' @description 校正 NetCDF 经度坐标（0-360 转 -180-180），对缺失时间步进行算术平均插值，重采样为 1°×1° 的 GeoTIFF 栅格堆叠。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{terra}: 栅格旋转、插值（resample）与导出
#'     \item \code{data.table}: 时间步索引匹配
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{meta_table.rda}, 原始 NetCDF 文件}
#'     \item{输出数据}{\code{Rough.3.75x2.5/*.tif}, \code{Fine.1x1/*.tif}}
#'   }
```

<a id="config-05"></a>
## [5] 5.env_2_hexagon.r

```r
#' @file 5.env_2_hexagon.r
#' @title 离散六边形网格系统构建与气候数据入库
#' @description 生成全球 ISEA3H8 六边形网格，标记美洲大陆与陆桥，建立拓扑邻接表并将 1° 古气候栅格采样入库。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{dggridR}: 生成 ISEA3H8 离散全球网格系统
#'     \item \code{sf}, \code{terra}: 空间拓扑相交与点值提取
#'     \item \code{RSQLite}, \code{DBI}: 数据库持久化存储
#'     \item \code{furrr}, \code{data.table}: 并行拓扑检索
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{continent.shp}, \code{bridges.shp}, \code{Fine.1x1/*.tif}}
#'     \item{输出数据}{\code{N_S_America.shp}, \code{configuration.sqlite}}
#'   }

#' @title 检索相邻触碰的六边形网格
#' @description 并行检索每个六边形单元拓扑相接的邻居单元编号。
#' @param hexagons sf。六边形网格图层。
#' @return tibble。包含网格索引与对应邻居 ID 向量。
#' @noRd
find_connected_hexagon <- function(hexagons) {}
```

<a id="config-06"></a>
## [6] 6.iucn_niche_breadth.r

```r
#' @file 6.iucn_niche_breadth.r
#' @title 基于 IUCN 评估美洲哺乳动物现实生态位跨度
#' @description 提取 IUCN 陆生哺乳动物分布区并与网格现代气候求交，计算温度与降水占据跨度，为虚拟物种参数提供分位数经验参考。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{sf}: 空间包含求交（\code{st_contains}）
#'     \item \code{data.table}: 环境变量分位数与方差统计
#'     \item \code{RSQLite}, \code{DBI}: 气候网格读取
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{MAMMALS_TERRESTRIAL_ONLY.shp}, \code{configuration_continent.sqlite}}
#'     \item{输出数据}{\code{Mammals.World.rda}, \code{nb_range_mammals_iucn.rda}}
#'   }
```

<a id="config-07"></a>
## [7] 7.create_virtual_species.r

```r
#' @file 7.create_virtual_species.r
#' @title 生成虚拟物种参数与模拟方案配置文件
#' @description 基于 IUCN 生态位分位数划定 4 级 NB 与 2 级 DA，抽样初始种源并输出模拟引擎所需的配置数据库。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 笛卡尔积方案组合与格式化
#'     \item \code{RSQLite}, \code{DBI}: 配置数据库写入
#'     \item \code{sf}: 空间采样
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{nb_range_mammals_iucn.rda}, \code{N_S_America.shp}, \code{configuration_continent.sqlite}}
#'     \item{输出数据}{\code{conf.sqlite}, \code{configuration.sqlite}}
#'   }
```

<a id="config-08"></a>
## [8] 8.example.r

```r
#' @file 8.example.r
#' @title 单物种模拟动态交互式调试与可视化
#' @description 提供交互式终端控制，逐步展示指定参数组合下物种在六边形网格中的时空扩散动态。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 日志读取与过滤
#'     \item \code{ggplot2}, \code{sf}: 空间逐步动态制图
#'     \item \code{RSQLite}, \code{DBI}: 环境值检验
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{模拟日志 \code{.log}, \code{N_S_America.shp}}
#'     \item{输出数据}{终端交互式空间地图}
#'   }
```

<a id="config-09"></a>
## [9] 9.remove.unfinished.r

```r
#' @file 9.remove.unfinished.r
#' @title 批量清理未正常完成的模拟结果目录
#' @description 巡检模拟输出目录，安全删除文件缺失（<4）且无完成标记的异常结果目录。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 文件系统路径过滤
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{空模型模拟目录 \code{Results_NULL}}
#'     \item{输出数据}{控制台删除记录并释放存储空间}
#'   }
```

<a id="config-10"></a>
## [10] 10.create.null.env.r

```r
#' @file 10.create.null.env.r
#' @title 构建空模型静态环境 SQLite 数据库
#' @description 复制 3.6 Ma（year = 1800）基准年的环境图层填满所有时步，生成无气候波动的空模型对照数据库与运行配置。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{RSQLite}, \code{DBI}: 数据库覆盖重构
#'     \item \code{data.table}: 大规模环境数据表快速克隆
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{configuration.sqlite}, \code{conf.sqlite}, 种子清单}
#'     \item{输出数据}{\code{null.sqlite}, \code{conf.null.sqlite}}
#'   }
```

# 模块二：Analysis 脚本库

<a id="analysis-01"></a>
## [1] compare.3vs4.r

```r
#' @file compare.3vs4.r
#' @title 模拟结果对比分析（版本 3.x vs 4.0）
#' @description 对比 GABI 模拟模型在 3.x（日志格式）与 4.0（RDS 格式）下最终步（year = 0）的物种分布预测，
#'   计算网格重叠率、Jaccard 相似度指数及丰富度相关系数。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 数据合并与快速筛选
#'     \item \code{RSQLite}, \code{DBI}: 配置数据库读取
#'     \item \code{sf}, \code{ggplot2}: 空间网格重叠可视化
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{conf.sqlite}, 3.x 模拟日志 \code{.log}, 4.0 分布数据 \code{distribution.rda}, \code{N_S_America.shp}}
#'     \item{输出数据}{\code{../Data/Tables/3.5vs4.rda}}
#'   }
```

<a id="analysis-02"></a>
## [2] distanct.2.another.continent.r

```r
#' @file distanct.2.another.continent.r
#' @title 计算离散六边形网格至目标大陆的最短拓扑距离
#' @description 基于六边形网格邻接表构建图结构，运用广度优先搜索算法（BFS）计算南北美洲所有陆地网格
#'   至对侧大陆/陆桥通道的最短拓扑步长距离。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 邻接矩阵与拓扑节点索引
#'     \item \code{sf}, \code{ggplot2}: 网格距离填色制图
#'     \item \code{RSQLite}, \code{DBI}: 读取网格邻接数据库
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{configuration.sqlite}, \code{N_S_America.shp}}
#'     \item{输出数据}{\code{../Data/Tables/cells.with.dist.rda}}
#'   }

#' @title 构建图邻接表
#' @description 根据网格对之间的单位邻接拓扑关系生成无向图邻接表。
#' @param dt data.table。包含 \code{i} 与 \code{j} 两列整型节点的邻接表。
#' @return list。以节点 ID 为键名、邻居 ID 向量为值的列表。
#' @noRd
build_adjacency_list <- function(dt) {}

#' @title BFS 广度优先搜索计算最短拓扑距离
#' @description 计算无权网络中起始节点与目标节点之间的最短拓扑步数。
#' @param start_node integer。起始网格 ID。
#' @param end_node integer。目标网格 ID。
#' @param adj_list list。图邻接表。
#' @return integer。最短拓扑距离，不可达返回 -1。
#' @noRd
shortest_distance_bfs <- function(start_node, end_node, adj_list) {}
```

<a id="analysis-03"></a>
## [3] generate.tree.4.0.r

```r
#' @file generate.tree.4.0.r
#' @title 生成模拟系统的系统发育树（版本 4.0）
#' @description 遍历 4.0 版本的物种演化记录，解析谱系亲子拓扑及存续时长，递归拼接为标准 Newick 格式演化树。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 节点谱系查询
#'     \item \code{RSQLite}, \code{DBI}: 读取模拟数据库
#'     \item \code{ape}, \code{treeio}, \code{ggtree}: 系统发育树解析与绘制校验
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{conf.sqlite}, \code{species_record.rda}}
#'     \item{输出数据}{\code{tree.rda}（存储 Newick 树字符串）}
#'   }

#' @title 递归拼接 Newick 系统发育树字符串
#' @description 依据物种亲子谱系与生存区间递归构建子树及枝长。
#' @param current_id character。当前处理的物种编号。
#' @param data data.table。包含物种亲缘与存活区间的记录表。
#' @return character。Newick 格式树文本。
#' @noRd
build_tree_string <- function(current_id, data) {}
```

<a id="analysis-04"></a>
## [4] get.distribution.n.isthmus.r

```r
#' @file get.distribution.n.isthmus.r
#' @title 统计最终物种丰富度（地峡独立 vs 合并入北美）
#' @description 读取末期分布记录并与大洲网格匹配，对比巴拿马地峡独立划分与合并入北美洲两种规则下的丰富度格局。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 高效分组汇总与多线程运算
#'     \item \code{sf}, \code{ggplot2}: 空间属性匹配
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{cells.with.dist.rda}, \code{final.dis.rda}}
#'     \item{输出数据}{\code{Richness.with.isthmus.rda}, \code{Richness.merged.isthmus.rda}}
#'   }
```

<a id="analysis-05"></a>
## [5] get_nb.r

```r
#' @file get_nb.r
#' @title 提取物种已实现生态位动态演变（温度与降水跨度）
#' @description 结合历史气候数据库与分布日志，动态计算各物种在模拟各时段占据网格的环境极值与幅度。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 逐年网格环境匹配与聚合
#'     \item \code{RSQLite}, \code{DBI}: 环境数据库直连
#'     \item \code{ggplot2}, \code{ggh4x}: 生态位直方图与轨迹制图
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{configuration.sqlite}, 模拟日志 \code{.log}}
#'     \item{输出数据}{\code{nb.rda}（单任务及全局汇总表）}
#'   }
```

<a id="analysis-06"></a>
## [6] get_range.r

```r
#' @file get_range.r
#' @title 提取物种最终地理分布区与边界范围
#' @description 提取末期（year = 0）存活物种的适生网格分布，计算占据单元数及经纬度边界框（Bounding Box）。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 极值计算与坐标聚合
#'     \item \code{sf}, \code{ggh4x}: 空间数据辅助
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{.log}, \code{cells.with.dist.rda}}
#'     \item{输出数据}{\code{final.dis.rda}, \code{range.rda}, \code{Species.Range.NULL.rda}}
#'   }
```

<a id="analysis-07"></a>
## [7] get_range_burn_in.r

```r
#' @file get_range_burn_in.r
#' @title 提取模拟预热期（Burn-in）末期物种分布数据
#' @description 从模拟日志中截取预热平衡阶段（year = 1801）物种的实际网格分布，作为演化起始前基准。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 大容量日志高效过滤与导出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{模拟运行日志 \code{.log}}
#'     \item{输出数据}{\code{dis.burn.in.1801.1.rda}, \code{Burn.in.Distribution.rda}}
#'   }
```

<a id="analysis-08"></a>
## [8] get_stat.3.x.r

```r
#' @file get_stat.3.x.r
#' @title 统计 3.x 版本网格占用与物种多样性格局
#' @description 解析 3.x 版本模拟日志，生成逐年分大洲的网格占用量及逐网格的物种丰富度时空数据。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 多维分组汇总与排序
#'     \item \code{sf}: 大陆图层属性关联
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{.log}, \code{N_S_America.shp}}
#'     \item{输出数据}{\code{continent_n_cells.rda}, \code{species.richness.rda}}
#'   }
```

<a id="analysis-09"></a>
## [9] get_stat.4.0.r

```r
#' @file get_stat.4.0.r
#' @title 统计 4.0 版本网格占用与物种丰富度
#' @description 针对 4.0 版本列表格式分布数据，统计物种在各大洲的网格占用规模及各单元的物种丰富度。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 跨时段列表合并与空间统计
#'     \item \code{RSQLite}, \code{DBI}: 配置库读取
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{conf.sqlite}, \code{distribution.rda}, \code{N_S_America.shp}}
#'     \item{输出数据}{\code{continent_n_cells.rda}, \code{species.richness.rda}}
#'   }
```

<a id="analysis-10"></a>
## [10] lat.gradient.r

```r
#' @file lat.gradient.r
#' @title 纬度多样性梯度（LDG）时空演变分析
#' @description 将美洲六边形网格划分为 1 纬度带，剔除陆桥后统计各纬度带物种丰富度在演化历程中的动态。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 坐标区间离散化与聚合
#'     \item \code{ggplot2}, \code{ggh4x}: 纬度多样性曲线绘制
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{cells.with.dist.rda}, \code{distribution.rda} 或 \code{.log}}
#'     \item{输出数据}{\code{lat.N.1degree.without.bridges.rda}, \code{Lat.N.1defree.without.bridges.NULL.rda}}
#'   }
```

<a id="analysis-11"></a>
## [11] lat.low.high.NULL.r

```r
#' @file lat.low.high.NULL.r
#' @title 空模型下高低纬度本土与迁入物种多样性对比
#' @description 在静态气候下按 40°、54° 纬度阈值划分高低纬度，通过 100 次抽样对比本土种与迁入种的丰富度与占比。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 重采样聚合与排序
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{Final.Distribution.NULL.Unique.rda}, \code{cells.with.dist.rda}, \code{random.seeds.threshold.by.nb.distance.rda}}
#'     \item{输出数据}{\code{Lat.low.high.without.bridges.NULL.rda}, \code{Lat.low.mid.high.without.bridges.NULL.rda}}
#'   }
```

<a id="analysis-12"></a>
## [12] lat.low.high.r

```r
#' @file lat.low.high.r
#' @title 动态气候下高低纬度本土与迁入物种多样性格局
#' @description 基于真实动态气候模拟，量化高低纬度区间内本土与外来迁入物种的多样性比例，检验迁入不对称性。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 大规模矩阵合并与分面统计
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{Final.Distribution.Unique.rda}, \code{cells.with.dist.rda}, 抽样种子表}
#'     \item{输出数据}{\code{Lat.low.high.without.bridges.rda}, \code{Lat.low.mid.high.without.bridges.rda}}
#'   }
```

<a id="analysis-13"></a>
## [13] merge.stat.3.x.NULL.r

```r
#' @file merge.stat.3.x.NULL.r
#' @title 汇总 3.x 版本空模型所有模拟结果
#' @description 遍历并合并空模型下所有 3.x 模拟任务的大陆占用量与网格丰富度，生成不同参数级别的基准汇总大表。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 批量追加与跨维度聚合
#'     \item \code{RSQLite}, \code{DBI}: 空模型配置库读取
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{conf.null.sqlite}, 各子任务 \code{continent_n_cells.rda} 及 \code{species.richness.rda}}
#'     \item{输出数据}{\code{virtual.species.NULL.rda}, \code{virtual.species.richness.all.NULL.rda} 等}
#'   }
```

<a id="analysis-14"></a>
## [14] merge.stat.3.x.r

```r
#' @file merge.stat.3.x.r
#' @title 汇总 3.x 版本真实气候模拟结果
#' @description 遍历并合并动态气候下所有 3.x 模拟任务，构建全量物种大陆占用表与不同生态位/扩散分组的网格丰富度大表。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 高通量数据提取与汇总
#'     \item \code{RSQLite}, \code{DBI}: 数据库访问
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{conf.sqlite}, 各任务统计结果}
#'     \item{输出数据}{\code{virtual.species.rda}, \code{virtual.species.richness.all.rda} 等}
#'   }
```

<a id="analysis-15"></a>
## [15] N.Speciation.Extinction.r

```r
#' @file N.Speciation.Extinction.r
#' @title 解析发育树拓扑并统计逐年物种形成与灭绝动态
#' @description 重建系统发育树对象，区分内部节点（成种）与叶节点（绝灭或存活），统计从起始年代至现今每年的演化事件数。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ape}, \code{phytools}, \code{geiger}: 系统发育树解析与节点操作
#'     \item \code{data.table}, \code{stringr}: 节点标签文本解析
#'     \item \code{RSQLite}, \code{DBI}: 树数据库直连
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{tree.rda} 或 \code{.sqlite}, \code{conf.sqlite}}
#'     \item{输出数据}{\code{[sp].N.speciation.extinction.rda}, \code{N.Speciation.Extinction.All.NB.rda}}
#'   }
```

<a id="analysis-16"></a>
## [16] N.species.By.Type.with.bridge.merge.isthmus.r

```r
#' @file N.species.By.Type.with.bridge.merge.isthmus.r
#' @title 追踪物种时空拓殖类型（巴拿马地峡并入北美）
#' @description 将巴拿马地峡网格合并入北美洲，基于状态转移矩阵判断物种的成种、迁入、局域灭绝与全局灭绝事件。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 状态转移矩阵构建与高效赋值
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{virtual.species.merge.isthmus.rda}, \code{full.combination.csv}}
#'     \item{输出数据}{\code{sp_full_continents.merge.isthmus.rda}, \code{full.event.N.merge.isthmus.csv}}
#'   }
```

<a id="analysis-17"></a>
## [17] N.species.By.Type.with.bridge.r

```r
#' @file N.species.By.Type.with.bridge.r
#' @title 细分陆桥通道的物种生物地理状态转移追踪
#' @description 独立保留巴拿马地峡与加勒比陆桥通道，追踪物种全生命周期在各大洲与陆桥间的扩散、驻留与灭绝演变。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 大规模时空数据关联与条件筛选
#'     \item \code{ape}, \code{phytools}: 谱系支持
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{virtual.species.rda}, \code{species.type.N.rda}, \code{full.combination.csv}}
#'     \item{输出数据}{\code{sp_full_continents.rda}, \code{full.event.N.csv}}
#'   }
```

<a id="analysis-18"></a>
## [18] NB.Prec.r

```r
#' @file NB.Prec.r
#' @title 模拟任务状态监控与种子参数空间特征核验
#' @description 解析模拟数据库中的环境阈值参数，监控各任务目录完成状态，绘制种子空间分布与边界多边形。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 文本反序列化与快速拆分
#'     \item \code{sf}, \code{ggplot2}: 六边形网格空间可视化
#'     \item \code{RSQLite}, \code{DBI}: 数据库访问
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{conf.sqlite}, \code{N_S_America.shp}, 模拟结果目录}
#'     \item{输出数据}{控制台任务巡检状态及空间分布图}
#'   }
```

<a id="analysis-19"></a>
## [19] netcdf.info.r

```r
#' @file netcdf.info.r
#' @title NetCDF 古气候文件结构探查
#' @description 交互式读取单份 3600 Ma 古气候模拟 NetCDF 文件，快速查看其变量定义、时间/空间维度与全局属性。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ncdf4}: NetCDF 接口
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{古气候模拟 NetCDF 文件 \code{.nc}}
#'     \item{输出数据}{控制台输出文件结构诊断信息}
#'   }
```

<a id="analysis-20"></a>
## [20] packages.used.r

```r
#' @file packages.used.r
#' @title 收集并输出项目依赖 R 包版本与文献引用
#' @description 遍历项目引用的核心 R 包，自动检测其安装版本并提取标准学术引用文本，导出为规范的 CSV 汇总清单。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 汇总信息构建与带 BOM CSV 导出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{本地 R 环境已安装包}
#'     \item{输出数据}{\code{../Data/Tables/r_packages_citation_summary.csv}}
#'   }

#' @title 提取单包版本与文献引用
#' @description 提取指定 R 包的版本及标准学术引用。
#' @param pkg character。R 包名称。
#' @return data.table。包含 Package、Version 和 Citation 的单行数据表。
#' @noRd
get_pkg_info <- function(pkg) {}
```

<a id="analysis-21"></a>
## [21] richness.r

```r
#' @file richness.r
#' @title 生成周期性物种丰富度时间切片数据集
#' @description 提取每间隔 100 个模拟时步（100 kyr）的网格物种丰富度，融合地理坐标后导出为按年份划分的独立切片。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 快速追加与列表切片
#'     \item \code{RSQLite}, \code{DBI}: 配置任务读取
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{conf.sqlite}, 各模拟 \code{species.richness.rda}, \code{cells.with.dist.rda}}
#'     \item{输出数据}{\code{../Data/Tables/Yearly.Richness/[year].rda}}
#'   }
```

<a id="analysis-22"></a>
## [22] species.type.with.bridge.merge.isthmus.r

```r
#' @file species.type.with.bridge.merge.isthmus.r
#' @title 构建谱系拓扑结构与分类特征（地峡合并，空模型）
#' @description 针对空模型地峡并入北美的数据，重构系统发育树对象（\code{phylo}），提取后代节点并统计各类型演化事件。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ape}, \code{phangorn}, \code{phytools}: 谱系树构建、枝长设定与后代检索
#'     \item \code{data.table}: 拓扑矩阵转换
#'     \item \code{ggplot2}, \code{ggh4x}: 统计分布图绘制
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{virtual.species.merge.isthmus.NULL.rda}}
#'     \item{输出数据}{\code{temp.N.sp.NULL.isthmus/[label].rda}, \code{species.type.N.merge.isthmus.NULL.rda}}
#'   }

#' @title 向 data.table 指定行赋值
#' @description 原地更新 data.table 指定行的多列值。
#' @param dt data.table。待更新目标表。
#' @param row integer。行号。
#' @param vals list。具名列表。
#' @return NULL。
#' @noRd
assign_named_row <- function(dt, row, vals) {}
```

<a id="analysis-23"></a>
## [23] species.type.with.bridge.NULL.r

```r
#' @file species.type.with.bridge.NULL.r
#' @title 构建谱系树并统计物种分化与灭绝（独立陆桥，空模型）
#' @description 针对未合并陆桥的空模型，构建系统发育树并递归检索各节点后代（Descendants），判定其演化存活状态。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ape}, \code{phangorn}, \code{phytools}: 谱系树分析
#'     \item \code{data.table}: 数据高效过滤
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{virtual.species.NULL.rda}}
#'     \item{输出数据}{\code{temp.N.sp.NULL/[label].rda}, \code{species.type.N.NULL.rda}}
#'   }

#' @title 向 data.table 指定行快速赋值
#' @description 原地更新指定行的列值。
#' @param dt data.table。目标表。
#' @param row integer。行索引。
#' @param vals list。具名值列表。
#' @return NULL。
#' @noRd
assign_named_row <- function(dt, row, vals) {}
```

<a id="analysis-24"></a>
## [24] species.type.with.bridge.r

```r
#' @file species.type.with.bridge.r
#' @title 动态气候下谱系树重建与陆桥利用分类
#' @description 针对真实气候模拟，将物种生活史重构为树对象，追踪陆桥与美洲大陆占用历史，量化成种、灭绝与扩散行为。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ape}, \code{phangorn}, \code{phytools}: 树构建与后代子树提取
#'     \item \code{data.table}: 高性能多列操作
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{virtual.species.rda}}
#'     \item{输出数据}{\code{temp.N.sp/[label].rda}, \code{species.type.N.rda}}
#'   }

#' @title 向 data.table 指定行赋具名值
#' @description 原地更新 data.table 指定行中的数据。
#' @param dt data.table。目标数据表。
#' @param row integer。目标行。
#' @param vals list。包含列名与更新值的列表。
#' @return NULL。
#' @noRd
assign_named_row <- function(dt, row, vals) {}
```

---

# 模块三：Figures 脚本库

<a id="fig-01"></a>
## [1] bootstrapping.analysis.r

```r
#' @file bootstrapping.analysis.r
#' @title 评估种子抽样截断阈值（95% 至 99.5%）的鲁棒性
#' @description 对比 95%、96%、99% 和 99.5% 四种截断阈值下的物种丰富度与迁入比例，评估爆发性分化对宏观格局的影响。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 多阈值数据匹配
#'     \item \code{ggplot2}, \code{ggh4x}, \code{patchwork}: 敏感性箱线图与分面组合
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{各分位数种子表 \code{random.seeds.*.rda}, \code{N.with.bridge.seed.continent.rda}}
#'     \item{输出数据}{敏感性对比图版}
#'   }
```

<a id="fig-02"></a>
## [2] bridge.usage.r

```r
#' @file bridge.usage.r
#' @title 陆桥利用偏好分析（巴拿马地峡 vs 加勒比通道）
#' @description 统计南北美洲物种利用不同陆桥通道成功扩散及停滞的比例，绘制带误差棒均值图并导出 Word/CSV 报表。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 扩散路径追踪与 Bootstrap 汇总
#'     \item \code{ggplot2}: 陆桥选择偏好点图与误差棒
#'     \item \code{flextable}, \code{officer}: 统计表格导出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{sp_full_continents.rda}, 抽样种子表}
#'     \item{输出数据}{\code{Bridge.Usage.pdf}, \code{Bridge.Usage.docx}, \code{Bridge.Usage.csv}}
#'   }
```

<a id="fig-03"></a>
## [3] climate.curve.r

```r
#' @file climate.curve.r
#' @title 绘制 3.8 Ma 以来美洲宏观古气候演化曲线
#' @description 统计美洲各大洲与陆桥网格历史降水和高低温序列的均值与离散度，利用双 Y 轴及纵向分面呈现古气候轨迹。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 区域气候均值计算
#'     \item \code{ggplot2}, \code{patchwork}: 双 Y 轴时序曲线
#'     \item \code{RSQLite}, \code{DBI}: 数据库直连
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{configuration.sqlite}, \code{N_S_America.shp}}
#'     \item{输出数据}{\code{Climate_curve_continent.pdf}, \code{Climate_curve.pdf}}
#'   }
```

<a id="fig-04"></a>
## [4] common.r

```r
#' @file common.r
#' @title 通用绘图主题、色板定义与 Word 报表导出工具
#' @description 定义制图投影（LAEA / Robinson）、色弱安全配色方案，提供将 data.table 输出为规范学术三线表 Word 文档的工具函数。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{flextable}, \code{officer}: Word 三线表生成
#'     \item \code{data.table}: 列格式化与有效数字截断
#'     \item \code{ggplot2}, \code{ggsci}: 绘图主题与色板
#'     \item \code{sf}: 坐标系定义
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{R 内存汇总 data.table}
#'     \item{输出数据}{规范化 Word 文档（.docx）}
#'   }

#' @title 导出 data.table 为 Word 表格文档
#' @description 将输入表格式化后通过 flextable 构建学术表格存为 Word 文件。
#' @param summary_dt data.table。待导出表格。
#' @param title character。表格标题。
#' @param output_file character。目标 .docx 文件路径。
#' @param digits integer。保留小数位数，默认 3。
#' @param in_place logical。是否就地修改原表，默认 FALSE。
#' @return NULL。
#' @export
to.doc <- function(summary_dt, title, output_file, digits = 3, in_place = FALSE) {}

#' @title 格式化 data.table 中的数值列
#' @description 统一格式化数值列为固定小数位数的字符串。
#' @param dt data.table。目标数据表。
#' @param digits integer。小数位数，默认 3。
#' @param in_place logical。是否就地修改，默认 TRUE。
#' @return data.table。格式化后的数据表。
#' @export
format_sigfigs_dt <- function(dt, digits = 3, in_place = TRUE) {}
```

<a id="fig-05"></a>
## [5] Immigrant.Propotion.Threshold.r

```r
#' @file Immigrant.Propotion.Threshold.r
#' @title 连续种子截断阈值（90%-100%）下演化事件与迁入比例动态
#' @description 遍历 90% 至 100% 的种子筛选阈值，统计成种、全域灭绝、局域绝灭与扩散事件，检验迁入不对称性的稳健度。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 批量阈值重采样汇总
#'     \item \code{ggplot2}, \code{patchwork}: 多面板图版组合
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{各阈值种子表, \code{N.Speciation.Extinction.Dispersal.rda}, \code{N.with.bridge.seed.continent.rda}}
#'     \item{输出数据}{\code{richness.by.threshold.rda}, 统计图版与 Word 报表}
#'   }
```

<a id="fig-06"></a>
## [6] Invader.per.r

```r
#' @file Invader.per.r
#' @title 绘制网格尺度迁入物种比例（入侵度）空间分布格局
#' @description 计算六边形网格系统中迁入物种占总物种数的比例，生成空间 Shapefile 并绘制入侵比例渐变地图。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 逐网格迁入物种比率计算
#'     \item \code{sf}: 空间图层导出（Shapefile）
#'     \item \code{ggplot2}: 连续渐变色空间制图
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{Final.Distribution.Unique.rda}, 抽样种子表, \code{N_S_America.shp}}
#'     \item{输出数据}{\code{Invader.Per.shp}, \code{Invader.Per.pdf}, \code{Invader.Per.NB.DA.pdf}}
#'   }
```

<a id="fig-07"></a>
## [7] Lat.Gradient.NULL.r

```r
#' @file Lat.Gradient.NULL.r
#' @title 空模型下纬度多样性梯度与迁入比例曲线
#' @description 统计静态气候下 1 纬度带内的本土与外来丰富度，利用独立翻转 Y 轴呈现空模型基准曲线。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 纬度区间统计与均值计算
#'     \item \code{ggplot2}, \code{ggh4x}: 独立 Y 轴翻转曲线
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{Lat.N.1defree.without.bridges.NULL.rda}, 种子表}
#'     \item{输出数据}{\code{Lat.Gradient.NULL.pdf}, \code{Lat.Gradient.Immigrant.proportion.NULL.pdf}}
#'   }
```

<a id="fig-08"></a>
## [8] Lat.Gradient.r

```r
#' @file Lat.Gradient.r
#' @title 动态气候下纬度多样性梯度与迁入比例曲线
#' @description 统计真实气候下 1 纬度区间内的物种丰富度与迁入比例，结合南美洲坐标轴翻转，直观展示热带到温带的渗透不对称性。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 纬度多轮抽样聚合
#'     \item \code{ggplot2}, \code{ggh4x}: 分面 Y 轴独立反转绘图
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{Lat.N.1defree.without.bridges.rda}, 种子表}
#'     \item{输出数据}{\code{Lat.Gradient.pdf}, \code{Lat.Gradient.Immigrant.proportion.pdf}, \code{Lat.Gradient.docx}}
#'   }
```

<a id="fig-09"></a>
## [9] Lat.Low.High.r

```r
#' @file Lat.Low.High.r
#' @title 空模型高低纬度生物多样性指标汇总
#' @description 汇总静态气候下高低纬度区间的本土/迁入丰富度均值与比例，导出 CSV 提供量化对照基准。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 统计指标计算与 CSV 导出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{Lat.low.high.without.bridges.NULL.rda}, \code{Lat.low.mid.high.without.bridges.NULL.rda}}
#'     \item{输出数据}{\code{Lat.low.high.without.bridges.nb.da.NULL.csv} 等}
#'   }
```

<a id="fig-10"></a>
## [10] lat.null.analysis.r

```r
#' @file lat.null.analysis.r
#' @title 空模型极高纬度物种多样性异常分析
#' @description 针对空模型在南美高纬度（<= -50°）区域出现的聚集现象进行排查，评估几何边界效应的影响。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 极值网格过滤与多阶汇总
#'     \item \code{ggplot2}, \code{scales}: 对数直方图与误差棒点图
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{Final.Distribution.NULL.Unique.rda}, \code{N_S_America.shp}}
#'     \item{输出数据}{极值探索图表及控制台统计}
#'   }
```

<a id="fig-11"></a>
## [11] N.Seed.Dispersal.r

```r
#' @file N.Seed.Dispersal.r
#' @title 种源水平跨大陆扩散成功率分析与空间制图
#' @description 追踪每个初始种子在历史时段内是否成功跨越大洲建立种群，绘制全美洲种子扩散能力空间地图。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 初始种子与末期存活比对
#'     \item \code{sf}, \code{ggplot2}, \code{patchwork}: 六边形地图与分面箱线图
#'     \item \code{flextable}, \code{officer}: Word 表格输出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{sp_full_continents.rda}, \code{Seed.Pool.rda}, \code{cells.with.dist.rda}}
#'     \item{输出数据}{\code{Seed.Dispersal.Map.pdf}, \code{Seed.Dispersal.boxplot.full.pdf}, \code{seed.2.other.continent.docx}}
#'   }
```

<a id="fig-12"></a>
## [12] N.Speciation.Extinction.Dispersal.distance.5.r

```r
#' @file N.Speciation.Extinction.Dispersal.distance.5.r
#' @title 近陆桥种子子集（距离阈值 = 5）演化与扩散分析
#' @description 选取距陆桥 5 个步长以内的种源子集重算宏演化指标，检验排除距离衰减后扩散不对称性是否稳健。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 子集筛选与聚合
#'     \item \code{ggplot2}, \code{patchwork}: 拼版箱线图
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{random.seeds.by.nb.distance.5.rda}, 宏演化事件表, 丰富度表}
#'     \item{输出数据}{演化事件箱线图版}
#'   }
```

<a id="fig-13"></a>
## [13] N.Speciation.Extinction.Dispersal.merge.isthmus.r

```r
#' @file N.Speciation.Extinction.Dispersal.merge.isthmus.r
#' @title 宏演化与扩散事件统计分析（巴拿马地峡并入北美）
#' @description 地峡并入北美规则下执行 100 次抽样，统计本土成种、异地成种、灭绝、局域绝灭及初/次级入侵事件。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 事件分类与 Bootstrap 聚合
#'     \item \code{ggplot2}, \code{patchwork}: 出版级图版拼合
#'     \item \code{flextable}, \code{officer}: 统计表格导出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{sp_full_continents.merge.isthmus.rda}, 种子表}
#'     \item{输出数据}{\code{N.Speciation.Extinction.Richness.merge.isthmus.pdf}, \code{N.Richness.Per.ALL.merge.isthmus.pdf}, 报表文档}
#'   }
```

<a id="fig-14"></a>
## [14] N.Speciation.Extinction.Dispersal.no.outliers.r

```r
#' @file N.Speciation.Extinction.Dispersal.no.outliers.r
#' @title 剔除极端爆发离群值后的宏演化与扩散分析
#' @description 剔除单次爆发产生超万种的极少数异常模拟，重新评估成种、绝灭和入侵动态的不对称性。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 离群值排除与重抽样
#'     \item \code{ggplot2}, \code{patchwork}: 箱线图组
#'     \item \code{flextable}, \code{officer}: Word 导出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{random.seeds.threshold.by.nb.distance.no.outliers.rda}, 演化事件表}
#'     \item{输出数据}{\code{N.Speciation.Extinction.Richness.FULL.pdf}, 配套 Word 报表}
#'   }
```

<a id="fig-15"></a>
## [15] N.Speciation.Extinction.Dispersal.NULL.r

```r
#' @file N.Speciation.Extinction.Dispersal.NULL.r
#' @title 空模型下宏演化与扩散事件统计分析
#' @description 针对静态气候空模型统计成种、绝灭、扩散事件及最终丰富度，检验仅凭地理拓扑是否能够诱发不对称性。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 空模型事件清洗与聚合
#'     \item \code{ggplot2}, \code{patchwork}: 空模型图版
#'     \item \code{flextable}, \code{officer}: Word 文档输出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{sp_full_continents.NULL.rda}, 种子表}
#'     \item{输出数据}{\code{N.Speciation.Extinction.Richness.NULL.pdf}, 空模型统计报表}
#'   }
```

<a id="fig-16"></a>
## [16] N.Speciation.Extinction.Dispersal.r

```r
#' @file N.Speciation.Extinction.Dispersal.r
#' @title 动态气候下宏演化与扩散事件核心分析
#' @description 提取全量真实气候模拟中的成种、全局绝灭、局域绝灭及入侵事件，执行 100 次抽样，生成论文核心主图与附表。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 事件分类提取与 Bootstrap 计算
#'     \item \code{ggplot2}, \code{patchwork}: 核心图版组合排版
#'     \item \code{flextable}, \code{officer}: 论文三线表生成
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{sp_full_continents.rda}, 种子表}
#'     \item{输出数据}{\code{N.Speciation.Extinction.Richness.pdf}, \code{N.Richness.Per.pdf}, 统计报表}
#'   }
```

<a id="fig-17"></a>
## [17] N.Speciation.Extinction.Dispersal.random.seeds.r

```r
#' @file N.Speciation.Extinction.Dispersal.random.seeds.r
#' @title 随机种子敏感性检验（剔除前 5% 繁盛谱系）
#' @description 排除分化规模前 5% 的极度繁盛物种群，评估极端支系对生物大迁徙整体结论的潜在影响。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 顶端支系剔除与抽样
#'     \item \code{ggplot2}, \code{patchwork}: 拼版箱线图
#'     \item \code{flextable}, \code{officer}: 报表输出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{random.seeds.100.remove.top5.big.rda}, 演化事件表}
#'     \item{输出数据}{检验图版与统计表}
#'   }
```

<a id="fig-18"></a>
## [18] N.Species.Biome.r

```r
#' @file N.Species.Biome.r
#' @title 生物群系（Biome）尺度物种丰富度与入侵比例分析
#' @description 将模拟结果与 WWF 陆地生态区图层空间求交，统计各生物群系内本土/迁入物种数，绘制带饼图标注的群系入侵地图。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{sf}: 生态区图层简化（st_simplify）与相交提取
#'     \item \code{data.table}: 生物群系物种统计
#'     \item \code{ggplot2}, \code{ggrepel}, \code{ggforce}: 饼图标注与空间地图绘制
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{Ecoregions2017.shp}, \code{Final.Distribution.Unique.rda}, \code{cells.with.dist.rda}}
#'     \item{输出数据}{\code{hexagon_biome.shp}, \code{Invader.by.biome.[type].png}, \code{biome_invader.[type].pdf}}
#'   }
```

<a id="fig-19"></a>
## [19] N.Species.Dispersal.by.seed.end.NULL.r

```r
#' @file N.Species.Dispersal.by.seed.end.NULL.r
#' @title 空模型下末期种源扩散与留存物种数统计
#' @description 针对静态气候空模型，分析每个初始种子在最终时间步（year = 0）留在原产大陆（in_source_continent）
#'   与扩散至目标大陆（to_target_continent）的物种数量，执行 100 次重采样并导出汇总文档。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 模拟记录合并与大洲归属判定
#'     \item \code{ggplot2}, \code{sf}: 散点图与箱线图绘制
#'     \item \code{flextable}, \code{officer}: Word 报表导出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{sp_full_continents.NULL.rda}, \code{random.seeds.threshold.by.nb.distance.rda}}
#'     \item{输出数据}{\code{N.with.bridge.seed.continent.NULL.rda}, \code{N.Species.Dispersal.by.seed.end.rep.NULL.rda}, \code{species.2.other.continent.detail.NULL.docx}}
#'   }
```

<a id="fig-20"></a>
## [20] N.Species.Dispersal.by.seed.end.r

```r
#' @file N.Species.Dispersal.by.seed.end.r
#' @title 动态气候下末期种源扩散与留存物种数统计
#' @description 基于动态古气候模拟结果，统计每个种子演化谱系在末期留在起源大陆与成功扩散至对侧大陆的物种多样性，
#'   执行 100 轮重采样，评估气候波动驱动下的物种不对称交换量。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 高效分组汇总与谱系去重
#'     \item \code{ggplot2}, \code{sf}: 散点分布与跨大陆箱线图
#'     \item \code{flextable}, \code{officer}: 学术表格自动生成
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{sp_full_continents.rda}, \code{random.seeds.99.rda}}
#'     \item{输出数据}{\code{N.with.bridge.seed.continent.rda}, \code{N.Species.Dispersal.by.seed.end.rep.rda}, \code{species.2.other.continent.detail.docx}}
#'   }
```

<a id="fig-21"></a>
## [21] NB.Definition.r

```r
#' @file NB.Definition.r
#' @title 经验生态位宽度等级定义（排除翼手目）
#' @description 基于 IUCN 陆生哺乳动物分布区数据，在排除高扩散力的翼手目（Chiroptera）后，
#'   计算温度与降水现实生态位跨度的分位数（20%、40%、60%、80%），定义 TINY、NARROW、MODERATE 与 BROAD 四档生态位宽度等级。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 分位数计算与物种分类信息匹配
#'     \item \code{sf}: IUCN 空间多边形属性读取
#'     \item \code{ggplot2}: 生态位跨度直方图与截断线绘制
#'     \item \code{flextable}, \code{officer}: 导出定义阈值表
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{MAMMALS_TERRESTRIAL_ONLY.shp}, \code{nb_range_mammals_iucn.rda}}
#'     \item{输出数据}{\code{no.bat.nb.csv}, \code{NB.pdf}, \code{NB.docx}, \code{IUCN.NB.csv}}
#'   }
```

<a id="fig-22"></a>
## [22] net.diversity.null.r

```r
#' @file net.diversity.null.r
#' @title 空模型下净物种分化速率与滑动窗口分析
#' @description 采用 50 kyr 滑动窗口计算静态气候空模型下各时段的净分化速率、物种丰富度以及成种/灭绝/扩散速率，
#'   构建广义线性模型（GLM）与多元线性回归，探讨无气候扰动下的基准分化驱动机制。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 滑动时间窗口划分与速率向量化计算
#'     \item \code{ggplot2}: LOESS 平滑拟合曲线绘制
#'     \item \code{lme4}, \code{broom.mixed}: 回归模型拟合与系数提取
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{sp_full_continents.NULL.rda}, \code{N.Extinction.NULL.rda}, \code{N.Speciation.NULL.rda}, \code{N.Dispersal.NULL.rda}}
#'     \item{输出数据}{\code{Net.Diversification.Rate.continent.NULL.pdf}, \code{Net.Extinction.continent.NULL.pdf} 等}
#'   }
```

<a id="fig-23"></a>
## [23] net.diversity.r

```r
#' @file net.diversity.r
#' @title 动态气候下净物种分化速率与宏观驱动力检验
#' @description 基于 50 kyr 窗口追踪动态古气候下南北美洲本土与迁入物种的净分化速率时序演变，
#'   拟合多元线性回归模型量化成种率、局部绝灭率、初级/次级扩散率对净分化速率的贡献大小。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 时序滞后差分（\code{shift}）与大表快速合并
#'     \item \code{ggplot2}: 净分化速率平滑轨迹与模型效应量森林图
#'     \item \code{lme4}, \code{broom.mixed}: 回归建模与置信区间提取
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{sp_full_continents.rda}, \code{N.Extinction.rda}, \code{N.Speciation.rda}, \code{N.Dispersal.rda}}
#'     \item{输出数据}{\code{Net.Diversification.Rate.continent.pdf}, \code{Net.Diversification.Rate.continent.csv} 等}
#'   }
```

<a id="fig-24"></a>
## [24] Null.Model.Movies.r

```r
#' @file Null.Model.Movies.r
#' @title 逐年空间分布对比地图切片生成（动画制作）
#' @description 针对特定模拟情景，提取各时间步在静态无变化（No Change）、动态气候变化（Change）
#'   以及无成种空模型（No Speciation）下的网格物种分布，生成连续年份的对比地图切片（PNG），用于制作演化动态视频。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 逐年网格数据抽取
#'     \item \code{ggplot2}, \code{sf}, \code{patchwork}: 多模型并排地图渲染
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{不同模型目录下的单次运行日志 \code{.log}, \code{N_S_America.shp}}
#'     \item{输出数据}{\code{../Figures/Null.Example/By.year/[year].png}}
#'   }
```

<a id="fig-25"></a>
## [25] number.check.r

```r
#' @file number.check.r
#' @title 论文正文关键统计数值与样本量核验
#' @description 校验论文正文提及的关键参数与模拟数据量，包括陆地六边形网格总数（5,449 格）、
#'   通过预热期（burn-in, year = 1801）存活的模拟数（5,610 次）以及各 NB/DA 分组中存活至末期的种子数量与比例。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 多时序节点物种存活量过滤与统计
#'     \item \code{sf}: 陆地网格数量核实
#'     \item \code{flextable}, \code{officer}: 导出核验附表
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{N_S_America.shp}, \code{N.Speciation.Extinction.All.NB.rda}}
#'     \item{输出数据}{\code{survived_N.docx}, \code{N.Final.docx}}
#'   }
```

<a id="fig-26"></a>
## [26] outlier.contribution.r

```r
#' @file outlier.contribution.r
#' @title 极端高物种形成离群种源逐步剔除敏感性测试
#' @description 按最终产出物种数对南美洲种子降序排序，依次逐步剔除前 1 至 10 个极端离群种子（Outliers），
#'   在每个剔除梯度下重复 100 次抽样并统计宏演化事件，检验大迁徙不对称性是否受少数极端超级繁盛谱系主导。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 种子累积贡献率排序与循环过滤
#'     \item \code{ggplot2}, \code{patchwork}: 步进剔除敏感性纵向大图拼版
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{N.Speciation.Extinction.Dispersal.rda}, \code{random.seeds.threshold.by.nb.distance.50.rda}, \code{outliers.1801.rda}}
#'     \item{输出数据}{\code{outlier.contributions.png}}
#'   }
```

<a id="fig-27"></a>
## [27] packages.usage.r

```r
#' @file packages.usage.r
#' @title 递归扫描代码库依赖包并输出规范文献引用格式
#' @description 正则扫描指定项目目录下的全部 R 脚本，自动提取 `library()`、`require()` 以及 `pkg::` 语法引用的所有扩展包，
#'   匹配本机已安装版本并打印输出标准的学术引用条目。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item 本地 R 基础环境（无需第三方依赖）
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{项目目录下全部 \code{.R} 源码文件}
#'     \item{输出数据}{控制台输出格式化的包版本清单与 BibTeX/文本引用}
#'   }

#' @title 正则提取单个 R 脚本依赖包名称
#' @description 读取 R 代码文件并去除注释，正则匹配所有的库引用与命名空间函数调用。
#' @param file_path character。R 脚本文件路径。
#' @return character。去重后的依赖 R 包名称向量。
#' @noRd
extract_packages <- function(file_path) {}
```

<a id="fig-28"></a>
## [28] prec_cutoff.r

```r
#' @file prec_cutoff.r
#' @title 降水生态位下限截断效应评估与空间制图
#' @description 识别模拟参数中因降水下限计算为负值而被模型截断为 0（`pr_low < 0`）的模拟种子，
#'   统计受截断影响的种源频次与大陆分布，并在地图上呈现降水截断深度的空间分布格局。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 参数字符串解构（\code{tstrsplit}）与截断统计
#'     \item \code{sf}, \code{ggplot2}: 空间六边形网格截断量渐变图
#'     \item \code{RSQLite}, \code{DBI}: 数据库读取
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{conf.null.sqlite}, \code{cells.with.dist.rda}, \code{random.seeds.threshold.by.nb.distance.rda}}
#'     \item{输出数据}{\code{Prec_Cutoff.pdf}, \code{Prec_Cutoff.png}}
#'   }
```

<a id="fig-29"></a>
## [29] Range.r

```r
#' @file Range.r
#' @title 模拟物种地理分布区规模与实证 IUCN 数据对比
#' @description 汇总动态模拟、空模型以及经验 IUCN 哺乳动物的地理分布范围大小（网格占用单元数），
#'   绘制概率密度曲线与经验累积分布函数（eCDF），并通过 Kolmogorov-Smirnov 检验（ks.test）评估空间拟合优度。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 分布区网格数汇总
#'     \item \code{ggplot2}, \code{patchwork}: 密度曲线与累积概率图拼图
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{Species.Range.rda}, \code{Species.Range.NULL.rda}, \code{Mammals.World.rda}}
#'     \item{输出数据}{\code{Range.pdf}, \code{Range.png}, 控制台 KS 检验统计量}
#'   }
```

<a id="fig-30"></a>
## [30] Richness.r

```r
#' @file Richness.r
#' @title 全局丰富度格局制图与纬度梯度实证比对
#' @description 整合真实模拟、空模型、预热期以及实证 IUCN 哺乳动物多样性数据，绘制具有统一美学标准的美洲物种丰富度空间地图，
#'   并沿 1 纬度带计算纬度多样性梯度（LDG）实证对比曲线。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{sf}: 哺乳动物多边形与六边形网格求交（\code{st_intersects}）、纬度切线构建
#'     \item \code{data.table}: 丰富度均值与标准差聚合
#'     \item \code{ggplot2}, \code{patchwork}: 出版级四面板地图与双轴 LDG 图
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{Final.Distribution.Unique.rda}, \code{Final.Distribution.NULL.Unique.rda}, \code{Burn.in.Distribution.rda}, IUCN 哺乳动物图层}
#'     \item{输出数据}{\code{Figure.Richness.pdf}, \code{Final.Richness.rda}, \code{IUCN.dis.rda}, \code{IUCN.lat.rda}}
#'   }

#' @title 创建指定纬度的跨球水平线段
#' @description 构建用于空间相交统计的跨经度水平线几何对象（sf linestring）。
#' @param lat numeric。纬度数值。
#' @return sfg。水平线段空间要素几何。
#' @noRd
create_lat_line <- function(lat) {}
```

<a id="fig-31"></a>
## [31] Richness.Stat.r

```r
#' @file Richness.Stat.r
#' @title 最终物种丰富度网格统计量计算（独立模块）
#' @description 独立提取模拟最终步在各个网格上的物种去重记录，跨 100 次重采样计算各网格单元的物种丰富度均值与标准差。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 大规模分布数据去重与逐网格均值统计
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{Final.Distribution.Unique.rda}, \code{random.seeds.threshold.by.nb.distance.rda}}
#'     \item{输出数据}{\code{Final.Richness.rda}}
#'   }
```

<a id="fig-32"></a>
## [32] sample.seed.by.nb.5.degree.r

```r
#' @file sample.seed.by.nb.5.degree.r
#' @title 5 步长距离分段分层种子抽样
#' @description 将种源到对侧大陆的最短拓扑距离划分为步长为 5 的距离段，在每个距离段内对南北美洲的种子执行对称配对随机抽样（每段最多 10 个），
#'   生成空间距离代表性均衡的 100 组 Bootstrap 抽样种子集。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 拓扑步长离散化与分层抽样
#'     \item \code{sf}: 种子坐标关联
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{seeds.rda}, \code{cells.with.dist.rda}, \code{N.Speciation.Extinction.All.NB.rda}}
#'     \item{输出数据}{\code{random.seeds.by.nb.distance.5.rda}}
#'   }
```

<a id="fig-33"></a>
## [33] sample.seed.by.nb.no.outliers.r

```r
#' @file sample.seed.by.nb.no.outliers.r
#' @title 剔除离群值后的严格距离配对种子分层抽样
#' @description 在排除极端爆发分化种子的基础上，针对南北美洲在相同拓扑距离、生态位宽度和扩散能力下的种源执行对称配对抽样（每格最多 5 个），
#'   构建稳健性检验专用的 100 组重采样种子库。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 多维精确匹配与分层平衡抽样
#'     \item \code{flextable}, \code{officer}: 抽样利用率统计 Word 文档生成
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{seeds.rda}, \code{cells.with.dist.rda}, \code{N.Speciation.Extinction.All.NB.rda}}
#'     \item{输出数据}{\code{random.seeds.threshold.by.nb.distance.no.outliers.rda}, \code{propotion.of.seed.usage.no.outliers.docx}}
#'   }
```

<a id="fig-34"></a>
## [34] sample.seed.by.nb.r

```r
#' @file sample.seed.by.nb.r
#' @title 多分位数阈值（90% 至 100%）下种子分层抽样主流程
#' @description 遍历 90% 到 100%（步长 1%）的分化规模分位数截断阈值，对未超限的种子按距离、NB 和 DA 进行对称配对抽样，
#'   生成用于敏感性评估的系列种子表，并输出离群值分布图与统计表。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 分位数计算与多轮分层循环抽样
#'     \item \code{ggplot2}: 离群值截断直方图与分面图
#'     \item \code{flextable}, \code{officer}: 离群值清单 Word 导出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{seeds.rda}, \code{cells.with.dist.rda}, \code{N.Speciation.Extinction.All.NB.rda}}
#'     \item{输出数据}{\code{random.seeds.threshold.by.nb.distance.[threshold].rda}, \code{Outlier.distribution.[threshold].pdf} 等}
#'   }
```

<a id="fig-35"></a>
## [35] sample.seed.by.nb.test.r

```r
#' @file sample.seed.by.nb.test.r
#' @title 种子逐个剥离抽样检验与事件敏感性评估
#' @description 针对分化规模排名前列的离群种源，依次剔除 1 至 48 个种子，在每个削减水平下重新执行分层重采样并重算宏演化事件指标，
#'   以曲线图展示迁入物种占比随离群种子剔除数量增加的响应趋势。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 嵌套循环重采样与事件指标快速计算
#'     \item \code{ggplot2}, \code{patchwork}: 敏感性响应趋势拼版
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{seeds.rda}, \code{cells.with.dist.rda}, \code{N.Speciation.Extinction.Dispersal.rda}}
#'     \item{输出数据}{\code{../Data/Tables/outliers/random.seeds.threshold.by.nb.distance.[j].rda}}
#'   }
```

<a id="fig-36"></a>
## [36] sample.seed.random.r

```r
#' @file sample.seed.random.r
#' @title 随机种子抽样（剔除 BIG 类群前 5 繁盛支系）
#' @description 针对成种能力极强的 "BIG" 生态位宽度组，剔除物种形成数最高的前 5 个超级繁盛种子，
#'   在每个 continent/NB/DA 组合中完全随机抽取 40 个种子，生成 100 组无距离平衡限制的随机对照种子集。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 顶端支系排序剔除与无权重随机抽样
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{seeds.rda}, \code{N.Speciation.Extinction.All.NB.rda}}
#'     \item{输出数据}{\code{random.seeds.100.remove.top5.big.rda}}
#'   }
```

<a id="fig-37"></a>
## [37] Seed.Bootstrap.R

```r
#' @file Seed.Bootstrap.R
#' @title 抽样种子拓扑距离分布与空间利用频次可视化
#' @description 统计重采样种子库在南北美洲沿最短距离维度的分布直方图，计算各网格单元在 100 次抽样中的重复使用频次，
#'   利用双重填充色标（`ggnewscale`）在地图上呈现距离梯度与抽样热点。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 抽样利用率统计
#'     \item \code{sf}, \code{ggplot2}, \code{ggnewscale}: 空间地图双色阶叠加展示
#'     \item \code{flextable}, \code{officer}: 抽样样本统计 Word 导出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{cells.with.dist.rda}, \code{random.seeds.threshold.by.nb.distance.rda}}
#'     \item{输出数据}{\code{Seed.Bootstrap.pdf}, \code{Seed.Bootstrap.png}, \code{Seed.Bootstrap.docx}}
#'   }
```

<a id="fig-38"></a>
## [38] Seed.Overview.r

```r
#' @file Seed.Overview.r
#' @title 模拟种源全局状态与起始古气候综合图版
#' @description 制作论文主图全局概览图版：整合种源最终演化状态（未存活、纳入候选池、离群值、背景网格），
#'   叠加起始年代（3.6 Ma，year = 1800）的年均降水、最高温、最低温空间分布以及长时间气候波动轨迹。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{sf}, \code{ggplot2}, \code{patchwork}: 空间要素与多环境变量地图排版
#'     \item \code{RSQLite}, \code{DBI}: 环境数据库直连
#'     \item \code{data.table}: 种子状态合并
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{configuration.sqlite}, \code{seeds.rda}, \code{N.Speciation.Extinction.All.NB.rda}, \code{Climate_curve.rda}}
#'     \item{输出数据}{\code{Overview.pdf}, \code{Overview.png}}
#'   }
```

<a id="fig-39"></a>
## [39] Seed.Status.r

```r
#' @file Seed.Status.r
#' @title 种源预热期生存状态核验与耐受参数评估
#' @description 提取种子参数中定义的降水和温度生理耐受阈值（`nb_v`），比对预热期起始（year = 1800）的实际环境，
#'   系统分类并统计未能存活通过预热期的种子数量及原因，保存种子池基准数据表。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: 生态位区间生理适生性逻辑判断
#'     \item \code{sf}, \code{ggplot2}: 种子状态空间图
#'     \item \code{RSQLite}, \code{DBI}: 环境与配置库访问
#'     \item \code{flextable}, \code{officer}: 存活状态统计报表生成
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{configuration.sqlite}, \code{conf.sqlite}, \code{seeds.rda}}
#'     \item{输出数据}{\code{Seed.Pool.rda}, \code{Seeds.pdf}, \code{no.surveve.docx}, \code{bootstraping.seeds.docx}}
#'   }
```

<a id="fig-40"></a>
## [40] Simulation.Mask.r

```r
#' @file Simulation.Mask.r
#' @title 研究区空间掩膜与大洲/陆桥网格划分图
#' @description 绘制模拟系统的地理范围多边形，区分并填色标注北美洲、南美洲、巴拿马地峡（bridge1）与加勒比通道（bridge2），
#'   统计各区域包含的有效六边形网格数并导出 Word 报表。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{sf}, \code{ggplot2}: 空间六边形网格分类填色制图
#'     \item \code{data.table}: 各大陆单元频次统计
#'     \item \code{flextable}, \code{officer}: 掩膜统计 Word 导出
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{cells.with.dist.rda}}
#'     \item{输出数据}{\code{Figure.Mask.pdf}, \code{Figure.Mask.png}, \code{Figure.Mask.docx}}
#'   }
```

<a id="fig-41"></a>
## [41] why.seeds.dispersed.r

```r
#' @file why.seeds.dispersed.r
#' @title 种源成功跨大陆扩散的二项式 GLM 驱动力分析
#' @description 建立二项分布广义线性模型（Logistic Regression, `glm(family = binomial)`），
#'   以是否成功跨越大陆建立稳定种群为因变量，以距陆桥最短拓扑距离（`min.dist`）、生态位宽度（`NB`）和扩散能力（`DA`）为自变量，
#'   使用 `ggeffects` 计算边际预测概率并绘制效应曲线。
#' @author Huijie Qiao
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ggeffects}: 广义线性模型边际预测概率估计（\code{ggpredict}）
#'     \item \code{ggplot2}, \code{patchwork}: 边际效应曲线与分面组合图
#'     \item \code{data.table}: 属性关联与因子类型转换
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{输入数据}{\code{N.with.bridge.simulation.rda}, \code{cells.with.dist.rda}}
#'     \item{输出数据}{\code{Seed.Dispersal.GLM.pdf}, \code{Seed.Dispersal.GLM.png}, 控制台模型 summary}
#'   }
```