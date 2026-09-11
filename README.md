[English](README.md) | [简体中文](README_zh.md)

# Project Name
Macroecological and Macroevolutionary Simulations of the Great American Biotic Interchange (GABI)

# Project Overview and File Index

This project contains complete code documentation for spatially explicit eco-evolutionary simulations of the Great American Biotic Interchange (GABI). The codebase is organized into three modules: 
**Configuration** (environmental data construction and configuration management), 
**Analysis** (processing simulation output logs and intermediate data), and 
**Figures** (figure generation, hypothesis testing, and reporting).

### 1. Configuration Module
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


### 2. Analysis Module
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

### 3. Figures Module
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

# Module 1: Configuration Module, used to generate environmental layers, virtual species parameters, and configuration files required for simulations, as well as monitor simulation progress.

<a id="config-01"></a>
## [1] 1.raw_env_info.r

```r
#' @file 1.raw_env_info.r
#' @title Extract Palaeoclimate Metadata from NetCDF
#' @description Extracts metadata from NetCDF files and exports a unified metadata dictionary.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ncdf4}: NetCDF attribute reading
#'     \item \code{data.table}: Summary table construction
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{NetCDF climate layers \code{.nc}, \code{name_year.csv}}
#'     \item{Output data}{\code{meta_table.csv}, \code{meta_table.rda}}
#'   }
```

<a id="config-02"></a>
## [2] 2.env_mean_sd.r

```r
#' @file 2.env_mean_sd.r
#' @title Extract Palaeoclimate Variables
#' @description Traverses variables across palaeoclimate NetCDF files and builds climate datasets for simulations.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ncdf4}: Variable matrix extraction
#'     \item \code{data.table}: Data formatting and organization
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{meta_table.rda}, raw NetCDF files}
#'     \item{Output data}{\code{env_fullstat.rda}}
#'   }
```

<a id="config-03"></a>
## [3] 3.fig.env_mean_sd.r

```r
#' @file 3.fig.env_mean_sd.r
#' @title Plot Palaeoclimate Variable Mean Curves
#' @description Calculates global means of climatic factors across time steps and plots multi-panel time-series curves.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Statistical ranking and time-series differencing
#'     \item \code{ggplot2}: Multi-panel time-series visualization
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{env_fullstat.rda}}
#'     \item{Output data}{Time-series curves on console or graphics device}
#'   }
```

<a id="config-04"></a>
## [4] 4.extract_values.r

```r
#' @file 4.extract_values.r
#' @title Palaeoclimate Raster Rotation, Imputation of Missing Values, and Resampling
#' @description Corrects NetCDF longitude coordinates (0–360 to -180–180), interpolates missing time steps via arithmetic averaging, and resamples to 1° × 1° GeoTIFF raster stacks.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{terra}: Raster rotation, resampling, and export
#'     \item \code{data.table}: Time-step index matching
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{meta_table.rda}, raw NetCDF files}
#'     \item{Output data}{\code{Rough.3.75x2.5/*.tif}, \code{Fine.1x1/*.tif}}
#'   }
```

<a id="config-05"></a>
## [5] 5.env_2_hexagon.r

```r
#' @file 5.env_2_hexagon.r
#' @title Discrete Hexagonal Grid System Construction and Climate Database Ingestion
#' @description Generates a global ISEA3H8 hexagonal discrete global grid, labels the Americas and land bridges, builds a topological adjacency list, resamples 1° palaeoclimate rasters, and generates the database used for simulations.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{dggridR}: Generation of ISEA3H8 discrete global grid system
#'     \item \code{sf}, \code{terra}: Spatial topological intersection and point extraction
#'     \item \code{RSQLite}, \code{DBI}: Database persistence and storage
#'     \item \code{furrr}, \code{data.table}: Parallel topological query
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{continent.shp}, \code{bridges.shp}, \code{Fine.1x1/*.tif}}
#'     \item{Output data}{\code{N_S_America.shp}, \code{configuration.sqlite}}
#'   }

#' @title Retrieve Adjacent Hexagonal Cells and Generate Adjacency Matrix for Simulation Dispersal Grid Calculation
#' @description Retrieves adjacent cell IDs topologically connected to each hexagonal grid cell.
#' @param hexagons sf. Hexagonal grid layer.
#' @return tibble. Contains grid cell index and corresponding neighbor ID vectors.
#' @noRd
find_connected_hexagon <- function(hexagons) {}
```

<a id="config-06"></a>
## [6] 6.iucn_niche_breadth.r

```r
#' @file 6.iucn_niche_breadth.r
#' @title Quantify Realized Climatic Niche Breadth of American Mammals Based on IUCN
#' @description Extracts IUCN terrestrial mammal geographic ranges, intersects them with present-day climate, calculates temperature and precipitation niche breadths, and provides empirical reference parameters for virtual species niche breadths.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{sf}: Spatial containment query (\code{st_contains})
#'     \item \code{data.table}: Quantile and variance statistics of environmental variables
#'     \item \code{RSQLite}, \code{DBI}: Climate grid database queries
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{MAMMALS_TERRESTRIAL_ONLY.shp}, \code{configuration_continent.sqlite}}
#'     \item{Output data}{\code{Mammals.World.rda}, \code{nb_range_mammals_iucn.rda}}
#'   }
```

<a id="config-07"></a>
## [7] 7.create_virtual_species.r

```r
#' @file 7.create_virtual_species.r
#' @title Generate Virtual Species Parameters and Configuration Files for Simulations
#' @description Defines four niche breadth (NB) and two dispersal ability (DA) categories based on IUCN realized niche quantiles, samples initial seed cells, and exports configuration databases required by the simulation engine.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Cartesian product combinations and formatting
#'     \item \code{RSQLite}, \code{DBI}: Configuration database writing
#'     \item \code{sf}: Spatial sampling
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{nb_range_mammals_iucn.rda}, \code{N_S_America.shp}, \code{configuration_continent.sqlite}}
#'     \item{Output data}{\code{conf.sqlite}, \code{configuration.sqlite}}
#'   }
```

<a id="config-08"></a>
## [8] 8.example.r

```r
#' @file 8.example.r
#' @title Interactive Simulation Debugging and Visualization for Inspecting Simulation Outcomes
#' @description Provides interactive terminal control to step through and display the spatio-temporal dispersal dynamics of species across hexagonal grid cells under specified parameter combinations.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Log reading and filtering
#'     \item \code{ggplot2}, \code{sf}: Stepwise spatial dynamic mapping
#'     \item \code{RSQLite}, \code{DBI}: Environmental value inspection
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{Simulation log \code{.log}, \code{N_S_America.shp}}
#'     \item{Output data}{Terminal interactive spatial maps}
#'   }
```

<a id="config-09"></a>
## [9] 9.remove.unfinished.r

```r
#' @file 9.remove.unfinished.r
#' @title Batch Cleanup of Incomplete Simulation Output Directories
#' @description Inspects simulation output directories and safely removes incomplete simulation runs.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: File system path filtering
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{Null model simulation directory \code{Results_NULL}}
#'     \item{Output data}{Console deletion records and released storage space}
#'   }
```

<a id="config-10"></a>
## [10] 10.create.null.env.r

```r
#' @file 10.create.null.env.r
#' @title Construct Null Model Static Environmental Database for Null Simulations
#' @description Replicates baseline 3.6 Ma environmental layers across all time steps to generate a climate-stable null control database and execution configurations.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{RSQLite}, \code{DBI}: Database overwrite and reconstruction
#'     \item \code{data.table}: Fast cloning of large environmental tables
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{configuration.sqlite}, \code{conf.sqlite}, seed list}
#'     \item{Output data}{\code{null.sqlite}, \code{conf.null.sqlite}}
#'   }
```

# Module 2: Analysis Module, used to analyze simulation logs and generate intermediate results for subsequent analysis and visualization

<a id="analysis-01"></a>
## [1] compare.3vs4.r

```r
#' @file compare.3vs4.r
#' @title Comparative Analysis of Simulation Results (Version 3.x vs 4.0). Note: v4.0 is under development; current analyses are based on v3.9.
#' @description Compares final-step (year = 0) predicted species distributions of GABI simulations between 3.x (log format) and 4.0 (RDS format),
#'   calculating grid overlap rates, Jaccard similarity indices, and richness correlation coefficients.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Data merging and fast filtering
#'     \item \code{RSQLite}, \code{DBI}: Configuration database reading
#'     \item \code{sf}, \code{ggplot2}: Spatial grid overlap visualization
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{conf.sqlite}, 3.x simulation log \code{.log}, 4.0 distribution data \code{distribution.rda}, \code{N_S_America.shp}}
#'     \item{Output data}{\code{../Data/Tables/3.5vs4.rda}}
#'   }
```

<a id="analysis-02"></a>
## [2] distanct.2.another.continent.r

```r
#' @file distanct.2.another.continent.r
#' @title Calculate Shortest Topological Distance from Discrete Hexagonal Grid Cells to Target Continent
#' @description Constructs a graph structure from the hexagonal adjacency list and applies breadth-first search (BFS) to compute the shortest
#'   topological step distance from all terrestrial grid cells in North and South America to the opposing continent / land bridge corridors.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Adjacency matrix and topological node indexing
#'     \item \code{sf}, \code{ggplot2}: Grid distance choropleth mapping
#'     \item \code{RSQLite}, \code{DBI}: Reading grid adjacency database
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{configuration.sqlite}, \code{N_S_America.shp}}
#'     \item{Output data}{\code{../Data/Tables/cells.with.dist.rda}}
#'   }

#' @title Build Graph Adjacency List
#' @description Generates an undirected graph adjacency list based on unit topological adjacency relations between grid cell pairs.
#' @param dt data.table. Adjacency list containing two integer node columns \code{i} and \code{j}.
#' @return list. List with node IDs as keys and neighbor ID vectors as values.
#' @noRd
build_adjacency_list <- function(dt) {}

#' @title Breadth-First Search (BFS) for Shortest Topological Distance
#' @description Calculates shortest topological step count between start and target nodes in an unweighted graph.
#' @param start_node integer. Starting grid cell ID.
#' @param end_node integer. Target grid cell ID.
#' @param adj_list list. Graph adjacency list.
#' @return integer. Shortest topological distance; returns -1 if unreachable.
#' @noRd
shortest_distance_bfs <- function(start_node, end_node, adj_list) {}
```

<a id="analysis-03"></a>
## [3] generate.tree.4.0.r

```r
#' @file generate.tree.4.0.r
#' @title Generate Phylogenetic Trees for the Simulation System (Version 4.0)
#' @description Traverses version 4.0 species evolutionary records, parses parent-offspring lineage topologies and persistence times, and recursively concatenates them into standard Newick-format phylogenetic trees.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Node lineage querying
#'     \item \code{RSQLite}, \code{DBI}: Reading simulation databases
#'     \item \code{ape}, \code{treeio}, \code{ggtree}: Phylogenetic tree parsing, plotting, and validation
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{conf.sqlite}, \code{species_record.rda}}
#'     \item{Output data}{\code{tree.rda} (storing Newick tree string)}
#'   }

#' @title Recursively Assemble Newick Phylogenetic Tree String
#' @description Recursively constructs subtrees and branch lengths based on parent-offspring lineages and survival intervals.
#' @param current_id character. ID of current species being processed.
#' @param data data.table. Record table containing species parentage and survival durations.
#' @return character. Newick-format tree string.
#' @noRd
build_tree_string <- function(current_id, data) {}
```

<a id="analysis-04"></a>
## [4] get.distribution.n.isthmus.r

```r
#' @file get.distribution.n.isthmus.r
#' @title Quantify Final Species Richness (Excluding Isthmus vs Merging Isthmus into North America)
#' @description Reads distribution records and matches them with continental grid cells to compare richness patterns when the Isthmus is treated separately versus merged into North America.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: High-performance grouped summaries and multithreaded computing
#'     \item \code{sf}, \code{ggplot2}: Spatial attribute matching
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{cells.with.dist.rda}, \code{final.dis.rda}}
#'     \item{Output data}{\code{Richness.with.isthmus.rda}, \code{Richness.merged.isthmus.rda}}
#'   }
```

<a id="analysis-05"></a>
## [5] get_nb.r

```r
#' @file get_nb.r
#' @title Extract Dynamics of Species Realized Niches (Temperature and Precipitation Ranges)
#' @description Integrates palaeoclimate databases and distribution logs to dynamically calculate environmental extremes and ranges across grid cells occupied by each species across simulation intervals.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Annual grid environmental matching and aggregation
#'     \item \code{RSQLite}, \code{DBI}: Direct database connection
#'     \item \code{ggplot2}, \code{ggh4x}: Niche histograms and trajectory plotting
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{configuration.sqlite}, simulation log \code{.log}}
#'     \item{Output data}{\code{nb.rda} (per-task and global summary tables)}
#'   }
```

<a id="analysis-06"></a>
## [6] get_range.r

```r
#' @file get_range.r
#' @title Extract Final Geographic Ranges and Boundary Envelopes of Species
#' @description Extracts suitable grid cell distributions for extant species at the end of simulations, computing number of occupied cells and bounding boxes (longitude/latitude).
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Extreme value calculations and coordinate aggregation
#'     \item \code{sf}, \code{ggh4x}: Spatial data utilities
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{.log}, \code{cells.with.dist.rda}}
#'     \item{Output data}{\code{final.dis.rda}, \code{range.rda}, \code{Species.Range.NULL.rda}}
#'   }
```

<a id="analysis-07"></a>
## [7] get_range_burn_in.r

```r
#' @file get_range_burn_in.r
#' @title Extract Species Distributions at the End of Burn-in Period
#' @description Extracts realized grid cell distributions of species at the end of the burn-in equilibrium period (year = 1801) from simulation logs as a pre-evolutionary baseline.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: High-efficiency filtering and export of large log files
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{Simulation run log \code{.log}}
#'     \item{Output data}{\code{dis.burn.in.1801.1.rda}, \code{Burn.in.Distribution.rda}}
#'   }
```

<a id="analysis-08"></a>
## [8] get_stat.3.x.r

```r
#' @file get_stat.3.x.r
#' @title Quantify Grid Cell Occupancy and Species Diversity Patterns (Version 3.x)
#' @description Parses 3.x simulation logs to generate spatio-temporal datasets of annual continental distributions and cell-level species richness.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Multi-dimensional grouped aggregation and sorting
#'     \item \code{sf}: Continental layer attribute association
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{.log}, \code{N_S_America.shp}}
#'     \item{Output data}{\code{continent_n_cells.rda}, \code{species.richness.rda}}
#'   }
```

<a id="analysis-09"></a>
## [9] get_stat.4.0.r

```r
#' @file get_stat.4.0.r
#' @title Quantify Grid Cell Occupancy and Species Diversity Patterns (Version 4.0)
#' @description Analyzes list-formatted distribution data in version 4.0 to calculate species continental distributions and cell-level species richness.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Cross-interval list merging and spatial statistics
#'     \item \code{RSQLite}, \code{DBI}: Configuration database queries
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{conf.sqlite}, \code{distribution.rda}, \code{N_S_America.shp}}
#'     \item{Output data}{\code{continent_n_cells.rda}, \code{species.richness.rda}}
#'   }
```

<a id="analysis-10"></a>
## [10] lat.gradient.r

```r
#' @file lat.gradient.r
#' @title Spatio-Temporal Analysis of the Latitudinal Diversity Gradient (LDG)
#' @description Bins American hexagonal grid cells into 1° latitudinal bands (excluding land bridges) and tracks the temporal dynamics of species richness across latitudes through the simulation.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Coordinate binning and aggregation
#'     \item \code{ggplot2}, \code{ggh4x}: Plotting latitudinal diversity curves
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{cells.with.dist.rda}, \code{distribution.rda} or \code{.log}}
#'     \item{Output data}{\code{lat.N.1degree.without.bridges.rda}, \code{Lat.N.1defree.without.bridges.NULL.rda}}
#'   }
```

<a id="analysis-11"></a>
## [11] lat.low.high.NULL.r

```r
#' @file lat.low.high.NULL.r
#' @title Compare Native and Immigrant Species Diversity Across Low and High Latitudes under Null Model
#' @description Bins American hexagonal grid cells into 1° latitudinal bands (excluding land bridges) to quantify latitudinal richness dynamics under the null model.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Resampling aggregation and ordering
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{Final.Distribution.NULL.Unique.rda}, \code{cells.with.dist.rda}, \code{random.seeds.threshold.by.nb.distance.rda}}
#'     \item{Output data}{\code{Lat.low.high.without.bridges.NULL.rda}, \code{Lat.low.mid.high.without.bridges.NULL.rda}}
#'   }
```

<a id="analysis-12"></a>
## [12] lat.low.high.r

```r
#' @file lat.low.high.r
#' @title Latitudinal Patterns of Native and Immigrant Species Diversity Across Low and High Latitudes under Dynamic Climate
#' @description Quantifies diversity proportions of native versus immigrant species across low, mid, and high latitudinal bands under dynamic climate simulations to test for dispersal and diversification asymmetry.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Large-scale matrix merging and faceted summaries
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{Final.Distribution.Unique.rda}, \code{cells.with.dist.rda}, sampled seeds table}
#'     \item{Output data}{\code{Lat.low.high.without.bridges.rda}, \code{Lat.low.mid.high.without.bridges.rda}}
#'   }
```

<a id="analysis-13"></a>
## [13] merge.stat.3.x.NULL.r

```r
#' @file merge.stat.3.x.NULL.r
#' @title Aggregate All 3.x Null Model Simulation Results
#' @description Traverses and merges continental occupancy and cell richness across all 3.x null simulations to generate baseline summary tables across parameter levels.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Batch row-binding and cross-dimensional aggregation
#'     \item \code{RSQLite}, \code{DBI}: Reading null model configuration database
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{conf.null.sqlite}, subtask \code{continent_n_cells.rda} and \code{species.richness.rda}}
#'     \item{Output data}{\code{virtual.species.NULL.rda}, \code{virtual.species.richness.all.NULL.rda}, etc.}
#'   }
```

<a id="analysis-14"></a>
## [14] merge.stat.3.x.r

```r
#' @file merge.stat.3.x.r
#' @title Aggregate All 3.x Dynamic Climate Simulation Results
#' @description Traverses and merges all 3.x dynamic climate simulation runs to construct complete continental occupancy tables and cell-level richness tables across niche breadth and dispersal categories.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: High-throughput data extraction and aggregation
#'     \item \code{RSQLite}, \code{DBI}: Database querying
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{conf.sqlite}, subtask summary outputs}
#'     \item{Output data}{\code{virtual.species.rda}, \code{virtual.species.richness.all.rda}, etc.}
#'   }
```

<a id="analysis-15"></a>
## [15] N.Speciation.Extinction.r

```r
#' @file N.Speciation.Extinction.r
#' @title Parse Tree Topology and Quantify Temporal Dynamics of Speciation and Extinction
#' @description Reconstructs phylogenetic tree objects, distinguishes internal nodes (speciation) from tip nodes (extinction or survival), and tallies evolutionary events across time steps from 3.6 Ma to present.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ape}, \code{phytools}, \code{geiger}: Tree parsing and node operations
#'     \item \code{data.table}, \code{stringr}: Node label text parsing
#'     \item \code{RSQLite}, \code{DBI}: Direct tree database connection
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{tree.rda} or \code{.sqlite}, \code{conf.sqlite}}
#'     \item{Output data}{\code{[sp].N.speciation.extinction.rda}, \code{N.Speciation.Extinction.All.NB.rda}}
#'   }
```

<a id="analysis-16"></a>
## [16] N.species.By.Type.with.bridge.merge.isthmus.r

```r
#' @file N.species.By.Type.with.bridge.merge.isthmus.r
#' @title Track Spatio-Temporal Colonization Types of Species (Merging Isthmus into North America)
#' @description Merges Isthmus grid cells into North America and applies state transition matrices to evaluate speciation, immigration, local extinction (extirpation), and global extinction events.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: State transition matrix construction and vectorized assignment
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{virtual.species.merge.isthmus.rda}, \code{full.combination.csv}}
#'     \item{Output data}{\code{sp_full_continents.merge.isthmus.rda}, \code{full.event.N.merge.isthmus.csv}}
#'   }
```

<a id="analysis-17"></a>
## [17] N.species.By.Type.with.bridge.r

```r
#' @file N.species.By.Type.with.bridge.r
#' @title Track Biogeographic State Transitions Across Continents and Detailed Land Bridge Corridors
#' @description Retains the Isthmus and Caribbean land bridge corridors independently to trace species dispersal, persistence, and extinction dynamics across continents throughout their lifetimes.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Large-scale spatio-temporal joins and filtering
#'     \item \code{ape}, \code{phytools}: Phylogenetic support
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{virtual.species.rda}, \code{species.type.N.rda}, \code{full.combination.csv}}
#'     \item{Output data}{\code{sp_full_continents.rda}, \code{full.event.N.csv}}
#'   }
```

<a id="analysis-18"></a>
## [18] NB.Prec.r

```r
#' @file NB.Prec.r
#' @title Parse Ecological Niches in Simulation Database
#' @description Parses ecological niche parameters from simulation databases, monitors completion status across task directories, and plots seed spatial distributions with boundary polygons.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: String deserialization and fast splitting
#'     \item \code{sf}, \code{ggplot2}: Spatial visualization on hexagonal grids
#'     \item \code{RSQLite}, \code{DBI}: Database access
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{conf.sqlite}, \code{N_S_America.shp}, simulation output directories}
#'     \item{Output data}{Console task inspection status and spatial distribution maps}
#'   }
```

<a id="analysis-19"></a>
## [19] netcdf.info.r

```r
#' @file netcdf.info.r
#' @title Inspect NetCDF Palaeoclimate File Structures
#' @description Interactively inspects single 3.6 Ma palaeoclimate NetCDF files to quickly verify variable definitions, temporal/spatial dimensions, and global attributes.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ncdf4}: NetCDF interface
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{Palaeoclimate simulation NetCDF files \code{.nc}}
#'     \item{Output data}{Console diagnostic output of file structures}
#'   }
```

<a id="analysis-20"></a>
## [20] packages.used.r

```r
#' @file packages.used.r
#' @title Collect and Export Dependent R Package Versions and Citations for Automatic Manuscript Citations
#' @description Traverses core R packages used across the project, automatically detects installed versions, extracts standard academic citations, and exports a formatted CSV summary.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Summary compilation and CSV export with BOM
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{Installed packages in local R environment}
#'     \item{Output data}{\code{../Data/Tables/r_packages_citation_summary.csv}}
#'   }

#' @title Extract Single Package Version and Literature Citation
#' @description Extracts installed version and standard academic citation for a specified R package.
#' @param pkg character. Name of R package.
#' @return data.table. Single-row data table with columns Package, Version, and Citation.
#' @noRd
get_pkg_info <- function(pkg) {}
```

<a id="analysis-21"></a>
## [21] richness.r

```r
#' @file richness.r
#' @title Generate Species Richness Time-Slice Datasets
#' @description Extracts grid cell species richness every 100 simulation time steps, merges geographic coordinates, and exports standalone time-slice datasets partitioned by year.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Fast appending and list slicing
#'     \item \code{RSQLite}, \code{DBI}: Reading task configurations
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{conf.sqlite}, per-simulation \code{species.richness.rda}, \code{cells.with.dist.rda}}
#'     \item{Output data}{\code{../Data/Tables/Yearly.Richness/[year].rda}}
#'   }
```

<a id="analysis-22"></a>
## [22] species.type.with.bridge.merge.isthmus.r

```r
#' @file species.type.with.bridge.merge.isthmus.r
#' @title Construct Lineage Topologies and Classification Characteristics (Isthmus Merged, Null Model)
#' @description Reconstructs phylogenetic tree objects (\code{phylo}) for null model data with the Isthmus merged into North America, extracts descendant nodes, and quantifies evolutionary events.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ape}, \code{phangorn}, \code{phytools}: Tree construction, branch length assignment, and descendant retrieval
#'     \item \code{data.table}: Topological matrix conversion
#'     \item \code{ggplot2}, \code{ggh4x}: Statistical distribution plotting
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{virtual.species.merge.isthmus.NULL.rda}}
#'     \item{Output data}{\code{temp.N.sp.NULL.isthmus/[label].rda}, \code{species.type.N.merge.isthmus.NULL.rda}}
#'   }

#' @title Assign Values to Specified Rows in data.table
#' @description Updates multiple column values in-place for specified rows in a data.table.
#' @param dt data.table. Target data table.
#' @param row integer. Row index.
#' @param vals list. Named list of values.
#' @return NULL.
#' @noRd
assign_named_row <- function(dt, row, vals) {}
```

<a id="analysis-23"></a>
## [23] species.type.with.bridge.NULL.r

```r
#' @file species.type.with.bridge.NULL.r
#' @title Construct Lineage Trees and Quantify Diversification and Extinction (Independent Land Bridges, Null Model)
#' @description Reconstructs phylogenetic trees for null models with separate land bridges, recursively traverses descendant nodes, and determines evolutionary survival status.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ape}, \code{phangorn}, \code{phytools}: Phylogenetic tree analysis
#'     \item \code{data.table}: High-performance data filtering
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{virtual.species.NULL.rda}}
#'     \item{Output data}{\code{temp.N.sp.NULL/[label].rda}, \code{species.type.N.NULL.rda}}
#'   }

#' @title Fast Named Assignment to Specified Rows in data.table
#' @description Updates column values in-place for a specified row index.
#' @param dt data.table. Target table.
#' @param row integer. Row index.
#' @param vals list. Named list of values.
#' @return NULL.
#' @noRd
assign_named_row <- function(dt, row, vals) {}
```

<a id="analysis-24"></a>
## [24] species.type.with.bridge.r

```r
#' @file species.type.with.bridge.r
#' @title Reconstruct Phylogenetic Trees and Classify Land Bridge Utilization under Dynamic Climate
#' @description Reconstructs species life histories as tree objects under realistic dynamic climate, tracks occupancy across land bridges and continents, and quantifies speciation, extinction, and dispersal events.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ape}, \code{phangorn}, \code{phytools}: Tree construction and subtree extraction
#'     \item \code{data.table}: High-performance multi-column operations
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{virtual.species.rda}}
#'     \item{Output data}{\code{temp.N.sp/[label].rda}, \code{species.type.N.rda}}
#'   }

#' @title Assign Named Values to Specified Rows in data.table
#' @description Updates values in-place for a target row in a data.table.
#' @param dt data.table. Target data table.
#' @param row integer. Target row index.
#' @param vals list. List containing column names and updated values.
#' @return NULL.
#' @noRd
assign_named_row <- function(dt, row, vals) {}
```

---

# Module 3: Figures Module, used for results analysis, hypothesis testing, and visualization.

<a id="fig-01"></a>
## [1] bootstrapping.analysis.r

```r
#' @file bootstrapping.analysis.r
#' @title Evaluate Robustness of Sampling Thresholds (90% to 99.5%) Based on Final Species Richness (Sensitivity Analysis)
#' @description Compares species richness and immigrant proportions across 90% to 99.5% cutoff thresholds to evaluate the influence of explosive diversification bursts on macroecological patterns.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Multi-threshold data matching
#'     \item \code{ggplot2}, \code{ggh4x}, \code{patchwork}: Sensitivity boxplots and faceted layouts
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{Quantile seed tables \code{random.seeds.*.rda}, \code{N.with.bridge.seed.continent.rda}}
#'     \item{Output data}{Sensitivity comparison figure panels}
#'   }
```

<a id="fig-02"></a>
## [2] bridge.usage.r

```r
#' @file bridge.usage.r
#' @title Land Bridge Usage Preference Analysis (Isthmus vs Caribbean Islands)
#' @description Quantifies proportions of species from North and South America that successfully crossed versus stalled across corridors, plotting mean values with error bars and exporting Word/CSV reports.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Dispersal pathway tracking and bootstrap summaries
#'     \item \code{ggplot2}: Corridor preference dot plots with error bars
#'     \item \code{flextable}, \code{officer}: Exporting formatted statistical tables
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{sp_full_continents.rda}, sampled seeds table}
#'     \item{Output data}{\code{Bridge.Usage.pdf}, \code{Bridge.Usage.docx}, \code{Bridge.Usage.csv}}
#'   }
```

<a id="fig-03"></a>
## [3] climate.curve.r

```r
#' @file climate.curve.r
#' @title Plot Macro-Palaeoclimate Curves for the Americas Since 3.8 Ma (Burn-in + 3.6 Ma)
#' @description Calculates mean and dispersion of historical precipitation, maximum temperature, and minimum temperature across continents and land bridge cells, using dual y-axes and vertical facets to present palaeoclimate trajectories.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Regional climate mean calculations
#'     \item \code{ggplot2}, \code{patchwork}: Dual-axis time-series curves
#'     \item \code{RSQLite}, \code{DBI}: Direct database queries
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{configuration.sqlite}, \code{N_S_America.shp}}
#'     \item{Output data}{\code{Climate_curve_continent.pdf}, \code{Climate_curve.pdf}}
#'   }
```

<a id="fig-04"></a>
## [4] common.r

```r
#' @file common.r
#' @title Common Plotting Themes, Palettes, and Word Table Export Utilities to Maintain Styling Consistency
#' @description Defines map projections (LAEA / Robinson), colorblind-safe palettes, and helper functions to export data.tables as publication-ready three-line Word tables.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{flextable}, \code{officer}: Word three-line table generation
#'     \item \code{data.table}: Column formatting and significant figure rounding
#'     \item \code{ggplot2}, \code{ggsci}: Plotting themes and palettes
#'     \item \code{sf}: Coordinate reference system definitions
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{Summary data.table in R memory}
#'     \item{Output data}{Standardized Word document (.docx)}
#'   }

#' @title Export data.table to Word Table Document
#' @description Formats an input table and exports it as an academic three-line table in a Word document via flextable.
#' @param summary_dt data.table. Table to export.
#' @param title character. Table title.
#' @param output_file character. Target .docx file path.
#' @param digits integer. Number of decimal places to retain, default 3.
#' @param in_place logical. Whether to modify table in-place, default FALSE.
#' @return NULL.
#' @export
to.doc <- function(summary_dt, title, output_file, digits = 3, in_place = FALSE) {}

#' @title Format Numeric Columns in data.table
#' @description Formats numeric columns uniformly to fixed decimal digits.
#' @param dt data.table. Target data table.
#' @param digits integer. Decimal places, default 3.
#' @param in_place logical. Whether to modify in-place, default TRUE.
#' @return data.table. Formatted data table.
#' @export
format_sigfigs_dt <- function(dt, digits = 3, in_place = TRUE) {}
```

<a id="fig-05"></a>
## [5] Immigrant.Propotion.Threshold.r

```r
#' @file Immigrant.Propotion.Threshold.r
#' @title Evolutionary Events and Immigrant Proportions Across Continuous Seed Truncation Thresholds (90%–100%)
#' @description Evaluates speciation, extinction, extirpation, and dispersal events across seed cutoff thresholds (90% to 100%) to test the robustness of immigrant asymmetry.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Multi-threshold resampling summaries
#'     \item \code{ggplot2}, \code{patchwork}: Multi-panel figure layouts
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{Seed tables across thresholds, \code{N.Speciation.Extinction.Dispersal.rda}, \code{N.with.bridge.seed.continent.rda}}
#'     \item{Output data}{\code{richness.by.threshold.rda}, statistical figures and Word reports}
#'   }
```

<a id="fig-06"></a>
## [6] Invader.per.r

```r
#' @file Invader.per.r
#' @title Map Spatial Patterns of Immigrant Species Proportions at Grid Scale
#' @description Calculates the proportion of immigrant species relative to total richness in hexagonal grid cells, generates spatial shapefiles, and plots continuous gradient maps of immigrant proportions.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Cell-level immigrant proportion calculations
#'     \item \code{sf}: Spatial layer export (Shapefile)
#'     \item \code{ggplot2}: Continuous gradient mapping
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{Final.Distribution.Unique.rda}, sampled seeds table, \code{N_S_America.shp}}
#'     \item{Output data}{\code{Invader.Per.shp}, \code{Invader.Per.pdf}, \code{Invader.Per.NB.DA.pdf}}
#'   }
```

<a id="fig-07"></a>
## [7] Lat.Gradient.NULL.r

```r
#' @file Lat.Gradient.NULL.r
#' @title Latitudinal Diversity Gradients and Immigrant Proportions under Null Model
#' @description Computes native and immigrant species richness across 1° latitudinal bands under static climate, utilizing inverted y-axes to display null baseline curves.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Latitudinal binning and mean calculations
#'     \item \code{ggplot2}, \code{ggh4x}: Inverted y-axis curves
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{Lat.N.1defree.without.bridges.NULL.rda}, seed tables}
#'     \item{Output data}{\code{Lat.Gradient.NULL.pdf}, \code{Lat.Gradient.Immigrant.proportion.NULL.pdf}}
#'   }
```

<a id="fig-08"></a>
## [8] Lat.Gradient.r

```r
#' @file Lat.Gradient.r
#' @title Latitudinal Diversity Gradients and Immigrant Proportions under Dynamic Climate
#' @description Calculates species richness and immigrant proportions across 1° latitudinal bands under dynamic climate, with inverted South American axes to illustrate tropical-to-temperate dispersal asymmetry.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Multi-replicate bootstrap aggregation across latitudes
#'     \item \code{ggplot2}, \code{ggh4x}: Faceted plotting with independent inverted y-axes
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{Lat.N.1defree.without.bridges.rda}, seed tables}
#'     \item{Output data}{\code{Lat.Gradient.pdf}, \code{Lat.Gradient.Immigrant.proportion.pdf}, \code{Lat.Gradient.docx}}
#'   }
```

<a id="fig-09"></a>
## [9] Lat.Low.High.r

```r
#' @file Lat.Low.High.r
#' @title Summary of Biodiversity Metrics Across Low and High Latitudes under Null Model
#' @description Summarizes mean native and immigrant richness and proportions across low and high latitudinal bands under static climate, exporting CSV benchmarks.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Statistical calculations and CSV export
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{Lat.low.high.without.bridges.NULL.rda}, \code{Lat.low.mid.high.without.bridges.NULL.rda}}
#'     \item{Output data}{\code{Lat.low.high.without.bridges.nb.da.NULL.csv}, etc.}
#'   }
```

<a id="fig-10"></a>
## [10] lat.null.analysis.r

```r
#' @file lat.null.analysis.r
#' @title Diagnostic Analysis of High-Latitude Diversity Anomalies in the Null Model
#' @description Diagnoses species clustering at high southern latitudes (<= -50°) in the null model and assesses potential geometric boundary effects.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Extreme cell filtering and hierarchical aggregation
#'     \item \code{ggplot2}, \code{scales}: Logarithmic histograms and point plots with error bars
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{Final.Distribution.NULL.Unique.rda}, \code{N_S_America.shp}}
#'     \item{Output data}{Diagnostic plots and console summary statistics}
#'   }
```

<a id="fig-11"></a>
## [11] N.Seed.Dispersal.r

```r
#' @file N.Seed.Dispersal.r
#' @title Seed-Level Cross-Continental Dispersal Success and Spatial Mapping
#' @description Tracks whether each initial seed successfully established lineages across continents during the simulation, generating spatial maps of seed dispersal capabilities across the Americas.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Matching initial seeds with final survivors
#'     \item \code{sf}, \code{ggplot2}, \code{patchwork}: Hexagonal maps and faceted boxplots
#'     \item \code{flextable}, \code{officer}: Word table export
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{sp_full_continents.rda}, \code{Seed.Pool.rda}, \code{cells.with.dist.rda}}
#'     \item{Output data}{\code{Seed.Dispersal.Map.pdf}, \code{Seed.Dispersal.boxplot.full.pdf}, \code{seed.2.other.continent.docx}}
#'   }
```

<a id="fig-12"></a>
## [12] N.Speciation.Extinction.Dispersal.distance.5.r

```r
#' @file N.Speciation.Extinction.Dispersal.distance.5.r
#' @title Diversification and Dispersal Analysis of Proximal Seeds (Distance Threshold = 5)
#' @description Re-evaluates macroevolutionary metrics for a subset of seeds located within 5 topological steps of the land bridge to verify dispersal asymmetry after controlling for distance decay.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Subset filtering and aggregation
#'     \item \code{ggplot2}, \code{patchwork}: Multi-panel boxplots
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{random.seeds.by.nb.distance.5.rda}, macroevolutionary event tables, richness tables}
#'     \item{Output data}{Boxplot panels of evolutionary events}
#'   }
```

<a id="fig-13"></a>
## [13] N.Speciation.Extinction.Dispersal.merge.isthmus.r

```r
#' @file N.Speciation.Extinction.Dispersal.merge.isthmus.r
#' @title Statistical Analysis of Macroevolutionary and Dispersal Events (Isthmus Merged into North America)
#' @description Runs 100 bootstrap replicates with the Isthmus merged into North America, quantifying native speciation, allopatric speciation, extinction, extirpation, and primary/secondary invasion events.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Event classification and bootstrap aggregation
#'     \item \code{ggplot2}, \code{patchwork}: Publication-grade multi-panel figures
#'     \item \code{flextable}, \code{officer}: Statistical table export
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{sp_full_continents.merge.isthmus.rda}, seed tables}
#'     \item{Output data}{\code{N.Speciation.Extinction.Richness.merge.isthmus.pdf}, \code{N.Richness.Per.ALL.merge.isthmus.pdf}, report documents}
#'   }
```

<a id="fig-14"></a>
## [14] N.Speciation.Extinction.Dispersal.no.outliers.r

```r
#' @file N.Speciation.Extinction.Dispersal.no.outliers.r
#' @title Macroevolutionary and Dispersal Analysis Excluding Extreme Radiation Outliers
#' @description Excludes anomalous simulations with excessive speciation bursts and re-evaluates asymmetry in speciation, extinction, and invasion dynamics.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Outlier filtering and resampling
#'     \item \code{ggplot2}, \code{patchwork}: Boxplot panels
#'     \item \code{flextable}, \code{officer}: Word document export
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{random.seeds.threshold.by.nb.distance.no.outliers.rda}, evolutionary event tables}
#'     \item{Output data}{\code{N.Speciation.Extinction.Richness.FULL.pdf}, companion Word reports}
#'   }
```

<a id="fig-15"></a>
## [15] N.Speciation.Extinction.Dispersal.NULL.r

```r
#' @file N.Speciation.Extinction.Dispersal.NULL.r
#' @title Statistical Analysis of Macroevolutionary and Dispersal Events under Null Model
#' @description Quantifies speciation, extinction, and dispersal events alongside final richness under the static climate null model to test whether geographic topology alone can induce asymmetry (sampling procedure matches [16]).
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Null model event cleaning and aggregation
#'     \item \code{ggplot2}, \code{patchwork}: Null model figure panels
#'     \item \code{flextable}, \code{officer}: Word document export
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{sp_full_continents.NULL.rda}, seed tables}
#'     \item{Output data}{\code{N.Speciation.Extinction.Richness.NULL.pdf}, null model statistical reports}
#'   }
```

<a id="fig-16"></a>
## [16] N.Speciation.Extinction.Dispersal.r

```r
#' @file N.Speciation.Extinction.Dispersal.r
#' @title Core Analysis of Macroevolutionary and Dispersal Events under Dynamic Climate
#' @description Extracts speciation, global extinction, extirpation, and invasion events across full dynamic climate simulations, performs 100 distance-constrained bootstrap replicates, and generates core figures and supplementary tables for the manuscript.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Event classification extraction and bootstrap computing
#'     \item \code{ggplot2}, \code{patchwork}: Main figure layout assembly
#'     \item \code{flextable}, \code{officer}: Publication three-line table generation
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{sp_full_continents.rda}, seed tables}
#'     \item{Output data}{\code{N.Speciation.Extinction.Richness.pdf}, \code{N.Richness.Per.pdf}, statistical reports}
#'   }
```

<a id="fig-17"></a>
## [17] N.Speciation.Extinction.Dispersal.random.seeds.r

```r
#' @file N.Speciation.Extinction.Dispersal.random.seeds.r
#' @title Random Seed Sensitivity Analysis (Excluding Top 5% Species-Rich Simulations)
#' @description Extracts speciation, extinction, extirpation, and invasion events from dynamic climate simulations and executes 100 unconstrained random bootstrap replicates.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Clade filtering and sampling
#'     \item \code{ggplot2}, \code{patchwork}: Multi-panel boxplots
#'     \item \code{flextable}, \code{officer}: Report generation
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{random.seeds.100.remove.top5.big.rda}, evolutionary event tables}
#'     \item{Output data}{Sensitivity plots and statistical tables}
#'   }
```

<a id="fig-18"></a>
## [18] N.Species.Biome.r

```r
#' @file N.Species.Biome.r
#' @title Species Richness and Immigrant Proportions Across Biomes
#' @description Intersects simulation distributions with WWF terrestrial ecoregions, quantifies native and immigrant richness within biomes, and plots biome-level immigrant proportion maps annotated with pie charts.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{sf}: Ecoregion layer simplification (st_simplify) and intersection extraction
#'     \item \code{data.table}: Biome-level species statistics
#'     \item \code{ggplot2}, \code{ggrepel}, \code{ggforce}: Pie chart annotations and spatial cartography
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{Ecoregions2017.shp}, \code{Final.Distribution.Unique.rda}, \code{cells.with.dist.rda}}
#'     \item{Output data}{\code{hexagon_biome.shp}, \code{Invader.by.biome.[type].png}, \code{biome_invader.[type].pdf}}
#'   }
```

<a id="fig-19"></a>
## [19] N.Species.Dispersal.by.seed.end.NULL.r

```r
#' @file N.Species.Dispersal.by.seed.end.NULL.r
#' @title Quantify Final Seed-Level Dispersal and Retained Species Counts under Null Model
#' @description Evaluates the number of species derived from each initial seed that remained in the source continent (in_source_continent) versus dispersed to the target continent (to_target_continent) under static climate, running 100 bootstrap replicates and exporting summary documents.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Merging simulation records and determining continental assignment
#'     \item \code{ggplot2}, \code{sf}: Scatter plots and boxplots
#'     \item \code{flextable}, \code{officer}: Word report export
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{sp_full_continents.NULL.rda}, \code{random.seeds.threshold.by.nb.distance.rda}}
#'     \item{Output data}{\code{N.with.bridge.seed.continent.NULL.rda}, \code{N.Species.Dispersal.by.seed.end.rep.NULL.rda}, \code{species.2.other.continent.detail.NULL.docx}}
#'   }
```

<a id="fig-20"></a>
## [20] N.Species.Dispersal.by.seed.end.r

```r
#' @file N.Species.Dispersal.by.seed.end.r
#' @title Quantify Final Seed-Level Dispersal and Retained Species Counts under Dynamic Climate
#' @description Quantifies extant species per seed lineage remaining in the source continent versus successfully dispersing to the target continent under dynamic palaeoclimate across 100 bootstrap replicates, assessing climate-driven asymmetric exchange.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: High-performance grouped summaries and lineage deduplication
#'     \item \code{ggplot2}, \code{sf}: Scatter distributions and cross-continental boxplots
#'     \item \code{flextable}, \code{officer}: Automated publication-ready table generation
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{sp_full_continents.rda}, \code{random.seeds.99.rda}}
#'     \item{Output data}{\code{N.with.bridge.seed.continent.rda}, \code{N.Species.Dispersal.by.seed.end.rep.rda}, \code{species.2.other.continent.detail.docx}}
#'   }
```

<a id="fig-21"></a>
## [21] NB.Definition.r

```r
#' @file NB.Definition.r
#' @title Empirical Climatic Niche Breadth Classification (Comparing Bats Included vs Excluded)
#' @description Calculates the 20th, 40th, 60th, and 80th percentiles of temperature and precipitation niche breadths from IUCN terrestrial mammal distribution data (with and without bats / Chiroptera) to define four niche breadth categories: TINY, NARROW, MODERATE, and BROAD.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Quantile calculations and taxonomic matching
#'     \item \code{sf}: IUCN polygon attribute extraction
#'     \item \code{ggplot2}: Niche breadth histograms and threshold line plotting
#'     \item \code{flextable}, \code{officer}: Threshold table export
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{MAMMALS_TERRESTRIAL_ONLY.shp}, \code{nb_range_mammals_iucn.rda}}
#'     \item{Output data}{\code{no.bat.nb.csv}, \code{NB.pdf}, \code{NB.docx}, \code{IUCN.NB.csv}}
#'   }
```

<a id="fig-22"></a>
## [22] net.diversity.null.r

```r
#' @file net.diversity.null.r
#' @title Net Diversification Rates and Rolling-Window Analysis under Null Model
#' @description Applies a 50-kyr rolling window to calculate net diversification rates, standing species richness, and speciation, extinction, and dispersal rates under the static climate null model, fitting generalized linear models (GLMs) and multiple regressions to assess baseline diversification dynamics without climate change.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Rolling time window binning and vectorized rate calculations
#'     \item \code{ggplot2}: LOESS smoothed trend curve plotting
#'     \item \code{lme4}, \code{broom.mixed}: Regression model fitting and coefficient extraction
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{sp_full_continents.NULL.rda}, \code{N.Extinction.NULL.rda}, \code{N.Speciation.NULL.rda}, \code{N.Dispersal.NULL.rda}}
#'     \item{Output data}{\code{Net.Diversification.Rate.continent.NULL.pdf}, \code{Net.Extinction.continent.NULL.pdf}, etc.}
#'   }
```

<a id="fig-23"></a>
## [23] net.diversity.r

```r
#' @file net.diversity.r
#' @title Net Diversification Rates and Macroevolutionary Drivers under Dynamic Climate
#' @description Tracks time series of net diversification rates for native and immigrant species across North and South America using a 50-kyr rolling window under dynamic palaeoclimate, fitting multiple linear regression models to quantify the relative contributions of speciation, extirpation, and primary/secondary dispersal.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Time-lag differencing (\code{shift}) and fast table merging
#'     \item \code{ggplot2}: Smoothed net diversification curves and model effect size forest plots
#'     \item \code{lme4}, \code{broom.mixed}: Regression modeling and confidence interval extraction
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{sp_full_continents.rda}, \code{N.Extinction.rda}, \code{N.Speciation.rda}, \code{N.Dispersal.rda}}
#'     \item{Output data}{\code{Net.Diversification.Rate.continent.pdf}, \code{Net.Diversification.Rate.continent.csv}, etc.}
#'   }
```

<a id="fig-24"></a>
## [24] Null.Model.Movies.r

```r
#' @file Null.Model.Movies.r
#' @title Generate Annual Spatial Distribution Comparison Map Slices (Animation / Video Production)
#' @description Extracts grid-level species distributions at each time step under static climate (No Change), dynamic climate (Change), and no-speciation null models (No Speciation), generating sequential comparative map slices (PNG) for evolutionary dynamic video generation.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Annual grid data extraction
#'     \item \code{ggplot2}, \code{sf}, \code{patchwork}: Multi-model side-by-side map rendering
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{Single run log \code{.log} under different model directories, \code{N_S_America.shp}}
#'     \item{Output data}{\code{../Figures/Null.Example/By.year/[year].png}}
#'   }
```

<a id="fig-25"></a>
## [25] number.check.r

```r
#' @file number.check.r
#' @title Verify Key Manuscript Statistics and Sample Sizes
#' @description Validates key parameters and simulation sample sizes reported in the manuscript text, including total terrestrial hexagonal cells (5,449 cells), simulations surviving the burn-in period (year = 1801; 5,610 runs), and count/proportion of seeds surviving to the simulation end across NB and DA categories.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Multi-temporal species survival filtering and statistics
#'     \item \code{sf}: Verification of terrestrial grid cell counts
#'     \item \code{flextable}, \code{officer}: Exporting validation supplementary tables
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{N_S_America.shp}, \code{N.Speciation.Extinction.All.NB.rda}}
#'     \item{Output data}{\code{survived_N.docx}, \code{N.Final.docx}}
#'   }
```

<a id="fig-26"></a>
## [26] outlier.contribution.r

```r
#' @file outlier.contribution.r
#' @title Sensitivity Analysis of Stepwise Exclusion of Outlier Seeds with Exceptionally High Speciation
#' @description Ranks South American seeds in descending order of final species richness, progressively excludes top 1 to 10 extreme outliers, repeats 100 bootstrap replicates per exclusion step, and evaluates whether the GABI asymmetry is driven by a few hyper-diversified lineages.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Cumulative contribution ranking and iterative filtering
#'     \item \code{ggplot2}, \code{patchwork}: Multi-panel vertical sensitivity plots
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{N.Speciation.Extinction.Dispersal.rda}, \code{random.seeds.threshold.by.nb.distance.50.rda}, \code{outliers.1801.rda}}
#'     \item{Output data}{\code{outlier.contributions.png}}
#'   }
```

<a id="fig-27"></a>
## [27] packages.usage.r

```r
#' @file packages.usage.r
#' @title Recursively Scan Repository Dependencies and Output Standard Citation Formats
#' @description Scans all R scripts in the project directory using regular expressions to extract extension packages imported via `library()`, `require()`, and `pkg::`, matches installed versions, and prints standard academic citation entries.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item Base R environment (no third-party dependencies required)
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{All \code{.R} source files in project directory}
#'     \item{Output data}{Console output of formatted package versions and BibTeX/plain text citations}
#'   }

#' @title Extract Dependent Package Names from Single R Script
#' @description Reads R code files, strips comments, and regex-matches library imports and namespace calls.
#' @param file_path character. Path to R script.
#' @return character. Deduplicated character vector of dependent R package names.
#' @noRd
extract_packages <- function(file_path) {}
```

<a id="fig-28"></a>
## [28] prec_cutoff.r

```r
#' @file prec_cutoff.r
#' @title Evaluation and Spatial Mapping of Precipitation Niche Truncation Effects
#' @description Identifies simulation seeds whose lower precipitation niche limit was truncated to 0 due to negative calculated bounds (`pr_low < 0`), quantifies affected seed frequencies and continental distributions, and maps the spatial patterns of truncation depth.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Parameter string parsing (\code{tstrsplit}) and truncation statistics
#'     \item \code{sf}, \code{ggplot2}: Spatial gradient mapping of hexagonal grid truncation depth
#'     \item \code{RSQLite}, \code{DBI}: Database querying
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{conf.null.sqlite}, \code{cells.with.dist.rda}, \code{random.seeds.threshold.by.nb.distance.rda}}
#'     \item{Output data}{\code{Prec_Cutoff.pdf}, \code{Prec_Cutoff.png}}
#'   }
```

<a id="fig-29"></a>
## [29] Range.r

```r
#' @file Range.r
#' @title Comparison of Simulated Geographic Range Sizes with Empirical IUCN Data
#' @description Aggregates geographic range sizes (number of occupied grid cells) across dynamic simulations, null models, and empirical IUCN mammals, plotting probability density curves and empirical cumulative distribution functions (eCDFs), and evaluating spatial goodness-of-fit via Kolmogorov-Smirnov tests (ks.test).
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Range size grid cell aggregation
#'     \item \code{ggplot2}, \code{patchwork}: Density curves and eCDF composite plots
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{Species.Range.rda}, \code{Species.Range.NULL.rda}, \code{Mammals.World.rda}}
#'     \item{Output data}{\code{Range.pdf}, \code{Range.png}, KS test statistics in console}
#'   }
```

<a id="fig-30"></a>
## [30] Richness.r

```r
#' @file Richness.r
#' @title Global Richness Pattern Mapping and Empirical Comparison with Latitudinal Diversity Gradients
#' @description Integrates dynamic simulations, null models, burn-in, and empirical IUCN mammal diversity data to generate standardized spatial richness maps across the Americas, computing empirical comparison curves along 1° latitudinal diversity gradients (LDGs).
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{sf}: Intersecting mammal polygons with hexagonal grids (\code{st_intersects}) and constructing latitude transects
#'     \item \code{data.table}: Richness mean and standard deviation aggregation
#'     \item \code{ggplot2}, \code{patchwork}: Publication-ready four-panel maps and dual-axis LDG curves
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{Final.Distribution.Unique.rda}, \code{Final.Distribution.NULL.Unique.rda}, \code{Burn.in.Distribution.rda}, IUCN mammal layers}
#'     \item{Output data}{\code{Figure.Richness.pdf}, \code{Final.Richness.rda}, \code{IUCN.dis.rda}, \code{IUCN.lat.rda}}
#'   }

#' @title Create Trans-Longitudinal Line Segment at Specified Latitude
#' @description Constructs a horizontal linestring geometry across longitudes for spatial intersection analysis.
#' @param lat numeric. Latitude coordinate value.
#' @return sfg. Horizontal line spatial geometry feature.
#' @noRd
create_lat_line <- function(lat) {}
```

<a id="fig-31"></a>
## [31] Richness.Stat.r

```r
#' @file Richness.Stat.r
#' @title Calculate Final Species Richness Grid Statistics
#' @description Extracts unique species distribution records per grid cell at the final simulation time step and computes mean and standard deviation of species richness across 100 bootstrap replicates.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Large-scale distribution deduplication and cell-level mean statistics
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{Final.Distribution.Unique.rda}, \code{random.seeds.threshold.by.nb.distance.rda}}
#'     \item{Output data}{\code{Final.Richness.rda}}
#'   }
```

<a id="fig-32"></a>
## [32] sample.seed.by.nb.5.degree.r

```r
#' @file sample.seed.by.nb.5.degree.r
#' @title Stratified Seed Sampling Binned by 5 Topological Step Distance Intervals (Sensitivity Analysis)
#' @description Bins shortest topological distances from seeds to the opposing continent into 5-step distance intervals, performs symmetric paired random sampling between North and South America within each interval (maximum 10 per interval), and generates 100 balanced bootstrap seed replicates.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Topological step discretization and stratified sampling
#'     \item \code{sf}: Seed coordinate association
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{seeds.rda}, \code{cells.with.dist.rda}, \code{N.Speciation.Extinction.All.NB.rda}}
#'     \item{Output data}{\code{random.seeds.by.nb.distance.5.rda}}
#'   }
```

<a id="fig-33"></a>
## [33] sample.seed.by.nb.no.outliers.r

```r
#' @file sample.seed.by.nb.no.outliers.r
#' @title Distance-Constrained Paired Stratified Seed Sampling Excluding Outliers
#' @description Excludes extreme explosive radiation seeds and performs symmetric paired sampling of seeds from North and South America matched by identical topological distance, niche breadth, and dispersal ability (maximum 5 per distance value), constructing 100 resampled seed sets for robustness validation.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Multi-dimensional exact matching and balanced stratified sampling
#'     \item \code{flextable}, \code{officer}: Generating Word documents of sampling utilization statistics
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{seeds.rda}, \code{cells.with.dist.rda}, \code{N.Speciation.Extinction.All.NB.rda}}
#'     \item{Output data}{\code{random.seeds.threshold.by.nb.distance.no.outliers.rda}, \code{propotion.of.seed.usage.no.outliers.docx}}
#'   }
```

<a id="fig-34"></a>
## [34] sample.seed.by.nb.r

```r
#' @file sample.seed.by.nb.r
#' @title Main Workflow for Stratified Seed Sampling Across Multiple Quantile Thresholds (90% to 100%, Sensitivity Analysis)
#' @description Iterates through diversification quantile truncation thresholds from 90% to 100% (1% increments), performs symmetric paired sampling of non-outlier seeds matched by distance, NB, and DA, generates seed tables for sensitivity assessment, and exports outlier distribution figures and statistical tables.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Quantile calculations and multi-round iterative stratified sampling
#'     \item \code{ggplot2}: Outlier truncation histograms and faceted plots
#'     \item \code{flextable}, \code{officer}: Word export of outlier lists
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{seeds.rda}, \code{cells.with.dist.rda}, \code{N.Speciation.Extinction.All.NB.rda}}
#'     \item{Output data}{\code{random.seeds.threshold.by.nb.distance.[threshold].rda}, \code{Outlier.distribution.[threshold].pdf}, etc.}
#'   }
```

<a id="fig-35"></a>
## [35] sample.seed.by.nb.test.r

```r
#' @file sample.seed.by.nb.test.r
#' @title Stepwise Seed Pruning Sampling Test and Event Sensitivity Assessment
#' @description Progressively removes top-ranked outlier seeds (1 to 50 seeds, 90% threshold), reruns stratified resampling at each pruning level, and recalculates macroevolutionary event metrics to plot response curves of immigrant proportions as a function of excluded outlier count.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Nested loop resampling and fast event metric calculations
#'     \item \code{ggplot2}, \code{patchwork}: Composite sensitivity response trend plots
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{seeds.rda}, \code{cells.with.dist.rda}, \code{N.Speciation.Extinction.Dispersal.rda}}
#'     \item{Output data}{\code{../Data/Tables/outliers/random.seeds.threshold.by.nb.distance.[j].rda}}
#'   }
```

<a id="fig-36"></a>
## [36] sample.seed.random.r

```r
#' @file sample.seed.random.r
#' @title Random Seed Sampling (Testing)
#' @description Performs random seed sampling for testing purposes.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Clade ranking, pruning, and unweighted random sampling
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{seeds.rda}, \code{N.Speciation.Extinction.All.NB.rda}}
#'     \item{Output data}{\code{random.seeds.100.remove.top5.big.rda}}
#'   }
```

<a id="fig-37"></a>
## [37] Seed.Bootstrap.R

```r
#' @file Seed.Bootstrap.R
#' @title Topological Distance Distribution and Spatial Sampling Frequency Visualization
#' @description Computes histograms of resampled seeds along shortest topological distances across North and South America, calculates reuse frequencies of individual grid cells across 100 bootstrap replicates, and maps distance gradients and sampling hotspots using dual fill color scales (`ggnewscale`).
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Sampling utilization rate calculation
#'     \item \code{sf}, \code{ggplot2}, \code{ggnewscale}: Spatial map overlay with dual color gradients
#'     \item \code{flextable}, \code{officer}: Word document export of sampling statistics
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{cells.with.dist.rda}, \code{random.seeds.threshold.by.nb.distance.rda}}
#'     \item{Output data}{\code{Seed.Bootstrap.pdf}, \code{Seed.Bootstrap.png}, \code{Seed.Bootstrap.docx}}
#'   }
```

<a id="fig-38"></a>
## [38] Seed.Overview.r

```r
#' @file Seed.Overview.r
#' @title Comprehensive Overview Panel of Simulation Seeds and Baseline Palaeoclimate
#' @description Produces the primary manuscript overview panel: integrates final seed evolutionary status (unsuccessful, candidate pool, outlier, background cells) and overlays 3.6 Ma baseline spatial layers of mean annual precipitation, maximum temperature, and minimum temperature alongside long-term climate trajectories.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{sf}, \code{ggplot2}, \code{patchwork}: Spatial features and multi-variable climate map layouts
#'     \item \code{RSQLite}, \code{DBI}: Direct environmental database querying
#'     \item \code{data.table}: Seed status table merging
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{configuration.sqlite}, \code{seeds.rda}, \code{N.Speciation.Extinction.All.NB.rda}, \code{Climate_curve.rda}}
#'     \item{Output data}{\code{Overview.pdf}, \code{Overview.png}}
#'   }
```

<a id="fig-39"></a>
## [39] Seed.Status.r

```r
#' @file Seed.Status.r
#' @title Burn-in Survival Verification and Tolerance Parameter Assessment
#' @description Extracts physiological tolerance thresholds for precipitation and temperature (`nb_v`) defined in seed parameters, compares them against empirical burn-in conditions, systematically categorizes and quantifies seeds failing to survive burn-in and their causes, and saves the baseline seed pool.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{data.table}: Logical evaluation of physiological suitability within niche intervals
#'     \item \code{sf}, \code{ggplot2}: Spatial maps of seed survival status
#'     \item \code{RSQLite}, \code{DBI}: Environmental and configuration database queries
#'     \item \code{flextable}, \code{officer}: Word summary report generation
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{configuration.sqlite}, \code{conf.sqlite}, \code{seeds.rda}}
#'     \item{Output data}{\code{Seed.Pool.rda}, \code{Seeds.pdf}, \code{no.surveve.docx}, \code{bootstraping.seeds.docx}}
#'   }
```

<a id="fig-40"></a>
## [40] Simulation.Mask.r

```r
#' @file Simulation.Mask.r
#' @title Study Area Spatial Mask and Continental/Land Bridge Grid Delineation
#' @description Maps geographic boundaries of the simulation domain, differentiating and color-coding North America, South America, Isthmus of Panama (bridge1), and Caribbean islands (bridge2), quantifying valid hexagonal cells per region, and exporting Word summary tables.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{sf}, \code{ggplot2}: Spatial hexagonal grid classification and thematic mapping
#'     \item \code{data.table}: Cell frequency counts per continent
#'     \item \code{flextable}, \code{officer}: Word table export of mask statistics
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{cells.with.dist.rda}}
#'     \item{Output data}{\code{Figure.Mask.pdf}, \code{Figure.Mask.png}, \code{Figure.Mask.docx}}
#'   }
```

<a id="fig-41"></a>
## [41] why.seeds.dispersed.r

```r
#' @file why.seeds.dispersed.r
#' @title Binomial GLM Analysis of Drivers Governing Cross-Continental Dispersal Success
#' @description Fits a binomial generalized linear model (Logistic Regression, `glm(family = binomial)`) with cross-continental dispersal success (establishment of viable populations) as the response variable and minimum topological distance to the land bridge (`min.dist`), niche breadth (`NB`), and dispersal ability (`DA`) as predictor variables, using `ggeffects` to calculate marginal predicted probabilities and plot effect curves.
#' @author Huijie Qiao & Erin Saupe
#' @date 2026-09-08
#' @section Dependencies:
#'   \itemize{
#'     \item \code{ggeffects}: Marginal predicted probability estimation for generalized linear models (\code{ggpredict})
#'     \item \code{ggplot2}, \code{patchwork}: Marginal effect curves and multi-panel composite figures
#'     \item \code{data.table}: Attribute joins and factor conversions
#'   }
#' @section Input & Output:
#'   \describe{
#'     \item{Input data}{\code{N.with.bridge.simulation.rda}, \code{cells.with.dist.rda}}
#'     \item{Output data}{\code{Seed.Dispersal.GLM.pdf}, \code{Seed.Dispersal.GLM.png}, console model summary}
#'   }
```
