library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)
library(ggforce)
library(viridis)
library(scales)

sf_use_s2(FALSE)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")

if (F){
  biome<-read_sf("../Shape/Ecoregions2017/Ecoregions2017.shp")
  biome_meter <- st_transform(biome, crs = 3035)
  biome.sim <- st_simplify(biome_meter, preserveTopology = TRUE, dTolerance = 10000)
  biome.sim.lat <- st_transform(biome.sim, crs = st_crs(biome))
  write_sf(biome.sim.lat, "../Shape/Ecoregions2017/Ecoregions2017.simpify.shp")
  biome_group <- biome_meter %>%
    group_by(BIOME_NAME, REALM) %>%
    summarise(
      geometry = st_union(geometry) 
    ) %>%
    ungroup()
  
  
  biome.lat <- st_transform(biome_group, crs = st_crs(biome))
  write_sf(biome.lat, "../Shape/Ecoregions2017/biome.shp")
  biome_group_sim<-st_simplify(biome_group, preserveTopology = TRUE, dTolerance = 10000)
  biome.lat <- st_transform(biome_group_sim, crs = st_crs(biome))
  
  write_sf(biome.lat, "../Shape/Ecoregions2017/biome.simplify.shp")
  
  
  hexagon<-readRDS("../Data/Tables/cells.with.dist.rda")
  cells<-data.table(global_id=as.numeric(hexagon$seqnum), continent=hexagon$continent,
                    lon=hexagon$lon, lat=hexagon$lat)
  biome<-read_sf("../Shape/Ecoregions2017/biome.simplify.shp")
  
  cells <- st_as_sf(
    cells, 
    coords = c("lon", "lat"), 
    crs = 4326 
  )
  
  cells.biome <- st_join(
    cells, 
    biome, 
    join = st_intersects, 
    left = F
  )
  saveRDS(cells.biome, "../Data/Tables/cells.biome.rda")
  
  cells.biome.dt<-data.table(seqnum=cells.biome$global_id, BIOME_NAME=cells.biome$BIOME_NAME)
  hexagon_biome<-merge(hexagon, cells.biome.dt, by="seqnum", all.x=T)
  hexagon_biome[which(hexagon_biome$BIOME_NAME=="N/A"), "BIOME_NAME"]<-NA
  ggplot(hexagon_biome)+geom_sf(aes(fill=BIOME_NAME))
  
  
  while(T){
    dt_data <- as.data.table(hexagon_biome)
    blank_idx <- which(is.na(dt_data$BIOME_NAME) | trimws(dt_data$BIOME_NAME) == "")
    if (length(blank_idx)==0){
      break()
    }
    neighbors <- st_intersects(hexagon_biome[blank_idx, ], hexagon_biome)
    fill_biomes <- sapply(seq_along(blank_idx), function(i) {
      nb_idx <- neighbors[[i]]
      nb_biomes <- dt_data$BIOME_NAME[nb_idx]
      valid_biomes <- nb_biomes[!is.na(nb_biomes) & trimws(nb_biomes) != ""]
      if (length(valid_biomes) > 0) {
        return(valid_biomes[1])
      } else {
        return(NA_character_)
      }
    })
    
    dt_data[blank_idx, BIOME_NAME := fill_biomes]
    
    hexagon_biome <- st_as_sf(dt_data)
    st_crs(hexagon_biome) <- st_crs(hexagon_biome)
  }
  hexagon_biome[which(hexagon_biome$BIOME_NAME=="Flooded Grasslands & Savannas"),]
  dt_data <- as.data.table(hexagon_biome)
  dt_merged <- dt_data[, .(
    geometry = st_union(geometry)
  ), by = .(continent, BIOME_NAME)]
  sf_merged <- st_as_sf(dt_merged)
  
  st_crs(sf_merged) <- st_crs(hexagon_biome)
  sf_merged <- st_collection_extract(sf_merged, "POLYGON")
  
  sf_merged <- st_cast(sf_merged, "MULTIPOLYGON")
  
  sf_merged <- sf_merged[!st_is_empty(sf_merged), ]
  
  hexagon_biome<-sf_merged
  ggplot(hexagon_biome)+geom_sf(aes(fill=BIOME_NAME))
  
  ggplot(sf_merged)+geom_sf(fill=NA)+
    geom_sf(data=sf_merged[which(is.na(sf_merged$BIOME_NAME)|(sf_merged$BIOME_NAME=="N/A")),],
            aes(fill=BIOME_NAME))
  
  write_sf(hexagon_biome, "../Shape/hexagon_biome/hexagon_biome.shp")
}
if (F){
  species.dis<-readRDS("../Data/Tables/Final.Distribution.Unique.rda")
  species.dis.sub<-species.dis
  cells.biome<-readRDS("../Data/Tables/cells.biome.rda")
  species.dis.geo<-merge(species.dis.sub, cells.biome, by="global_id")
  
  species.dis.geo$geometry<-NULL
  hexagon<-readRDS("../Data/Tables/cells.with.dist.rda")
  
  seeds<-data.table(seed_id=as.numeric(hexagon$seqnum), seed_continent=hexagon$continent)
  species.dis.geo<-merge(species.dis.geo, seeds, by="seed_id")
  
  
  species.dis.geo$type<-ifelse(species.dis.geo$continent==species.dis.geo$seed_continent, 
                               "Aborigines", "Invader")
  
  saveRDS(species.dis.geo, "../Data/Tables/species.dis.biome.rda")
  
  
  
}


if (F){
  cells.biome<-readRDS("../Data/Tables/cells.biome.rda")
  
  species.dis<-readRDS("../Data/Tables/Final.Distribution.NULL.Unique.rda")
  species.dis.geo<-merge(species.dis, cells.biome, by="global_id")
  
  species.dis.geo$geometry<-NULL
  hexagon<-readRDS("../Data/Tables/cells.with.dist.rda")
  
  seeds<-data.table(seed_id=as.numeric(hexagon$seqnum), seed_continent=hexagon$continent)
  species.dis.geo<-merge(species.dis.geo, seeds, by="seed_id")
  
  
  species.dis.geo$type<-ifelse(species.dis.geo$continent==species.dis.geo$seed_continent, 
                               "Aborigines", "Invader")
  
  saveRDS(species.dis.geo, "../Data/Tables/species.dis.biome.NULL.rda")
  
}

if (F){
  species.dis.geo<-readRDS("../Data/Tables/species.dis.biome.rda")
  species.dis.geo$label<-sprintf("%d.%s.%s", species.dis.geo$seed_id, species.dis.geo$nb, species.dis.geo$da)
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.995.rda")
  seeds.all[!(label %in% species.dis.geo$label)]
  rep.list<-list()
  rep.list.all<-list()
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-species.dis.geo[label %in% seeds$label]
    type.N<-item[, .(N.species=length(unique(sp_label))),
                 by=list(type, nb, da, BIOME_NAME, continent)]
    
    type.N<-type.N[!is.na(BIOME_NAME)]
    type.N<-type.N[BIOME_NAME!="N/A"]
    type.N$rep<-rrrr
    rep.list[[rrrr]]<-type.N
    type.N.sum<-type.N[, .(N.species=sum(N.species)),
                       by=list(type, BIOME_NAME, continent, rep)]
    
    rep.list.all[[rrrr]]<-type.N.sum
    
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.list.all)
  saveRDS(rep.df, "../Data/Tables/biome.N.species.by.nb.995.rda")
  saveRDS(rep.df.all, "../Data/Tables/biome.N.species.995.rda")
  
}

if (F){
  species.dis.geo<-readRDS("../Data/Tables/species.dis.biome.NULL.rda")
  species.dis.geo$label<-sprintf("%d.%s.%s", species.dis.geo$seed_id, species.dis.geo$nb, species.dis.geo$da)
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.99.rda")
  seeds.all[seed_id==41164 & nb=="NARROW" & da=="GOOD"]
  rep.list<-list()
  rep.list.all<-list()
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-species.dis.geo[label %in% seeds$label]
    type.N<-item[, .(N.species=length(unique(sp_label))),
                 by=list(type, nb, da, BIOME_NAME, continent)]
    
    type.N<-type.N[!is.na(BIOME_NAME)]
    type.N<-type.N[BIOME_NAME!="N/A"]
    type.N$rep<-rrrr
    rep.list[[rrrr]]<-type.N
    type.N.sum<-type.N[, .(N.species=sum(N.species)),
                       by=list(type, BIOME_NAME, continent, rep)]
    
    if (F){
      type.N.sum[BIOME_NAME=="Temperate Grasslands, Savannas & Shrublands" & continent=="South America"]
      
      cells<-read_sf("../Shape/isea3h8/N_S_America.shp")
      item_ll<-merge(item, cells, by.x="global_id", by.y="seqnum")
      item_ll[continent.x!=continent.y]
      biome_Cells<-item_ll[BIOME_NAME=="Temperate Grasslands, Savannas & Shrublands" & continent.x=="South America"]
      biome_Cells$lat_bin<-floor((biome_Cells$lat+0.5)/1)*1
      biome_Cells$sp_label<-sprintf("%s.%s.%s", biome_Cells$sp_id, biome_Cells$nb, biome_Cells$da)
      xxx<-biome_Cells[, .(N_SP=length(unique(sp_label))),
                  by=list(type, lat_bin)]
      
      xxx.native<-xxx[type=="Aborigines"]
      colnames(xxx.native)[3]<-"N_SP_Native"
      
      xxx.invader<-xxx[type=="Invader"]
      colnames(xxx.invader)[3]<-"N_SP_Immigrant"
      
      xxx.merge<-merge(xxx.invader, xxx.native, by=c("lat_bin"), all=T)
      xxx.merge[is.na(N_SP_Immigrant), N_SP_Immigrant :=0]
      xxx.merge<-xxx.merge[, c("lat_bin", "N_SP_Native", "N_SP_Immigrant")]
      xxx.merge$Immigrant_Per<-xxx.merge$N_SP_Immigrant/(xxx.merge$N_SP_Immigrant+xxx.merge$N_SP_Native)
      xxx.merge
    }
    rep.list.all[[rrrr]]<-type.N.sum
    
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.list.all)
  saveRDS(rep.df, "../Data/Tables/biome.N.species.by.nb.NULL.99.rda")
  saveRDS(rep.df.all, "../Data/Tables/biome.N.species.NULL.99.rda")
  
}

simulation.type<-"NULL"
#simulation.type<-"Sim"
if (simulation.type=="NULL"){
  rep.list.all<-readRDS(sprintf("../Data/Tables/biome.N.species.%s.rda", simulation.type))
}else{
  file.info("../Data/Tables/biome.N.species.rda")
  rep.list.all<-readRDS("../Data/Tables/biome.N.species.rda")
}

rep.list.all[rep==1]
Aborigines<-rep.list.all[type=="Aborigines"]
colnames(Aborigines)[5]<-"N.Aborigines"
Aborigines$type<-NULL
Invader<-rep.list.all[type=="Invader"]
colnames(Invader)[5]<-"N.Invader"
Invader$type<-NULL
N.merge<-merge(Aborigines, Invader, 
               by=c("BIOME_NAME", "rep", "continent"), all=T)

N.merge[is.na(N.Aborigines), N.Aborigines:=0]

N.merge[is.na(N.Invader), N.Invader:=0]

N.merge$Invader_per<-N.merge$N.Invader/(N.merge$N.Invader+N.merge$N.Aborigines)

mean(N.merge[continent %in% c("North America", "South America")]$Invader_per)
sd(N.merge[continent %in% c("North America", "South America")]$Invader_per)
        

N.merge.mean<-N.merge[, .(N.Aborigines=mean(N.Aborigines), sd.N.Aborigines=sd(N.Aborigines),
                          N.Invader=mean(N.Invader), sd.N.Invader=sd(N.Invader),
                          Invader_per=mean(Invader_per), sd.Invader_per=sd(Invader_per)),
                      by=list(BIOME_NAME, continent)]
N.merge.mean[,c("BIOME_NAME", "Invader_per", "sd.Invader_per")]

ggplot(N.merge[continent %in% c("North America", "South America")])+
  geom_boxplot(aes(x=BIOME_NAME, y=Invader_per))+
  geom_hline(yintercept = 0.5, linetype=2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~continent)

p<-ggplot(N.merge.mean[continent %in% c("North America", "South America")])+
  geom_point(aes(x=BIOME_NAME, y=Invader_per))+
  geom_errorbar(aes(x=BIOME_NAME, 
                    ymin = Invader_per-sd.Invader_per, 
                    ymax=Invader_per+sd.Invader_per), width=0.5)+
  geom_hline(yintercept = 0.5, linetype=2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~continent)
p

ggsave(p, filename=sprintf("../Figures/Figure.Biome/biome_invader.%s.pdf", simulation.type), width=8, height=6)
ggsave(p, filename=sprintf("../Figures/Figure.Biome/biome_invader.%s.png", simulation.type), width=8, height=6)

biome<-read_sf("../Shape/hexagon_biome/hexagon_biome.shp")
anchor_points <- st_point_on_surface(biome)
coords <- st_coordinates(anchor_points)
biome$label_x <- coords[, "X"]
biome$label_y <- coords[, "Y"]
bbox <- st_bbox(biome)
bbox[3]<- -33
mid_x <- (bbox["xmin"] + bbox["xmax"]) / 2
margin_dist <- (bbox["xmax"] - bbox["xmin"]) * 0.1 

biome <- biome %>%
  mutate(
    nudge_x = bbox["xmin"] - label_x - margin_dist,
    hjust = 1
  )

pp<-ggplot(data = biome) +
  geom_sf(aes(fill = BIOME_NAME), color = "white", linewidth = 0.2) +
  
  geom_label_repel(
    aes(x = label_x, y = label_y, label = BIOME_NAME, fill = BIOME_NAME),
    nudge_x = biome$nudge_x,
    hjust = biome$hjust,
    direction = "y",      
    color = "black",
    segment.color = "grey40",     
    segment.linewidth = 0.6,
    segment.curvature = -0.1,
    min.segment.length = 0,       
    box.padding = 0.3,            
    show.legend = FALSE           
  ) +
  coord_sf(
    xlim = c(bbox["xmin"] - margin_dist * 3, bbox["xmax"] + margin_dist * 3), 
    clip = "off"
  ) +
  
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 20, 10, 20, "pt")
  )
pp
ggsave(pp, filename="../Figures/Figure.Biome/Biomes.png", width=15, height=12,
       bg="white")

biome.per<-merge(biome, N.merge.mean, by=c("BIOME_NAME", "continent"))
biome.per[, c("BIOME_NAME", "continent", "N.Aborigines", "N.Invader", "Invader_per", "sd.Invader_per")]
unique(biome.per$BIOME_NAME)
pp<-ggplot(data = biome.per)+
  geom_sf(aes(fill = Invader_per), color = "white", linewidth = 0.2)+
  coord_sf(
    xlim = c(bbox["xmin"] - margin_dist * 3, bbox["xmax"] + margin_dist * 3), 
    clip = "off"
  )+
  scale_fill_gradient2(low=color_low,
                       mid=color_mid,
                       high=color_high,
                       midpoint =0.5,
                       limits=c(0, 1))+
  theme_void()
pp
ggsave(pp, filename=sprintf("../Figures/Figure.Biome/Invader.by.biome.%s.png", simulation.type), width=15, height=12,
       bg="white")

sf_data<-biome.per
sf_data$plot_fill <- ifelse(
  sf_data$continent %in% c("bridge1", "bridge2"), 
  "Bridge (100% Invader)", 
  sf_data$BIOME_NAME
)
sf_data_full<-sf_data
sf_poly <- suppressWarnings(st_cast(sf_data, "POLYGON"))
dt_poly <- as.data.table(sf_poly)
dt_poly[, poly_area := as.numeric(st_area(geometry))]

dt_largest_idx <- dt_poly[, .I[which.max(poly_area)], by = .(BIOME_NAME, continent)]$V1
dt_largest <- dt_poly[dt_largest_idx]

sf_largest <- st_as_sf(dt_largest)
coords <- st_coordinates(suppressMessages(st_point_on_surface(sf_largest)))
coords.dt<-data.table(coords)
coords.dt$continent<-dt_largest$continent
coords.dt$BIOME_NAME<-dt_largest$BIOME_NAME
dt_largest[, `:=`(X = coords[, 1], Y = coords[, 2])]

dt_pies_base <- dt_largest[!(continent %in% c("bridge1", "bridge2"))]
#area_threshold <- median(dt_pies_base$poly_area)
area_threshold <- 1e100

dt_pies_base[, placement := ifelse(poly_area < area_threshold, "LEFT", "INSIDE")]
dt_pies_base[BIOME_NAME == "Flooded Grasslands & Savannas" & continent == "North America", placement := "RIGHT"]
dt_pies_base[BIOME_NAME == "Mangroves" & continent == "South America", placement := "RIGHT"]
dt_pies_base[BIOME_NAME == "Montane Grasslands & Shrublands" & continent == "South America", placement := "LEFT"]
dt_pies_base[BIOME_NAME == "Tropical & Subtropical Dry Broadleaf Forests" & continent == "South America", placement := "RIGHT"]
dt_pies_base[BIOME_NAME == "Boreal Forests/Taiga" & continent == "North America", placement := "RIGHT"]
dt_pies_base[BIOME_NAME == "Temperate Broadleaf & Mixed Forests" & continent == "North America", placement := "RIGHT"]
dt_pies_base[BIOME_NAME == "Tropical & Subtropical Moist Broadleaf Forests" & continent == "South America", placement := "RIGHT"]
dt_pies_base[BIOME_NAME == "Temperate Grasslands, Savannas & Shrublands" & continent == "South America", placement := "RIGHT"]
dt_pies_base[BIOME_NAME == "Tropical & Subtropical Grasslands, Savannas & Shrublands" & continent == "South America", placement := "RIGHT"]
dt_pies_base[BIOME_NAME == "Temperate Grasslands, Savannas & Shrublands" & continent == "North America", placement := "RIGHT"]
dt_pies_base[BIOME_NAME == "Tundra" & continent == "North America", placement := "RIGHT"]
dt_pies_base[BIOME_NAME == "Tropical & Subtropical Grasslands, Savannas & Shrublands" & continent == "North America", placement := "RIGHT"]

dt_pies_base[, `:=`(pie_X = X, pie_Y = Y)]
bbox <- st_bbox(sf_data)

idx_left <- dt_pies_base[placement == "LEFT", which = TRUE]
if (length(idx_left) > 0) {
  idx_left_sorted <- idx_left[order(dt_pies_base$Y[idx_left])]
  N_left <- length(idx_left_sorted)
  dt_pies_base[idx_left_sorted, pie_Y := seq(bbox["ymin"] - 5, bbox["ymax"] - 30, length.out = N_left)]
  arc_curve <- sin(seq(0, pi, length.out = N_left)) 
  dt_pies_base[idx_left_sorted, pie_X := bbox["xmin"] + 50 - 25 * arc_curve]
}
dt_pies_base[BIOME_NAME == "Temperate Conifer Forests" & continent == "North America", pie_X:=-140]
idx_right <- dt_pies_base[placement == "RIGHT", which = TRUE]
if (length(idx_right) > 0) {
  idx_right_sorted <- idx_right[order(dt_pies_base$Y[idx_right])]
  N_right <- length(idx_right_sorted)
  
  dt_pies_base[idx_right_sorted, pie_Y := seq(bbox["ymin"] - 5, bbox["ymax"] + 5, length.out = N_right)]
  
  arc_curve <- sin(seq(0, pi, length.out = N_right)) 
  
  dt_pies_base[idx_right_sorted, pie_X := bbox["xmax"] + 0 + 25 * arc_curve] 
}

dt_pies_base[, total_N := N.Aborigines + N.Invader]

max_radius <- 6  
scale_factor <- max_radius / max(sqrt(dt_pies_base$total_N), na.rm = TRUE)  
dt_pies_base[, radius := sqrt(total_N) * scale_factor]

dt_pies_base[, label_invader := paste0(round(N.Invader, 0), " ± ", round(sd.N.Invader, 0))]
dt_pies_base[, label_aborigines := paste0(round(N.Aborigines, 0), " ± ", round(sd.N.Aborigines, 0))]

dt_invader <- copy(dt_pies_base)
dt_invader[, `:=`(start_angle = 0, end_angle = Invader_per * 2 * pi, pie_category = BIOME_NAME)]

dt_aborigine <- copy(dt_pies_base)
dt_aborigine[, `:=`(start_angle = Invader_per * 2 * pi, end_angle = 2 * pi, pie_category = "Native (Aborigines)")]

dt_pie <- rbindlist(list(dt_invader, dt_aborigine))

biome_names <- unique(sf_data$BIOME_NAME)
op<-"user.defined.blind.friendly"
#for (op in c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo",
#             "user.defined", "user.defined.blind.friendly")){
for (op in c("user.defined.blind.friendly")){
  if (op %in% c("user.defined", "user.defined.blind.friendly")){
    if (op=="user.defined"){
      sunny_days_colors <- c(
        "#FB3618", "#F88022", "#FDD616", "#FBB91B", 
        "#ACD055", "#709D0D", "#078A48", "#05E1A8", 
        "#68D3C8", "#025174", "#587C8F", "#F84B8E", 
        "#F6B1D5", "#F8A8A1"
      )
    }
    if (op=="user.defined.blind.friendly"){
      sunny_days_colors<-c(
        "#D55E00", "#E69F00", "#F0E442", "#DDCC77", 
        "#CC6677", "#CC79A7", "#882255", "#56B4E9", 
        "#88CCEE", "#009E73", "#44AA99", "#0072B2", 
        "#332288", "#AA4499"
      )
    }
    color_palette <- setNames(sunny_days_colors, biome_names)
    
  }else{
    color_palette <- setNames(viridis(length(biome_names), option = op), biome_names)
  }
  color_palette["Native (Aborigines)"] <- "#E0E0E0"          
  color_palette["Bridge (100% Invader)"] <- "grey50"         
  legend_breaks <- setdiff(
    names(color_palette), 
    c("Native (Aborigines)", "Bridge (100% Invader)")
  )
  
  pp2<-ggplot() +
    geom_sf(data = sf_data, aes(fill = plot_fill), 
            color = "white", linewidth = 0.2, alpha = 0.4) +
    
    geom_segment(data = dt_pies_base[placement != "INSIDE"],
                 aes(x = X, y = Y, xend = pie_X, yend = pie_Y),
                 color = "grey40", linewidth = 0.4, linetype = "dotted") +
    
    geom_arc_bar(data = dt_pie,
                 aes(x0 = pie_X, y0 = pie_Y, r0 = 0, r = radius, 
                     start = start_angle, end = end_angle, fill = pie_category),
                 color = "white", linewidth = 0.3) +
    
    geom_text(data = dt_pies_base,
              aes(x = pie_X, y = pie_Y, label = percent(Invader_per, accuracy = 0.1)),
              size = 3, fontface = "bold", color = "black") +
    
    geom_text(data = dt_pies_base,
              aes(x = pie_X, y = pie_Y + radius, label = label_invader),
              size = 2.5, vjust = -0.5, color = "black", fontface = "italic") +
    
    geom_text(data = dt_pies_base,
              aes(x = pie_X, y = pie_Y - radius, label = label_aborigines),
              size = 2.5, vjust = 1.5, color = "grey30", fontface = "italic") +
    
    scale_fill_manual(values = color_palette, breaks = legend_breaks) +
    theme_minimal() +
    labs(
      title = "Proportion of Invaders across Biomes",
      #subtitle = "Pie area proportional to Total Population",
      fill = "Biome / Status"
    ) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),  
      axis.title = element_blank()
    )       
  #pp2
  #ggsave(pp2, filename="../Figures/Figure.Biome/N.nvader.by.biome.pie2.pdf", width=15, height=10)
  #ggsave(pp2, filename="../Figures/Figure.Biome/N.nvader.by.biome.pie2.png", width=15, height=10, bg="white")
  
  ppp<-ggplot() +
    geom_sf(data = sf_data, aes(fill = plot_fill), 
            color = "white", linewidth = 0.2, alpha = 0.7) +
    geom_segment(data = dt_pies_base[placement != "INSIDE"],
                 aes(x = X, y = Y, xend = pie_X, yend = pie_Y),
                 color = "grey40", linewidth = 0.4, linetype = "dotted") +
    geom_arc_bar(data = dt_pie,
                 aes(x0 = pie_X, y0 = pie_Y, r0 = 0, r = radius, 
                     start = start_angle, end = end_angle, 
                     fill = pie_category),
                 color = "white", linewidth = 0.3) +
    geom_text(data = dt_pies_base,
              aes(x = pie_X, y = pie_Y, label = percent(Invader_per, accuracy = 0.1)),
              size = 3, color = "black") +
    scale_fill_manual(values = color_palette, breaks = legend_breaks) +
    theme_minimal() +
    labs(
      title = "Proportion of Invaders across Biomes",
      #subtitle = "Pie area proportional to Total Population",
      fill = "Biome"
    ) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),  
      axis.title = element_blank()
    )
  #ppp
  ggsave(ppp, filename=sprintf("../Figures/Figure.Biome/N.nvader.by.biome.pie.%s.%s.pdf", op, simulation.type), width=12, height=7)
  ggsave(ppp, filename=sprintf("../Figures/Figure.Biome/N.nvader.by.biome.pie.%s.%s.png", op, simulation.type), width=12, height=7, bg="white")
}


dt<-data.table(sf_data[, c("BIOME_NAME","continent",
                           "N.Aborigines","sd.N.Aborigines",
                           "N.Invader","sd.N.Invader",
                           "Invader_per","sd.Invader_per")])
dt$geometry<-NULL
dt<-dt[continent %in% c("North America", "South America")]
dt.output<-dt
dt.output$Invader_per<-dt.output$Invader_per*100
dt.output$sd.Invader_per<-dt.output$sd.Invader_per*100
colnames(dt.output)<-c("Biome", "Continent", "Native", "SD(Native)", "Immigrant",
                       "SD(Immigrant)", "Immigrant proportion", "SD(Immigrant proportion)")
to.doc(dt.output, "Number of invaders per biome", 
       sprintf("../Figures/Figure.Biome/N.nvader.by.biome.pie.%s.docx", simulation.type),
       digits=1, in_place=F)
fwrite(dt.output, sprintf("../Figures/Figure.Biome/N.nvader.by.biome.pie.%s.csv", simulation.type))

dt[continent=="South America"]
#By NB and DA

rep.df<-readRDS("../Data/Tables/biome.N.species.by.nb.rda")
combs<-unique(rep.df[, c("nb", "da")])

biome<-read_sf("../Shape/hexagon_biome/hexagon_biome.shp")
anchor_points <- st_point_on_surface(biome)
coords <- st_coordinates(anchor_points)
biome$label_x <- coords[, "X"]
biome$label_y <- coords[, "Y"]
bbox <- st_bbox(biome)
bbox[3]<- -33
mid_x <- (bbox["xmin"] + bbox["xmax"]) / 2
margin_dist <- (bbox["xmax"] - bbox["xmin"]) * 0.1 

biome <- biome %>%
  mutate(
    nudge_x = bbox["xmin"] - label_x - margin_dist,
    hjust = 1
  )

Aborigines<-rep.df[type=="Aborigines"]
colnames(Aborigines)[6]<-"N.Aborigines"
Aborigines$type<-NULL
Invader<-rep.df[type=="Invader"]
colnames(Invader)[6]<-"N.Invader"
Invader$type<-NULL
N.merge<-merge(Aborigines, Invader, 
               by=c("BIOME_NAME", "rep", "continent", "nb", "da"), all=T)

N.merge[is.na(N.Aborigines), N.Aborigines:=0]

N.merge[is.na(N.Invader), N.Invader:=0]

N.merge$Invader_per<-N.merge$N.Invader/(N.merge$N.Invader+N.merge$N.Aborigines)
N.merge.mean<-N.merge[, .(N.Aborigines=mean(N.Aborigines), sd.N.Aborigines=sd(N.Aborigines),
                          N.Invader=mean(N.Invader), sd.N.Invader=sd(N.Invader),
                          Invader_per=mean(Invader_per), sd.Invader_per=sd(Invader_per)),
                      by=list(BIOME_NAME, continent, nb, da)]
biome.list<-list()
for (i in c(1:nrow(combs))){
  comb<-combs[i]
  item<-N.merge.mean[nb==comb$nb & da==comb$da]

  biome.per<-merge(biome, item, by=c("BIOME_NAME", "continent"))
  biome.list[[i]]<-biome.per
  sf_data<-biome.per
  sf_data$plot_fill <- ifelse(
    sf_data$continent %in% c("bridge1", "bridge2"), 
    "Bridge (100% Invader)", 
    sf_data$BIOME_NAME
  )
  
  sf_poly <- suppressWarnings(st_cast(sf_data, "POLYGON"))
  dt_poly <- as.data.table(sf_poly)
  dt_poly[, poly_area := as.numeric(st_area(geometry))]
  
  dt_largest_idx <- dt_poly[, .I[which.max(poly_area)], by = .(BIOME_NAME, continent)]$V1
  dt_largest <- dt_poly[dt_largest_idx]
  
  sf_largest <- st_as_sf(dt_largest)
  coords <- st_coordinates(suppressMessages(st_point_on_surface(sf_largest)))
  dt_largest<-merge(dt_largest, coords.dt, by=c("continent", "BIOME_NAME"))
  
  
  dt_pies_base <- dt_largest[!(continent %in% c("bridge1", "bridge2"))]
  area_threshold <- 1e100
  
  dt_pies_base[, placement := ifelse(poly_area < area_threshold, "LEFT", "INSIDE")]
  dt_pies_base[BIOME_NAME == "Flooded Grasslands & Savannas" & continent == "North America", placement := "RIGHT"]
  dt_pies_base[BIOME_NAME == "Mangroves" & continent == "South America", placement := "RIGHT"]
  dt_pies_base[BIOME_NAME == "Montane Grasslands & Shrublands" & continent == "South America", placement := "LEFT"]
  dt_pies_base[BIOME_NAME == "Tropical & Subtropical Dry Broadleaf Forests" & continent == "South America", placement := "RIGHT"]
  dt_pies_base[BIOME_NAME == "Boreal Forests/Taiga" & continent == "North America", placement := "RIGHT"]
  dt_pies_base[BIOME_NAME == "Temperate Broadleaf & Mixed Forests" & continent == "North America", placement := "RIGHT"]
  dt_pies_base[BIOME_NAME == "Tropical & Subtropical Moist Broadleaf Forests" & continent == "South America", placement := "RIGHT"]
  dt_pies_base[BIOME_NAME == "Temperate Grasslands, Savannas & Shrublands" & continent == "South America", placement := "RIGHT"]
  dt_pies_base[BIOME_NAME == "Tropical & Subtropical Grasslands, Savannas & Shrublands" & continent == "South America", placement := "RIGHT"]
  dt_pies_base[BIOME_NAME == "Temperate Grasslands, Savannas & Shrublands" & continent == "North America", placement := "RIGHT"]
  dt_pies_base[BIOME_NAME == "Tundra" & continent == "North America", placement := "RIGHT"]
  dt_pies_base[BIOME_NAME == "Tropical & Subtropical Grasslands, Savannas & Shrublands" & continent == "North America", placement := "RIGHT"]
  
  dt_pies_base[, `:=`(pie_X = X, pie_Y = Y)]
  bbox <- st_bbox(sf_data)
  
  idx_left <- dt_pies_base[placement == "LEFT", which = TRUE]
  if (length(idx_left) > 0) {
    idx_left_sorted <- idx_left[order(dt_pies_base$Y[idx_left])]
    N_left <- length(idx_left_sorted)
    dt_pies_base[idx_left_sorted, pie_Y := seq(bbox["ymin"] - 5, bbox["ymax"] - 30, length.out = N_left)]
    arc_curve <- sin(seq(0, pi, length.out = N_left)) 
    dt_pies_base[idx_left_sorted, pie_X := bbox["xmin"] + 50 - 25 * arc_curve]
  }
  
  dt_pies_base[BIOME_NAME == "Temperate Conifer Forests" & continent == "North America", pie_X:=-140]
  idx_right <- dt_pies_base[placement == "RIGHT", which = TRUE]
  if (length(idx_right) > 0) {
    idx_right_sorted <- idx_right[order(dt_pies_base$Y[idx_right])]
    N_right <- length(idx_right_sorted)
    
    dt_pies_base[idx_right_sorted, pie_Y := seq(bbox["ymin"] - 5, bbox["ymax"] + 5, length.out = N_right)]
    
    arc_curve <- sin(seq(0, pi, length.out = N_right)) 
    
    dt_pies_base[idx_right_sorted, pie_X := bbox["xmax"] + 0 + 25 * arc_curve] 
  }
  dt_pies_base[, total_N := N.Aborigines + N.Invader]
  
  max_radius <- 6  
  scale_factor <- max_radius / max(sqrt(dt_pies_base$total_N), na.rm = TRUE)  
  dt_pies_base[, radius := sqrt(total_N) * scale_factor]
  
  dt_pies_base[, label_invader := paste0(round(N.Invader, 0), " ± ", round(sd.N.Invader, 0))]
  dt_pies_base[, label_aborigines := paste0(round(N.Aborigines, 0), " ± ", round(sd.N.Aborigines, 0))]
  
  dt_invader <- copy(dt_pies_base)
  dt_invader[, `:=`(start_angle = 0, end_angle = Invader_per * 2 * pi, pie_category = BIOME_NAME)]
  
  dt_aborigine <- copy(dt_pies_base)
  dt_aborigine[, `:=`(start_angle = Invader_per * 2 * pi, end_angle = 2 * pi, pie_category = "Native (Aborigines)")]
  
  dt_pie <- rbindlist(list(dt_invader, dt_aborigine))
  
  biome_names <- unique(sf_data$BIOME_NAME)
  sunny_days_colors<-c(
    "#D55E00", "#E69F00", "#F0E442", "#DDCC77", 
    "#CC6677", "#CC79A7", "#882255", "#56B4E9", 
    "#88CCEE", "#009E73", "#44AA99", "#0072B2", 
    "#332288", "#AA4499"
  )
  #color_palette <- setNames(sunny_days_colors, biome_names)
  
  color_palette["Native (Aborigines)"] <- "#E0E0E0"          
  color_palette["Bridge (100% Invader)"] <- "grey50"         
  legend_breaks <- setdiff(
    names(color_palette), 
    c("Native (Aborigines)", "Bridge (100% Invader)")
  )
  
  ppp<-ggplot() +
    geom_sf(data=biome, fill=NA, color="lightgrey")+
    geom_sf(data = sf_data, aes(fill = plot_fill), 
            color = "white", linewidth = 0.2, alpha = 0.7) +
    geom_segment(data = dt_pies_base[placement != "INSIDE"],
                 aes(x = X, y = Y, xend = pie_X, yend = pie_Y),
                 color = "grey40", linewidth = 0.4, linetype = "dotted") +
    geom_arc_bar(data = dt_pie,
                 aes(x0 = pie_X, y0 = pie_Y, r0 = 0, r = radius, 
                     start = start_angle, end = end_angle, 
                     fill = pie_category),
                 color = "white", linewidth = 0.3) +
    geom_text(data = dt_pies_base,
              aes(x = pie_X, y = pie_Y, label = percent(Invader_per, accuracy = 0.1)),
              size = 3, fontface = "bold", color = "black") +
    scale_fill_manual(values = color_palette, breaks = legend_breaks) +
    theme_minimal() +
    labs(
      title = sprintf("Proportion of Invaders across Biomes (%s & %s)", comb$nb, comb$da),
      #subtitle = "Pie area proportional to Total Population",
      fill = "Biome"
    ) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),  
      axis.title = element_blank()
    )
  ppp
  ggsave(ppp, 
         filename=sprintf("../Figures/Figure.Biome/Biome.by.nb.da/N.nvader.by.biome.pie.%s.%s.pdf", 
                          comb$nb, comb$da), width=12, height=7)
  ggsave(ppp, 
         filename=sprintf("../Figures/Figure.Biome/Biome.by.nb.da/N.nvader.by.biome.pie.%s.%s.png", 
                          comb$nb, comb$da), width=12, height=7, bg="white")
  
  dt<-data.table(sf_data[, c("BIOME_NAME","continent",
                             "N.Aborigines","sd.N.Aborigines",
                             "N.Invader","sd.N.Invader",
                             "Invader_per","sd.Invader_per",
                             "nb", "da")])
  dt$geometry<-NULL
  dt<-dt[continent %in% c("North America", "South America")]
  to.doc(dt, sprintf("Number of invaders per biome (%s & %s)", comb$nb, comb$da), 
         sprintf("../Figures/Figure.Biome/Biome.by.nb.da/N.nvader.by.biome.pie.%s.%s.docx", 
         comb$nb, comb$da),
         digits=2, in_place=F)
}
biome.df<-rbindlist(biome.list)
biome.df$geometry<-NULL
biome.df<-data.table(biome.df[, c("nb", "da",
                                  "BIOME_NAME","continent",
                                  "N.Aborigines","sd.N.Aborigines",
                                  "N.Invader","sd.N.Invader",
                                  "Invader_per","sd.Invader_per"
                                  )])
dt.output<-biome.df
dt.output$Invader_per<-dt.output$Invader_per*100
dt.output$sd.Invader_per<-dt.output$sd.Invader_per*100
colnames(dt.output)<-c("NB", "DA", "Biome", "Continent", "Native", "SD(Native)", "Immigrant",
                       "SD(Immigrant)", "Immigrant proportion", "SD(Immigrant proportion)")

fwrite(biome.df, "../Figures/Figure.Biome/Biome.by.nb.da/biome.invader.per.all.csv")
N.merge$nb<-factor(N.merge$nb, 
                     levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                     labels = c("BROAD", "MODERATE", "NARROW", "TINY"))

p<-ggplot(N.merge[continent %in% c("North America", "South America")])+
  geom_boxplot(aes(y=BIOME_NAME, x=Invader_per))+
  geom_vline(xintercept = 0.5, linetype=2)+
  facet_grid(continent~nb+da)+
  theme_bw()
p
ggsave(p, filename="../Figures/Figure.Biome/Figure.Biome.full.pdf", width=18, height=8)
ggsave(p, filename="../Figures/Figure.Biome/Figure.Biome.full.png", width=18, height=8, bg="white")




