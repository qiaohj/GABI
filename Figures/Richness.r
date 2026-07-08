library(data.table)
library(ggplot2)
library(sf)
library(stringr)
library(flextable)
library(officer)
library(RSQLite)
library(DBI)
library(patchwork)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
if (F){
  distribution<-readRDS("../Data/Tables/Final.Distribution.rda")
  distribution$label<-sprintf("%d.%s.%s", distribution$seed_id, distribution$nb, distribution$da)
  distribution$sp_label<-sprintf("%s.%s", distribution$sp_id, distribution$label)
  distribution$group_id<-NULL
  distribution$n<-NULL
  distribution$suitable<-NULL
  distribution<-unique(distribution)
  saveRDS(distribution, "../Data/Tables/Final.Distribution.Unique.rda")
  
  distribution<-readRDS("../Data/Tables/Final.Distribution.NULL.rda")
  distribution$label<-sprintf("%d.%s.%s", distribution$seed_id, distribution$nb, distribution$da)
  distribution$sp_label<-sprintf("%s.%s", distribution$sp_id, distribution$label)
  distribution$group_id<-NULL
  distribution$n<-NULL
  distribution$suitable<-NULL
  distribution<-unique(distribution)
  saveRDS(distribution, "../Data/Tables/Final.Distribution.NULL.Unique.rda")
}

if (F){
  #For simulations
  distribution<-readRDS("../Data/Tables/Final.Distribution.Unique.rda")
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  
  range(distribution$year)
  i=1
  richness.list<-list()
  for (i in c(1:max(seeds.all$rep))){
    print(i)
    item<-seeds.all[rep==i]
    dis.item<-distribution[label %in% item$label]
    richness.item<-dis.item[,.(N.sp=length(unique(sp_label))), by=list(global_id)]
    richness.item$rep<-i
    richness.list[[i]]<-richness.item
  }
  richness<-rbindlist(richness.list)
  richness<-richness[, .(N.sp=mean(N.sp), sd=sd(N.sp)), by=list(global_id)]
  
  
  saveRDS(richness, "../Data/Tables/Final.Richness.rda")
  
  #For NULL models
  distribution<-readRDS("../Data/Tables/Final.Distribution.NULL.Unique.rda")
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  
  range(distribution$year)
  i=1
  richness.list<-list()
  for (i in c(1:max(seeds.all$rep))){
    print(i)
    item<-seeds.all[rep==i]
    dis.item<-distribution[label %in% item$label]
    richness.item<-dis.item[,.(N.sp=length(unique(sp_label))), by=list(global_id)]
    richness.item$rep<-i
    richness.list[[i]]<-richness.item
  }
  richness<-rbindlist(richness.list)
  richness<-richness[, .(N.sp=mean(N.sp), sd=sd(N.sp)), by=list(global_id)]
  saveRDS(richness, "../Data/Tables/Final.Richness.NULL.rda")
  
  #For IUCN
  sf_use_s2(FALSE)
  species.america<-readRDS(sprintf("../Data/IUCN_NB/Mammals.%s.rda", "World"))
  species.america<-species.america[continent=="America"]
  mammals<-st_read("../Shape/IUCN/MAMMALS/MAMMALS_TERRESTRIAL_ONLY.shp")
  mammals<-mammals[which(mammals$binomial %in% species.america$species),]
  length(unique(mammals$binomial))
  continents<-read_sf("../Shape/isea3h8/N_S_America.shp")
  continents$N.sp <- sapply(st_intersects(continents, mammals), function(idx) {
    if (length(idx) == 0) {
      return(0) 
    } else {
      overlapping_binomials <- mammals$binomial[idx]
      return(length(unique(na.omit(overlapping_binomials))))
    }
  })
  saveRDS(continents, "../Data/Tables/IUCN.dis.rda")
  
  lats <- seq(-54, 65, by = 1)
  
  create_lat_line <- function(lat) {
    st_linestring(matrix(c(-180, lat, 180, lat), ncol = 2, byrow = TRUE))
  }
  lat_lines_sfc <- st_sfc(lapply(lats, create_lat_line), crs = 4326)
  lat_sf <- st_sf(latitude = lats, geometry = lat_lines_sfc)
  
  intersects_list <- st_intersects(lat_sf, mammals)
  
  lat_sf$species_richness <- sapply(intersects_list, function(idx) {
    if (length(idx) == 0) {
      return(0)
    } else {
      overlapping_binomials <- mammals$binomial[idx]
      return(length(unique(na.omit(overlapping_binomials))))
    }
  })
  
  iucn.lat<-data.table(lat_bin=lat_sf$latitude, N_SP=lat_sf$species_richness)
  saveRDS(iucn.lat, "../Data/Tables/IUCN.lat.rda")
  
  p1.2<-ggplot()+ 
    geom_sf(data=continents, fill=NA, color="lightgray")+
    geom_sf(data=continents,  aes(fill=N.sp),
            color=NA, linewidth=0.1) +
    scale_fill_gradient2(low="#2166AC",
                         mid="#F7F7F7",
                         high="#B2182B",
                         midpoint = median(continents$N.sp))+
    theme(
      axis.line = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
      plot.background = element_rect(fill="#fde7c0"),
      panel.background = element_rect(fill="#fde7c0"),
      legend.background = element_rect(fill = "#fde7c0", color = NA),
      legend.title = element_blank(),
      legend.position="bottom",
      legend.key.width = unit(1, 'cm'),
      plot.title = element_text(hjust = 0.5)
    )
}


richness<-readRDS("../Data/Tables/Final.Richness.rda")
colnames(richness)<-c("seqnum", "N.sp.sim", "sd.sim")
continents<-read_sf("../Shape/isea3h8/N_S_America.shp")
continents.sim<-merge(continents, richness, by="seqnum", all=T)
continents.sim[is.na(continents.sim$N.sp.sim),]$N.sp.sim<-0

richness.null<-readRDS("../Data/Tables/Final.Richness.NULL.rda")
colnames(richness.null)<-c("seqnum", "N.sp.null", "sd.null")
continents.sim.null<-merge(continents.sim, richness.null, by="seqnum", all=T)
continents.sim.null[is.na(continents.sim.null$N.sp.null),]$N.sp.null<-0

iucn<-readRDS("../Data/Tables/IUCN.dis.rda")
iucn<-data.table(seqnum=iucn$seqnum, N.iucn=iucn$N.sp)

continents.full<-merge(continents.sim.null, iucn, by="seqnum", all=T)
#continents.full[is.na(continents.full$N.iucn),]$N.iucn<-0


p1<-ggplot()+ 
  geom_sf(data=continents.full, fill=NA, color="lightgray")+
  geom_sf(data=continents.full,  aes(fill=N.sp.sim),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low="#2166AC",
                       mid="#FEE090",
                       high="#B2182B",
                       #midpoint = mean(continents.full$N.sp.sim),
                       midpoint = 15000,
                         #breaks = c(0, 50, 100, 200),
                         guide = guide_colorbar(
                           title.position = "top", 
                           title.hjust = 0.5,
                           barwidth = unit(4, "cm"),
                           barheight = unit(0.4, "cm")
                         ))+
  
  labs(tag = "(a) Simulations", fill="Number of species")+
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(), 
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    
    panel.grid.major = element_line(linetype = "solid", linewidth = 0.2, color = "#e8e8e8"), 
    panel.grid.minor = element_blank(),
    
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.05, 0.05),
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    #legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    
    plot.tag.position = c(0.01, 0.99), 
    plot.tag = element_text(hjust = 0, size = 12, face = "bold")
  )


p2<-ggplot()+ 
  geom_sf(data=continents.full, fill=NA, color="lightgray")+
  geom_sf(data=continents.full,  aes(fill=N.sp.null),
          color=NA, linewidth=0.1) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    #trans = "sqrt",
    na.value = "#f0f0f0",
    #breaks = c(0, 50, 100, 200),
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,
      barwidth = unit(6, "cm"),
      barheight = unit(0.4, "cm")
    )
  )+
  labs(tag = "(b) Null")+
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(), 
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    
    panel.grid.major = element_line(linetype = "solid", linewidth = 0.2, color = "#e8e8e8"), 
    panel.grid.minor = element_blank(),
    
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    
    plot.tag.position = c(0.01, 0.99), 
    plot.tag = element_text(hjust = 0, size = 12, face = "bold"),
    
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

p3<-ggplot()+ 
  geom_sf(data=continents.full, fill=NA, color="lightgray")+
  geom_sf(data=continents.full,  aes(fill=N.iucn),
          color=NA, linewidth=0.1) +
  scale_fill_gradient2(low="#2166AC",
                       mid="#FEE090",
                       high="#B2182B",
                       midpoint = 120,
                       #breaks = c(0, 50, 100, 200),
                       guide = guide_colorbar(
                         title.position = "top", 
                         title.hjust = 0.5,
                         barwidth = unit(4, "cm"),
                         barheight = unit(0.4, "cm")
                       ))+
  labs(tag = "(b) IUCN", fill="Number of species")+
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(), 
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    
    panel.grid.major = element_line(linetype = "solid", linewidth = 0.2, color = "#e8e8e8"), 
    panel.grid.minor = element_blank(),
    
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.05, 0.05),
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    #legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    
    plot.tag.position = c(0.01, 0.99), 
    plot.tag = element_text(hjust = 0, size = 12, face = "bold")
  )

lat<-readRDS("../Data/Tables/N.Sp.Lat.full.rep.rda")
lat<-lat[,.(N_SP=mean(N_SP), sd=sd(N_SP)), by=lat_bin]
iucn.lat<-readRDS("../Data/Tables/IUCN.lat.rda")
colnames(iucn.lat)[2]<-"N_SP_IUCN"
lat<-merge(lat, iucn.lat, by="lat_bin")

max_iucn <- max(lat$N_SP_IUCN, na.rm = TRUE)
max_sim <- max(lat$N_SP + lat$sd, na.rm = TRUE)
ratio <- max_sim / max_iucn 

color_iucn <- "#2166AC"
color_sim <- "#B2182B"

p_ldg <- ggplot(lat, aes(y = lat_bin)) +
  
  geom_ribbon(
    aes(xmin = (N_SP - sd) / ratio, 
        xmax = (N_SP + sd) / ratio),
    fill = color_sim, alpha = 0.2, color = NA
  ) +
  
  geom_line(orientation = "y",
    aes(x = N_SP / ratio), 
    color = color_sim, linewidth = 1.2
  ) +
  geom_line(orientation = "y",
    aes(x = N_SP_IUCN), 
    color = color_iucn, linewidth = 1.2
  ) +
  
  scale_x_continuous(
    name = "IUCN Species Richness",
    sec.axis = sec_axis(~ . * ratio, name = "Simulation Richness") 
  ) +
  
  scale_y_continuous(
    name = "Latitude",
    breaks = seq(-60, 80, by = 20),
    labels = function(x) paste0(abs(x), ifelse(x > 0, "°N", ifelse(x < 0, "°S", "°")))
  ) +
  
  theme_classic() +
  theme(
    axis.title.x.bottom = element_text(color = color_iucn, face = "bold", size = 12, margin = margin(t = 10)),
    axis.text.x.bottom = element_text(color = color_iucn, size = 10),
    axis.ticks.x.bottom = element_line(color = color_iucn),
    axis.line.x.bottom = element_line(color = color_iucn),
    axis.title.x.top = element_text(color = color_sim, face = "bold", size = 12, margin = margin(b = 10)),
    axis.text.x.top = element_text(color = color_sim, size = 10),
    axis.ticks.x.top = element_line(color = color_sim),
    axis.line.x.top = element_line(color = color_sim),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_line(color = "#f0f0f0", linetype = "dashed"),
    
    plot.margin = margin(20, 20, 20, 20)
  )
p_ldg
ggplot(lat, aes(y = lat_bin, x = N_SP)) +
  
  geom_ribbon(aes(xmin = pmax(0, N_SP - sd), xmax = N_SP + sd), 
              alpha = 0.3,
              color = NA) +
  
  geom_line(orientation = "y", linewidth = 1) +
  
  labs(
    x = "Number of Species",
    y = "Latitude"
  ) +
  
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines")
  )

cor(continents.full$N.sp.null, continents.full$N.sp.sim)
cor(continents.full$N.iucn, continents.full$N.sp.sim)
cor(continents.full$N.iucn, continents.full$N.sp.null)
p<-p1+p2+p3+p_ldg
p
ggsave(p, filename="../Figures/Figure.Richness/Figure.Richness.pdf", width=12, height=5)
ggsave(p, filename="../Figures/Figure.Richness/Figure.Richness.png", width=12, height=5)
