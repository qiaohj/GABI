library(dggridR)
library(terra)
library(sf)
library(dplyr)
library(furrr)
library(RSQLite)
library(reshape2)
library(data.table)
library(ggplot2)
library(ggmap)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
if (F){
  shpfname<-"../Data/Shape/isea3h8/isea3h8_sf.shp"
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
  pr<-data.table(dbReadTable(conn, "pr"))
  tasmax<-data.table(dbReadTable(conn, "tasmax"))
  tasmin<-data.table(dbReadTable(conn, "tasmin"))
  dist<-data.table(dbReadTable(conn, "distances"))
  dbDisconnect(conn)
  
  #before 3.1My 
  pr_item<-pr[year==1800]
  hexagon_pr_no_brige<-merge(hexagon, pr_item, by.x="seqnum", by.y="global_id")
  
  #after 3.1My 
  pr_item<-pr[year==0]
  hexagon_pr_bridge<-merge(hexagon, pr_item, by.x="seqnum", by.y="global_id")
  bridge<-hexagon_pr_bridge[
    which(!(hexagon_pr_bridge$seqnum %in% hexagon_pr_no_brige$seqnum)),]
  plot(bridge$geometry)
  
}

hexagon_ns<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")

#coords<-data.table(st_coordinates(st_centroid(hexagon_ns)))
#hexagon_ns$c_lon<-coords$X
#hexagon_ns$c_lat<-coords$Y

points<-data.table(hexagon_ns[, c("seqnum", "continent", "lon", "lat")])
points$geometry<-NULL

points$within<-between(points$lat, -22, 50)

points[, .(N=.N), by=list(continent, within)]
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
range(hexagon_ns[which(hexagon_ns$continent=="South America"), ]$lon)
l_s <- sf::st_linestring(as.matrix(data.frame(shape_pt_lon=seq(-125, -35, 1), 
                                            shape_pt_lat=rep(-22, length(seq(-125, -35, 1))))))

l_n <- sf::st_linestring(as.matrix(data.frame(shape_pt_lon=seq(-150, -15, 1), 
                                              shape_pt_lat=rep(50, length(seq(-150, -15, 1))))))

l_m <- sf::st_linestring(as.matrix(data.frame(shape_pt_lon=seq(-125, -35, 1), 
                                              shape_pt_lat=rep(14, length(seq(-125, -35, 1))))))

l_s <- sf::st_linestring(as.matrix(data.frame(shape_pt_lon=seq(-125, -35, 1), 
                                              shape_pt_lat=rep(-40, length(seq(-125, -35, 1))))))

l_n <- sf::st_linestring(as.matrix(data.frame(shape_pt_lon=seq(-150, -15, 1), 
                                              shape_pt_lat=rep(40, length(seq(-150, -15, 1))))))

l_m <- sf::st_linestring(as.matrix(data.frame(shape_pt_lon=seq(-125, -35, 1), 
                                              shape_pt_lat=rep(0, length(seq(-125, -35, 1))))))

lines<-st_sfc(list(l_s, l_n, l_m), crs = st_crs(hexagon_ns))

ggplot()+ 
  geom_sf(data=hexagon_ns,  aes(fill=continent),
          color=NA, linewidth=0.1) +
  geom_sf(data=lines, linetype=2)+
  coord_sf(crs = st_crs(crs_america))+
  #geom_hline(yintercept = c(-40, 40), linetype=2)+
  #geom_hline(yintercept = c(-22, 14, 50), linetype=2)+
  scale_fill_manual (breaks=c("North America", "South America",
                                "bridge1", "bridge2"),
                      labels=c("North America", "South America",
                               "bridge1", "bridge2"),
                     values=c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f"))+
  theme(
    axis.line = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
    #panel.grid.minor = element_line(color="black"),
    plot.background = element_rect(fill="#fde7c0"),
    panel.background = element_rect(fill="#fde7c0"),
    legend.background = element_rect(fill = "#fde7c0", color = NA),
    legend.title = element_blank(),
    #panel.border = element_blank(),
    legend.position="bottom",
    #legend.key.width=unit(0.8,"in"),
    #strip.background.x = element_blank(),
    #strip.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


ggplot()+ 
  geom_sf(data=hexagon_ns,  aes(fill=continent),
          color=NA, linewidth=0.1) +
  #coord_sf(crs = st_crs(crs_america))+
  geom_hline(yintercept = c(-40, 40), linetype=2)+
  #geom_hline(yintercept = c(-22, 14, 50), linetype=2)+
  scale_fill_manual (breaks=c("North America", "South America",
                              "bridge1", "bridge2"),
                     labels=c("North America", "South America",
                              "bridge1", "bridge2"),
                     values=c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f"))+
  theme(
    axis.line = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
    #panel.grid.minor = element_line(color="black"),
    plot.background = element_rect(fill="#fde7c0"),
    panel.background = element_rect(fill="#fde7c0"),
    legend.background = element_rect(fill = "#fde7c0", color = NA),
    legend.title = element_blank(),
    #panel.border = element_blank(),
    legend.position="bottom",
    #legend.key.width=unit(0.8,"in"),
    #strip.background.x = element_blank(),
    #strip.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
