library(data.table)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(RSQLite)
library(DBI)
library(heatmaply)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  base_db<-"../Configuration/configuration.sqlite"
  conn <- dbConnect(RSQLite::SQLite(), base_db)
  pr<-data.table(dbReadTable(conn, "pr"))
  tasmax<-data.table(dbReadTable(conn, "tasmax"))
  tasmin<-data.table(dbReadTable(conn, "tasmin"))
  dbDisconnect(conn)
  
  pr.mean<-pr[,.(v=mean(v), sd=sd(v), var="Annual Precipitation"), by=list(year)]
  tasmax.mean<-tasmax[,.(v=mean(v), sd=sd(v), var="Annual Maximum Temperature"), by=list(year)]
  tasmin.mean<-tasmin[,.(v=mean(v), sd=sd(v), var="Annual Minimum Temperature"), by=list(year)]
  env_yearly_avg<-rbindlist(list(pr.mean, tasmax.mean, tasmin.mean))
  saveRDS(env_yearly_avg, "../Figures/Figure1.Overview/Data/env_yearly_avg.rda")
}
world <- ne_countries(scale = "small", returnclass = "sf")
env_df<-readRDS("../Figures/Figure1.Overview/Data/env_yearly_avg.rda")
wc_col<-c("#4477AA", "#EE6677")
v_tmax_mean<-env_df[var=="Annual Maximum Temperature"]
v_tmin_mean<-env_df[var=="Annual Minimum Temperature"]
v_prcp_mean<-env_df[var=="Annual Precipitation"]

range(v_tmax_mean$v)
range(v_prcp_mean$v)
years<-seq(1900, 0, by=-10)
p <- ggplot(v_tmax_mean[year %in% years], aes(x = year * -2))+
  geom_line(aes(y = v), colour = wc_col[2])+
  #geom_line(data=v_tmin_mean, aes(y = mean_v), colour = wc_col[1])+
  geom_line(data=v_prcp_mean[year %in% years], aes(y = v /7.5 - 100), colour = "#4477AA")+
  scale_y_continuous(sec.axis = sec_axis(~(.+100)*7.5, name = "Precipitation (mm/year)"))+
  labs(y = "Maximum Temperature (degree)",
       x = x_label)+
  xlim(-3800, 0)+
  theme_bw()+
  theme(legend.position.inside = c(0.8, 0.9),
        axis.title.y = element_text(colour = "#EE6677"),
        axis.line.y = element_line(color = "#EE6677"), 
        axis.ticks.y = element_line(color = "#EE6677"),
        axis.text.y = element_text(colour = "#EE6677"),
        axis.title.y.right = element_text(colour = "#4477AA"),
        axis.line.y.right = element_line(color = "#4477AA"), 
        axis.ticks.y.right = element_line(color = "#4477AA"),
        axis.text.y.right = element_text(colour = "#4477AA"))

p
ggsave(p, filename="../Figures/Figure1.Overview/env_change.pdf", width=12, height=4)

years<-seq(1900, 0, by=-1)
p.full <- ggplot(v_tmax_mean[year %in% years], aes(x = year * -2))+
  geom_line(aes(y = v), colour = wc_col[2])+
  #geom_line(data=v_tmin_mean, aes(y = mean_v), colour = wc_col[1])+
  geom_line(data=v_prcp_mean[year %in% years], aes(y = v /7.5 - 100), colour = "#4477AA")+
  scale_y_continuous(sec.axis = sec_axis(~(.+100)*7.5, name = "Precipitation (mm/year)"))+
  labs(y = "Maximum Temperature (degree)",
       x = x_label)+
  xlim(-3800, 0)+
  theme_bw()+
  theme(legend.position.inside = c(0.8, 0.9),
        axis.title.y = element_text(colour = "#EE6677"),
        axis.line.y = element_line(color = "#EE6677"), 
        axis.ticks.y = element_line(color = "#EE6677"),
        axis.text.y = element_text(colour = "#EE6677"),
        axis.title.y.right = element_text(colour = "#4477AA"),
        axis.line.y.right = element_line(color = "#4477AA"), 
        axis.ticks.y.right = element_line(color = "#4477AA"),
        axis.text.y.right = element_text(colour = "#4477AA"))

p.full
ggsave(p.full, filename="../Figures/Figure1.Overview/env_change_full.pdf", width=20, height=4)

base_db<-"../Configuration/configuration.sqlite"
conn <- dbConnect(RSQLite::SQLite(), base_db)
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
dbDisconnect(conn)

polygon<-read_sf("../Shape/isea3h8/N_S_America.shp")
y=1800

polygon_tmax<-merge(polygon, tasmax[year==y], by.x="seqnum", by.y="global_id")
max_t<-max(tasmax[year==y]$v)
min_t<-min(tasmin[year==y]$v)

fake_colors<-data.table(x=c(0, 0), y=c(0, 0), v=c(max_t, min_t))

mycol <- cool_warm(max_tmax - min_tmax + 1)

p_america_tmax<-ggplot(polygon_tmax, aes(colour=v)) +
  geom_point(data=fake_colors, aes(x=x, y=y, colour=v))+
  geom_sf() + 
  scale_colour_gradientn(colours  = mycol)+
  coord_sf(crs = st_crs(crs_america))+
  labs(colour="TMAX")+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank())
p_america_tmax
polygon_tmin<-merge(polygon, tasmin[year==y], by.x="seqnum", by.y="global_id")

p_america_tmin<-ggplot(polygon_tmin, aes(colour=v)) +
  geom_point(data=fake_colors, aes(x=x, y=y, colour=v))+
  geom_sf() + 
  scale_colour_gradientn(colours  = mycol)+
  coord_sf(crs = st_crs(crs_america))+
  labs(colour="TMIN")+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank())

p_america_tmin
polygon_pr<-merge(polygon, pr[year==y], by.x="seqnum", by.y="global_id")
max_prcp<-max(pr[year==y]$v)
min_prcp<-min(pr[year==y]$v)

mycol <- Blues((max_prcp - min_prcp + 1) * 1.5)[((max_prcp - min_prcp + 1) * 0.5) : 
                                                  ((max_prcp - min_prcp + 1) * 1.5)]
polygon_pr$color<-mycol[floor(polygon_pr$v) - min_prcp + 2]

p_america_pr<-ggplot(polygon_pr, aes(colour=v)) +
  geom_sf() + 
  scale_colour_gradientn(colours  = mycol)+
  coord_sf(crs = st_crs(crs_america))+
  labs(colour="PREC")+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank())



