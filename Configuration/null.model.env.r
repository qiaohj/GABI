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
library(patchwork)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (T){
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
  pr<-data.table(dbReadTable(conn, "pr"))
  tasmax<-data.table(dbReadTable(conn, "tasmax"))
  tasmin<-data.table(dbReadTable(conn, "tasmin"))
  dist<-data.table(dbReadTable(conn, "distances"))
  dbDisconnect(conn)
  cells<-read_sf("../Shape/isea3h8/N_S_America.shp")
  cells<-data.table(global_id=cells$seqnum, continent=cells$continent)
  pr_se<-pr[, .(v=mean(v), sd_v=sd(v)), by=list(year)]
  tasmax_se<-tasmax[, .(v=mean(v), sd_v=sd(v)), by=list(year)]
  tasmin_se<-tasmin[, .(v=mean(v), sd_v=sd(v)), by=list(year)]
  
  pr_continent<-merge(pr, cells, by="global_id")
  tasmax_continent<-merge(tasmax, cells, by="global_id")
  tasmin_continent<-merge(tasmin, cells, by="global_id")
  
  pr_se_continent<-pr_continent[, .(v=mean(v), sd_v=sd(v)), by=list(year, continent)]
  tasmax_se_continent<-tasmax_continent[, .(v=mean(v), sd_v=sd(v)), by=list(year, continent)]
  tasmin_se_continent<-tasmin_continent[, .(v=mean(v), sd_v=sd(v)), by=list(year, continent)]
  
  tasmax_se_continent$continent<-factor(tasmax_se_continent$continent,
                                        levels=c("North America",
                                                 "South America",
                                                 "bridge1",
                                                 "bridge2"),
                                        labels=c("North America",
                                                 "South America",
                                                 "Isthmus", 
                                                 "Caribbean"))
  tasmin_se_continent$continent<-factor(tasmin_se_continent$continent,
                                        levels=c("North America",
                                                 "South America",
                                                 "bridge1",
                                                 "bridge2"),
                                        labels=c("North America",
                                                 "South America",
                                                 "Isthmus", 
                                                 "Caribbean"))
  
  pr_se_continent$continent<-factor(pr_se_continent$continent,
                                    levels=c("North America",
                                             "South America",
                                             "bridge1",
                                             "bridge2"),
                                    labels=c("North America",
                                             "South America",
                                             "Isthmus", 
                                             "Caribbean"))
  
}

max_tas_year<-tasmax_se[v==max(tasmax_se$v)]$year
min_tas_year<-tasmin_se[v==min(tasmin_se$v)]$year
max_pr_year<-pr_se[v==max(pr_se$v)]$year
min_pr_year<-pr_se[v==min(pr_se$v)]$year

wc_col<-c(color_low, color_high)
x_label<-"Million years ago (Mya)"

null_year<-data.frame(year=c(-3600, c(max_tas_year, min_tas_year, max_pr_year, min_pr_year) * -2),
           label=c("Start", "Hottest", "Coldest", "Wettest", "Driest"))

p_curve <- ggplot(tasmax_se, aes(x = year * -2, y=v))+
  geom_line(aes(y = v), colour = color_high)+
  geom_line(data=tasmin_se, aes(y = v), colour = color_mid2)+
  geom_line(data=pr_se, aes(y = v /3 - 300), colour = color_low)+
  geom_vline(aes(xintercept = -3600), linetype=2)+
  geom_vline(data=null_year, aes(xintercept = year),linetype=2)+
  geom_text(
    data = null_year,
    aes(x = year, y = Inf, label = label),
    vjust = 1.5,
    angle = 0,
    inherit.aes = FALSE
  )+
  scale_y_continuous(sec.axis = sec_axis(~(.+300) * 3, name = "Precipitation (mm/year)"))+
  scale_x_continuous(
    breaks = c(-3800, -3600, -3000, -2000, -1000, 0), 
    labels = c("3.8", "3.6", "3.0", "2.0", "1.0", "0.0")
  )+
  labs(y = "Temperature (°C)",
       x = x_label)+
  #xlim(-120, 0)+
  theme_bw()+
  theme(#axis.title.y = element_text(colour = color_high),
    #axis.line.y = element_line(color = color_high), 
    #axis.ticks.y = element_line(color = color_high),
    #axis.text.y = element_text(colour = color_high),
    axis.title.y.right = element_text(colour = color_low),
    axis.line.y.right = element_line(color = color_low), 
    axis.ticks.y.right = element_line(color = color_low),
    axis.text.y.right = element_text(colour = color_low))

p_curve
ggsave(p_curve, filename="../Figures/Climate_curve/Climate_curve_with_NULL.png", width=10, height=5)
ggsave(p_curve, filename="../Figures/Climate_curve/Climate_curve_with_NULL.pdf", width=10, height=5, bg="white")

set.seed(1024)
new_years<-sample(unique(tasmax_se[year<1800]$year))

saveRDS(new_years, "../Data/Tables/shuffling.year.rda")
tasmax_se[year<1800]$year<-new_years
tasmin_se[year<1800]$year<-new_years
pr_se[year<1800]$year<-new_years


p_curve <- ggplot(tasmax_se, aes(x = year * -2, y=v))+
  geom_line(aes(y = v), colour = color_high)+
  geom_line(data=tasmin_se, aes(y = v), colour = color_mid2)+
  geom_line(data=pr_se, aes(y = v /3 - 300), colour = color_low)+
  scale_y_continuous(sec.axis = sec_axis(~(.+300) * 3, name = "Precipitation (mm/year)"))+
  scale_x_continuous(
    breaks = c(-3800, -3600, -3000, -2000, -1000, 0), 
    labels = c("3.8", "3.6", "3.0", "2.0", "1.0", "0.0")
  )+
  labs(y = "Temperature (°C)",
       x = x_label)+
  #xlim(-120, 0)+
  theme_bw()+
  theme(#axis.title.y = element_text(colour = color_high),
    #axis.line.y = element_line(color = color_high), 
    #axis.ticks.y = element_line(color = color_high),
    #axis.text.y = element_text(colour = color_high),
    axis.title.y.right = element_text(colour = color_low),
    axis.line.y.right = element_line(color = color_low), 
    axis.ticks.y.right = element_line(color = color_low),
    axis.text.y.right = element_text(colour = color_low))

p_curve
ggsave(p_curve, filename="../Figures/Climate_curve/Climate_curve_shuffing.png", width=10, height=5)
ggsave(p_curve, filename="../Figures/Climate_curve/Climate_curve_shuffing.pdf", width=10, height=5, bg="white")
