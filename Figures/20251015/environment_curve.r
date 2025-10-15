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
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
  pr<-data.table(dbReadTable(conn, "pr"))
  tasmax<-data.table(dbReadTable(conn, "tasmax"))
  tasmin<-data.table(dbReadTable(conn, "tasmin"))
  dist<-data.table(dbReadTable(conn, "distances"))
  dbDisconnect(conn)
  
  pr_se<-pr[, .(v=mean(v), sd_v=sd(v)), by=list(year)]
  tasmax_se<-tasmax[, .(v=mean(v), sd_v=sd(v)), by=list(year)]
  tasmin_se<-tasmin[, .(v=mean(v), sd_v=sd(v)), by=list(year)]
  
  
}
wc_col<-c("#4477AA", "#EE6677")
x_label<-"Years before present (kyr)"
p <- ggplot(tasmax_se, aes(x = year * -2, y=v))+
  geom_line(aes(y = v), colour = wc_col[2])+
  #geom_line(data=v_tmin_mean, aes(y = mean_v), colour = wc_col[1])+
  geom_line(data=pr_se, aes(y = v /15 - 35), colour = "#4477AA")+
  scale_y_continuous(sec.axis = sec_axis(~(.+35) * 15, name = "Precipitation (mm/year)"))+
  labs(y = "Maximum Temperature (degree)",
       x = x_label)+
  #xlim(-120, 0)+
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

