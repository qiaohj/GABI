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
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
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
wc_col<-c(color_low, color_high)
x_label<-"Years before present (kyr)"
p <- ggplot(tasmax_se, aes(x = year * -2, y=v))+
  geom_line(aes(y = v), colour = color_high)+
  geom_line(data=tasmin_se, aes(y = v), colour = color_mid2)+
  geom_line(data=pr_se, aes(y = v /3 - 300), colour = color_low)+
  geom_vline(aes(xintercept = -3600), linetype=2)+
  scale_y_continuous(sec.axis = sec_axis(~(.+300) * 3, name = "Precipitation (mm/year)"))+
  labs(y = "Temperature (degree)",
       x = x_label)+
  #xlim(-120, 0)+
  theme_bw()+
  theme(axis.title.y = element_text(colour = color_high),
        axis.line.y = element_line(color = color_high), 
        axis.ticks.y = element_line(color = color_high),
        axis.text.y = element_text(colour = color_high),
        axis.title.y.right = element_text(colour = color_low),
        axis.line.y.right = element_line(color = color_low), 
        axis.ticks.y.right = element_line(color = color_low),
        axis.text.y.right = element_text(colour = color_low))

p
ggsave(p, filename="../Figures/Climate_curve/Climate_curve.pdf", width=10, height=5)
ggsave(p, filename="../Figures/Climate_curve/Climate_curve.png", width=10, height=5, bg="white")
