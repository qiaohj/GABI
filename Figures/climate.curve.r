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
x_label<-"Million years ago (Mya)"
p_curve <- ggplot(tasmax_se, aes(x = year * -2, y=v))+
  geom_line(aes(y = v), colour = color_high)+
  geom_line(data=tasmin_se, aes(y = v), colour = color_mid2)+
  geom_line(data=pr_se, aes(y = v /3 - 300), colour = color_low)+
  geom_vline(aes(xintercept = -3600), linetype=2)+
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
saveRDS(p_curve, "../Figures/Climate_curve/Climate_curve.rda")
ggsave(p_curve, filename="../Figures/Climate_curve/Climate_curve.pdf", width=10, height=5)
ggsave(p_curve, filename="../Figures/Climate_curve/Climate_curve.png", width=10, height=5, bg="white")
