library(astrochron)
library(ggplot2)
library(RSQLite)
library(DBI)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
la04<-getLaskar(sol="la04", verbose=T)
la10a<-getLaskar(sol="la10a", verbose=T)
la10b<-getLaskar(sol="la10b", verbose=T)
la10c<-getLaskar(sol="la10c", verbose=T)
la10d<-getLaskar(sol="la10d", verbose=T)
la11<-getLaskar(sol="la11", verbose=T)
insolation<-getLaskar(sol="insolation", verbose=T)

la<-merge(la04, la10a, by="Time_ka", all=T)
la<-merge(la, la10b, by="Time_ka", all=T)
la<-merge(la, la10c, by="Time_ka", all=T)
la<-merge(la, la10d, by="Time_ka", all=T)
#la<-merge(la, la11, by="Time_ka", all=T)

saveRDS(la, "../Data/la2010.rda")

format_dt<-function(dt, type){
  colnames(dt)<-c("Time_ka", "v")
  dt$v_scale<-scale(dt$v)
  dt$type<-type
  dt
}


range(la11$Time_ka)
range(la10a$Time_ka)
p<-ggplot(la_gg)+geom_line(aes(x=Time_ka, y=v, color=type))+
  xlim(0, 1e3)
p

base_db<-"../Configuration/configuration_continent.sqlite"
envdb <- dbConnect(SQLite(), base_db)
v_prcp<-dbReadTable(envdb, "pr")
v_tmax<-dbReadTable(envdb, "tasmax")
v_tmin<-dbReadTable(envdb, "tasmin")
dbDisconnect(envdb)
v_tmax<-data.table(v_tmax)
v_tmax_mean<-v_tmax[, .(v=mean(v)), by=list(year)]
colnames(v_tmax_mean)<-c("Time_ka", "v")
la_gg<-rbindlist(list(
  format_dt(la04[, c("Time_ka", "ecc_LA04")], "la04"),
  format_dt(la04[, c("Time_ka", "prec_LA04")], "prec_LA04"),
  format_dt(la04[, c("Time_ka", "tilt_LA04")], "tilt_LA04"),
  format_dt(la10a, "la10a"),
  format_dt(la10b, "la10b"),
  format_dt(la10c, "la10c"),
  format_dt(la10d, "la10d"),
  format_dt(la11, "la11"),
  format_dt(v_tmax_mean, "tasmax")
))


ggplot(la_gg)+
  geom_line(aes(x=Time_ka, y=v_scale, color=type))+
  xlim(0, 1e2)
