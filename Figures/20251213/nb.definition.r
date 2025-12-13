library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
dfs<-readRDS(sprintf("../Data/IUCN_NB/%s/%s.rda",label,  "Mammals"))
dfs$group<-"Mammals"
#dfs<-dfs[N_CELLS>3]
dfs$iqr<-dfs$q3 - dfs$q1
dfs$range_iqr<-dfs$q3 + dfs$iqr*1.5 - (dfs$q1 - dfs$iqr*1.5)
dfs$range_3sd<-dfs$sd * 6
dfs$range_min_max<-dfs$max - dfs$min

pr_item<-dfs[var=="pr"]
tasmax_item<-dfs[var=="tasmax"]
tasmin_item<-dfs[var=="tasmin"]

pr_nb<-pr_item[, c("species", "N_CELLS", "group", "range_iqr", "range_3sd", "range_min_max")]
pr_nb$type<-"pr"
tas<-merge(tasmax_item, tasmin_item, by=c("species", "N_CELLS", "group"))
tas$range_3sd<-tas$mean.x + tas$sd.x * 3  - (tas$mean.y - tas$sd.y * 3)
tas$range_iqr<-tas$q3.x + tas$iqr.x * 1.5 - (tas$q1.y - tas$iqr.y * 1.5)
tas$range_min_max<-tas$max.x - tas$min.y
tas_nb<-tas[, c("species", "N_CELLS", "group", "range_iqr", "range_3sd", "range_min_max")]
tas_nb$type<-"tm"
nb<-rbindlist(list(pr_nb, tas_nb))
nb[, .(N=.N), by=list(group, type)]

hexagon<-read_sf("../Shape/isea3h8/N_S_America.shp")

sp<-"Urocyon cinereoargenteus"
mammals<-st_read("../Shape/IUCN/MAMMALS/MAMMALS_TERRESTRIAL_ONLY.shp")
sp_range<-mammals[which(mammals$binomial==sp),]
ggplot(sp_range)+geom_sf(data=hexagon)+geom_sf(aes(fill=factor(presence)))

conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration_continent.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
dbDisconnect(conn)
pr$var<-"pr"
tasmax$var<-"tasmax"
tasmin$var<-"tasmin"
all_v<-rbindlist(list(pr, tasmax, tasmin))
all_v_last<-all_v[year==0]
centroids<-st_centroid(hexagon)
all_v_last<-merge(centroids, data.frame(all_v_last), by.y="global_id", by.x="seqnum")
index<-st_contains(sp_range, all_v_last)
index<-unique(unlist(index))
v_items<-data.table(all_v_last[index,])
v_item.pr<-v_items[var=="pr"]
colnames(v_item.pr)[5]<-"v.pr"
v_item.tasmin<-v_items[var=="tasmin"]
colnames(v_item.tasmin)[5]<-"v.tasmin"
v_item.tasmax<-v_items[var=="tasmax"]
colnames(v_item.tasmax)[5]<-"v.tasmax"
v_item.df<-merge(v_item.pr, merge(v_item.tasmin, v_item.tasmax, by="seqnum"), by="seqnum")
nb.sp<-dfs[species==sp]
nb.sp$low<-nb.sp$mean-3*nb.sp$sd
nb.sp$high<-nb.sp$mean+3*nb.sp$sd
nb.sp[low<0 & var=="pr", low:=0]
ggplot(v_item.df)+
  geom_point(aes(x=v.tasmin, y=v.pr), color="lightblue", alpha=0.2)+
  geom_point(aes(x=v.tasmax, y=v.pr), color="red", alpha=0.2)+
  geom_hline(data=nb.sp[var=="pr"], 
             aes(yintercept = low), linetype=1)+
  geom_hline(data=nb.sp[var=="pr"], 
             aes(yintercept = mean), linetype=2)+
  geom_hline(data=nb.sp[var=="pr"], 
             aes(yintercept = high), linetype=1)+
  geom_vline(data=nb.sp[var=="tasmin"], 
             aes(xintercept = low+14), linetype=1, color="lightblue")+
  geom_vline(data=nb.sp[var=="tasmin"], 
             aes(xintercept = mean+7), linetype=2, color="lightblue")+
  geom_vline(data=nb.sp[var=="tasmax"], 
             aes(xintercept = high-4), linetype=1, color="red")+
  geom_vline(data=nb.sp[var=="tasmax"], 
             aes(xintercept = mean-2), linetype=2, color="red")+
  theme_bw()
nb_sp<-
label<-"World"



nb.sim<-readRDS("../Data/nb.rda")

nb.sim<-data.table(type=c("pr", "pr", "tm", "tm"),
                   nb=c("MODERATE-MODERATE", "BIG-BIG",
                        "MODERATE-MODERATE", "BIG-BIG"),
                   v=c(nb.sim$pr[2], nb.sim$pr[13],
                       nb.sim$t[2], nb.sim$t[13]))

ggplot(nb)+geom_histogram(aes(x=range_3sd))+
  geom_vline(data=nb.sim, aes(color=nb, xintercept=v))+
  scale_x_sqrt()+
  facet_wrap(~type, scale="free", nrow=2)

