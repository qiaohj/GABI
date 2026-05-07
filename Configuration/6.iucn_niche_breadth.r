library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

label<-"World"
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration_continent.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmean<-data.table(dbReadTable(conn, "tasmean"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
dbDisconnect(conn)
pr$var<-"pr"
tasmean$var<-"tasmean"
tasmax$var<-"tasmax"
tasmin$var<-"tasmin"


all_v<-rbindlist(list(pr, tasmean, tasmin, tasmax))
all_v_last<-all_v[year==0]
shpfname = "../Shape/isea3h8/isea3h8_sf.shp"
hexagon<-read_sf(shpfname)
hexagon<-hexagon[which(hexagon$seqnum %in% all_v_last$global_id),]
#plot(hexagon$geometry)
centroids<-st_centroid(hexagon)
all_v_last<-merge(centroids, data.frame(all_v_last), by.y="global_id", by.x="seqnum")
#sf_use_s2(FALSE)

continents<-read_sf("../Shape/isea3h8/N_S_America.shp")
centroids<-st_centroid(continents)
centroids<-data.table(seqnum=centroids$seqnum, continent=centroids$continent)

mammals<-st_read("../Shape/IUCN/MAMMALS/MAMMALS_TERRESTRIAL_ONLY.shp")

i=1
species<-unique(mammals$binomial)
all_v_last<-merge(all_v_last, centroids, by="seqnum", all=T)
#all_v_last<-all_v_last[which(!all_v_last$seqnum %in% c(8920, 8921, 9002)),]
sf_use_s2(FALSE)
nb_full<-list()
for (i in c(1:length(species))){
  print(paste(i, length(species)))
  sp<-species[i]
  item<-mammals[which(mammals$binomial ==sp),]
  item<-item[which(item$presence %in% c(1)),]
  item<-item[which(item$origin %in% c(1, 2, 3)),]
  item<-item[which(item$seasonal %in% c(1, 2)),]
  if (nrow(item)==0){
    next()
  }
  
  index<-st_contains(item, all_v_last)
  index<-unique(unlist(index))
  if (F){
    plot(hexagon$geometry)
    plot(item$geometry, add=T)
    plot(all_v_last[index,]$geometry, add=T)
  }
  v_items<-data.table(all_v_last[index,])
  continent<-ifelse(nrow(v_items[continent %in% c("North America", "South America ")])>0,
                    "America", "None")
  
  if (nrow(v_items)>0){
    item_df<-v_items[, .(species=sp,
                         N_CELLS=length(unique(seqnum)),
                         min=min(v),
                         max=max(v),
                         sd=sd(v),
                         mean=mean(v),
                         q1=quantile(v, 0.25),
                         q3=quantile(v, 0.75),
                         q01=quantile(v, 0.01),
                         q99=quantile(v, 0.99)),
                     by=list(var)]
    vlist<-list()
    for (vv in unique(v_items$var)){
      if (F){
        hist(v_items[var=="tasmin"]$v)
      }
      range<-item_df[var==vv]
      range<-c(range$mean - range$sd * 3, range$mean + range$sd * 3)
      v_items_sub<-v_items[var==vv & between(v, range[1], range[2])]
      nb<-data.table(species=sp, 
                     min_3sd=min(v_items_sub$v), 
                     max_3sd=max(v_items_sub$v),
                     min=min(v_items[var==vv]$v),
                     max=max(v_items[var==vv ]$v))
      nb$range<-nb$max-nb$min
      nb$range_3sd<-nb$max_3sd-nb$min_3sd
      nb$var<-vv
      vlist[[length(vlist)+1]]<-nb
    }
    vdf<-rbindlist(vlist)
    vdf$continent<-continent
    nb_full[[length(nb_full)+1]]<-vdf
  }
}
nb_full_df<-rbindlist(nb_full)

saveRDS(nb_full_df, sprintf("../Data/IUCN_NB/Mammals.%s.rda", label))

table(nb_full_df$continent)/4

nb_full_df<-readRDS(sprintf("../Data/IUCN_NB/Mammals.%s.rda", label))

max.temp<-nb_full_df[var=="tasmax"]
min.temp<-nb_full_df[var=="tasmin"]
min.temp<-min.temp[, c("species", "min", "continent")]
max.temp<-max.temp[, c("species", "max", "continent")]
temp<-merge(max.temp, min.temp, by=c("species", "continent"))
temp$range<-temp$max-temp$min
hist(temp[continent=="America" & range>1]$range)


quantile(round(temp[species %in% nb_full_df[continent=="America" & range>0 & var=="pr"]$species]$range, 2), 
         c(0, 0.2, 0.4, 0.6, 0.8, 1))

quantile(round(nb_full_df[continent=="America" & range>0 & var=="pr"]$range, 2), 
         c(0, 0.2, 0.4, 0.6, 0.8, 1))




nb_full_df_sub<-nb_full_df[continent=="America"& range>0 & var=="pr"]
nb_full_df_sub<-nb_full_df_sub[, c("species", "continent", "max", "min", "range", "var")]
nb_full_df_sub2<-temp[species %in% nb_full_df_sub$species]
nb_full_df_sub2$var<-"tas"

nb_full_df_sub<-rbindlist(list(nb_full_df_sub, nb_full_df_sub2), fill=T)
saveRDS(nb_full_df_sub, "../Data/Tables/nb_range_mammals_iucn.rda")


