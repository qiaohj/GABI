library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

label<-"World"
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration_continent.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmean<-data.table(dbReadTable(conn, "tasmean"))
dbDisconnect(conn)
pr$var<-"pr"
tasmean$var<-"tasmean"
all_v<-rbindlist(list(pr, tasmean))
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
      range<-item_df[var==vv]
      range<-c(range$mean - range$sd * 3, range$mean + range$sd * 3)
      v_items_sub<-v_items[var==vv & between(v, range[1], range[2])]
      nb<-data.table(species=sp, min=min(v_items_sub$v), max=max(v_items_sub$v))
      nb$range<-nb$max-nb$min
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

nb_full_df<-readRDS(sprintf("../Data/IUCN_NB/Mammals.%s.rda", label))


ggplot(nb_full_df)+geom_histogram(aes(x=range))+
  facet_wrap(~var, scale="free")

ggplot(nb_full_df)+geom_histogram(aes(x=range))+
  facet_grid(continent~var, scale="free")

quantile(round(nb_full_df[range>1 & var=="tasmean"]$range, 2), 
         c(0, 0.25, 0.5, 0.75, 0.9, 0.99, 1))

quantile(round(nb_full_df[range>1 & var=="pr"]$range, 2), 
         c(0, 0.25, 0.5, 0.75, 0.9, 0.99, 1))

quantile(round(nb_full_df[continent=="America" & range>1 & var=="tasmean"]$range, 2), 
         c(0, 0.25, 0.5, 0.75, 0.9, 0.99, 1))

quantile(round(nb_full_df[continent=="America" & range>1 & var=="pr"]$range, 2), 
         c(0, 0.25, 0.5, 0.75, 0.9, 0.99, 1))

nb_full_df_sub<-nb_full_df[continent=="America"]
sp1<-nb_full_df_sub[var=="pr" & range>=1129.680]$species
sp2<-nb_full_df_sub[var=="tasmean" & range>=17.2900]$species
sp_american_large<-unique(c(sp1, sp2))
dddd<-data.table(all_v_last)
range(dddd[var=="pr"]$v)
range(pr$v)
saveRDS(nb_full_df_sub, "../Data/Tables/nb_range_mammals_iucn.rda")


