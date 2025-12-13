df<-fread("/media/huijieqiao/Butterfly/GABI/Results.NULL/9328.BIG-BIG.POOR/9328.BIG-BIG.POOR.log")
xx<-hexagon[which(hexagon$seqnum %in% unique(df$V2)),]
ggplot()+geom_sf(data=hexagon, aes(fill=NA))+
  geom_sf(data=xx, aes(fill=continent))


nee_2019_no_change<-readRDS("../Data/NEE.2019/no_change_maps.rda")

range(nee_2019_no_change$year)

nee_2019_no_change[year==100]

y100<-nee_2019_no_change[year==100]

y1200<-nee_2019_no_change[year==120000]


richness<-y1200[,.(N.species=length(unique(sp_id))), by=list(x, y, lon, lat, nb, da)]

richness.full<-richness[, .(N.species=sum(N.species)), by=list(x, y, lon, lat)]
ggplot(richness.full)+geom_tile(aes(x=lon, y=lat, fill=N.species))+
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint = mean(richness.full$N.species))

ggplot(richness)+geom_tile(aes(x=lon, y=lat, fill=N.species))+
  scale_fill_gradient(low="blue", high="red")+
  facet_grid(nb~da)
