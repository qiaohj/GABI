library(data.table)
library(ggplot2)
library(patchwork)
library(sf)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
if (F){
  cells<-readRDS("../Data/cells.with.dist.rda")
  cells<-data.table(global_id=cells$seqnum, 
                    continent=cells$continent,
                    lon=cells$lon,
                    lat=cells$lat)
  cells[continent %in% c("bridge1", "bridge2"), continent:="North America"]
  
  final.distribution<-readRDS("../Data/Tables/Final.Distribution.rda")
  if (F){
    
    ggplot(hexagon[which(hexagon$seqnum %in% 
                          unique(final.distribution.continent[continent=="bridge2"]$global_id)),])+
      geom_sf(data=hexagon)+geom_sf(fill="red")
  }
  final.distribution.continent<-merge(final.distribution, cells, by.x="global_id",
                                      by.y="global_id")
  
  unique(final.distribution.continent[continent=="bridge2"]$seed_id)
  
  table(final.distribution.continent$continent)
  cells<-readRDS("../Data/cells.with.dist.rda")
  
  
  cells<-data.table(seed_id=cells$seqnum, 
                    seed_continent=cells$continent)
  cells[seed_continent %in% c("bridge1", "bridge2"), seed_continent:="North America"]
  final.distribution.continent<-merge(final.distribution.continent, cells,
                                      by.x="seed_id", by.y="seed_id")
  species.range<-final.distribution.continent[, .(max.lat=max(lat),
                                                  max.lon=max(lon),
                                                  min.lat=min(lat),
                                                  min.lon=min(lon),
                                                  abs.max.lat=max(abs(lat))),
                                              by=list(continent, seed_continent, sp_id, seed_id,
                                                      nb, da)]
  saveRDS(species.range, "../Data/Tables/Species.Range.with.continent.rda")
}
species.range<-readRDS("../Data/Tables/Species.Range.with.continent.rda")
species.range$seed_id<-as.numeric(species.range$seed_id)
species.range$label<-sprintf("%s.%s", species.range$nb, species.range$da)


seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")
hexagon<-readRDS("../Data/cells.with.dist.rda")
cells<-data.table(seed_id=hexagon$seqnum, seed_continent=hexagon$continent)
range.list<-list()

for (r in c(1:100)){
  print(r)
  item<-species.range[seed_id %in% seeds.all[rep==r]$seed_id]
  item$rep<-r
  range.list[[r]]<-item
}
range.df<-rbindlist(range.list)

range.df$label<-sprintf("%s.%s", range.df$nb, range.df$da)
range.se<-range.df[, .(max.lat=mean(max.lat), 
                       mas.lat.sd=sd(max.lat),
                       abs.max.lat=mean(abs.max.lat),
                       abs.max.lat.sd=sd(abs.max.lat)),
                   by=list(nb, da, continent, seed_continent, label)]
ggplot(range.se)+
  geom_errorbar(aes(x=continent, 
                    ymin=abs.max.lat-abs.max.lat.sd, 
                    ymax=abs.max.lat+abs.max.lat.sd, 
                    group=seed_continent),
                position=position_dodge(width = 0.8), width=0.2)+
  geom_point(aes(x=continent, y=abs.max.lat, color=seed_continent),
             position=position_dodge(width = 0.8), size=5)+
  facet_grid(nb~da)+
  theme_bw()


range.se<-range.df[, .(max.lat=mean(max.lat), 
                       mas.lat.sd=sd(max.lat),
                       abs.max.lat=mean(abs.max.lat),
                       abs.max.lat.sd=sd(abs.max.lat)),
                   by=list(continent, seed_continent)]
ggplot(range.se)+
  geom_errorbar(aes(x=continent, 
                    ymin=abs.max.lat-abs.max.lat.sd, 
                    ymax=abs.max.lat+abs.max.lat.sd, 
                    group=seed_continent),
                position=position_dodge(width = 0.8), width=0.2)+
  geom_point(aes(x=continent, y=abs.max.lat, color=seed_continent),
             position=position_dodge(width = 0.8), size=5)+
  theme_bw()


#############useless.
p1<-ggplot(species.range.all)+
  geom_boxplot(aes(x=label, y=abs.max.lat, color=seed_continent))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2<-ggplot(species.range.all[max.lat>0])+
  geom_boxplot(aes(x=label, y=max.lat, color=seed_continent))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3<-ggplot(species.range.all[min.lat<0])+
  geom_boxplot(aes(x=label, y=abs(min.lat), color=seed_continent))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p<-p1+p2+p3+plot_layout(guides = 'collect') + plot_annotation(
  title = 'max abs lat (lat), max lat in NA (middle) and max lat in SA (right)',
  tag_levels = 'A'
)
ggsave(p, filename="../Figures/Max.Lat.pdf", width=12, height=5)


ggplot(species.range.all)+
  geom_boxplot(aes(x=seed_continent, y=abs.max.lat, color=seed_continent))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
