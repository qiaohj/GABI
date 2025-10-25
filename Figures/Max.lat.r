library(data.table)
library(ggplot2)
library(patchwork)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
species.range<-readRDS("../Data/Tables/Species.Range.rda")
species.range[, abs.max.lat:=pmax(abs(max.lat), abs(min.lat))]
species.range$seed_id<-as.numeric(species.range$seed_id)
species.range$label<-sprintf("%s.%s", species.range$nb, species.range$da)


seeds.all<-readRDS("../Data/Tables/random.seeds.rda")
hexagon<-readRDS("../Data/cells.with.dist.rda")
cells<-data.table(seed_id=hexagon$seqnum, seed_continent=hexagon$continent)
range.list<-list()

for (r in c(1:10)){
  print(r)
  item<-species.range[seed_id %in% seeds.all[rep==r]$seed_id]
  item$rep<-r
  range.list[[r]]<-item
}
range.df<-rbindlist(range.list)
range.df$seed_id<-as.numeric(range.df$seed_id)
range.df<-merge(range.df, cells, by="seed_id")
range.df$label<-sprintf("%s.%s", range.df$nb, range.df$da)
species.range.all<-merge(species.range, cells, by="seed_id")

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
