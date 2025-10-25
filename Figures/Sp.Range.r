library(data.table)
library(ggplot2)
library(sf)
library(patchwork)
setDTthreads(20)
sf::sf_use_s2(FALSE)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")


mammal.range<-readRDS("../Data/IUCN/mammals.dis.range.rda")
sim.range<-readRDS("../Data/Tables/Species.Range.rda")
nb<-readRDS("../Data/Tables/nb.rda")



mammal.nb<-readRDS(sprintf("../Data/IUCN_NB/%s/%s.rda","World",  "Mammals"))
mammal.nb$nb<-mammal.nb$sd * 6

mammal.nb.list<-list()
for (sp in unique(mammal.nb$species)){
  item<-mammal.nb[species==sp]
  item.sp<-data.table(species=sp,
                      nb.tasmin=item[var=="tasmin"]$nb,
                      nb.tasmax=item[var=="tasmax"]$nb,
                      nb.pr=item[var=="pr"]$nb)
  mammal.nb.list[[length(mammal.nb.list)+1]]<-item.sp
}
mammal.nb.df<-rbindlist(mammal.nb.list)

nb_pr<-quantile(mammal.nb.filter$nb.pr, 0.75)
nb_tm<-quantile(mammal.nb.filter$nb.tasmax, 0.75)

mammal.nb.filter<-mammal.nb.df[(nb.tasmin>=nb_tm) |
                                 (nb.tasmax>=nb_tm ) |
                                 (nb.pr>nb_pr)]
hist(mammal.range$N.Cell)
hist(sim.range$N.Cells)

p1<-ggplot(mammal.range[species %in% mammal.nb.filter$species])+
  geom_histogram(aes(x=N.Cell))
p1

mean(mammal.range[species %in% mammal.nb.filter$species]$N.Cell)
mean(sim.range$N.Cells)

p2<-ggplot(sim.range)+geom_histogram(aes(x=N.Cells))
p2

p1+p2 + plot_annotation(
  title = 'Range of mammals (left) and range of simulation (right)',
  tag_levels = 'A'
)
