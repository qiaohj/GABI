library(data.table)
library(ggh4x)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/GABI/GABI")

if (F){
  delta_Species<-readRDS("../Data/Tables/100k.speciation.years/delta_Species.rda")
  type_N<-readRDS("../Data/Tables/100k.speciation.years/type_N.rda")
  
  all_delta_Species<-list()
  for (rrrr in c(1:unique(delta_Species$rep))){
    item<-delta_Species[rep==rrrr]
    prev_y<-item[year==-1800]
    
    for (y in c(-1799:0)){
      print(paste(rrrr, y))
      item_y<-item[year==y]
      prev_y$year<-NULL
      colnames(item_y)<-c("NB", "origin_continent", "c.south.america", "c.north.america", "year", "rep")
      item_y<-merge(item_y, prev_y, by=c("NB", "rep", "origin_continent"))
      item_y$south.america<-item_y$south.america+item_y$c.south.america
      item_y$north.america<-item_y$north.america+item_y$c.north.america
      all_delta_Species[[length(all_delta_Species)+1]]<-item_y
      prev_y<-item_y[, c("NB", "year", "rep", "south.america", "north.america", "origin_continent")]
    }
    all_delta_Species<-rbindlist(all_delta_Species)
  }
  all_delta_Species<-all_delta_Species[, .(south.america=mean(south.america), sd.south.america=sd(south.america),
                                           north.america=mean(north.america), sd.north.america=sd(north.america)),
                                       by=list(NB, year, origin_continent)]
  
  saveRDS(all_delta_Species, "../Data/Tables/100k.speciation.years/all.N_Species.rda")
}
burn_in<--3100/2

all_delta_Species<-readRDS("../Data/Tables/100k.speciation.years/all.N_Species.rda")


north<-all_delta_Species[, c("NB", "year", "north.america", "sd.north.america", "origin_continent")]
colnames(north)[c(3,4)]<-c("N", "SD")
north$continent<-"North America"

south<-all_delta_Species[, c("NB", "year", "south.america", "sd.south.america", "origin_continent")]
colnames(south)[c(3,4)]<-c("N", "SD")
south$continent<-"South America"

N.species<-rbindlist(list(north, south))
p<-ggplot(N.species[year>=burn_in])+geom_line(aes(x=year, y=N, color=origin_continent))+
  facet_grid(continent~NB, scale="free")+
  theme_bw()
p
ggsave(p, filename="../Figures/N.Events/N.Species.png", width=8, height=6)


type_N<-readRDS("../Data/Tables/100k.speciation.years/type_N.rda")
type_N<-type_N[, .(south.america=mean(south.america), sd.south.america=sd(south.america),
                   north.america=mean(north.america), sd.north.america=sd(north.america)),
               by=list(NB, year, type, origin_continent)]

north.type<-type_N[, c("NB", "year", "north.america", "sd.north.america", "type", "origin_continent")]
colnames(north.type)[c(3,4)]<-c("N", "SD")
north.type$continent<-"North America"

south.type<-type_N[, c("NB", "year", "south.america", "sd.south.america", "type", "origin_continent")]
colnames(south.type)[c(3,4)]<-c("N", "SD")
south.type$continent<-"South America"
N.type<-rbindlist(list(north.type, south.type))

N.type<-N.type[type %in% c("Speciation", "Extinction", 
                           "New.Immigrants", "Local.Extinction")]
p<-ggplot(N.type[year>=burn_in])+geom_line(aes(x=year, y=abs(N), color=origin_continent))+
  facet_grid2(type~NB+continent, independent="y", scale="free")+
  scale_y_sqrt()+
  theme_bw()
p
ggsave(p, filename="../Figures/N.Events/N.Type.png", width=16, height=6)

p<-ggplot(N.type[origin_continent!=continent & year>=burn_in])+geom_line(aes(x=year, y=abs(N), color=origin_continent))+
  facet_grid2(type~NB+continent, independent="y", scale="free")+
  scale_y_sqrt()+
  theme_bw()
p
ggsave(p, filename="../Figures/N.Events/N.Type.Offsite.png", width=16, height=6)




type_N<-readRDS("../Data/Tables/100k.speciation.years/type_N.rda")
N.type.all<-type_N[year>burn_in+1, .(south.america=sum(south.america),
                                     north.america=sum(north.america)), 
                   by=list(NB, rep, type)]

north.N.type.all<-N.type.all[, c("NB", "type", "north.america")]
colnames(north.N.type.all)[c(3)]<-c("N")
north.N.type.all$continent<-"North America"

south.N.type.all<-N.type.all[, c("NB", "type", "south.america")]
colnames(south.N.type.all)[c(3)]<-c("N")
south.N.type.all$continent<-"South America"

N.type.all<-rbindlist(list(north.N.type.all, south.N.type.all))


N.type.all<-N.type.all[type %in% c("Speciation", "Extinction", 
                           "New.Immigrants", "Local.Extinction")]

p<-ggplot(N.type.all)+geom_boxplot(aes(x=NB, y=N, color=continent))+
  facet_wrap(~type, scale="free")+
  theme_bw()
ggsave(p, filename="../Figures/N.Events/N.Type.all.png", width=8, height=6)
