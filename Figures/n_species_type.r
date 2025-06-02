library(data.table)
library(ggh4x)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/GABI/GABI")

if (F){
  delta_Species<-readRDS("../Data/Tables/100k.speciation.years/delta_Species.rda")
  type_N<-readRDS("../Data/Tables/100k.speciation.years/type_N.rda")
  
  all_delta_Species<-list()
  for (rrrr in c(1:max(delta_Species$rep))){
    item<-delta_Species[rep==rrrr]
    prev_y<-item[year==-1800]
    print(paste(rrrr))
    for (y in c(-1799:0)){
      
      item_y<-item[year==y]
      prev_y$year<-NULL
      colnames(item_y)<-c("NB", "seed_continent", "c.south.america", "c.north.america", "year", "rep")
      item_y<-merge(item_y, prev_y, by=c("NB", "rep", "seed_continent"))
      item_y$south.america<-item_y$south.america+item_y$c.south.america
      item_y$north.america<-item_y$north.america+item_y$c.north.america
      all_delta_Species[[length(all_delta_Species)+1]]<-item_y
      prev_y<-item_y[, c("NB", "year", "rep", "south.america", "north.america", "seed_continent")]
    }
  }
  all_delta_Species<-rbindlist(all_delta_Species)
  all_delta_Species<-all_delta_Species[, .(south.america=mean(south.america), sd.south.america=sd(south.america),
                                           north.america=mean(north.america), sd.north.america=sd(north.america)),
                                       by=list(NB, year, seed_continent)]
  
  saveRDS(all_delta_Species, "../Data/Tables/100k.speciation.years/all.N_Species.rda")
}
burn_in<--3100/2

all_delta_Species<-readRDS("../Data/Tables/100k.speciation.years/all.N_Species.rda")


north<-all_delta_Species[, c("NB", "year", "north.america", "sd.north.america", "seed_continent")]
colnames(north)[c(3,4)]<-c("N", "SD")
north$continent<-"North America"

south<-all_delta_Species[, c("NB", "year", "south.america", "sd.south.america", "seed_continent")]
colnames(south)[c(3,4)]<-c("N", "SD")
south$continent<-"South America"

N.species<-rbindlist(list(north, south))
p<-ggplot(N.species[year>=burn_in])+
  geom_ribbon(aes(x=year, ymin=N-SD, ymax=N+SD, fill=seed_continent), color="lightgrey", alpha=0.3)+
  geom_line(aes(x=year, y=N, color=seed_continent))+
  facet_grid(continent~NB, scale="free")+
  theme_bw()
p
ggsave(p, filename="../Figures/N.Events/N.Species.png", width=8, height=6)


type_N<-readRDS("../Data/Tables/100k.speciation.years/type_N.rda")
type_N<-type_N[, .(south.america=mean(south.america), sd.south.america=sd(south.america),
                   north.america=mean(north.america), sd.north.america=sd(north.america)),
               by=list(NB, year, type, seed_continent)]

north.type<-type_N[, c("NB", "year", "north.america", "sd.north.america", 
                       "type", "seed_continent")]
colnames(north.type)[c(3,4)]<-c("N", "SD")
north.type$continent<-"North America"

south.type<-type_N[, c("NB", "year", "south.america", "sd.south.america", "type", "seed_continent")]
colnames(south.type)[c(3,4)]<-c("N", "SD")
south.type$continent<-"South America"
N.type<-rbindlist(list(north.type, south.type))

N.type<-N.type[type %in% c("Speciation", "Extinction", 
                           "New.Immigrants", "Local.Extinction",
                           "Switch")]

p<-ggplot(N.type[year>=burn_in])+
  geom_ribbon(aes(x=year, ymin=abs(N)-SD, ymax=abs(N)+SD, fill=seed_continent), color="lightgrey", alpha=0.3)+
  geom_line(aes(x=year, y=abs(N), color=seed_continent))+
  facet_grid2(type~NB+continent, independent="y", scale="free")+
  scale_y_sqrt()+
  theme_bw()
p
ggsave(p, filename="../Figures/N.Events/N.Type.png", width=16, height=8)
if (F){
  p<-ggplot(N.type[seed_continent!=continent & year>=burn_in])+geom_line(aes(x=year, y=abs(N), color=seed_continent))+
    facet_grid2(type~NB+continent, independent="y", scale="free")+
    scale_y_sqrt()+
    theme_bw()
  p
  ggsave(p, filename="../Figures/N.Events/N.Type.Offsite.png", width=16, height=6)
}



type_N<-readRDS("../Data/Tables/100k.speciation.years/type_N.rda")
N.type.all<-type_N[year>=burn_in, .(south.america=sum(south.america),
                                     north.america=sum(north.america)), 
                   by=list(NB, rep, type, seed_continent)]

north.N.type.all<-N.type.all[, c("NB", "type", "north.america", "seed_continent")]
colnames(north.N.type.all)[c(3)]<-c("N")
north.N.type.all$continent<-"North America"

south.N.type.all<-N.type.all[, c("NB", "type", "south.america", "seed_continent")]
colnames(south.N.type.all)[c(3)]<-c("N")
south.N.type.all$continent<-"South America"

N.type.all<-rbindlist(list(north.N.type.all, south.N.type.all))


N.type.all<-N.type.all[type %in% c("Speciation", "Extinction", 
                                   "New.Immigrants", "Local.Extinction",
                                   "Switch")]

p<-ggplot(N.type.all)+geom_boxplot(aes(x=NB, y=abs(N), color=seed_continent))+
  facet_grid2(continent~type, scale="free", independent="y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
ggsave(p, filename="../Figures/N.Events/N.Type.all.png", width=12, height=6)



N.type
N.species

N.full<-merge(N.type, N.species, by=c("NB", "year", "seed_continent", "continent"))
colnames(N.full)<-c("NB", "year", "seed_continent", "continent",
                    "N.type", "SD.type", "type", "N.species", "SD.species")

N.full$per<-N.full$N.type/N.full$N.species


p<-ggplot(N.full[year>=burn_in])+geom_line(aes(x=year, y=abs(per), color=seed_continent))+
  facet_grid2(type~NB+continent, independent="y", scale="free")+
  #scale_y_sqrt()+
  theme_bw()
p
ggsave(p, filename="../Figures/N.Events/N.Type.per.png", width=16, height=8)

p<-ggplot(N.full[year>=burn_in+500])+geom_line(aes(x=year, y=abs(per), color=seed_continent))+
  facet_grid2(type~NB+continent, independent="y", scale="free")+
  #scale_y_sqrt()+
  theme_bw()
p
ggsave(p, filename="../Figures/N.Events/N.Type.per.part.png", width=16, height=8)


N.full.mean<-N.full[year>=burn_in+500, .(per=mean(per)), 
                    by=list(NB, seed_continent, continent, type)] 
  
p<-ggplot(N.full.mean)+
  geom_boxplot(aes(x=NB, y=abs(per), color=seed_continent))+
  facet_grid2(continent~type, scale="free", independent="y")+
  theme_bw()+
  scale_y_sqrt()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
ggsave(p, filename="../Figures/N.Events/N.Type.per.boxplot.png", width=12, height=6)


