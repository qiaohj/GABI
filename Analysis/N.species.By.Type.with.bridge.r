library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(ggh4x)
library(ape)
library(phytools)
library(ggtree)
library(phangorn)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
if (F){
  sp<-readRDS("../Data/Tables/virtual.species.rda")
  table(sp$continent)
  sp$Parent<-sub("-[^-]*$", "", sp$sp_id)
  sp<-sp[nb!="HUGE-HUGE"]
  sp[sp_id==Parent, Parent:=""]
  head(sp)
  
  species.type.N<-readRDS("../Data/Tables/species.type.N.rda")
  species.type.N$sp_id<-as.character(species.type.N$sp_id)
  species.type.N$Parent<-as.character(species.type.N$Parent)
  species.type.N[sp_id==Parent, Parent:=""]

  seeds.all<-readRDS("../Data/Tables/random.seeds.rda")
  
  colnames(species.type.N)[c(8, 12)]<-c("origin_continent", "seed_continent")
  species.type.N[origin_continent!=seed_continent]
  
  species.type<-species.type.N[, c("sp_id", "NB", "DA", "from", "to", "type",
                                   "origin_continent", "seed_continent")]
  species.type<-species.type[NB!="HUGE-HUGE"]
  colnames(sp)[c(6,7)]<-c("NB", "DA")
  colnames(species.type)
  
  burn_in<-(-3100/2)
  
  sp_full<-merge(sp, species.type, by=c("sp_id", "NB", "DA"))
  colnames(sp_full)[5]<-"current_continent"
  sp_full$year<-sp_full$year * -1
  
  sp_full.N<-sp_full[, .(N=uniqueN(current_continent)), 
                     by=.(year, sp_id, NB, DA, seed_id, Parent, 
                             from, to, origin_continent, seed_continent)]
  sp_full$species.label.year<-sprintf("%s.%s.%s.%d", sp_full$sp_id,
                                 sp_full$NB, sp_full$DA, sp_full$year)
  sp_full.N$species.label.year<-sprintf("%s.%s.%s.%d", sp_full.N$sp_id,
                                   sp_full.N$NB, sp_full.N$DA, sp_full.N$year)
  sp_full.N$current_continent<-""
  bridge1.labels<-unique(sp_full[current_continent=="bridge1"]$species.label.year)
  bridge2.labels<-unique(sp_full[current_continent=="bridge2"]$species.label.year)
  sa.labels<-unique(sp_full[current_continent=="South America"]$species.label.year)
  na.labels<-unique(sp_full[current_continent=="North America"]$species.label.year)
  
  sp_full.N[species.label.year %in% bridge1.labels,
            current_continent:="bridge1"]
  sp_full.N[species.label.year %in% bridge2.labels,
            current_continent:="bridge2"]
  sp_full.N[species.label.year %in% sa.labels,
            current_continent:="South America"]
  sp_full.N[species.label.year %in% na.labels,
            current_continent:="North America"]
  sp_full.N[species.label.year %in% sa.labels & species.label.year %in% na.labels,
            current_continent:="Two continents"]
  table(sp_full.N$current_continent)
  sp_full.N[current_continent=="bridge1"]
  
  sp_prev<-sp_full.N[, c("current_continent", "sp_id", "NB", "DA", "year")]
  
  sp_prev$year<-sp_prev$year+1
  
  colnames(sp_prev)[1]<-"previous_continent"
  
  sp_full_continents<-merge(sp_full.N, sp_prev, by=c("sp_id", "NB", "DA", "year"), all=T)
  sp_full_continents.bak<-sp_full_continents
  
  #sp_full_continents[year>0]
  
  sp_full_continents<-sp_full_continents[year<=0]
  #sp_full_continents[is.na(seed_id)]
  
  sp_full_continents[is.na(previous_continent), previous_continent:="Unknown"]
  sp_full_continents[is.na(current_continent), current_continent:="Unknown"]
  
  sp_full_continents$seed_id<-as.numeric(sp_full_continents$seed_id)
  sp_full_continents[sp_id==Parent, Parent:=""]
  sp_full_continents$label<-sprintf("%d.%s.%s", sp_full_continents$seed_id,
                                    sp_full_continents$NB, sp_full_continents$DA)
  sp_full_continents$parent.label<-sprintf("%s.%s.%s", sp_full_continents$Parent,
                                           sp_full_continents$NB, sp_full_continents$DA)
  sp_full_continents$species.label<-sprintf("%s.%s.%s", sp_full_continents$sp_id,
                                            sp_full_continents$NB, sp_full_continents$DA)
  
  
  
  sp_parent<-sp_full_continents[species.label %in% sp_full_continents$parent.label, 
                                c("species.label", "year", "current_continent")]
  sp_parent$year<-sp_parent$year+1
  colnames(sp_parent)[c(1,3)]<-c("parent.label", "parent_continent")
  sp_full_continents<-merge(sp_full_continents, sp_parent, by=c("parent.label", "year"), all.x=T)
  sp_full_continents[is.na(parent_continent), parent_continent:=""]
  
  
  
  sp_full_extinct<-sp_full_continents[current_continent=="Unknown"]
  sp_full_extinct<-sp_full_extinct[!sp_id %in% sp_full_continents$Parent]
  sp_full_extinct$seed_id<-NULL
  sp_full_extinct$Parent<-NULL
  sp_full_extinct$from<-NULL
  sp_full_extinct$to<-NULL
  sp_full_extinct$origin_continent<-NULL
  sp_full_extinct$seed_continent<-NULL
  sp_full_extinct$label<-NULL
  sp_full_extinct$N<-0
  
  sp_raw<-unique(sp_full_continents[, c("sp_id", "seed_id", "Parent", "from", "to", "origin_continent", "seed_continent",
                                        "NB", "DA", "label")])
  
  sp_full_extinct<-merge(sp_full_extinct, sp_raw, by=c("sp_id", "NB", "DA"))
  sp_full_extinct<-sp_full_extinct[!is.na(seed_id)]
  
  sp_full_continents<-sp_full_continents[current_continent!="Unknown"]
  sp_full_continents<-rbindlist(list(sp_full_continents, sp_full_extinct), use.names=T)
  sp_full_continents[parent_continent=="Unknown" & year-from==1, parent_continent:=""]
  #sp_full_continents[type=="Speciation" & is.na(parent_continent)]
  
  #sp_full_continents[previous_continent=="North America" & current_continent=="North America" & parent_continent=="Unknown"]
  sp_full_continents[species.label=="10046-2-1.MODERATE-MODERATE.POOR"]
  
  sp_full_continents$type<-""
  sp_full_continents$gain.continent<-""
  sp_full_continents$loss.continent<-""
  
  combinations<-sp_full_continents[, .(N=.N), 
                                   by=list(parent_continent, previous_continent,
                                           current_continent, type, gain.continent,
                                           loss.continent)]
  setorderv(combinations, c("previous_continent", "current_continent", "parent_continent"))
  if (F){
    fwrite(combinations, 
           "../Data/full.combination.csv")
  }
  
  ##Detect the type of species
  combinations<-fread( "../Data/full.combination.csv")
  i=1
  for (i in c(1:nrow(combinations))){
    com<-combinations[i]
    print(paste(i, nrow(combinations), com$previous_continent, 
                com$current_continent, com$parent_continent,
                com$type,com$gain.continent,
                com$loss.continent))
    sp_full_continents[previous_continent==com$previous_continent &
                         current_continent==com$current_continent &
                         parent_continent==com$parent_continent, 
                       `:=`(type=com$type, gain.continent=com$gain.continent, 
                            loss.continent=com$loss.continent)]
    
  }
  event.N<-sp_full_continents[,.(N=.N), by=list(parent_continent, previous_continent, current_continent, type)]
  fwrite(event.N, 
         "../Data/full.event.N.csv")
  table(sp_full_continents$type)
  
  sp_full_continents$south.america<-0
  sp_full_continents$north.america<-0
  sp_full_continents[gain.continent %in% c("North America", "Two continents"),
                     north.america:=1]
  
  sp_full_continents[gain.continent %in% c("South America", "Two continents"),
                     south.america:=1]
  
  sp_full_continents[loss.continent %in% c("North America", "Two continents"),
                     north.america:=-1]
  
  sp_full_continents[loss.continent %in% c("South America", "Two continents"),
                     south.america:=-1]
  sp_full_continents[south.america==1 & north.america==1]
  
  saveRDS(sp_full_continents, "../Data/Tables/sp_full_continents.rda")
}

sp_full_continents<-readRDS("../Data/Tables/sp_full_continents.rda")
xxx<-sp_full_continents[, .(N=.N), by=list(NB, type, year)]
View(xxx[type=="New.Immigrants"])

seeds.all<-readRDS("../Data/Tables/random.seeds.rda")
rrrr<-1
delta_Species.all<-list()
type_N.all<-list()
for (rrrr in c(1:10)){
  print(rrrr)
  seeds<-seeds.all[rep==rrrr]
  sp_filter<-sp_full_continents[label %in% seeds$label]
  #xxx<-sp_filter[, .(N=.N), by=list(NB, type, year, origin_continent)]
  #setorderv(xxx, c("NB", "year"))
  #View(xxx[type=="New.Immigrants"])
  
  y=-500
  for (y in c(-1800:0)){
    print(paste(rrrr, y))
    item<-sp_filter[year==y]
    
    delta_Species<-item[, .(south.america=sum(south.america),
                            north.america=sum(north.america),
                            year=y, rep=rrrr), 
                        by=list(NB, seed_continent)]
    
    type_N<-item[, .(south.america=sum(south.america),
                     north.america=sum(north.america),
                     year=y, rep=rrrr), 
                 by=list(NB, type, seed_continent)]
    delta_Species.all[[length(delta_Species.all)+1]]<-delta_Species
    type_N.all[[length(type_N.all)+1]]<-type_N
  }
}
delta_Species.df<-rbindlist(delta_Species.all)
type_N.df<-rbindlist(type_N.all)
saveRDS(delta_Species.df, "../Data/Tables/delta_Species.rda")
saveRDS(type_N.df, "../Data/Tables/type_N.rda")


delta_Species.origin<-list()
type_N.origin<-list()
for (rrrr in c(1:10)){
  print(rrrr)
  seeds<-seeds.all[rep==rrrr]
  sp_filter<-sp_full_continents[label %in% seeds$label]
  
  y=-500
  for (y in c(-1800:0)){
    print(paste(rrrr, y))
    item<-sp_filter[year==y]
    
    delta_Species<-item[, .(south.america=sum(south.america),
                            north.america=sum(north.america),
                            year=y, rep=rrrr), 
                        by=list(NB, origin_continent)]
    
    type_N<-item[, .(south.america=sum(south.america),
                     north.america=sum(north.america),
                     year=y, rep=rrrr), 
                 by=list(NB, type, origin_continent)]
    delta_Species.origin[[length(delta_Species.origin)+1]]<-delta_Species
    type_N.origin[[length(type_N.origin)+1]]<-type_N
  }
}
delta_Species.origin.df<-rbindlist(delta_Species.origin)
type_N.origin.df<-rbindlist(type_N.origin)
saveRDS(delta_Species.origin.df, "../Data/Tables/delta_Species.origin.rda")
saveRDS(type_N.origin.df, "../Data/Tables/type_N.origin.rda")
