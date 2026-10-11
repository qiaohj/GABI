library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration_continent.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmean<-data.table(dbReadTable(conn, "tasmean"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
dbDisconnect(conn)
pr$var<-"pr"
tasmean$var<-"tasmean"
all_v<-rbindlist(list(pr, tasmean))
all_v_first<-all_v[year==3600]
if (F) {
  nb<-readRDS("../Data/Tables/nb_range_mammals_iucn.rda")
  pr_percentile<-quantile(nb[range>0 & var=="pr"]$range, 
                          c(0.2, 0.4, 0.6, 0.8, 0.9))
  tasmean_percentile<-quantile(nb[range>0 & var=="tas"]$range, 
                               c(0.2, 0.4, 0.6, 0.8, 0.9))
  nb<-list(pr=ceiling(pr_percentile),
           t=ceiling(tasmean_percentile))
  
  saveRDS(nb, "../Data/Tables/nb.rda")
}

#-12.44,-2.44|119.47,768.47

nb<-readRDS("../Data/Tables/nb.rda")

nb_pr<-nb$pr
nb_tm<-nb$t

shpfname = "../Shape/isea3h8/N_S_America.shp"
hexagon<-read_sf(shpfname)
table(hexagon$continent)
ll<-data.table(global_id=hexagon$seqnum, lon=hexagon$lon, lat=hexagon$lat)

good_da<-"0.376596738,0.724239343,0.976679685,0.9995805560000001,1.0"
poor_da<-"0.710669364,0.999605965,0.9999990249999999,0.999999999299,1.0"

seeds<-readRDS("../Data/20261002/Tables/Seed.Pool.rda")
seeds<-unique(seeds[, .(global_id, continent)])
seeds$continent_id<-c(c(1:nrow(seeds[continent=="North America"])),
                      c(1:nrow(seeds[continent=="South America"])))
seeds<-merge(seeds, ll, by="global_id")
ggplot(seeds)+geom_point(aes(x=lon, y=lat, color=continent))
saveRDS(seeds, "../Data/Tables/Seed.Pool.rda")


pr<-all_v_first[which(var=="pr")]
colnames(pr)<-c("global_id", "pr", "year.pr", "var.pr")
tasmean<-all_v_first[which(var=="tasmean")]
colnames(tasmean)<-c("global_id", "tasmean", "year.tasmean", "var.tasmean")

seeds_v<-merge(seeds, pr, by=c("global_id"))
seeds_v<-merge(seeds_v, tasmean, by=c("global_id"))




i=1
nb_labels<-c("TINY", "NARROW", "MODERATE", "BROAD", "HUGE")

nb_list<-data.table(x=c(1:5), y=c(1:5))

simulations<-list()
for (i in c(1:nrow(seeds_v))){
  print(paste(i, nrow(seeds_v)))
  seed<-seeds_v[i]
  for (j in c(1:nrow(nb_list))){
    sim_item<-data.table(global_id=seed$global_id,
                         continent=seed$continent,
                         continent_id=seed$continent_id,
                         pr=seed$pr,
                         tasmean=seed$tasmean,
                         species_id=seed$global_id,
                         nb_range_pr=nb_pr[nb_list[j]$x],
                         nb_range_tm=nb_tm[nb_list[j]$y],
                         nb=sprintf("%s", nb_labels[nb_list[j]$x])
    )
    
    sim_item_good<-sim_item
    sim_item_good$da<-"GOOD"
    sim_item_good$dispersal_ability<-good_da
    
    sim_item_poor<-sim_item
    sim_item_poor$da<-"POOR"
    sim_item_poor$dispersal_ability<-poor_da
    
    sim_item_all<-rbindlist(list(sim_item_poor, sim_item_good))
    sim_item_all$label<-sprintf("%d.%s.%s", 
                                sim_item_all$global_id, sim_item_all$nb, sim_item_all$da)
    #if ((sim_item_all[1]$pr - sim_item_all[1]$nb_range_pr/2)<0){
    #  sim_item_all$nb_v<-sprintf("%.2f,%.2f|%.2f,%.2f",
    #                             sim_item_all$tasmean - sim_item_all$nb_range_tm/2,
    #                             sim_item_all$tasmean + sim_item_all$nb_range_tm/2,
    #                             0,
    #                             sim_item_all$nb_range_pr)
    #}else{
    sim_item_all$nb_v<-sprintf("%.2f,%.2f|%.2f,%.2f|%.2f,%.2f",
                               sim_item_all$tasmean - sim_item_all$nb_range_tm/2,
                               sim_item_all$tasmean + sim_item_all$nb_range_tm/2,
                               sim_item_all$tasmean - sim_item_all$nb_range_tm/2,
                               sim_item_all$tasmean + sim_item_all$nb_range_tm/2,
                               sim_item_all$pr - sim_item_all$nb_range_pr/2,
                               sim_item_all$pr + sim_item_all$nb_range_pr/2)
    #}
    sim_item_all$dispersal_speed<-1
    sim_item_all$dispersal_method<-2
    sim_item_all$number_of_path<--1
    sim_item_all$speciation_years<-50
    sim_item_all$species_extinction_threshold<-0
    sim_item_all$species_extinction_time_steps<-1
    sim_item_all$species_extinction_threahold_percentage<-1
    sim_item_all$group_extinction_threshold<-0
    sim_item_all$initial_seeds<-sim_item_all$global_id
    sim_item_all$environments<-"tasmin,tasmax,pr"
    sim_item_all$from<-1900
    sim_item_all$to<-0
    sim_item_all$step<- -1
    sim_item_all$mask<-"mask"
    sim_item_all$burn_in_year<-100
    sim_item_all$niche_breadth_evolution_ratio<-"1,1,1,1"
    sim_item_all$niche_breadth_evolution_random_range<-"0.01"
    sim_item_all$niche_breadth_evolution_parent_level<-5
    sim_item_all$niche_envolution_individual_ratio<-1
    sim_item_all$is_run<-1
    sim_item_all$evo_type<-1
    sim_item_all$species_evo_type<-1
    sim_item_all$directional_speed<-0
    sim_item_all$species_evo_level<-0
    simulations[[length(simulations)+1]]<-sim_item_all
  }
}
simulations<-rbindlist(simulations)
simulations$id<-c(1:nrow(simulations))
#
global_ids<-data.table(global_id=unique(simulations$global_id))
global_ids<-global_ids[sample(nrow(global_ids), nrow(global_ids))]
global_ids$random_index<-c(1:nrow(global_ids))
simulations<-merge(simulations, global_ids, by=c("global_id"))
timeline<-data.table(from=1900, to=0, step=-1)
dim(simulations)

for (i in c(1:ncol(simulations))){
  class_col<-class(simulations[[i]])
  print(class_col)
  if (class_col=="numeric"){
    simulations[[i]]<-as.integer(simulations[[i]])
  }
}
timeline$from<-as.integer(timeline$from)
timeline$to<-as.integer(timeline$to)
timeline$step<-as.integer(timeline$step)
simulations$niche_breadth_evolution_random_range<-
  as.numeric(simulations$niche_breadth_evolution_random_range)
simulations$from<-1900
simulations$to<-0
simulations$step<- -1

simulations$id<-c(1:nrow(simulations))

cols<-readRDS("../Configuration/conf.colnames.rda")

simulations<-simulations[, ..cols]
speciation_years<-50

for (speciation_rate in c(0.2, 0.5, 1, 2)){
  speciation_years<-50*speciation_rate
  
  simulations$speciation_years<-speciation_years
  simulations$label<-sprintf("%d.%s.%s.SY_%d", simulations$global_id, simulations$nb, simulations$da, simulations$speciation_years)
  base_db<-sprintf("../Configuration/conf.speciation_years_%d.sqlite", speciation_years)
  mydb <- dbConnect(RSQLite::SQLite(), base_db)
  dbWriteTable(mydb, "simulations", simulations, overwrite=T)
  dbWriteTable(mydb, "timeline", timeline, overwrite=T)
  dbDisconnect(mydb)
}

simulations[global_id==2623]

./ees_3d /media/huijieqiao/Butterfly/GABI/Configuration/configuration.sqlite /media/huijieqiao/Butterfly/GABI/Configuration/conf.sqlite /media/huijieqiao/Butterfly/GABI/Results -1 64 0 0 0
./ees_3d /media/huijieqiao/Butterfly/GABI/Configuration/null.sqlite /media/huijieqiao/Butterfly/GABI/Configuration/conf.sqlite /media/huijieqiao/Butterfly/GABI/Results.NULL -1 64 0 0 0

9984596
