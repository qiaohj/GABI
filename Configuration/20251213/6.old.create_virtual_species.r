library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
dbDisconnect(conn)
pr$var<-"pr"
tasmax$var<-"tasmax"
tasmin$var<-"tasmin"
all_v<-rbindlist(list(pr, tasmax, tasmin))
all_v_first<-all_v[year==1800]
#nb_df<-readRDS(sprintf("../Data/IUCN_NB/%s/Mammals.rda", "N_S_America"))
#length(unique(nb_df$species))
nb<-readRDS("../Data/nb.rda")

nb

shpfname = "../Data/Shape/isea3h8/N_S_America.shp"
hexagon<-read_sf(shpfname)
table(hexagon$continent)
ggplot(hexagon)+geom_sf(aes(fill=continent))



good_da<-"0.376596738,0.724239343,0.976679685,0.9995805560000001,1.0"
poor_da<-"0.710669364,0.999605965,0.9999990249999999,0.999999999299,1.0"

mask_seeds<-hexagon[which(hexagon$continent %in% c("North America", "South America")),]
seeds<-data.table(global_id=mask_seeds$seqnum,
                  lon=mask_seeds$lon,
                  lat=mask_seeds$lat,
                  continent=mask_seeds$continent)
seeds<-seeds[between(lat, -45, 45)]
seeds<-seeds[,.SD[sample(.N, .N)],by = continent]


seeds$continent_id<-c(c(1:nrow(seeds[continent=="North America"])),
                      c(1:nrow(seeds[continent=="South America"])))

if (F){
  ggplot(seeds)+geom_point(aes(x=lon, y=lat, color=continent))
  ggplot(seeds[continent_id<=100])+geom_point(aes(x=lon, y=lat, color=continent))
}
pr<-all_v_first[which(var=="pr")]
colnames(pr)<-c("global_id", "pr", "year.pr", "var.pr")
tasmin<-all_v_first[which(var=="tasmin")]
colnames(tasmin)<-c("global_id", "tasmin", "year.tasmin", "var.tasmin")
tasmax<-all_v_first[which(var=="tasmax")]
colnames(tasmax)<-c("global_id", "tasmax", "year.tasmax", "var.tasmax")
seeds_v<-merge(seeds, pr, by=c("global_id"))
seeds_v<-merge(seeds_v, tasmin, by=c("global_id"))
seeds_v<-merge(seeds_v, tasmax, by=c("global_id"))

nb_pr<-nb$pr[c(5, 6, 8)]
nb_tm<-nb$t[c(5, 6, 8)]


i=1
nb_labels<-c("NARROW", "MODERATE", "BROAD")

nb_list<-data.table(expand.grid(x=c(1:3), y=c(1:3)))

simulations<-list()
for (i in c(1:nrow(seeds_v))){
  print(paste(i, nrow(seeds_v)))
  seed<-seeds_v[i]
  for (j in c(1:nrow(nb_list))){
    sim_item<-data.table(global_id=seed$global_id,
                         continent=seed$continent,
                         continent_id=seed$continent_id,
                         pr=seed$pr,
                         tasmin=seed$tasmin,
                         tasmax=seed$tasmax,
                         species_id=seed$global_id,
                         nb_range_pr=nb_pr[nb_list[j]$x],
                         nb_range_tm=nb_tm[nb_list[j]$y],
                         nb=sprintf("%s-%s", nb_labels[nb_list[j]$x],
                                    nb_labels[nb_list[j]$y])
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
    sim_item_all$tasmean<-(sim_item_all$tasmax+sim_item_all$tasmin)/2
    sim_item_all$nb_v<-sprintf("%.2f,%.2f|%.2f,%.2f|%.2f,%.2f",
                               sim_item_all$tasmean - sim_item_all$nb_range_tm/2,
                               sim_item_all$tasmean + sim_item_all$nb_range_tm/2,
                               sim_item_all$tasmean - sim_item_all$nb_range_tm/2,
                               sim_item_all$tasmean + sim_item_all$nb_range_tm/2,
                               sim_item_all$pr - sim_item_all$nb_range_pr/2,
                               sim_item_all$pr + sim_item_all$nb_range_pr/2)
    sim_item_all$dispersal_speed<-1
    sim_item_all$dispersal_method<-2
    sim_item_all$number_of_path<--1
    sim_item_all$speciation_years<-250
    sim_item_all$species_extinction_threshold<-0
    sim_item_all$species_extinction_time_steps<-1
    sim_item_all$species_extinction_threahold_percentage<-1
    sim_item_all$group_extinction_threshold<-0
    sim_item_all$initial_seeds<-sim_item_all$global_id
    sim_item_all$environments<-"tasmin,tasmax,pr"
    sim_item_all$from<-1800
    sim_item_all$to<-0
    sim_item_all$step<- -1
    sim_item_all$mask<-"mask"
    sim_item_all$burn_in_year<-0
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
simulations$random_index<-simulations[sample(nrow(simulations), nrow(simulations))]$id
timeline<-data.table(from=1800, to=0, step=-1)

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
simulations$from<-1800
simulations$to<-0
simulations$step<- -1

simulations$id<-c(1:nrow(simulations))
simulations$random_index<-simulations[sample(nrow(simulations), nrow(simulations))]$id

cols<-readRDS("../Configuration/conf.colnames.rda")

simulations<-simulations[, ..cols]


base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
dbWriteTable(mydb, "simulations", simulations, overwrite=T)
dbWriteTable(mydb, "timeline", timeline, overwrite=T)
dbDisconnect(mydb)


if (F){
  shpfname = "../Data/Shape/isea3h8/N_S_America.shp"
  hexagon<-read_sf(shpfname)
  simulations<-data.table(simulations)
  seeds<-simulations[continent_id<=100]
  seeds<-unique(seeds$global_id)
  ggplot(hexagon)+geom_sf()+ geom_sf(data=hexagon[which(hexagon$seqnum %in% seeds),], aes(fill=continent))
}

if (F){
  environments<-data.table(names=c("tasmin", "tasmax", "pr"), begin_year=1800, end_year=0, step=-1)
  for (i in c(1:ncol(environments))){
    class_col<-class(environments[[i]])
    print(class_col)
    if (class_col=="numeric"){
      environments[[i]]<-as.integer(environments[[i]])
    }
  }
  
  mask<-data.table(global_id=unique(all_v$global_id), v=1)
  for (i in c(1:ncol(mask))){
    class_col<-class(mask[[i]])
    print(class_col)
    if (class_col=="numeric"){
      mask[[i]]<-as.integer(mask[[i]])
    }
  }
  base_db<-"../Configuration/configuration.sqlite"
  mydb <- dbConnect(RSQLite::SQLite(), base_db)
  dbWriteTable(mydb, "mask", mask, overwrite=T)
  dbWriteTable(mydb, "environments", environments, overwrite=T)
  dbDisconnect(mydb)
  
  base_db<-"../Configuration/configuration.sqlite"
  mydb <- dbConnect(RSQLite::SQLite(), base_db)
  pr<-dbReadTable(mydb, "pr")
  pr$year<-as.integer(pr$year/2)
  dbWriteTable(mydb, "pr", pr, overwrite=T)
  
  tasmin<-dbReadTable(mydb, "tasmin")
  tasmin$year<-as.integer(tasmin$year/2)
  dbWriteTable(mydb, "tasmin", tasmin, overwrite=T)
  
  tasmax<-dbReadTable(mydb, "tasmax")
  tasmax$year<-as.integer(tasmax$year/2)
  dbWriteTable(mydb, "tasmax", tasmax, overwrite=T)
  
  dbDisconnect(mydb)
}
./ees_3d /media/huijieqiao/WD22T_11/GABI/Configuration/configuration.sqlite /media/huijieqiao/WD22T_11/GABI/Configuration/conf.sqlite /media/huijieqiao/WD22T_11/GABI/Results -1 64 0 0 0

9984596
