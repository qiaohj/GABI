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
all_v_first<-all_v[year==3600]
dfs<-readRDS(sprintf("../Data/IUCN_NB/%s/%s.rda","N_S_America",  "Mammals"))
dfs<-dfs[N_CELLS>1]
dfs$range_min_max<-dfs$max - dfs$min
dfs$range_q01_q99<-dfs$q99 - dfs$q01
pr_item<-dfs[var=="pr"]
tasmax_item<-dfs[var=="tasmax"]
tasmin_item<-dfs[var=="tasmin"]

pr_nb<-pr_item[, c("species", "N_CELLS", "range_min_max", "range_q01_q99")]
pr_nb$type<-"pr"
tas<-merge(tasmax_item, tasmin_item, by=c("species", "N_CELLS"))
tas$range_min_max<-tas$max.x - tas$min.y
tas$range_q01_q99<-tas$q99.x - tas$q01.y
tas_nb<-tas[, c("species", "N_CELLS", "range_min_max", "range_q01_q99")]
tas_nb$type<-"tm"
nb<-rbindlist(list(pr_nb, tas_nb))

nb
if (F){
  ggplot(nb)+geom_point(aes(x=range_min_max, y=range_q01_q99))+
    geom_abline()
}

species<-data.table(species=unique(nb$species), id=c(1:length(unique(nb$species))))
nb<-merge(nb, species, by=c("species"))
setorderv(nb, "id")
ggplot(nb)+
  geom_density(aes(x=range_q01_q99))+
  geom_density(data=nb[id<=100], aes(x=range_q01_q99), color="red")+
  geom_density(data=nb[id<=1000], aes(x=range_q01_q99), color="blue")+
  facet_wrap(~type, nrow=2, scale="free")

if (F){
  base_db<-"../Data/Temp/conf.sqlite"
  mydb <- dbConnect(RSQLite::SQLite(), base_db)
  temp<-dbReadTable(mydb, "simulations")
  dbDisconnect(mydb)
}
shpfname = "../Data/Shape/isea3h8/N_S_America.shp"
hexagon<-read_sf(shpfname)
table(hexagon$continent)
ggplot(hexagon)+geom_sf(aes(fill=continent))
head(temp, 1)

good_da<-"0.376596738,0.724239343,0.976679685,0.9995805560000001,1.0"
poor_da<-"0.710669364,0.999605965,0.9999990249999999,0.999999999299,1.0"
seeds<-data.table(global_id=hexagon[which(hexagon$continent %in% c("North America", "South America")), ]$seqnum,
                  continent=hexagon[which(hexagon$continent %in% c("North America", "South America")), ]$continent)
seeds<-seeds[,.SD[sample(.N, .N)],by = continent]


seeds$continent_id<-c(c(1:nrow(seeds[continent=="North America"])),
                      c(1:nrow(seeds[continent=="South America"])))

pr<-all_v_first[which(var=="pr")]
colnames(pr)<-c("global_id", "pr", "year.pr", "var.pr")
tasmin<-all_v_first[which(var=="tasmin")]
colnames(tasmin)<-c("global_id", "tasmin", "year.tasmin", "var.tasmin")
tasmax<-all_v_first[which(var=="tasmax")]
colnames(tasmax)<-c("global_id", "tasmax", "year.tasmax", "var.tasmax")
seeds_v<-merge(seeds, pr, by=c("global_id"))
seeds_v<-merge(seeds_v, tasmin, by=c("global_id"))
seeds_v<-merge(seeds_v, tasmax, by=c("global_id"))

nb_pr<-nb[type=="pr"]
colnames(nb_pr)<-c("species", "N_CELLS", "range_min_max.pr", "range_q01_q99.pr", "type.pr", "id")
nb_tm<-nb[type=="tm"]
colnames(nb_tm)<-c("species", "N_CELLS", "range_min_max.tm", "range_q01_q99.tm", "type.tm", "id")
nb_df<-merge(nb_tm, nb_pr, by=c("species", "N_CELLS", "id"))

simulations<-list()
for (i in c(1:nrow(seeds_v))){
  print(paste(i, nrow(seeds_v)))
  seed<-seeds_v[i]
  sim_item<-data.table(global_id=seed$global_id,
                       continent=seed$continent,
                       continent_id=seed$continent_id,
                       pr=seed$pr,
                       tasmin=seed$tasmin,
                       tasmax=seed$tasmax,
                       species_id=nb_df$id,
                       nb_range_pr=nb_df$range_q01_q99.pr,
                       nb_range_tm=nb_df$range_q01_q99.tm,
                       nb=nb_df$species
                       )
  sim_item_good<-sim_item
  sim_item_good$da<-"GOOD"
  sim_item_good$dispersal_ability<-good_da
  
  sim_item_poor<-sim_item
  sim_item_poor$da<-"POOR"
  sim_item_poor$dispersal_ability<-poor_da
  
  sim_item_all<-rbindlist(list(sim_item_poor, sim_item_good))
  sim_item_all$label<-sprintf("%d_%d_%s", 
                              sim_item_all$global_id, sim_item_all$species_id, sim_item_all$da)
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
  sim_item_all$speciation_years<-99999
  sim_item_all$species_extinction_threshold<-0
  sim_item_all$species_extinction_time_steps<-1
  sim_item_all$species_extinction_threahold_percentage<-1
  sim_item_all$group_extinction_threshold<-0
  sim_item_all$initial_seeds<-sim_item_all$global_id
  sim_item_all$environments<-"tasmin,tasmax,pr"
  sim_item_all$from<-3600
  sim_item_all$to<-0
  sim_item_all$step<- -2
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
  simulations[[i]]<-sim_item_all
}
simulations<-rbindlist(simulations)
head(temp, 1)
colnames(simulations)
colnames(temp)
simulations$id<-c(1:nrow(simulations))
simulations$random_index<-simulations[sample(nrow(simulations), nrow(simulations))]$id
cols<-c(colnames(temp), "continent_id", "species_id", "continent")
simulations_sort<-simulations[, ..cols]
timeline<-data.table(from=1800, to=0, step=-1)

for (i in c(1:ncol(simulations_sort))){
  class_col<-class(simulations_sort[[i]])
  print(class_col)
  if (class_col=="numeric"){
    simulations_sort[[i]]<-as.integer(simulations_sort[[i]])
  }
}
timeline$from<-as.integer(timeline$from)
timeline$to<-as.integer(timeline$to)
timeline$step<-as.integer(timeline$step)
simulations_sort$niche_breadth_evolution_random_range<-
  as.numeric(simulations_sort$niche_breadth_evolution_random_range)
simulations_sort$from<-1800
simulations_sort$to<-0
simulations_sort$step<- -1
base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
dbWriteTable(mydb, "simulations", simulations_sort, overwrite=T)
dbWriteTable(mydb, "timeline", timeline, overwrite=T)
dbDisconnect(mydb)

base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations_sort<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)

i=1
global_ids<-unique(simulations_sort$global_id)
for (i in c(1:length(global_ids))){
  print(paste(i, length(global_ids)))
  item<-global_ids[i]
  dir.create(sprintf("../Results/%d", item))
}
if (F){
  shpfname = "../Data/Shape/isea3h8/N_S_America.shp"
  hexagon<-read_sf(shpfname)
  simulations_sort<-data.table(simulations_sort)
  seeds<-simulations_sort[continent_id<=1000]
  seeds<-unique(seeds$global_id)
  ggplot(hexagon)+geom_sf()+ geom_sf(data=hexagon[which(hexagon$seqnum %in% seeds),], aes(fill=continent))
}
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

./ees_3d /media/huijieqiao/WD22T_11/GABI/Configuration/configuration.sqlite /media/huijieqiao/WD22T_11/GABI/Configuration/conf.sqlite /media/huijieqiao/WD22T_11/GABI/Results 1 64 1 1 1


