library(data.table)
library(ggplot2)
library(RSQLite)
library(DBI)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)
simulations<-data.table(simulations)
simulations<-simulations[continent_id<=100]

env<-readRDS("../Data/Tables/env_full.rda")

simulations[, c("tas_min","tas_max", "pr") := data.table(str_split_fixed(nb_v,"\\|",3))] 

simulations[, c("tas_low","tas_high") := data.table(str_split_fixed(tas_min,",",2))]
simulations[, c("pr_low","pr_high") := data.table(str_split_fixed(pr,",",2))] 

simulations$tas_low<-as.numeric(simulations$tas_low)
simulations$tas_high<-as.numeric(simulations$tas_high)
simulations$pr_low<-as.numeric(simulations$pr_low)
simulations$pr_high<-as.numeric(simulations$pr_high)

nb_labels<-c("NARROW", "MODERATE", "BROAD")
da_labels<-c("GOOD", "POOR")
nb_list<-data.table(expand.grid(x=nb_labels, y=nb_labels, z=da_labels))
all<-list()

for (j in c(1:nrow(nb_list))){
  lab<-sprintf("%s-%s-%s", nb_list[j]$x, nb_list[j]$y, nb_list[j]$z)
  all[[lab]]<-list()
}
allbak<-all

all<-allbak


for (i in c(1:nrow(simulations))){
  print(paste(i, nrow(simulations)))
  item<-simulations[i]
  lab<-sprintf("%s-%s", item$nb, item$da)
  
  suitable<-env[between(pr, item$pr_low, item$pr_high) &
                  between(tasmax, item$tas_low, item$tas_high) &
                  between(tasmin, item$tas_low, item$tas_high)]
  suitable$nb<-item$nb
  suitable$da<-item$da
  suitable$seed_id<-item$global_id
  suitable$continent<-item$continent
  all[[lab]][[length(all[[lab]])+1]]<-suitable
}

j=1
for (j in c(2:nrow(nb_list))){
  lab<-sprintf("%s-%s-%s", nb_list[j]$x, nb_list[j]$y, nb_list[j]$z)
  print(paste(j, lab))
  df<-rbindlist(all[[lab]])
  saveRDS(df, sprintf("../Data/Tables/500k.NB_BROAD.potential_distribution/%s.rda", lab))
}
