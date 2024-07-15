library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")

if (F){
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
  simulations<-data.table(dbReadTable(conn, "simulations"))
  dbDisconnect(conn)
  simulations_sub<-simulations[continent_id<=100]
  
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
  pr<-data.table(dbReadTable(conn, "pr"))
  tasmax<-data.table(dbReadTable(conn, "tasmax"))
  tasmin<-data.table(dbReadTable(conn, "tasmin"))
  dist<-data.table(dbReadTable(conn, "distances"))
  dbDisconnect(conn)
  
  env_full<-merge(pr, tasmax, by=c("global_id", "year"))
  colnames(env_full)<-c("global_id", "year", "pr", "tasmax")
  env_full<-merge(env_full, tasmin, by=c("global_id", "year"))
  colnames(env_full)<-c("global_id", "year", "pr", "tasmax", "tasmin")
  saveRDS(env_full, "../Data/Tables/env_full.rda")
  env_full_sample<-env_full[sample(nrow(env_full), 1e4),]
  env_full_sample$seed<-"F"
  seeds<-env_full[year==1800 & global_id %in% sub_dispersal_result$seed_id]
  seeds$seed<-"T"
  seeds$tasmax<-(seeds$tasmax+seeds$tasmin)/2
  env_full_sample<-rbindlist(list(env_full_sample, seeds))
  saveRDS(env_full_sample, "../Data/Tables/env_full_sample.rda")
}



dispersal_result<-readRDS(sprintf("../Data/Tables/%s/dispersal_result.rda", t_folder))

dispersal_result$Label<-sprintf("%s.%d.%d.%d.%d", dispersal_result$status,
                                dispersal_result$init_n, dispersal_result$init_s,
                                dispersal_result$final_n, dispersal_result$final_s)
table(dispersal_result$Label)

xx<-dispersal_result[, .(N=.N), by=list(Label, status, nb)]
xx[nb=="BROAD-BROAD"]

sub_dispersal_result<-dispersal_result[nb=="BROAD-BROAD" & da=="GOOD"]

simulations_sub<-simulations_sub[nb=="BROAD-BROAD" & da=="GOOD"]
sub_dispersal_result$seed_id<-as.numeric(sub_dispersal_result$seed_id)
seed_status<-merge(sub_dispersal_result, simulations_sub,
                   by.x=c("seed_id", "nb", "da", "continent"),
                   by.y = c("global_id", "nb", "da", "continent"))

seed_status[, c("tas_min","tas_max", "pr") := data.table(str_split_fixed(nb_v,"\\|",3))] 

seed_status[, c("tas_low","tas_high") := data.table(str_split_fixed(tas_min,",",2))]
seed_status[, c("pr_low","pr_high") := data.table(str_split_fixed(pr,",",2))] 

seed_status$tas_low<-as.numeric(seed_status$tas_low)
seed_status$tas_high<-as.numeric(seed_status$tas_high)
seed_status$pr_low<-as.numeric(seed_status$pr_low)
seed_status$pr_high<-as.numeric(seed_status$pr_high)

ggplot(env_full_sample)+
  geom_point(aes(y=pr, x=tasmax), size=0.1, alpha=0.1)+
  geom_point(aes(y=pr, x=tasmin), size=0.1, alpha=0.1)+
  geom_rect(data =seed_status,
            aes(xmin = tas_low, xmax = tas_high, 
                ymin = pr_low, ymax = pr_high, 
                color = continent), fill=NA, alpha = 0.1)+
  labs(x="Temperature", y="Precipitation")+
  facet_wrap(~status)

ggplot(env_full_sample)+
  geom_point(aes(y=pr, x=tasmax), size=0.1, alpha=0.1)+
  geom_point(aes(y=pr, x=tasmin), size=0.1, alpha=0.1)+
  geom_rect(data =seed_status,
            aes(xmin = tas_low, xmax = tas_high, 
                ymin = pr_low, ymax = pr_high, 
                color = status), fill=NA, alpha = 0.1)+
  labs(x="Temperature", y="Precipitation")+
  facet_wrap(~continent)

ns<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
ns<-data.table(global_id=ns$seqnum, continent=ns$continent)
env_full_sample_c<-merge(env_full_sample, ns, by="global_id")

env_full_sample_c<-rbindlist(list(data.table(pr=env_full_sample_c$pr, 
                                             tas=env_full_sample_c$tasmax, 
                                             continent=env_full_sample_c$continent,
                                             seed=env_full_sample_c$seed),
                                  data.table(pr=env_full_sample_c$pr, 
                                             tas=env_full_sample_c$tasmin,
                                             continent=env_full_sample_c$continent,
                                             seed=env_full_sample_c$seed)))

seeds_c<-merge(seeds, ns, by="global_id")
ggplot(env_full_sample_c)+
  geom_bin2d(aes(y=pr, x=tas), bins=50)+
  geom_point(data=seeds_c,
             aes(y=pr, x=tasmax), color="red", size=0.5)+
  facet_wrap(~continent, nrow=2, ncol=2)
  
geom_point(aes(y=pr, x=tasmax, color=continent), size=0.5)+
  geom_point(aes(y=pr, x=tasmin, color=continent), size=0.5)
