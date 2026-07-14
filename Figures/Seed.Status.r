library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
seeds<-readRDS("../Data/Tables/seeds.rda")
seeds[global_id==9745]
#conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
#simulations<-data.table(dbReadTable(conn, "simulations"))
#dbDisconnect(conn)
#target.seeds<-simulations[continent_id<=100]$global_id
#seeds<-seeds[global_id %in% target.seeds]
#ggplot(seeds)+geom_sf(data=seed.dist)+geom_point(aes(x=lon, y=lat, color=continent))

seed.dist<-readRDS("../Data/Tables/cells.with.dist.rda")
seed.dist[which(seed.dist$seqnum==9745),]
df<-readRDS("../Data/Tables/N.Speciation.Extinction.All.NB.rda")
df[seed_id==9745]

table(df$nb)
df[,(N=length(unique(seed_id))), by="continent"]
nrow(df)
df$continent<-NULL
df.detail<-merge(df, seeds, by.x="seed_id", by.y="global_id")
df.detail[,.(N=length(unique(seed_id))), by=list(continent)]
df[!seed_id %in% df.detail$seed_id]

#df<-df[between(lat, -35, 45)]
range(df.detail$lat)

burn_in<-3200/2
unique(df.detail$nb)

df_N_checked<-df.detail[year==burn_in & N_SPECIES>0, 
                        .(N=.N), by=list(seed_id, nb)]

df_N_checked[N==1]

df.detail[seed_id==506 & nb=="BIG" & year>=burn_in]

df_N_checked$label<-sprintf("%d.%s", df_N_checked$seed_id, df_N_checked$nb)
df.detail$label<-sprintf("%d.%s", df.detail$seed_id, df.detail$nb)
df_filtered_seeds<-df.detail[label %in% df_N_checked[N==2]$label]
table(df_N_checked$N)

df.detail[,.(N=length(unique(seed_id))), by=list(nb)]

df.detail$label2<-sprintf("%d.%s.%s", df.detail$seed_id, df.detail$nb, df.detail$da)


all_ramdom_seeds_df<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")

no.boot.seeds.id<-df.detail[!seed_id %in% df_N_checked[N==2]$seed_id]
no.boot.seeds.id[,.(N=length(unique(seed_id))), by=list(continent)]
no.boot.seeds.id<-unique(no.boot.seeds.id$seed_id)
no.boot.seeds.id<-no.boot.seeds.id[sample(length(no.boot.seeds.id), 37)]

df_filtered_seeds[,.(N=length(unique(seed_id))), by=list(continent)]



df_filtered_N<-df_filtered_seeds[, .(N_SPECIES=sum(N_SPECIES), 
                                     N_SPECIATION=sum(N_SPECIATION),
                                     N_EXTINCTION=sum(N_EXTINCTION),
                                     N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                                     N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                                     N_ALL_SPECIES=sum(N_ALL_SPECIES),
                                     N_SEED=length(unique(seed_id))),
                                 by=list(continent, year)]

df_filtered_N_detals<-df_filtered_seeds[, .(N_SPECIES=sum(N_SPECIES), 
                                            N_SPECIATION=sum(N_SPECIATION),
                                            N_EXTINCTION=sum(N_EXTINCTION),
                                            N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                                            N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                                            N_ALL_SPECIES=sum(N_ALL_SPECIES),
                                            N_SEED=length(unique(seed_id))),
                                        by=list(continent, year, nb, da)]
df_filtered_N_detals[year==burn_in+1, 
                     c("N_SEED", "continent", "nb", "da")]
df_filtered_N[year==burn_in]

table(df_N_checked$N)


#define outliers
N_species<-df_filtered_seeds[year==0 & N_SPECIES>0]
N_species.all<-df_filtered_seeds[year==0]
df_filtered_seeds[,.(N=length(unique(seed_id))), by="continent"]


length(unique(df.detail[! seed_id %in% df_filtered_seeds[continent=="South America"]$seed_id 
                        & continent=="South America"]$seed_id))

table(seeds$continent)

quantiles<-quantile(N_species$N_SPECIES, c(0, 1, 0.99, 0.98, 0.95, 0.90, 0.999))
names(quantiles)<-NULL

N_species$seed_label<-paste(N_species$seed_id, N_species$nb, N_species$da)
outliers<-unique(N_species[(N_SPECIES>quantiles[3])])
dt.outliers<-outliers[,.(N=.N), by=list(nb, da, continent)]
dt.outliers$nb<-factor(dt.outliers$nb, 
                       levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                       labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
setorderv(dt.outliers, c("nb", "da", "continent"))

dt.outliers<-outliers[,.(N=.N), by=list(nb, da)]
dt.outliers$nb<-factor(dt.outliers$nb, 
                       levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                       labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
setorderv(dt.outliers, c("nb", "da"))

dt.outliers


no.boot.seeds.id
outliers<-outliers[, c("seed_id", "nb", "da", "continent")]
outliers$type<-"outliers"
outliers$label<-sprintf("%d.%s.%s", outliers$seed_id, outliers$nb, outliers$da)
no.boot.seeds.id<-data.table(expand.grid(seed_id=no.boot.seeds.id, 
                                         nb=unique(df.detail$nb),
                                         da=unique(df.detail$da),
                                         continent="North America"))
no.boot.seeds.id$type<-"no.survive"
no.boot.seeds.id[,.(N=.N), by=list(nb, da, continent, type)]

boot<-unique(all_ramdom_seeds_df[, c("seed_id", "nb", "da", "continent")])
boot$type<-"bootstrapping"
boot$label<-sprintf("%d.%s.%s", boot$seed_id, boot$nb, boot$da)

boot.pool<-unique(df_filtered_seeds[, c("seed_id", "nb", "da", "continent")])
boot.pool[, .(N=length(unique(seed_id))), by=list(continent)]
boot.pool$type<-"seed.pool"

all.seeds<-rbindlist(list(boot.pool, no.boot.seeds.id))
all.seeds$label<-sprintf("%d.%s.%s", all.seeds$seed_id, all.seeds$nb, all.seeds$da)
all.seeds[label %in% outliers$label, type:="outlier"]
all.seeds[label %in% boot$label, type:="bootstrapping"]
N.burn.in.failed<-all.seeds[da=="GOOD",.(N=length(unique(seed_id))), by=list(continent, type, nb)]
N.burn.in.failed<-N.burn.in.failed[type %in% c("seed.pool", "no.survive")]
N.burn.in.failed<-N.burn.in.failed[,.(N=sum(N)), by=list(continent, nb)]
setorderv(N.burn.in.failed, c("nb", "continent"))
all.seeds[type=="bootstrapping", type:="seed.pool"]
all.seeds[,.(N=length(unique(seed_id))), by=list(continent, type, nb)]
all.seeds[type %in% c("seed.pool", "no.survive"),
          .(N=length(unique(seed_id))), by=list(type, nb)]


all.seeds.unique<-all.seeds
all.seeds.unique$nb<-NULL
all.seeds.unique$da<-NULL
all.seeds.unique$label<-NULL
all.seeds.unique<-unique(all.seeds.unique)
all.seeds.unique[,.(N=length(unique(seed_id))), by=list(continent, type)]

cells<-read_sf("../Shape/isea3h8/N_S_America.shp")

all.seeds.shp<-merge(cells, all.seeds, by.x="seqnum", by.y="seed_id", all=T)
all.seeds.shp[is.na(all.seeds.shp$type), "type"]<-"Background"

all.seeds.shp$type<-factor(all.seeds.shp$type, 
                           levels=c("no.survive", "seed.pool", "outlier", "Background"),
                           labels=c("No Survive", "Seed Pool", "Outlier", "Background"))
p<-ggplot()+
  geom_sf(data=cells, fill=NA, color="grey90")+
  geom_sf(data=all.seeds.shp[all.seeds.shp$type!="Background",], aes(fill=type), color=NA)+
  geom_sf(data=all.seeds.shp[all.seeds.shp$type=="Outlier",], aes(fill=type), color=NA)+
  scale_fill_manual(values=c("No Survive"=color_1,
                             "Seed Pool"=color_low,
                             "Outlier"=color_high,
                             "Background"=color_mid))+
  labs(fill="Seed Type")+
  theme_bw()+
  theme(legend.position = "bottom")
p

no_su<-data.table(global_id=all.seeds.shp$seqnum, continent=all.seeds.shp$continent.x,
                  NB=all.seeds.shp$nb, DA=all.seeds.shp$da,
                  type=all.seeds.shp$type)

no_su<-no_su[type=="No Survive"]
no_su_N<-no_su[,.(N=.N), by=list(continent, NB, DA)]

ggsave(p, filename="../Figures/Seeds/Seeds.pdf", width=5, height=5)
ggsave(p, filename="../Figures/Seeds/Seeds.png", width=5, height=5, bg="white")

saveRDS(all.seeds.shp, "../Figures/Seeds/Seeds.rda")

library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(ggh4x)
library(stringr)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
dist<-data.table(dbReadTable(conn, "distances"))
dbDisconnect(conn)
base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-data.table(dbReadTable(mydb, "simulations"))
dbDisconnect(mydb)
simulations[, c("tas_min","tas_max", "pr") := data.table(str_split_fixed(nb_v,"\\|",3))] 
simulations[, c("tas_low","tas_high") := data.table(str_split_fixed(tas_min,",",2))]
simulations[, c("pr_low","pr_high") := data.table(str_split_fixed(pr,",",2))] 

simulations$tas_low<-as.numeric(simulations$tas_low)
simulations$tas_high<-as.numeric(simulations$tas_high)
simulations$pr_low<-as.numeric(simulations$pr_low)
simulations$pr_high<-as.numeric(simulations$pr_high)

simulations<-simulations[, c("global_id", "nb", "da", "continent", "tas_low", "tas_high", "pr_low", "pr_high")]

simulations_pr<-merge(simulations, pr[year==1800], by="global_id")
colnames(simulations_pr)[9]<-"pr"
simulations_pr$year<-NULL
simulations_pr_tasmax<-merge(simulations_pr, tasmax[year==1800], by="global_id")
colnames(simulations_pr_tasmax)[10]<-"tasmax"
simulations_pr_tasmax$year<-NULL

simulations_pr_tasmax_tasmin<-merge(simulations_pr_tasmax, tasmin[year==1800], by="global_id")
colnames(simulations_pr_tasmax_tasmin)[11]<-"tasmin"
simulations_pr_tasmax_tasmin$year<-NULL

simulations_filter<-simulations_pr_tasmax_tasmin[global_id %in% all.seeds$seed_id]

simulations_filter$suitable<-F
simulations_filter[between(pr, pr_low, pr_high) &
                     between(tasmax, tas_low, tas_high) &
                   between(tasmin, tas_low, tas_high), suitable:=T]

NNNN<-simulations_filter[suitable==F, .(N=.N), by=list(nb, da, continent)]
NNNN$da<-NULL
NNNN<-unique(NNNN)
NNNN$nb<-factor(NNNN$nb, 
                          levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                          labels = c("BROAD", "MODERATE", "NARROW", "TINY"))

setorderv(NNNN, c("nb", "continent"))

saveRDS(simulations_filter, "../Data/Tables/Seed.Pool.rda")

simulations_filter<-readRDS("../Data/Tables/Seed.Pool.rda")
df_N_checked<-df.detail[year==burn_in & N_SPECIES>0, 
                        .(N=.N), by=list(seed_id, nb)]
simulations_filter$label<-sprintf("%d.%s", simulations_filter$global_id, simulations_filter$nb)
no_su<-simulations_filter[label %in% df_N_checked[N!=2]$label]

NNNN<-simulations_filter[suitable==F, .(N=.N), by=list(nb, da, continent)]
NNNN$da<-NULL
NNNN<-unique(NNNN)
NNNN$nb<-factor(NNNN$nb, 
                levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                labels = c("BROAD", "MODERATE", "NARROW", "TINY"))

setorderv(NNNN, c("nb", "continent"))
NNNN
sum(NNNN$N)
to.doc(NNNN, "No Survive", "../Figures/Seeds/no.surveve.docx", digits = 0)

all_ramdom_seeds_df<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")

N_seed_usage<-all_ramdom_seeds_df[, .(N=.N/100), by=c("continent", "nb", "da")]
seeed_rep<-unique(all_ramdom_seeds_df[,c("continent", "seed_id", "nb", "da")])

N_seed_rep<-seeed_rep[, .(N=.N), by=list(continent, nb, da)]
N_seed_rep$nb<-factor(N_seed_rep$nb, 
                levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
N_seed_rep$per<-round(N_seed_rep$N/500, 2)
setorderv(N_seed_rep, c("nb", "da", "continent"))
to.doc(N_seed_rep, "Bootstraping seeds", 
       "../Figures/Seeds/bootstraping.seeds.docx", digits = 2)
  