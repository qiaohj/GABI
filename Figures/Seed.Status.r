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
no.boot.seeds.id<-data.table(expand.grid(seed_id=no.boot.seeds.id, nb=unique(df.detail$nb),
                                         da=unique(df.detail$da),
                                         continent="North America"))
boot<-unique(all_ramdom_seeds_df[, c("seed_id", "nb", "da", "continent")])

all.seeds<-rbindlist(list(boot, outliers, no.boot.seeds.id))
all.seeds[,.(N=length(unique(seed_id))), by=list(nb, da, continent)]
