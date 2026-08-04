library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
library(RSQLite)
library(DBI)
library(stringr)
library(patchwork)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
#divided into 5,449 terrestrial grid cells (~87 km side length, ~99 km cell spacing and 7,774 km2 in area)
cells<-read_sf("../Shape/isea3h8/N_S_America.shp")
cells.ll<-data.table(seed_id=cells$seqnum, continent=cells$continent, lon=cells$lon, lat=cells$lat)
table(cells$continent)
nrow(cells)

#Of the initial 8,000 simulations, only 5,610 survived the burn-in period (Table SXX)
df<-readRDS("../Data/Tables/N.Speciation.Extinction.All.NB.rda")
df_last<-df[year==1600]
df_last$nb.label<-sprintf("%d.%s", df_last$seed_id, df_last$nb)
dim(df_last)

survived<-df_last[N_SPECIES>0]
survived_seed<-survived[, .(N=.N), by=.(nb.label)]
survived<-survived[nb.label %in% survived_seed[N==2]$nb.label]
#survived<-merge(survived, cells.ll, by="seed_id")
survived_N<-survived[,.(N=.N), by=.(nb, da, continent)]
survived_N$nb<-factor(survived_N$nb, 
                         levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                         labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
setorderv(survived_N, c("continent", "nb", "da"))
sum(survived_N$N)
to.doc(survived_N, "survived_N", "../Figures/Seeds/survived_N.docx", digits = 0)
#Do you have the proportion of seeds that made it to the end of the simulation 
#in each niche breadth category and dispersal ability?

df_end<-df[year==0]
df_end$nb.label<-sprintf("%d.%s", df_last$seed_id, df_last$nb)
dim(df_end)

survived_end<-df_end[N_SPECIES>0]
survived_seed_end<-survived_end[, .(N=.N), by=.(nb.label)]
survived_end<-survived_end[nb.label %in% survived_seed[N==2]$nb.label]
#survived<-merge(survived, cells.ll, by="seed_id")
survived_N_end<-survived_end[,.(N=.N), by=.(nb, da, continent)]
survived_N_end$nb<-factor(survived_N_end$nb, 
                      levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                      labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
setorderv(survived_N_end, c("continent", "nb", "da"))
sum(survived_N_end$N)
to.doc(survived_N_end, "survived_N_end", "../Figures/Seeds/N.Final.docx", digits = 0)


seeds<-readRDS("../Data/Tables/Seed.Pool.rda")
seeds[pr_low<0]
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
dist<-data.table(dbReadTable(conn, "distances"))
dbDisconnect(conn)

#
seeds.bt<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
seeds.bt$nb.label<-sprintf("%d.%s", seeds.bt$seed_id, seeds.bt$nb)
seeds.bt.N<-seeds.bt[, .(N.simulation=.N, N.unique.simulation=length(unique(nb.label))), by=.(rep, continent, nb)]
seeds.bt.N.se<-seeds.bt.N[, .(N.unique.simulation=mean(N.unique.simulation),
                             N.unique.simulation.sd=sd(N.unique.simulation),
                             N.simulation=mean(N.simulation),
                             N.simulation.sd=sd(N.simulation)),
                          by=.(continent, nb)]



