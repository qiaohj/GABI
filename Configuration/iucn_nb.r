library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
label<-"World"
nb_full_df<-readRDS(sprintf("../Data/IUCN_NB/Mammals.%s.rda", label))

max.temp<-nb_full_df[var=="tasmax"]
min.temp<-nb_full_df[var=="tasmin"]
min.temp<-unique(min.temp[, c("species", "min")])
max.temp<-unique(max.temp[, c("species", "max")])
temp<-merge(max.temp, min.temp, by=c("species"))
temp$var<-"tas"
temp$range<-temp$max-temp$min

hist(temp$range)
pr<-nb_full_df[var=="pr"]
pr<-unique(pr[, c("species", "range", "min", "max", "var")])

nb<-rbindlist(list(temp, pr), use.names = T)
saveRDS(nb, "../Data/Tables/nb_range_mammals_iucn.rda")

quantile(round(nb[range>0 & var=="pr"]$range, 2), 
         c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.99, 1))
quantile(round(nb[range>0 & var=="tas"]$range, 2), 
         c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.99, 1))
