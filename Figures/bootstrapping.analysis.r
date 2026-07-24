library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
library(dplyr)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")

seed.95<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.95.rda")
seed.99<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.99.rda")
seed.995<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.995.rda")

N95<-seed.95[,.(N.Usaged.95=.N), by=list(continent, seed_id, nb, da, seed_label, N_SPECIES, min.dist)]
N99<-seed.99[,.(N.Usaged.99=.N), by=list(continent, seed_id, nb, da, seed_label, N_SPECIES, min.dist)]
N995<-seed.995[,.(N.Usaged.995=.N), by=list(continent, seed_id, nb, da, seed_label, N_SPECIES, min.dist)]

N.ALL<-merge(N95, 
             merge(N99, N995, by=c("continent", "seed_id", "nb", "da", "seed_label", "N_SPECIES", "min.dist"), all=T),
             by=c("continent", "seed_id", "nb", "da", "seed_label", "N_SPECIES", "min.dist"), all=T)


N.ALL[is.na(N.Usaged.95), N.Usaged.95:=0]
N.ALL[is.na(N.Usaged.99), N.Usaged.99:=0]
N.ALL[is.na(N.Usaged.995), N.Usaged.995:=0]
N.ALL[N.Usaged.95==0]
N.ALL[N.Usaged.99==0]
N.ALL[N.Usaged.995==0]

N.SP.95<-N.ALL[N.Usaged.95>0, .(N_SPECIES=sum(N_SPECIES)), by=.(continent)]
N.ALL[N.Usaged.95>0,.(N_Simulations=.N), by=.(continent, nb, da)]

N.SP.99<-N.ALL[N.Usaged.99>0, .(N_SPECIES=sum(N_SPECIES)), by=.(continent)]
N.SP.995<-N.ALL[N.Usaged.995>0, .(N_SPECIES=sum(N_SPECIES)), by=.(continent)]

richness.df<-readRDS("../Data/Tables/N.with.bridge.seed.continent.rda")
richness.df$label<-sprintf("%d.%s.%s", richness.df$seed_id, richness.df$NB, richness.df$DA)

N.SP.Type.95<-richness.df[label %in% N95$seed_label, 
                          .(Native=sum(in_source_continent),
                            Immigrant=sum(to_target_continent)),
                          by=.(seed_continent)]
N.SP.Type.95$Immigrant_Per<-N.SP.Type.95$Immigrant/(N.SP.Type.95$Native+N.SP.Type.95$Immigrant)

N.SP.Type.99<-richness.df[label %in% N99$seed_label, 
                          .(Native=sum(in_source_continent),
                            Immigrant=sum(to_target_continent)),
                          by=.(seed_continent)]
N.SP.Type.99$Immigrant_Per<-N.SP.Type.99$Immigrant/(N.SP.Type.99$Native+N.SP.Type.99$Immigrant)

N.SP.Type.995<-richness.df[label %in% N995$seed_label, 
                          .(Native=sum(in_source_continent),
                            Immigrant=sum(to_target_continent)),
                          by=.(seed_continent)]
N.SP.Type.995$Immigrant_Per<-N.SP.Type.995$Immigrant/(N.SP.Type.995$Native+N.SP.Type.995$Immigrant)


richness.all<-richness[, c("seed_continent", "type", "N", "rep", "continent")]
richness.all$event<-"Richness"


ggplot(N.ALL)+geom_point(aes(x=continent, y=N_SPECIES, color=continent, size=min.dist))