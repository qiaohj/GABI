library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

#80: 663 vs 2041
#70: 600 vs 1509
#60: 527 vs 1070
threshold<-70
seeds<-readRDS("../Data/Tables/seeds.rda")
seed.dist<-readRDS("../Data/cells.with.dist.rda")
selected.seeds<-data.table(seed.dist)
selected.seeds$geometry<-NULL
table(selected.seeds[between(min.dist, 1, threshold)]$continent)

selected.seeds<-selected.seeds[between(min.dist, 1, threshold)]
seeds<-seeds[global_id %in% selected.seeds$seqnum]
ggplot(seeds)+geom_sf(data=seed.dist)+geom_point(aes(x=lon, y=lat, color=continent))

df<-readRDS("../Data/Tables/N.Speciation.Extinction.rda")
nrow(df)
df$continent<-NULL
df<-merge(df, seeds, by.x="seed_id", by.y="global_id")

#df<-df[between(lat, -35, 45)]
range(df$lat)
#burn in 3100/2
burn_in<-3100/2
unique(df$nb)
df<-df[nb %in% c("BIG-BIG", "MODERATE-MODERATE")]


df_N<-df[, .(N_SPECIES=sum(N_SPECIES), 
             N_SPECIATION=sum(N_SPECIATION),
             N_EXTINCTION=sum(N_EXTINCTION),
             N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
             N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
             N_ALL_SPECIES=sum(N_ALL_SPECIES),
             N_SEED=length(unique(seed_id))),
         by=list(continent, year)]

df[seed_id=="5412" & nb=="MODERATE-MODERATE"]
df_N[year==1799]$N_ALL_SPECIES<-df_N[year==1799]$N_SEED


df_N_checked<-df[year==burn_in+1 & N_SPECIES>0, 
                 .(N=.N), by=list(seed_id)]

df_filtered_seeds<-df[seed_id %in% df_N_checked[N==4]$seed_id]

ggplot(seeds)+
  geom_sf(data=seed.dist)+
  geom_point(aes(x=lon, y=lat, color=continent), alpha=0.2)+
  geom_point(data=seeds[global_id %in% df_N_checked[N==4]$seed_id],
             aes(x=lon, y=lat, color=continent))
table(seeds[global_id %in% df_N_checked[N==4]$seed_id]$continent)
table(seeds$continent)

df_filtered_seeds[seed_id=="5412"]
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
df_filtered_N_detals[year==burn_in+1 & nb!="HUGE-HUGE", 
                     c("N_SEED", "continent", "nb", "da")]
df_filtered_N[year==burn_in+1]

table(df_N_checked$N)

#df_N$label<-paste(df_N$nb, df_N$da)

ggplot(df_filtered_N)+
  geom_line(aes(y=N_SPECIES, x=year * -1, color=continent))+
  geom_vline(xintercept = burn_in * -1, linetype=2)+
  geom_text(data=df_N[year==1799], 
            aes(x=-1900, y=c(6e4, 7e4), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)+
  geom_text(data=df_filtered_N[year==burn_in+1], 
            aes(x=burn_in * -1 + 10, y=c(5.5e4, 6.5e4), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)

#define outliers
N_species<-df_filtered_seeds[year==0 & N_SPECIES>0]


if (F){
  ggplot(N_species[N_SPECIES<113])+
    geom_histogram(aes(x=N_SPECIES))+
    scale_x_sqrt()+
    facet_grid(nb+da~continent)
  
  seed.end<-N_species[N_SPECIES<113, .(N=.N), by=list(seed_id, continent)]
  seed.end<-seed.end[N==4]
  table(seed.end$continent)
  
  seed.na<-seed.end[continent=="North America"]
  random.seed.na<-seed.na[sample(nrow(seed.na), 1000)]
  
  seed.sa<-seed.end[continent=="South America"]
  random.seed.sa<-seed.sa[sample(nrow(seed.sa), 1000)]
  
  ramdom.N_species<-N_species[seed_id %in% c(random.seed.sa$seed_id, random.seed.na$seed_id)]
  ggplot(ramdom.N_species)+
    geom_histogram(aes(x=N_SPECIES))+
    scale_x_sqrt()+
    facet_grid(nb+da~continent)
  
  xxx<-ramdom.N_species[,.(N=sum(N_SPECIES)), by=list(continent, nb, da)]
  setorderv(xxx, c("nb", "da", "continent"))
}

quantiles<-quantile(N_species$N_SPECIES, c(0, 1, 0.99, 0.95, 0.90, 0.999))

N_species$seed_label<-paste(N_species$seed_id, N_species$nb, N_species$da)
outliers<-unique(N_species[N_SPECIES>quantiles[3]])

N_species_filter<-df_filtered_seeds[!seed_id %in% outliers$seed_id]
df_filtered_N_filter<-N_species_filter[, .(N_SPECIES=sum(N_SPECIES), 
                                     N_SPECIATION=sum(N_SPECIATION),
                                     N_EXTINCTION=sum(N_EXTINCTION),
                                     N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                                     N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                                     N_ALL_SPECIES=sum(N_ALL_SPECIES),
                                     N_SEED=length(unique(seed_id))),
                                 by=list(continent, year)]

ggplot(df_filtered_N_filter)+
  geom_line(aes(y=N_SPECIES, x=year * -1, color=continent))+
  geom_vline(xintercept = burn_in * -1, linetype=2)+
  geom_text(data=df_N[year==1799], 
            aes(x=-1900, y=c(2e4, 2.5e4), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)+
  geom_text(data=df_filtered_N[year==burn_in+1], 
            aes(x=burn_in * -1 + 10, y=c(1.7e4, 1.9e4), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)

df_filtered_N_filter<-N_species_filter[, .(N_SPECIES=sum(N_SPECIES), 
                                           N_SPECIATION=sum(N_SPECIATION),
                                           N_EXTINCTION=sum(N_EXTINCTION),
                                           N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                                           N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                                           N_ALL_SPECIES=sum(N_ALL_SPECIES),
                                           N_SEED=length(unique(seed_id))),
                                       by=list(continent, year, nb, da)]

ggplot(df_filtered_N_filter)+
  geom_line(aes(y=N_SPECIES, x=year * -1, color=continent))+
  geom_vline(xintercept = burn_in * -1, linetype=2)+
  facet_grid(nb~da, scale="free")

outliers_ID<-unique(outliers$seed_id)
unique(N_species$nb)
#random seeds
seed_pool<-df_filtered_seeds[(year==0 & !(seed_id %in% outliers_ID)), 
                             .(N_SPECIES=sum(N_SPECIES)), by=list(seed_id, continent)]



coms<-data.table(expand.grid(nb=c("MODERATE-MODERATE", "BIG-BIG"),
                  da=c("GOOD", "POOR")))
all_ramdom_seeds<-list()
for (rep in c(1:10)){
  ramdom_seeds<-seed_pool[,.SD[sample(.N, 500)],by = "continent"]
  all_seeds<-list()
  for (i in c(1:nrow(coms))){
    com_item<-coms[i]
    ramdom_item<-ramdom_seeds
    ramdom_item$rep<-rep
    ramdom_item$nb<-com_item$nb
    ramdom_item$da<-com_item$da
    all_seeds[[i]]<-ramdom_item
  }
  all_seeds<-rbindlist(all_seeds)
  all_seeds$label<-sprintf("%d.%s.%s", all_seeds$seed_id, all_seeds$nb, all_seeds$da)
  #all_seeds<-all_seeds[!label %in% outliers]
  all_ramdom_seeds[[rep]]<-all_seeds
}
all_ramdom_seeds_df<-rbindlist(all_ramdom_seeds)

unique(all_ramdom_seeds_df[, .(N=.N), by=list(continent, rep, nb, da)]$N)
saveRDS(all_ramdom_seeds_df, "../Data/Tables/random.seeds.rda")
