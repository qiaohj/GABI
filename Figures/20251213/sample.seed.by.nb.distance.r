library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

seeds<-readRDS("../Data/Tables/seeds.rda")
seed.dist<-readRDS("../Data/cells.with.dist.rda")


selected.seeds<-data.table(seed.dist)
selected.seeds$geometry<-NULL
selected.seeds[,.(max=max(min.dist)), by=list(continent)]
selected.seeds<-selected.seeds[min.dist<=107]
seeds<-seeds[global_id %in% selected.seeds$seqnum]
ggplot(seeds)+geom_sf(data=seed.dist)+geom_point(aes(x=lon, y=lat, color=continent))

df<-readRDS("../Data/Tables/N.Speciation.Extinction.All.NB.rda")
df<-df[nb %in% c("BIG-BIG", "MODERATE-MODERATE")]
table(df$nb)
nrow(df)
df$continent<-NULL
df.detail<-merge(df, seeds, by.x="seed_id", by.y="global_id")

#df<-df[between(lat, -35, 45)]
range(df.detail$lat)
#burn in 3100/2
burn_in<-3100/2
unique(df.detail$nb)
#df.detail<-df.detail[nb %in% c("BIG-BIG", "MODERATE-MODERATE")]

df_N_checked<-df.detail[year==burn_in+1 & N_SPECIES>0, 
                        .(N=.N), by=list(seed_id, nb)]

df_N_checked$label<-sprintf("%d.%s", df_N_checked$seed_id, df_N_checked$nb)
df.detail$label<-sprintf("%d.%s", df.detail$seed_id, df.detail$nb)
df_filtered_seeds<-df.detail[label %in% df_N_checked[N==2]$label]
df_filtered_seeds_dist<-merge(df_filtered_seeds, seed.dist[, c("min.dist", "seqnum")], 
                              by.x="seed_id", by.y="seqnum")
x<-df_filtered_seeds_dist[,.(N=.N), by=list(nb, continent, min.dist)]
x.N<-x[, .(N=.N), by=list(nb, min.dist)]
effective.band<-x.N[N==2]
df_filtered_seeds_dist$dist.label<-sprintf("%d.%s", df_filtered_seeds_dist$min.dist, 
                                           df_filtered_seeds_dist$nb)

View(x[nb=="MODERATE-MODERATE"])
df.detail[,.(N=length(unique(seed_id))), by=list(nb)]

df_N_checked_sf<-df[year==burn_in+1 & N_SPECIES>0, 
                        .(N=.N), by=list(seed_id, nb)]

df_N_checked_sf<-merge(seed.dist, df_N_checked_sf, by.x="seqnum", by.y="seed_id")
ggplot(seed.dist)+geom_sf(fill=NA, color="lightgray")+
  geom_sf(data=df_N_checked_sf, aes(fill=factor(N)))+
  facet_wrap(~nb)

df_filtered_seeds[,.(N=length(unique(seed_id))), by=list(nb)]



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
quantile<-107.710
N_species$seed_label<-paste(N_species$seed_id, N_species$nb, N_species$da)
outliers<-unique(N_species[N_SPECIES>quantile])
#outliers<-unique(N_species[N_SPECIES>107])

ggplot(seed.dist)+geom_sf(fill=NA, color="lightgray")+
  geom_sf(data=seed.dist[which(seed.dist$seqnum %in% outliers$seed_id),], fill="red")
#outliers<-unique(N_species[N_SPECIES>100])

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
  geom_text(data=df_filtered_N[year==1799], 
            aes(x=-1900, y=c(2e4, 2.5e4), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)+
  geom_text(data=df_filtered_N[year==burn_in+1], 
            aes(x=burn_in * -1 + 10, y=c(1.7e4, 1.9e4), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)
if (F){
  seeds.rep<-readRDS("../Data/Tables/random.seeds.threshold.full.nb.rda")
  df.list<-list()
  for (r in c(1:10)){
    print(r)
    item<-seeds.rep[rep==r]
    xxx<-N_species_filter[seed_id %in% item$seed_id, 
                          .(rep=r,
                            N_SPECIES=sum(N_SPECIES), 
                            N_SPECIATION=sum(N_SPECIATION),
                            N_EXTINCTION=sum(N_EXTINCTION),
                            N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                            N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                            N_ALL_SPECIES=sum(N_ALL_SPECIES),
                            N_SEED=length(unique(seed_id))),
                          by=list(continent, year, nb, da)]
    df.list[[r]]<-xxx
  }
  df.se<-rbindlist(df.list)
  df.se.se<-df.se[, .(N_SPECIES=mean(N_SPECIES),
                      N_SPECIES_sd=sd(N_SPECIES)),
                  by=list(continent, year, nb, da)]
  ggplot(df.se.se[nb!="BROAD-BROAD"])+
    geom_ribbon(aes(x=year * -1, ymin=N_SPECIES-N_SPECIES_sd,
                    ymax=N_SPECIES+N_SPECIES_sd,
                    group=continent), alpha=0.2)+
    geom_line(aes(y=N_SPECIES, x=year * -1, color=continent))+
    geom_vline(xintercept = burn_in * -1, linetype=2)+
    lims(x=c(-1000, 0))+
    labs(color="seed continent")+
    facet_grid(nb~da, scale="free")
  
}

outliers_ID<-unique(outliers$seed_id)
unique(N_species$nb)
#random seeds
seed_pool<-df_filtered_seeds[(year==0 & !(seed_id %in% outliers_ID)), 
                             .(N_SPECIES=sum(N_SPECIES)), 
                             by=list(seed_id, continent, nb, da)]
seed_pool$label2<-sprintf("%d.%s.%s", seed_pool$seed_id, seed_pool$nb, seed_pool$da)
#seed_pool<-seed_pool[label2 %in% df[to_target_continent>0]$label]

df<-readRDS("../Data/Tables/N.with.bridge.seed.continent.rda")
target.nb<-c("BIG-BIG", "MODERATE-MODERATE")
df<-df[NB %in% target.nb]
df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
df$weight<-1
df.test<-df[label %in% seed_pool$label2]
df.test$is_to<-df.test$to_target_continent>0
ggplot(df.test)+
  geom_point(aes(x=in_source_continent, y=to_target_continent, color=seed_continent, shape=is_to), size=0.8)+
  geom_abline(linetype=2)+
  coord_equal()+
  facet_grid(NB~DA)+
  theme_bw()


if (F){
  seed_pool.map<-merge(seed.dist, seed_pool[nb!="BROAD-BROAD"], by.y="seed_id", by.x="seqnum")
  seed_pool[, .(N=.N), by=list(nb, continent)]
  ggplot(seed.dist)+
    geom_sf(fill="lightgrey", color=NA)+
    geom_sf(data=seed_pool.map, aes(fill=continent.x))+
    facet_wrap(~nb)+
    map_theme
}
df_filtered_seeds[, .(N=length(unique(seed_id))), by=list(nb, da, continent)]

if (F){
  ggplot(seed_pool)+geom_histogram(aes(x=min.dist), bins=10)+
    facet_grid(continent~nb)
}

seed_pool<-merge(seed_pool, seed.dist[, c("seqnum", "min.dist")], 
                 by.x="seed_id", by.y="seqnum")
#seed_pool[nb=="MODERATE-MODERATE" & min.dist>86, min.dist:=86]
seed_pool<-seed_pool[nb=="BIG-BIG" | min.dist<=80]
n<-seed_pool[, .(N=.N), by=list(continent, min.dist, nb)]
n
setorderv(n, "min.dist")

coms<-unique(n[, c("nb", "min.dist")])
bins<-list()
for (i in c(1:nrow(coms))){
  com<-coms[i]
  item<-n[nb==com$nb & min.dist==com$min.dist]
  if (nrow(item)!=2){
    print(item)
    print("skip")
    next()
  }
  min.N<-min(item$N)
  bins[[length(bins)+1]]<-data.table(min.dist=com$min.dist,
                                   nb=com$nb,
                                   N=min.N)
}
bins<-rbindlist(bins)

all_ramdom_seeds<-list()
for (rep in c(1:100)){
  print(rep)
  seed_pool.rand<-list()
  for (i in c(1:nrow(bins))){
    bin<-bins[i]
    seed.item<-seed_pool[nb==bin$nb & min.dist==bin$min.dist]
    seed.item<-seed.item[,.SD[sample(.N, bin$N)],by = "continent"]
    seed_pool.rand[[length(seed_pool.rand)+1]]<-seed.item
  }
  seed_pool.rand<-rbindlist(seed_pool.rand)
  if (F){
    ggplot(seed_pool.rand)+geom_histogram(aes(x=min.dist, fill=continent))+
      facet_grid(nb~continent)
  }
  ramdom_seeds<-seed_pool.rand[,.SD[sample(.N, 100)],
                               by = c("continent", "nb")]
  
  if (F){
    ggplot(ramdom_seeds)+geom_histogram(aes(x=min.dist, fill=continent))+
      facet_grid(nb~continent)
  } 
  
  if (F){
    ramdom_seeds$geometry<-NULL
    ggplot(ramdom_seeds)+geom_histogram(aes(x=min.dist, fill=continent), binwidth = 2)+
      facet_wrap(~continent)
    ramdom_seeds.map<-merge(seed.dist, ramdom_seeds, by.x="seqnum", by.y="seed_id")
    ggplot(ramdom_seeds.map)+
      geom_sf(data=seed.dist, fill="lightgrey", color=NA)+geom_sf(aes(fill=continent.x))+
      facet_wrap(~nb)
  }
  ramdom_seeds$geometry<-NULL
  all_seeds<-list()
  for (da.str in c("GOOD", "POOR")){
    ramdom_item<-ramdom_seeds
    ramdom_item$rep<-rep
    ramdom_item$da<-da.str
    all_seeds[[length(all_seeds)+1]]<-ramdom_item
  }
  
  
  all_seeds<-rbindlist(all_seeds)
  all_seeds$label<-sprintf("%d.%s.%s", all_seeds$seed_id, all_seeds$nb, all_seeds$da)
  #all_seeds<-all_seeds[!label %in% outliers]
  all_ramdom_seeds[[rep]]<-all_seeds
}

all_ramdom_seeds_df<-rbindlist(all_ramdom_seeds)

unique(all_ramdom_seeds_df[, .(N=.N), by=list(continent, rep, nb, da)]$N)

saveRDS(all_ramdom_seeds_df, "../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")


all_ramdom_seeds_df<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distribution.rda")
all_ramdom_seeds_df[continent=="bridge1"]
N.seed<-all_ramdom_seeds_df[, .(N=.N/2), by=list(seed_id, nb, da)]
N.seed<-merge(seed.dist, N.seed, by.x="seqnum", by.y="seed_id")
ggplot()+geom_sf(data=seed.dist, fill=NA, color="lightgrey")+
  geom_sf(data=N.seed, aes(fill=N))+
  facet_grid(nb~da)
