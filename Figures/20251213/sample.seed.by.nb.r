library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

seeds<-readRDS("../Data/Tables/seeds.rda")
seed.dist<-readRDS("../Data/Tables/cells.with.dist.rda")
seeds<-seeds[global_id %in% all_dfx$seed_id]
ggplot(seeds)+geom_sf(data=seed.dist)+geom_point(aes(x=lon, y=lat, color=continent))

df<-readRDS("../Data/Tables/N.Speciation.Extinction.All.NB.rda")
if (F){
  test<-df[year==burn_in+1 & N_SPECIES>0]
  test[, .(N=.N), by=list(nb, da)]
  
  MODERATE<-test[nb=="MODERATE-MODERATE"]
  NARROW<-test[nb=="NARROW-NARROW"]
  
  NARROW[!seed_id %in% MODERATE$seed_id]
  df[seed_id==1163 & nb=="NARROW-NARROW" & da=="POOR"]
  
  xxx<-readRDS("/media/huijieqiao/Butterfly/GABI/Results/1163.NARROW-NARROW.POOR/1163.NARROW-NARROW.POOR.N.speciation.extinction.rda")
}
table(df$nb)
nrow(df)
df$continent<-NULL
df.detail<-merge(df, seeds, by.x="seed_id", by.y="global_id")

#df<-df[between(lat, -35, 45)]
range(df.detail$lat)
#burn in 3100/2
burn_in<-3000/2
unique(df.detail$nb)

df_N_checked<-df.detail[year==burn_in+1 & N_SPECIES>0, 
                 .(N=.N), by=list(seed_id, nb)]

df_N_checked$label<-sprintf("%d.%s", df_N_checked$seed_id, df_N_checked$nb)
df.detail$label<-sprintf("%d.%s", df.detail$seed_id, df.detail$nb)
df_filtered_seeds<-df.detail[label %in% df_N_checked[N==2]$label]
df.detail[,.(N=length(unique(seed_id))), by=list(nb)]

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
df_filtered_N_detals[year==burn_in+1, 
                     c("N_SEED", "continent", "nb", "da")]
df_filtered_N[year==burn_in+1]

table(df_N_checked$N)


#define outliers
N_species<-df_filtered_seeds[year==0 & N_SPECIES>0]


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
  geom_text(data=df_filtered_N[year==1064], 
            aes(x=-1100, y=c(2e3, 2.5e3), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)+
  geom_text(data=df_filtered_N[year==burn_in+1], 
            aes(x=burn_in * -1 + 10, y=c(1.7e3, 1.9e3), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)


outliers_ID<-unique(outliers$seed_id)
unique(N_species$nb)
#random seeds
seed_pool<-df_filtered_seeds[(year==0 & !(seed_id %in% outliers_ID)), 
                             .(N_SPECIES=sum(N_SPECIES)), by=list(seed_id, continent, nb)]
df_filtered_seeds[, .(N=length(unique(seed_id))), by=list(nb, da, continent)]


all_ramdom_seeds<-list()
for (rep in c(1:10)){
  
  all_seeds<-list()
  for (nb.str in c("MODERATE", 
                   "NARROW", "BROAD")){
    ramdom_seeds<-seed_pool[nb==nb.str,.SD[sample(.N, 50)],by = "continent"]
    for (da.str in c("GOOD", "POOR")){
      ramdom_item<-ramdom_seeds
      ramdom_item$rep<-rep
      ramdom_item$nb<-nb.str
      ramdom_item$da<-da.str
      all_seeds[[length(all_seeds)+1]]<-ramdom_item
    }
  }
  
  all_seeds<-rbindlist(all_seeds)
  all_seeds$label<-sprintf("%d.%s.%s", all_seeds$seed_id, all_seeds$nb, all_seeds$da)
  #all_seeds<-all_seeds[!label %in% outliers]
  all_ramdom_seeds[[rep]]<-all_seeds
}
all_ramdom_seeds_df<-rbindlist(all_ramdom_seeds)

unique(all_ramdom_seeds_df[, .(N=.N), by=list(continent, rep, nb, da)]$N)
saveRDS(all_ramdom_seeds_df, "../Data/Tables/random.seeds.threshold.by.nb.rda")


all_ramdom_seeds_df<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.rda")

N.seed<-all_ramdom_seeds_df[, .(N=.N/2), by=list(seed_id)]
N.seed<-merge(seed.dist, N.seed, by.x="seqnum", by.y="seed_id")
ggplot()+geom_sf(data=seed.dist, fill=NA, color="lightgrey")+
  geom_sf(data=N.seed, aes(fill=N))
