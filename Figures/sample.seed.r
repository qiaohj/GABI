library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

seeds<-readRDS("../Data/seeds.rda")
table(seeds[between(lat, -22, 50)]$continent)
df<-readRDS("../Data/Tables/100k.speciation.years/N.Speciation.Extinction.rda")
nrow(df)
df$continent<-NULL
df<-merge(df, seeds, by.x="seed_id", by.y="global_id")

df<-df[between(lat, -22, 50)]
range(df$lat)
#burn in 3100/2
burn_in<-3100/2




df_N<-df[, .(N_SPECIES=sum(N_SPECIES), 
             N_SPECIATION=sum(N_SPECIATION),
             N_EXTINCTION=sum(N_EXTINCTION),
             N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
             N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
             N_ALL_SPECIES=sum(N_ALL_SPECIES),
             N_SEED=length(unique(seed_id))),
         by=list(continent, year)]

df_N[year==1799]$N_ALL_SPECIES<-df_N[year==1799]$N_SEED


df_N_checked<-df[year==burn_in+1 & N_SPECIES>0, 
                 .(N=.N), by=list(seed_id)]

df_filtered_seeds<-df[seed_id %in% df_N_checked[N==8]$seed_id]

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
            aes(x=-1900, y=c(2e5, 2.2e5), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)+
  geom_text(data=df_filtered_N[year==burn_in+1], 
            aes(x=burn_in * -1 + 10, y=c(2e5, 2.2e5), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)

#define outliers
N_species<-df_filtered_seeds[year==0 & N_SPECIES>0]
N_species<-N_species[nb!="HUGE-HUGE"]
quantiles<-quantile(N_species$N_SPECIES, c(0, 1, 0.99, 0.95, 0.90, 0.999))
table(N_species[N_SPECIES>quantiles[3]]$continent)
N_species$seed_label<-paste(N_species$seed_id, N_species$nb, N_species$da)
outliers<-unique(N_species[N_SPECIES>quantiles[3]]$seed_label)
outliers_ID<-unique(N_species[N_SPECIES>quantiles[3]]$seed_id)
unique(N_species$nb)
#random seeds
seed_pool<-unique(df_filtered_seeds[!seed_id %in% outliers_ID, c("seed_id", "continent")])
table(seed_pool$continent)

coms<-data.table(expand.grid(nb=c("NARROW-NARROW", "MODERATE-MODERATE", "BROAD-BROAD"),
                  da=c("GOOD", "POOR")))
all_ramdom_seeds<-list()
for (rep in c(1:100)){
  ramdom_seeds<-seed_pool[,.SD[sample(.N, 1000)],by = "continent"]
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
saveRDS(all_ramdom_seeds_df, "../Data/Tables/100k.speciation.years/random.seeds.rda")
