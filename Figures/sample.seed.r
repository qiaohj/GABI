library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

seed.dist<-readRDS("../Data/cells.with.dist.rda")
df<-readRDS("../Data/Tables/N.Speciation.Extinction.rda")
nrow(df)
df$continent<-NULL
df<-df[nb %in% c("BIG-BIG", "MODERATE-MODERATE")]
seed.dist.dt<-data.table(seed_id=seed.dist$seqnum, 
                         min.dist=seed.dist$min.dist,
                         continent=seed.dist$continent)
df_dist<-merge(df, seed.dist.dt, by="seed_id")
burn_in<-3100/2
df_N_checked<-df_dist[year==burn_in+1 & N_SPECIES>0, 
                 .(N=.N), by=list(seed_id, min.dist)]
range(df_N_checked$N)

df_filtered_seeds<-df_dist[seed_id %in% df_N_checked[N==4]$seed_id]
N_species<-df_filtered_seeds[year==0 & N_SPECIES>0]

quantiles<-quantile(N_species$N_SPECIES, c(0, 1, 0.99, 0.95, 0.90, 0.999))

N_species$seed_label<-paste(N_species$seed_id, N_species$nb, N_species$da)
outliers<-unique(N_species[N_SPECIES>quantiles[3]])


df_filtered_seeds<-unique(df_filtered_seeds[!seed_id %in% outliers$seed_id,
                                            c("min.dist", "continent", "seed_id")])

filtered_seeds<-seed.dist[which(seed.dist$seqnum %in% df_filtered_seeds$seed_id),]

ggplot(filtered_seeds)+geom_sf(color="grey", fill=NA)+
  geom_sf(data=filtered_seeds[which(filtered_seeds$min.dist==25),],
          aes(fill=continent))
#North America:707/656  South America:2435/2341

seeds_na<-df_filtered_seeds[continent=="North America"]
seeds_sa<-df_filtered_seeds[continent=="South America"]
seeds_na<-seeds_na[min.dist<=max(seeds_sa$min.dist)]

sample_size<-200
sample_list<-list()
for (r in c(1:10)){
  sample_na<-seeds_na[sample(nrow(seeds_na), sample_size)]
  distances<-unique(sample_na$min.dist)
  item.list<-list()
  for (d in distances){
    item_na<-sample_na[min.dist==d]
    item_sa<-seeds_sa[min.dist==d]
    item_sa<-item_sa[sample(nrow(item_sa), nrow(item_na))]
    item<-rbindlist(list(item_sa, item_na))
    item.list[[length(item.list)+1]]<-item
  }
  item.df<-rbindlist(item.list)
  item.df$rep<-r
  sample_list[[r]]<-item.df
  
  ggplot(filtered_seeds)+geom_sf(color="grey", fill=NA)+
    geom_sf(data=filtered_seeds[which(filtered_seeds$seqnum %in% item.df$seed_id),],
            aes(fill=continent))
  
  ggplot(data=filtered_seeds[which(filtered_seeds$seqnum %in% item.df$seed_id),])+
    geom_histogram(aes(x=min.dist))+
    facet_wrap(~continent)
  
}

samples<-rbindlist(sample_list)


coms<-data.table(expand.grid(nb=c("MODERATE-MODERATE", "BIG-BIG"),
                             da=c("GOOD", "POOR")))
all_ramdom_seeds<-list()
for (r in c(1:10)){
  ramdom_seeds<-samples[rep==r]
  all_seeds<-list()
  for (i in c(1:nrow(coms))){
    com_item<-coms[i]
    ramdom_item<-ramdom_seeds
    ramdom_item$rep<-r
    ramdom_item$nb<-com_item$nb
    ramdom_item$da<-com_item$da
    all_seeds[[i]]<-ramdom_item
  }
  all_seeds<-rbindlist(all_seeds)
  all_seeds$label<-sprintf("%d.%s.%s", all_seeds$seed_id, all_seeds$nb, all_seeds$da)
  #all_seeds<-all_seeds[!label %in% outliers]
  all_ramdom_seeds[[r]]<-all_seeds
}
all_ramdom_seeds_df<-rbindlist(all_ramdom_seeds)

unique(all_ramdom_seeds_df[, .(N=.N), by=list(continent, rep, nb, da)]$N)
saveRDS(all_ramdom_seeds_df, "../Data/Tables/random.seeds.rda")
