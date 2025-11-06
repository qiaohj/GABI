library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

if (F){
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
  write_sf(filtered_seeds, "../Data/filtered_seeds.shp")
  seed.dist.N<-merge(seed.dist, df_N_checked, by.x="seqnum", by.y="seed_id")
  saveRDS(seed.dist.N, "../Data/Tables/seed.dist.N.rda")
  saveRDS(outliers, "../Data/Tables/seed.outliers.rda")
  
}
source("Figures/common.r")
seed.dist.N<-readRDS("../Data/Tables/seed.dist.N.rda")
seed.dist<-readRDS("../Data/cells.with.dist.rda")
outliers<-readRDS("../Data/Tables/seed.outliers.rda")
m_color<-c("4"=color_high,
           "2"=color_low,
           "3"=color_1,
           "1"=color_2)
threshold.north<-35
threshold.north2<-18
threshold.south2<- 8
threshold.south<- -10
latitudes <- c(threshold.north, 
               #threshold.north2, 
               #threshold.south2, 
               threshold.south)

min_lon <- -170
max_lon <- -30

latitude_lines_sf <- latitudes %>%
  purrr::map_dfr(~{
    st_linestring(matrix(c(min_lon, .x, max_lon, .x), ncol = 2, byrow = TRUE)) %>%
      st_sfc(crs = 4326) %>% # WGS84
      st_sf(latitude = .x, geometry = .)
  })


p<-ggplot(seed.dist)+
  geom_sf(color="grey", fill=NA)+
  geom_sf(data=seed.dist.N,
          aes(fill=factor(N)))+
  geom_sf(data=seed.dist[which(seed.dist$seqnum %in% outliers$seed_id),],
          fill="grey")+
  geom_sf(data=latitude_lines_sf, linetype=2)+
  labs(fill="Number of combinations survival in the end of the burn-in")+
  coord_sf(crs=map_crs)+
  scale_fill_manual(values=m_color)+
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))+
  map_theme
p
ggsave(p, filename="../Figures/N.survival.map.pdf", width=5, height=7)
df_filtered_seeds<-data.table(seed_id=seed.dist.N$seqnum,
                              continent=seed.dist.N$continent,
                              lon=seed.dist.N$lon,
                              lat=seed.dist.N$lat,
                              N=seed.dist.N$N,
                              min.dist=seed.dist.N$min.dist.x)
df_filtered_seeds<-df_filtered_seeds[!seed_id %in% outliers$seed_id]
seeds_na<-df_filtered_seeds[continent=="North America" & N==4 & lat>=threshold.north]
seeds_sa<-df_filtered_seeds[continent=="South America" & N==4 & lat<=threshold.south]
#seeds_na<-seeds_na[min.dist<55]


sample_size<-100
sample_list<-list()
for (r in c(1:10)){
  sample_na<-seeds_na[sample(nrow(seeds_na), sample_size)]
  distances<-unique(sample_na$min.dist)
  item.list<-list()
  for (d in distances){
    item_na<-sample_na[min.dist==d]
    item_sa<-seeds_sa[min.dist==d]
    item_sa<-item_sa[sample(nrow(item_sa), nrow(item_na), replace=T)]
    item<-rbindlist(list(item_sa, item_na))
    item.list[[length(item.list)+1]]<-item
  }
  item.df<-rbindlist(item.list)
  item.df$rep<-r
  sample_list[[r]]<-item.df
  
  ggplot(seed.dist)+geom_sf(color="grey", fill=NA)+
    geom_sf(data=seed.dist[which(seed.dist$seqnum %in% item.df$seed_id),],
            aes(fill=continent))
  
  ggplot(data=seed.dist[which(seed.dist$seqnum %in% item.df$seed_id),])+
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
