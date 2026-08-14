library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
library(dplyr)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (T){
    seeds<-readRDS("../Data/Tables/seeds.rda")
    seed.dist<-readRDS("../Data/Tables/cells.with.dist.rda")
    df<-readRDS("../Data/Tables/N.Speciation.Extinction.All.NB.rda")
    #11871.MODERATE.GOOD
    
    table(df$nb)
    df[,(N=length(unique(seed_id))), by="continent"]
    nrow(df)
    df$continent<-NULL
    df.detail<-merge(df, seeds, by.x="seed_id", by.y="global_id")
    df.detail[,.(N=length(unique(seed_id))), by=list(continent)]
    
    
    #df<-df[between(lat, -35, 45)]
    range(df.detail$lat)
    
    burn_in<-1801
    unique(df.detail$nb)
    
    df_N_checked<-df.detail[year==burn_in & N_SPECIES>0, 
                            .(N=.N), by=list(seed_id, nb)]
    
    sum(df.detail[year==burn_in & N_SPECIES>0, 
              .(N=.N), by=list(seed_id, nb)]$N)
    sum(df.detail[year==0 & N_SPECIES>0, 
                  .(N=.N), by=list(seed_id, nb)]$N)
    
    df_N_checked$label<-sprintf("%d.%s", df_N_checked$seed_id, df_N_checked$nb)
    df.detail$label<-sprintf("%d.%s", df.detail$seed_id, df.detail$nb)
    df_filtered_seeds<-df.detail[label %in% df_N_checked[N==2]$label]
    table(df_N_checked$N)
    
    df.detail[,.(N=length(unique(seed_id))), by=list(nb)]
    
    df.detail$seed_label<-sprintf("%d.%s.%s", df.detail$seed_id, df.detail$nb, df.detail$da)
    
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
    
    
    N_species$seed_label<-sprintf("%d.%s.%s", N_species$seed_id, N_species$nb, N_species$da)
    df_filtered_seeds$seed_label<-sprintf("%d.%s.%s", 
                                          df_filtered_seeds$seed_id, 
                                          df_filtered_seeds$nb, 
                                          df_filtered_seeds$da)
    
    N_species_filter<-df_filtered_seeds
    df_filtered_N_filter<-N_species_filter[, .(N_SPECIES=sum(N_SPECIES), 
                                               N_SPECIATION=sum(N_SPECIATION),
                                               N_EXTINCTION=sum(N_EXTINCTION),
                                               N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                                               N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                                               N_ALL_SPECIES=sum(N_ALL_SPECIES),
                                               N_SEED=length(unique(seed_label))),
                                           by=list(continent, year)]
    
    
    table(N_species$nb)
    no.outliers<-df_filtered_seeds[(year==0)]
    no.outliers[,.(N_SPECIES=sum(N_SPECIES)), by=list(continent)]
    
    seed_pool<-no.outliers[, .(N_SPECIES=sum(N_SPECIES)), 
                           by=list(seed_id, continent, nb, da, seed_label)]
    seed_pool[,.(N=.N, N_SPECIES=sum(N_SPECIES)), by=list(nb, da, continent)]
    seed_pool$weight<-1
    
    
    df_filtered_seeds[, .(N=length(unique(seed_label))), by=list(nb, da, continent)]
    
    
    
    seed_pool<-merge(seed_pool, seed.dist[, c("seqnum", "min.dist")], 
                     by.x="seed_id", by.y="seqnum")
    
    seed_pool$min.dist.band<-floor(seed_pool$min.dist/5)
    seed_pool[min.dist.band==5 & nb=="BROAD" & da=="GOOD"]
    seed_pool[min.dist==25 & nb=="BROAD" & da=="GOOD"]
    setorderv(seed_pool, "N_SPECIES", -1)
    seed_pool[min.dist.band==13 & nb=="MODERATE" & da=="GOOD"]
    seed_pool<-seed_pool[!seed_label %in% c("11871.MODERATE.GOOD",
                                            "9652.MODERATE.GOOD",
                                            "9241.MODERATE.GOOD")]
    View(seed_pool[nb %in% c("MODERATE", "NARROW") & da=="GOOD"])
    n<-seed_pool[, .(N=.N), by=list(continent, min.dist.band, nb, da)]
    n[min.dist.band==13]
    setorderv(n, "min.dist.band")
    

    
    coms<-unique(n[, c("nb", "da", "min.dist.band")])
    bins<-list()
    max_seeds<-10
    for (i in c(1:nrow(coms))){
      com<-coms[i]
      item<-n[nb==com$nb & da==com$da & min.dist.band==com$min.dist.band]
      if (nrow(item)!=2){
        print(item)
        
        print("skip")
        next()
      }
      min.N<-min(item$N, max_seeds)
      bins[[length(bins)+1]]<-data.table(min.dist.band=com$min.dist.band,
                                         nb=com$nb,
                                         da=com$da,
                                         N=min.N)
    }
    bins<-rbindlist(bins)
    table(bins$N)
    all_ramdom_seeds<-list()
    set.seed(1024)
    for (rep in c(1:100)){
      print(rep)
      seed_pool.rand<-list()
      for (i in c(1:nrow(bins))){
        bin<-bins[i]
        
        seed.item<-seed_pool[nb==bin$nb & da==bin$da & min.dist.band==bin$min.dist.band]
        seed.item<-seed.item[,.SD[sample(.N, bin$N, prob=weight)],by = "continent"]
        seed_pool.rand[[length(seed_pool.rand)+1]]<-seed.item
      }
      seed_pool.rand<-rbindlist(seed_pool.rand)
      
      seed_pool.rand[, .(N=.N), by=c("continent", "nb", "da")]
      #ramdom_seeds<-seed_pool.rand[,.SD[sample(.N, 100)],
      #                             by = c("continent", "nb", "da")]
      
      ramdom_seeds<-seed_pool.rand
      
      ramdom_seeds$geometry<-NULL
      all_seeds<-ramdom_seeds
      all_seeds$rep<-rep
      all_seeds$label<-sprintf("%d.%s.%s", all_seeds$seed_id, all_seeds$nb, all_seeds$da)
      all_ramdom_seeds[[rep]]<-all_seeds
    }
    
    all_ramdom_seeds_df<-rbindlist(all_ramdom_seeds)
    
    saveRDS(all_ramdom_seeds_df, sprintf("../Data/Tables/random.seeds.by.nb.distance.5.rda"))
  }

