library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
library(dplyr)
library(patchwork)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  threshold<-99
  seeds<-readRDS("../Data/Tables/seeds.rda")
  seed.dist<-readRDS("../Data/Tables/cells.with.dist.rda")
  
  df<-readRDS("../Data/Tables/N.Speciation.Extinction.All.NB.rda")
  df$continent<-NULL
  df.detail<-merge(df, seeds, by.x="seed_id", by.y="global_id")
  df.detail[,.(N=length(unique(seed_id))), by=list(continent)]
  
  burn_in<-1801
  unique(df.detail$nb)
  
  df_N_checked<-df.detail[year==burn_in & N_SPECIES>0, 
                          .(N=.N), by=list(seed_id, nb)]
  
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
  
  
  full.quantile<-quantile(N_species$N_SPECIES, 
                          seq(0.80, 1, by=0.005))
  
  names(full.quantile)<-gsub("%", "", names(full.quantile))
  quantile_index<-which(names(full.quantile)==as.character(threshold))
  
  
  N_species$seed_label<-sprintf("%d.%s.%s", N_species$seed_id, N_species$nb, N_species$da)
  outliers<-unique(N_species[(N_SPECIES>full.quantile[quantile_index])])
  saveRDS(outliers, "../Data/Tables/outliers.1801.rda")
  setorderv(outliers, "N_SPECIES")
  
  dt.outliers<-outliers[,.(N=.N), by=list(nb, da, continent)]
  dt.outliers$nb<-factor(dt.outliers$nb, 
                         levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                         labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
  setorderv(dt.outliers, c("nb", "da", "continent"))
  
  
  dt.outliers
  
  
  outliers$nb<-factor(outliers$nb, 
                      levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                      labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
  
  
  table(outliers$continent)
  df_filtered_seeds$seed_label<-sprintf("%d.%s.%s", 
                                        df_filtered_seeds$seed_id, 
                                        df_filtered_seeds$nb, 
                                        df_filtered_seeds$da)
  
  N_species_filter<-df_filtered_seeds[!seed_label %in% outliers$seed_label]
  df_filtered_N_filter<-N_species_filter[, .(N_SPECIES=sum(N_SPECIES), 
                                             N_SPECIATION=sum(N_SPECIATION),
                                             N_EXTINCTION=sum(N_EXTINCTION),
                                             N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                                             N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                                             N_ALL_SPECIES=sum(N_ALL_SPECIES),
                                             N_SEED=length(unique(seed_label))),
                                         by=list(continent, year)]
  
  setorderv(outliers, "N_SPECIES")
  
  for (j in c(2:(nrow(outliers)+1))){
    xxxx<-sprintf("../Data/Tables/outliers/random.seeds.threshold.by.nb.distance.%d.rda", j-1)
    if (file.exists(xxxx)){
      next
    }
    if (j>nrow(outliers)){
      no.outliers<-df_filtered_seeds[(year==0)]
    }else{
      outliers_xx<-outliers[j:nrow(outliers)]
      outliers_ID<-unique(outliers_xx$seed_id)
      table(N_species$nb)
      no.outliers<-df_filtered_seeds[(year==0 & !(seed_id %in% outliers_ID))]
    }
    no.outliers[,.(N_SPECIES=sum(N_SPECIES)), by=list(continent)]
    no.outliers[N_SPECIES==max(no.outliers$N_SPECIES)]
    
    seed_pool<-no.outliers[, .(N_SPECIES=sum(N_SPECIES)), 
                           by=list(seed_id, continent, nb, da, seed_label)]
    seed_pool[,.(N=.N, N_SPECIES=sum(N_SPECIES)), by=list(nb, da, continent)]
    seed_pool$weight<-1
    
    df_filtered_seeds[, .(N=length(unique(seed_label))), by=list(nb, da, continent)]
    
    
    
    seed_pool<-merge(seed_pool, seed.dist[, c("seqnum", "min.dist")], 
                     by.x="seed_id", by.y="seqnum")
    
    seed_pool[, .(min.dist=min(min.dist)), by=.(continent)]
    seed_pool[, .(min.dist=max(min.dist)), by=.(continent)]
    
    #seed_pool[min.dist>=90, min.dist:=90]
    #seed_pool[min.dist<=27, min.dist:=27]
    
    n<-seed_pool[, .(N=.N), by=list(continent, min.dist, nb, da)]
    n
    hist(n$N)
    setorderv(n, "min.dist")
    
    seed_pool[,.(N=.N), by=list(nb, da, continent)]   
    
    seed_pool.df<-unique(seed_pool[, c("continent", "seed_id", "min.dist")])
    ggplot(seed_pool.df)+geom_histogram(aes(x=min.dist), binwidth = 1)+
      facet_wrap(~continent)
    
    coms<-unique(n[, c("nb", "da", "min.dist")])
    bins<-list()
    max_seeds<-5
    skipped_simulations<-list()
    for (i in c(1:nrow(coms))){
      com<-coms[i]
      item<-n[nb==com$nb & da==com$da & min.dist==com$min.dist]
      if (nrow(item)!=2){
        print(item)
        print("skip")
        xxx<-seed_pool[nb==com$nb & da==com$da & min.dist==com$min.dist]
        skipped_simulations[[length(skipped_simulations)+1]]<-xxx
        next()
      }
      min.N<-min(item$N, max_seeds)
      bins[[length(bins)+1]]<-data.table(min.dist=com$min.dist,
                                         nb=com$nb,
                                         da=com$da,
                                         N=min.N)
    }
    skipped_simulations<-rbindlist(skipped_simulations)
    if (F){
      ggplot(skipped_simulations)+geom_point(aes(x=min.dist, y=N_SPECIES, color=continent))+
        ylim(0, 2500)+
        facet_grid(da~nb)
      skipped_simulations[continent=="South America" & nb=="BROAD", 
                          .(N=.N), by=.(min.dist)]
      
      skipped_simulations[continent=="North America" & nb=="BROAD", 
                          .(N=.N), by=.(min.dist)]
      
      grids<-read_sf("../Shape/isea3h8/N_S_America.shp")
      seed_pool$geometry<-NULL
      seed_pool.map<-merge(seed.dist, df[year==0], by.y="seed_id", by.x="seqnum")
      
      seed.df.sa<-seed_pool.map[which(seed_pool.map$min.dist %in% 
                                        skipped_simulations[continent=="South America" & nb=="BROAD", 
                                                            .(N=.N), by=.(min.dist)]$min.dist &
                                        seed_pool.map$nb=="BROAD" &
                                        seed_pool.map$da=="GOOD"),]
      
      seed.df.na<-seed_pool.map[which(seed_pool.map$min.dist %in% 
                                        skipped_simulations[continent=="North America" & nb=="BROAD", 
                                                            .(N=.N), by=.(min.dist)]$min.dist &
                                        seed_pool.map$nb=="BROAD" &
                                        seed_pool.map$da=="GOOD"),]
      ggplot()+
        geom_sf(data=grids, color=NA, fill="lightgrey")+
        geom_sf_text(data = seed.df.sa, aes(label = N_SPECIES), size = 3, color = "black")+
        geom_sf_text(data = seed.df.na, aes(label = N_SPECIES), size = 3, color = "red")
      
    }
    bins<-rbindlist(bins)
    all_ramdom_seeds<-list()
    set.seed(1024)
    for (rep in c(1:100)){
      print(paste(j, rep))
      seed_pool.rand<-list()
      for (i in c(1:nrow(bins))){
        bin<-bins[i]
        if (nrow(bin[nb=="BROAD" & da=="POOR" & min.dist==35])==1){
          #asdf
        }
        seed.item<-seed_pool[nb==bin$nb & da==bin$da & min.dist==bin$min.dist]
        seed.item<-seed.item[,.SD[sample(.N, bin$N, prob=weight)],by = "continent"]
        seed_pool.rand[[length(seed_pool.rand)+1]]<-seed.item
      }
      seed_pool.rand<-rbindlist(seed_pool.rand)
      if (F){
        ggplot(seed_pool.rand)+geom_histogram(aes(x=min.dist, fill=continent), binwidth = 1)+
          facet_grid(nb~continent)
        
        seed_pool.rand[,.(N=.N), by=c("continent", "nb", "da")]
        ggplot(seed_pool.rand)+geom_boxplot(aes(x=continent, y=N_SPECIES))+
          facet_grid(nb~da)
        seed_pool.rand[,.(N=sum(N_SPECIES)), by=list(nb, continent)]
      }
      seed_pool.rand[, .(N=.N), by=c("continent", "nb", "da")]
      #ramdom_seeds<-seed_pool.rand[,.SD[sample(.N, 100)],
      #                             by = c("continent", "nb", "da")]
      
      ramdom_seeds<-seed_pool.rand
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
      all_seeds<-ramdom_seeds
      all_seeds$rep<-rep
      all_seeds$label<-sprintf("%d.%s.%s", all_seeds$seed_id, all_seeds$nb, all_seeds$da)
      all_ramdom_seeds[[rep]]<-all_seeds
    }
    
    all_ramdom_seeds_df<-rbindlist(all_ramdom_seeds)
    saveRDS(all_ramdom_seeds_df, xxxx)
  }
  if (F){
    seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
    seeds.all[!seed_id %in% all_ramdom_seeds_df$seed_id]
    xxx<-all_ramdom_seeds_df[!seed_id %in% seeds.all$seed_id]
    table(xxx$seed_id) 
  }
}
all.df.se.list<-list()
for (j in c(2:49)){
  print(j)
  df<-readRDS("../Data/Tables/N.Speciation.Extinction.Dispersal.rda")
  
  df[,.(N=sum(N)), by=list(type)]
  table(df$type)
  richness.df<-readRDS("../Data/Tables/N.with.bridge.seed.continent.rda")
  richness.df$label<-sprintf("%d.%s.%s", richness.df$seed_id, richness.df$NB, richness.df$DA)
  
  
  table(df$NB)
  df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
  table(df$type)
  xxxx<-sprintf("../Data/Tables/outliers/random.seeds.threshold.by.nb.distance.%d.rda", j-1)
  
  seeds.all<-readRDS(xxxx)
  length(unique(seeds.all$seed_label))
  #seeds.all<-seeds.all[!label %in% c("9078.BROAD.GOOD", "11119.BIG.GOOD", "10794.BIG.GOOD", "12104.BROAD.GOOD")]
  
  
  #seeds.all<-readRDS("../Data/Tables/random.seeds.95.rda")
  
  rep.list<-list()
  rep.list.all<-list()
  
  rep.richness.list<-list()
  rep.richness.list.all<-list()
  
  for (rrrr in c(1:100)){
    print(paste(j, rrrr))
    seeds<-seeds.all[rep==rrrr]
    item<-df[label %in% seeds$label]
    item.rep<-item[, .(N=sum(N),
                       N_Seed=length(unique(label))), 
                   by=list(NB, DA, seed_continent, type)]
    item.rep$rep<-rrrr
    rep.list[[rrrr]]<-item.rep
    item.rep<-item[, .(N=sum(N),
                       N_Seed=length(unique(label))), 
                   by=list(seed_continent, type)]
    item.rep$rep<-rrrr
    rep.list.all[[rrrr]]<-item.rep
    
    item<-richness.df[label %in% seeds$label]
    rep.to_target_continent<-item[, 
                                  .(N.to_target_continent=sum(to_target_continent),
                                    N.in_source_continent=sum(in_source_continent)), 
                                  by=list(NB, DA, seed_continent)]
    rep.to_target_continent$rep<-rrrr
    rep.richness.list[[rrrr]]<-rep.to_target_continent
    
    rep.to_target_continent_all<-item[, 
                                      .(N.to_target_continent=sum(to_target_continent),
                                        N.in_source_continent=sum(in_source_continent)), 
                                      by=list(seed_continent)]
    rep.to_target_continent_all$rep<-rrrr
    rep.richness.list.all[[rrrr]]<-rep.to_target_continent_all
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.list.all)
  
  rep.richness.df<-rbindlist(rep.richness.list)
  rep.richness.df.all<-rbindlist(rep.richness.list.all)
  rep.richness.df.all_se<-rep.richness.df.all[]
  
  extinction<-rep.df.all[type %in% c("Failed Invader", "Extinction")]
  extinction$continent<-extinction$seed_continent
  extinction$other.continent<-ifelse(extinction$seed_continent=="South America",
                                     "North America", "South America")
  extinction[type=="Failed Invader", continent:=other.continent]
  
  extinction.all<-extinction
  extinction.all$event<-"Extinction"
  extinction.all$type<-ifelse(extinction.all$seed_continent==extinction.all$continent, 
                              "Native", "Immigrant")
  extinction.all$other.continent<-NULL
  
  ggplot(extinction.all)+geom_boxplot(aes(x=continent, y=N, color=type))
  
  local.extinction<-rep.df.all[type %in% c("Local Extinction", "Non-local Extinction")]
  local.extinction$continent<-local.extinction$seed_continent
  local.extinction$other.continent<-ifelse(local.extinction$seed_continent=="South America",
                                           "North America", "South America")
  local.extinction[type=="Non-local Extinction", continent:=other.continent]
  
  local.extinction.all<-local.extinction
  local.extinction.all$event<-"Local Extinction"
  local.extinction.all$type<-ifelse(local.extinction.all$seed_continent==local.extinction.all$continent, 
                                    "Native", "Immigrant")
  local.extinction.all$other.continent<-NULL
  
  ggplot(local.extinction.all)+geom_boxplot(aes(x=continent, y=N, color=type))
  
  speciation<-rep.df.all[type %in% c("Local Speciation", "Non-local Speciation")]
  speciation$continent<-speciation$seed_continent
  speciation$other.continent<-ifelse(speciation$seed_continent=="South America",
                                     "North America", "South America")
  
  speciation[type=="Non-local Speciation", continent:=other.continent]
  speciation.all<-speciation
  speciation.all$event<-"Speciation"
  speciation.all$type<-ifelse(speciation.all$seed_continent==speciation.all$continent, 
                              "Native", "Immigrant")
  speciation.all$other.continent<-NULL
  ggplot(speciation.all)+geom_boxplot(aes(x=continent, y=N, color=type))
  
  #Dispersal by species
  dispersal<-rep.df.all[type %in% c("Secondary Invader", "Primary Invader")]
  #dispersal$type2<-dispersal$type
  dispersal$continent<-dispersal$seed_continent
  dispersal$other.continent<-ifelse(dispersal$seed_continent=="South America",
                                    "North America", "South America")
  
  dispersal[type=="Primary Invader", continent:=other.continent]
  
  dispersal.all<-dispersal
  dispersal.all$event<-"Dispersal"
  #dispersal.all$type<-ifelse(dispersal.all$seed_continent==dispersal.all$continent, 
  #                            "Native", "Immigrant")
  dispersal.all$other.continent<-NULL
  dispersal.all$disp.type<-"N to S"
  dispersal.all[seed_continent=="North America" & type=="Primary Invader", disp.type:="N to S"]
  dispersal.all[seed_continent=="North America" & type=="Secondary Invader", disp.type:="S to N"]
  dispersal.all[seed_continent=="South America" & type=="Primary Invader", disp.type:="S to N"]
  dispersal.all[seed_continent=="South America" & type=="Secondary Invader", disp.type:="N to S"]
  custom_colors <- c(
    "N to S" = color_n2s,
    "S to N" = color_s2n
  )
  dispersal.all$type<-factor(dispersal.all$type, levels=c("Primary Invader",
                                                          "Secondary Invader"),
                             labels=c("Primary invader",
                                      "Secondary invader"))
  p.disp<-ggplot(dispersal.all)+geom_boxplot(aes(x=disp.type, y=N, color=disp.type))+
    facet_wrap(~type)+
    scale_color_manual(values=custom_colors)+
    labs(y="Number of species")+
    theme_bw()+
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          strip.background = element_blank())
  p.disp
  dispersal.all.se<-dispersal.all[,.(mean=mean(N), sd=sd(N)),
                                  by=list(type, disp.type)]
  setorderv(dispersal.all.se, c("type", "disp.type"))
  
  #Dispersal by seeds
  
  p.disp.seed<-ggplot(dispersal.all[type=="Primary invader"])+
    geom_boxplot(aes(x=disp.type, y=N_Seed, color=disp.type))+
    #facet_wrap(~type)+
    scale_color_manual(values=custom_colors)+
    labs(y="Number of seeds")+
    theme_bw()+
    theme(axis.title.x = element_blank(),
          legend.position = "none")
  p.disp.seed
  dispersal.all.se<-dispersal.all[,.(mean=mean(N_Seed), sd=sd(N_Seed)),
                                  by=list(type, disp.type)]
  setorderv(dispersal.all.se, c("type", "disp.type"))
  
  p<-p.disp.seed+p.disp+plot_layout(guides = "collect", widths = c(1, 2))
  p
  
  
  item1<-rep.richness.df.all[,c("seed_continent",   "rep", "N.to_target_continent")]
  colnames(item1)[3]<-"N"
  item1$type<-"to_target_continent"
  
  item2<-rep.richness.df.all[,c("seed_continent",   "rep", "N.in_source_continent")]
  colnames(item2)[3]<-"N"
  item2$type<-"in_source_continent"
  
  item.final<-rbindlist(list(item1, item2))
  
  item.final$final.continent<-ifelse(item.final$seed_continent=="North America", "South America", "North America")
  item.final[type=="in_source_continent", final.continent:=seed_continent]
  
  richness<-item.final
  richness$continent<-richness$final.continent
  richness$type<-ifelse(richness$type=="in_source_continent", "Native", "Immigrant")
  ggplot(richness)+geom_boxplot(aes(x=continent, y=N, color=type))
  
  richness.all<-richness[, c("seed_continent", "type", "N", "rep", "continent")]
  richness.all$event<-"Richness"
  all.df<-rbindlist(list(richness.all, speciation.all, extinction.all, local.extinction.all), fill=T)
  table(all.df$type)
  table(all.df$event)
  
  all.df$event<-factor(all.df$event, 
                       levels=c("Speciation", "Extinction", "Local Extinction", "Richness"),
                       labels=c("Speciation", "Extinction", "Extirpation", "Richness"))
  all.df$type<-factor(all.df$type, levels=c("Native", "Immigrant"))
  
  p1<-ggplot(all.df[event %in% c("Speciation", "Extinction", "Extirpation")])+
    geom_boxplot(aes(x=continent, y=N, color=type))+
    facet_wrap(~event, nrow=1, scale="free")+
    labs(y="Number of events", color="Species type")+
    scale_color_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
    theme_bw()+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          strip.background = element_blank())
  p1
  p2<-ggplot(all.df[event %in% c("Richness")])+
    geom_boxplot(aes(x=continent, y=N, color=type))+
    facet_wrap(~event, nrow=1, scale="free")+
    labs(y="Number of species", color="Species type")+
    scale_color_manual(values=c("Native"=color_native, "Immigrant"=color_immigrant))+
    theme_bw()+
    theme(legend.position = "bottom",
          axis.title.x = element_blank(),
          strip.background = element_blank()
    )
  p2
  all.df.all<-all.df
  
  p<-p1+p2+plot_layout(guides = "collect", widths = c(3.5, 1)) & 
    theme(legend.position = "bottom")
  
  p
  all.df.se<-all.df[,.(mean=mean(N), sd=sd(N)),
                    by=list(event, continent, type)]
  all.df.se$outliers<-j-1
  all.df.se.list[[length(all.df.se.list)+1]]<-all.df.se
}
all.df.se.df<-rbindlist(all.df.se.list)

richness.native<-all.df.se.df[event=="Richness" & type=="Native"]
richness.immigrant<-all.df.se.df[event=="Richness" & type=="Immigrant"]
colnames(richness.native)[4]<-"mean.native"
colnames(richness.immigrant)[4]<-"mean.immigrant"
richness<-merge(richness.native, richness.immigrant, 
                by=c("event", "continent", "outliers"))
richness$per<-richness$mean.immigrant/(richness$mean.immigrant+richness$mean.native)
ggplot(richness)+
  geom_line(aes(x=outliers, y=per, color=continent))+
  geom_point(aes(x=outliers, y=per, color=continent))+
  geom_hline(yintercept = 0.5, linetype=2)
