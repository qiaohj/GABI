library(data.table)
library(ggplot2)
library(sf)
library(terra)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
full_seeds<-readRDS("../Data/seeds.rda")
colnames(full_seeds)[c(1,2)]<-c("original_continent", "seed_id")
random_seeds<-readRDS("../Data/Tables/100k.speciation.years/random.seeds.rda")

t<-"N_Species"
for (t in c("Loss_speciation", "Loss_extinction", 
            "Loss_local", "New_arrival", "New_speciation")){
  print(t)
  
  
  df<-readRDS(sprintf("../Data/Tables/100k.speciation.years/N_%s.rda", t))
  df$seed_id<-as.numeric(df$seed_id)
  df<-merge(df, full_seeds, by="seed_id")
  df$Label<-sprintf("%d %s %s", df$seed_id, df$nb, df$da)
  df$continent_from_to<-sprintf("%s.%s", df$original_continent, df$continent)
  table(df$continent_from_to)
  all2<-list()
  for (rrr in c(1:100)){
    print(paste(t, rrr))
    seeds<-random_seeds[rep==rrr]
    df_item<-df[Label %in% seeds$Label]
    
    
    nb_template<-data.table(expand.grid(continent_from_to=unique(df$continent_from_to),
                                        nb=unique(df[nb!="HUGE-HUGE"]$nb)))
    
    all<-list()
    for (year in c(-1800:0)){
      item<-df_item[Year<=year]
      N_item<-item[,.(N=sum(N)), by=list(continent_from_to, nb)]
      N_item<-merge(N_item, nb_template, by=c("continent_from_to", "nb"), all=T)
      N_item[is.na(N_item)]<-0
      N_item$Year<-year
      all[[length(all)+1]]<-N_item
    }
    all<-rbindlist(all)
    all$rep<-rrr
    all2[[length(all2)+1]]<-all
  }
  all2_df<-rbindlist(all2)
  saveRDS(all2_df, sprintf("../Data/Tables/100k.speciation.years/N_%s_yearly_bootstrap.rda", t))
}

all2_se<-all2_df[, .(N=mean(N), sd_N=sd(N)), by=list(continent_from_to, nb, Year)]
ggplot(all2_se)+
  geom_ribbon(aes(x=Year, ymin=N-sd_N, ymax=N+sd_N, fill=continent_from_to), alpha=0.5)+
  geom_line(aes(x=Year, y=N, color=continent_from_to))+
  facet_wrap(~nb, nrow=3, scale="free")

t<-"N_Species"
  print(t)
  
  
  df<-readRDS(sprintf("../Data/Tables/100k.speciation.years/N_%s.rda", t))
  df$seed_id<-as.numeric(df$seed_id)
  df<-merge(df, full_seeds, by="seed_id")
  df$Label<-sprintf("%d %s %s", df$seed_id, df$nb, df$da)
  df$continent_from_to<-sprintf("%s.%s", df$original_continent, df$continent)
  table(df$continent_from_to)
  all2<-list()
  for (rrr in c(1:100)){
    print(paste(t, rrr))
    seeds<-random_seeds[rep==rrr]
    df_item<-df[Label %in% seeds$Label]
    
    
    nb_template<-data.table(expand.grid(continent_from_to=unique(df$continent_from_to),
                                        nb=unique(df[nb!="HUGE-HUGE"]$nb)))
    
    all<-list()
    for (year in c(-1800:0)){
      item<-df_item[Year==year]
      N_item<-item[,.(N=sum(N)), by=list(continent_from_to, nb)]
      N_item<-merge(N_item, nb_template, by=c("continent_from_to", "nb"), all=T)
      N_item[is.na(N_item)]<-0
      N_item$Year<-year
      all[[length(all)+1]]<-N_item
    }
    all<-rbindlist(all)
    all$rep<-rrr
    all2[[length(all2)+1]]<-all
  }
  all2_df<-rbindlist(all2)
  saveRDS(all2_df, sprintf("../Data/Tables/100k.speciation.years/N_%s_yearly_bootstrap.rda", t))




ggplot(df_item[Label=="10052 MODERATE-MODERATE GOOD"])+
  geom_line(aes(x=Year, y=cumulative_N, color=continent))+
  facet_wrap(~nb, nrow=3, scale="free")
