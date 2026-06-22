library(data.table)
library(ggplot2)
library(sf)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
if (F){
  sp.with.bridge<-readRDS("../Data/Tables/sp_full_continents.rda")
  sp.with.bridge[sp_id=="10050" & NB=="BIG" & DA=="GOOD"]
  
  sp.with.bridge.NA<-sp.with.bridge[current_continent %in% c("North America", "Two continents")]
  sp.with.bridge.NA$current_continent<-"North America"
  sp.with.bridge.SA<-sp.with.bridge[current_continent %in% c("South America", "Two continents")]
  sp.with.bridge.SA$current_continent<-"South America"
  sp.full<-rbindlist(list(sp.with.bridge.NA, sp.with.bridge.SA))
  N.species<-sp.full[,.(N_SP=length(unique(sp_id))), 
                            by=list(seed_id, year, NB, DA, current_continent, seed_continent)]
  
  N.species$label<-sprintf("%d.%s.%s", N.species$seed_id, N.species$NB, N.species$DA)
  N.species$type<-ifelse(N.species$current_continent==N.species$seed_continent, "Native", "Immigrant")
  table(N.species$current_continent)
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  
  rep.list<-list()
  rep.all.list<-list()
  
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-N.species[label %in% seeds$label]
    item.N<-item[,.(N_SP=sum(N_SP), rep=rrrr), by=list(year, NB, DA, seed_continent, type)]
    rep.list[[rrrr]]<-item.N
    item.N<-item[,.(N_SP=sum(N_SP), rep=rrrr), by=list(year, seed_continent, type)]
    rep.all.list[[rrrr]]<-item.N
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.all.list)
  
  saveRDS(rep.df, "../Data/Tables/N.net.diversity.rep.rda")
  saveRDS(rep.df.all, "../Data/Tables/N.net.diversity.all.rep.rda")
  
  
  dt_rep2 <- item
  setorder(dt_rep2, seed_continent, type, year)
  
  # 2. 计算每一年相较于上一年的差值 (N_SP变化量)
  dt_rep2[, diff := N_SP - shift(N_SP), by = .(seed_continent, type)]
  
  # 3. 找出差值最小的那一行（也就是跌幅最深、最异常的点）
  sudden_drop_row <- dt_rep2[which.min(diff)]
  
  # 打印结果，你可以看到出问题的具体年份和跌幅 (diff)
  print(sudden_drop_row)
  
}

rep.df<-readRDS("../Data/Tables/N.net.diversity.rep.rda")
rep.df.all<-readRDS("../Data/Tables/N.net.diversity.all.rep.rda")

setorder(rep.df.all, seed_continent, type, rep, year)
rep.df.all[, net_div_rate := (N_SP-shift(N_SP)) / shift(N_SP), by = .(seed_continent, type, rep)]
rep.df.all[net_div_rate<=-0.04 & year>-500]

rep.df.all.mean<-rep.df.all[,.(net_div_rate=mean(net_div_rate, na.rm=T),
                               sd=sd(net_div_rate, na.rm=T)),
                            by=list(year, seed_continent, type)]

ggplot(rep.df.all.mean[year!=-124])+
  geom_line(aes(x=year, y=net_div_rate, color=type, linetype=seed_continent))
