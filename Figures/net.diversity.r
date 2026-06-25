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
  year_window_size<-50
  sp.full$year_window<-floor(sp.full$year/year_window_size)*year_window_size
  N.species<-sp.full[,.(N_SP=length(unique(sp_id))), 
                            by=list(seed_id, year_window, 
                                    NB, DA, current_continent, seed_continent)]
  
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
    item.N<-item[,.(N_SP=sum(N_SP), rep=rrrr), by=list(year_window, NB, DA, seed_continent, type)]
    rep.list[[rrrr]]<-item.N
    item.N<-item[,.(N_SP=sum(N_SP), rep=rrrr), by=list(year_window, seed_continent, type)]
    rep.all.list[[rrrr]]<-item.N
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.all.list)
  
  saveRDS(rep.df, sprintf("../Data/Tables/N.net.diversity.rep.window.size.%d.rda", year_window_size))
  saveRDS(rep.df.all, sprintf("../Data/Tables/N.net.diversity.all.window.size.%d.rep.rda", year_window_size))
  
  
  dt_rep2 <- item
  setorder(dt_rep2, seed_continent, type, year)
  
  dt_rep2[, diff := N_SP - shift(N_SP), by = .(seed_continent, type)]
  sudden_drop_row <- dt_rep2[which.min(diff)]
  print(sudden_drop_row)
  
}
year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.net.diversity.rep.window.size.%d.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.net.diversity.all.window.size.%d.rep.rda", year_window_size))

setorder(rep.df.all, seed_continent, type, rep, year_window)
rep.df.all[, net_div_rate := (N_SP-shift(N_SP)) / shift(N_SP), by = .(seed_continent, type, rep)]

ggplot(rep.df.all[year_window>=-1000])+
  geom_smooth(aes(x=year_window, y=net_div_rate, color=type, linetype=seed_continent), 
              method = "loess", span = 0.2, se = FALSE, linewidth = 1.2) + 
  scale_color_manual(values = c("Native" = "#2166AC", "Immigrant" = "#B2182B")) +
  labs(x = "Year", y = "Net Diversification Rate", 
       color = "Type", linetype = "Original continent") +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.title = element_text(face = "bold")
  )


rep.df.all.mean<-rep.df.all[,.(net_div_rate=mean(net_div_rate, na.rm=T),
                               sd=sd(net_div_rate, na.rm=T)),
                            by=list(year_window, seed_continent, type)]

ggplot(rep.df.all.mean)+
  geom_line(aes(x=year_window, y=net_div_rate, color=type, linetype=seed_continent))
