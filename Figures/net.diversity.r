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

if (F){
  df<-readRDS("../Data/Tables/N.Extinction.rda")
  df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
  df$species.type<-"Unknown"
  df[current_continent==seed_continent & type=="Local.Extinction", species.type:="Immigrant"]
  df[current_continent!=seed_continent & type=="Local.Extinction", species.type:="Native"]
  df[type=="Extinction" & previous_continent==seed_continent, species.type:="Native"]
  df[type=="Extinction" & previous_continent!=seed_continent, species.type:="Immigrant"]
  df[type=="Extinction" & previous_continent %in% c("bridge1"), species.type:="Isthmus"]
  df[type=="Extinction" & previous_continent %in% c("bridge2"), species.type:="Caribbean"]
  
  df[species.type=="Unknown"]
  table(df$species.type)
  year_window_size<-50
  df$year_window<-floor(df$year/year_window_size)*year_window_size
  
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  
  rep.list<-list()
  rep.all.list<-list()
  rrrr=1
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-df[label %in% seeds$label]
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, NB, DA, seed_continent, type, species.type)]
    rep.list[[rrrr]]<-item.N
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, seed_continent, type, species.type)]
    rep.all.list[[rrrr]]<-item.N
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.all.list)
  
  saveRDS(rep.df, sprintf("../Data/Tables/N.Extinction.rep.window.size.%d.rda", year_window_size))
  saveRDS(rep.df.all, sprintf("../Data/Tables/N.Extinction.all.window.size.%d.rep.rda", year_window_size))
  
  
}

if (F){
  df<-readRDS("../Data/Tables/N.Speciation.rda")
  df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
  df$species.type<-"Unknown"
  df[current_continent==seed_continent, species.type:="Immigrant"]
  df[current_continent!=seed_continent, species.type:="Native"]
  table(df$species.type)
  df[species.type=="Unknown"]
  table(df$species.type)
  year_window_size<-50
  df$year_window<-floor(df$year/year_window_size)*year_window_size
  
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  
  rep.list<-list()
  rep.all.list<-list()
  rrrr=1
  for (rrrr in c(1:100)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    item<-df[label %in% seeds$label]
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, NB, DA, seed_continent, type, species.type)]
    rep.list[[rrrr]]<-item.N
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, seed_continent, type, species.type)]
    rep.all.list[[rrrr]]<-item.N
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.all.list)
  
  saveRDS(rep.df, sprintf("../Data/Tables/N.Speciation.rep.window.size.%d.rda", year_window_size))
  saveRDS(rep.df.all, sprintf("../Data/Tables/N.Speciation.all.window.size.%d.rep.rda", year_window_size))
  
  
}

year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.net.diversity.rep.window.size.%d.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.net.diversity.all.window.size.%d.rep.rda", year_window_size))

setorder(rep.df.all, seed_continent, type, rep, year_window)
rep.df.all[, net_div_rate := (N_SP-shift(N_SP)) / shift(N_SP), by = .(seed_continent, type, rep)]
rep.df.all$continent<-rep.df.all$seed_continent
rep.df.all$other_continent<-ifelse(rep.df.all$continent=="North America", "South America", "North America")
rep.df.all[type=="Immigrant", continent:=other_continent]
rep.df.all$seed_continent<-factor(rep.df.all$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))
p1<-ggplot(rep.df.all[between(year_window, -1000, -50)])+
  geom_smooth(aes(x=year_window, y=net_div_rate, color=type), 
              method = "loess", span = 0.3, se = T, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=net_div_rate, color=type)) + 
  scale_color_manual(values = c("Native" = color_low, "Immigrant" = color_high)) +
  labs(x = "Year", y = "Net Diversification Rate", 
       color = "Species Type") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2)

p2<-ggplot(rep.df.all[between(year_window, -1000, -50)])+
  geom_smooth(aes(x=year_window, y=net_div_rate, color=type), 
              method = "loess", span = 0.3, se = T, linewidth = 1) + 
  geom_point(aes(x=year_window, y=net_div_rate, color=type)) + 
  scale_color_manual(values = c("Native" = color_low, "Immigrant" = color_high)) +
  labs(x = "Year", y = "Net Diversification Rate", 
       color = "Species Type") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2)

p3<-ggplot(rep.df.all[between(year_window, -1000, -50)])+
  geom_smooth(aes(x=year_window, y=N_SP, color=type), 
              method = "loess", span = 0.3, se = T, linewidth = 1) + 
  geom_point(aes(x=year_window, y=N_SP, color=type)) + 
  scale_color_manual(values = c("Native" = color_low, "Immigrant" = color_high)) +
  labs(x = "Year", y = "Species Richness", 
       color = "Species Type") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2)
p3

ggsave(p1, filename="../Figures/NET/Net.Diversification.Rate.origin.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Net.Diversification.Rate.origin.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Diversification.Rate.continent.pdf", width=8, height=5)
ggsave(p2, filename="../Figures/NET/Net.Diversification.Rate.continent.png", width=8, height=5, bg="white")

year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.Extinction.rep.window.size.%d.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.Extinction.all.window.size.%d.rep.rda", year_window_size))

setorder(rep.df.all, seed_continent, type, species.type, rep, year_window)
rep.df.all[, net_ext_rate := (N-shift(N)) / shift(N), by = .(seed_continent, type, species.type, rep)]
table(rep.df.all$species.type)

rep.df.all[year_window==-500]
rep.df.all$species.type<-factor(rep.df.all$species.type, levels=c("Native", "Immigrant", "Isthmus", "Caribbean"))
rep.df.all$continent<-rep.df.all$seed_continent
rep.df.all$other_continent<-ifelse(rep.df.all$continent=="North America", "South America", "North America")
rep.df.all[species.type=="Immigrant", continent:=other_continent]
rep.df.all[species.type=="Native", continent:=seed_continent]
rep.df.all[species.type=="Isthmus", continent:="Isthmus"]
rep.df.all[species.type=="Caribbean", continent:="Caribbean"]
rep.df.all$seed_continent<-factor(rep.df.all$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))
p1<-ggplot(rep.df.all[between(year_window, -1000, -50) & type!="Local.Extinction"])+
  geom_smooth(aes(x=year_window, y=N, color=species.type), 
              method = "loess", span = 0.3, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_low, 
                                "Immigrant" = color_high, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Number of Extinction", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2, scale="free")


p2<-ggplot(rep.df.all[between(year_window, -1000, -50) & type!="Local.Extinction"])+
  geom_smooth(aes(x=year_window, y=N, color=species.type), 
              method = "loess", span = 0.3, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_low, 
                                "Immigrant" = color_high, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Number of Extinction", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=4, scale="free")

ggsave(p1, filename="../Figures/NET/Extinction.origin.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Extinction.origin.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Extinction.continent.pdf", width=8, height=8)
ggsave(p2, filename="../Figures/NET/Extinction.continent.png", width=8, height=8, bg="white")



year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.Speciation.rep.window.size.%d.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.Speciation.all.window.size.%d.rep.rda", year_window_size))

setorder(rep.df.all, seed_continent, type, species.type, rep, year_window)
rep.df.all[, net_ext_rate := (N-shift(N)) / shift(N), by = .(seed_continent, type, species.type, rep)]
table(rep.df.all$species.type)

rep.df.all[year_window==-500]
rep.df.all$species.type<-factor(rep.df.all$species.type, levels=c("Native", "Immigrant", "Isthmus", "Caribbean"))
rep.df.all$continent<-rep.df.all$seed_continent
rep.df.all$other_continent<-ifelse(rep.df.all$continent=="North America", "South America", "North America")
rep.df.all[species.type=="Immigrant", continent:=other_continent]
rep.df.all[species.type=="Native", continent:=seed_continent]
rep.df.all[species.type=="Isthmus", continent:="Isthmus"]
rep.df.all[species.type=="Caribbean", continent:="Caribbean"]
rep.df.all$seed_continent<-factor(rep.df.all$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))
p1<-ggplot(rep.df.all[between(year_window, -1000, -50)])+
  geom_smooth(aes(x=year_window, y=N, color=species.type), 
              method = "loess", span = 0.3, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_low, 
                                "Immigrant" = color_high, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Number of Speciation", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2, scale="free")


p2<-ggplot(rep.df.all[between(year_window, -1000, -50)])+
  geom_smooth(aes(x=year_window, y=N, color=species.type), 
              method = "loess", span = 0.3, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_low, 
                                "Immigrant" = color_high, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Number of Speciation", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=4, scale="free")

ggsave(p1, filename="../Figures/NET/Speciation.origin.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Speciation.origin.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Speciation.continent.pdf", width=8, height=5)
ggsave(p2, filename="../Figures/NET/Speciation.continent.png", width=8, height=5, bg="white")

