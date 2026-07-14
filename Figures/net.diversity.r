library(data.table)
library(ggplot2)
library(sf)
library(lme4)
library(broom.mixed)
library(ranger)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
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
  
  N.species<-sp.full[,.(N_SP_FULL=length(unique(sp_id)),
                        N_SP = length(unique(sp_id[year_window == year]))), 
                            by=list(seed_id, year_window, 
                                    NB, DA, current_continent, seed_continent)]
  ggplot(N.species)+
    geom_point(aes(x=N_SP_FULL, N_SP, color=current_continent))+
    geom_abline()+
    coord_equal()+
    facet_grid(DA~NB)
  
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


if (F){
  df<-readRDS("../Data/Tables/N.Dispersal.rda")
  df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
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
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, NB, DA, seed_continent, type)]
    rep.list[[rrrr]]<-item.N
    item.N<-item[,.(N=sum(N), rep=rrrr), by=list(year_window, seed_continent, type)]
    rep.all.list[[rrrr]]<-item.N
  }
  rep.df<-rbindlist(rep.list)
  rep.df.all<-rbindlist(rep.all.list)
  
  saveRDS(rep.df, sprintf("../Data/Tables/N.Dispersal.rep.window.size.%d.rda", year_window_size))
  saveRDS(rep.df.all, sprintf("../Data/Tables/N.Dispersal.all.window.size.%d.rep.rda", year_window_size))
  
  
}
#Net diversification rate
year_window_size<-50
rep.df.sp<-readRDS(sprintf("../Data/Tables/N.net.diversity.rep.window.size.%d.rda", year_window_size))
rep.df.all.sp<-readRDS(sprintf("../Data/Tables/N.net.diversity.all.window.size.%d.rep.rda", year_window_size))
setorder(rep.df.all.sp, seed_continent, type, rep, year_window)
rep.df.all.sp[, net_div_rate := (N_SP-shift(N_SP)) / shift(N_SP), by = .(seed_continent, type, rep)]

rep.df.all.sp$continent<-rep.df.all.sp$seed_continent
rep.df.all.sp$other_continent<-ifelse(rep.df.all.sp$continent=="North America", "South America", "North America")
rep.df.all.sp[continent==other_continent]
rep.df.all.sp[type=="Immigrant", continent:=other_continent]
rep.df.all.sp$seed_continent<-factor(rep.df.all.sp$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))
rep.df.all.sp$type<-factor(rep.df.all.sp$type, levels=c("Native", "Immigrant"),
                        labels=c("Native", "Immigrant"))

p1<-ggplot(rep.df.all.sp[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_div_rate, color=type), 
              method = "loess", span = 0.1, se = T, linewidth = 1) + 
  geom_point(aes(x=year_window, y=net_div_rate, color=type)) + 
  scale_color_manual(values = c("Native" = color_native, "Immigrant" = color_immigrant)) +
  labs(x = "Year", y = "Net Diversification Rate", 
       color = "Species Type") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2)
p1
p2<-ggplot(rep.df.all.sp[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_div_rate, color=type), 
              method = "loess", span = 0.1, se = T, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=net_div_rate, color=type)) + 
  scale_color_manual(values = c("Native" = color_native, "Immigrant" = color_immigrant)) +
  labs(x = "Year", y = "Net Diversification Rate", 
       color = "Species Type") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2)
p2
p3<-ggplot(rep.df.all.sp[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=N_SP, color=type), 
              method = "loess", span = 0.1, se = T, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N_SP, color=type)) + 
  scale_color_manual(values = c("Native" = color_native, "Immigrant" = color_immigrant)) +
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

ggsave(p3, filename="../Figures/NET/N.Species.continent.pdf", width=8, height=5)
ggsave(p3, filename="../Figures/NET/N.Species.continent.png", width=8, height=5, bg="white")


#Extinction
year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.Extinction.rep.window.size.%d.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.Extinction.all.window.size.%d.rep.rda", year_window_size))
colnames(rep.df.all.sp)[3]<-"species.type"


setorder(rep.df.all, seed_continent, type, species.type, rep, year_window)
table(rep.df.all$species.type)

rep.df.all[year_window==-500]
rep.df.all$species.type<-factor(rep.df.all$species.type, 
                                levels=c("Native", "Immigrant", "Isthmus", "Caribbean"))
rep.df.all$continent<-rep.df.all$seed_continent
rep.df.all$other_continent<-ifelse(rep.df.all$continent=="North America", "South America", "North America")
rep.df.all[species.type=="Immigrant", continent:=other_continent]
rep.df.all[species.type=="Native", continent:=seed_continent]
rep.df.all[species.type=="Isthmus", continent:="Isthmus"]
rep.df.all[species.type=="Caribbean", continent:="Caribbean"]

rep.df.all$seed_continent<-factor(rep.df.all$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))
table(rep.df.all$type)
all.comb<-data.table(expand.grid(year_window=unique(rep.df.all$year_window),
                     seed_continent=unique(rep.df.all$seed_continent),
                     species.type=unique(rep.df.all$species.type),
                     rep=unique(rep.df.all$rep),
                     continent=unique(rep.df.all$continent),
                     other_continent=unique(rep.df.all$other_continent),
                     type=unique(rep.df.all$type), stringsAsFactors = F))
rep.df.all<-merge(rep.df.all, all.comb, by=c("year_window", "seed_continent", "species.type", 
                                                "rep", "continent", "other_continent", "type"), all=T)
rep.df.all[is.na(type)]
rep.df.all<-merge(rep.df.all, rep.df.all.sp, by=c("year_window", "seed_continent", "species.type", 
                                                  "rep", "continent", "other_continent"), all=T)
rep.df.all[is.na(N), N:=0]
rep.df.all<-rep.df.all[!is.na(N_SP)]

rep.df.all$net_extinction<-rep.df.all$N/rep.df.all$N_SP
rep.df.all[species.type %in% c("Native", "Immigrant") & continent %in% c("North America", "South America")]
rep.df.all.extinction<-rep.df.all[species.type %in% c("Native", "Immigrant") & 
                                    continent %in% c("North America", "South America") &
                                    !is.na(type)]

p1<-ggplot(rep.df.all[between(year_window, -1800, -50) & type!="Local.Extinction"])+
  geom_smooth(aes(x=year_window, y=N, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
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

p1

rep.df.all.extinction$type<-factor(rep.df.all.extinction$type, levels=c("Extinction", "Local.Extinction"),
                        labels=c("Extinction", "Local Extinction"))
p2<-ggplot(rep.df.all.extinction[between(year_window, -1800, -50) & 
                                   continent %in% c("North America", "South America")])+
  geom_smooth(aes(x=year_window, y=net_extinction, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Extinction / Species", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_grid(type~continent, scale="free")
p2

ggsave(p1, filename="../Figures/NET/Extinction.origin.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Extinction.origin.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Extinction.continent.pdf", width=8, height=6)
ggsave(p2, filename="../Figures/NET/Net.Extinction.continent.png", width=8, height=6, bg="white")


#Speciation
year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.Speciation.rep.window.size.%d.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.Speciation.all.window.size.%d.rep.rda", year_window_size))

setorder(rep.df.all, seed_continent, type, species.type, rep, year_window)
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

all.comb<-data.table(expand.grid(year_window=unique(rep.df.all$year_window),
                                 seed_continent=unique(rep.df.all$seed_continent),
                                 species.type=unique(rep.df.all$species.type),
                                 rep=unique(rep.df.all$rep),
                                 continent=unique(rep.df.all$continent),
                                 other_continent=unique(rep.df.all$other_continent),
                                 type=unique(rep.df.all$type)))

rep.df.all<-merge(rep.df.all, all.comb, by=c("year_window", "seed_continent", "species.type", 
                                             "rep", "continent", "other_continent", "type"), all=T)

rep.df.all<-merge(rep.df.all, rep.df.all.sp, by=c("year_window", "seed_continent", "species.type", "rep", "continent", "other_continent"), all.x=T)
rep.df.all.sp[year_window==-1850]
rep.df.all[is.na(N), N:=0]
rep.df.all<-rep.df.all[!is.na(N_SP)]


rep.df.all$net_speciation<-rep.df.all$N/rep.df.all$N_SP
rep.df.all.speciation<-rep.df.all
p1<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=N, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
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


p2<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_speciation, color=species.type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values = c("Native" = color_native, 
                                "Immigrant" = color_immigrant, 
                                "Isthmus" = color_mid2,
                                "Caribbean" = color_2)) +
  labs(x = "Year", y = "Speciation / Species", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2, scale="free")
p2


ggsave(p1, filename="../Figures/NET/Speciation.origin.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Speciation.origin.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Speciation.continent.pdf", width=8, height=5)
ggsave(p2, filename="../Figures/NET/Net.Speciation.continent.png", width=8, height=5, bg="white")

#Dispersal

year_window_size<-50
rep.df<-readRDS(sprintf("../Data/Tables/N.Dispersal.rep.window.size.%d.rda", year_window_size))
rep.df.all<-readRDS(sprintf("../Data/Tables/N.Dispersal.all.window.size.%d.rep.rda", year_window_size))
setorder(rep.df.all, seed_continent, type, rep, year_window)

rep.df.all[year_window==-500]
rep.df.all$continent<-rep.df.all$seed_continent
rep.df.all$other_continent<-ifelse(rep.df.all$continent=="North America", "South America", "North America")
rep.df.all$seed_continent<-factor(rep.df.all$seed_continent, levels=c("North America", "South America"),
                                  labels=c("North American Origin", "South American Origin"))
rep.df.all[,.(N=.N), by=list(seed_continent, type)]

all.comb<-data.table(expand.grid(year_window=unique(rep.df.all$year_window),
                                 seed_continent=unique(rep.df.all$seed_continent),
                                 #species.type=unique(rep.df.all$species.type),
                                 rep=unique(rep.df.all$rep),
                                 continent=unique(rep.df.all$continent),
                                 other_continent=unique(rep.df.all$other_continent),
                                 type=unique(rep.df.all$type)))

rep.df.all<-merge(rep.df.all, all.comb, by=c("year_window", "seed_continent", 
                                             "rep", "continent", "other_continent", "type"), all=T)

rep.df.all<-merge(rep.df.all, rep.df.all.sp, by=c("year_window", "seed_continent", 
                                                  "rep", "continent", "other_continent"), all.x=T)

rep.df.all.sp[year_window==-1850]
rep.df.all[is.na(N), N:=0]
rep.df.all<-rep.df.all[!is.na(N_SP)]


rep.df.all$net_dispersal<-rep.df.all$N/rep.df.all$N_SP

#rep.df.all$disp.type<-"N to S"
rep.df.all[,.(N=.N), by=list(seed_continent, type)]
#rep.df.all[seed_continent=="North American Origin" & type=="Primary Invader", disp.type:="N to S"]
#rep.df.all[seed_continent=="North American Origin" & type=="Secondary Invader", disp.type:="S to N"]
#rep.df.all[seed_continent=="South American Origin" & type=="Primary Invader", disp.type:="S to N"]
#rep.df.all[seed_continent=="South American Origin" & type=="Secondary Invader", disp.type:="N to S"]
table(rep.df.all$disp.type)
custom_colors <- c(
  "Primary Invader" = color_n2s,
  "Secondary Invader" = color_s2n
)
rep.df.all.dispersal<-rep.df.all
p1<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=N, color=type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values=custom_colors) +
  labs(x = "Year", y = "Number of Dispersal", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~seed_continent, nrow=2, scale="free")

p1

p2<-ggplot(rep.df.all[between(year_window, -1800, -50)])+
  geom_smooth(aes(x=year_window, y=net_dispersal, color=type), 
              method = "loess", span = 0.1, se = FALSE, linewidth = 1) + 
  #geom_point(aes(x=year_window, y=N, color=species.type)) + 
  scale_color_manual(values=custom_colors) +
  labs(x = "Year", y = "Immigrant percentage", 
       color = "Species Type") +
  scale_y_sqrt()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold")
  )+facet_wrap(~continent, nrow=2, scale="free")
p2


ggsave(p1, filename="../Figures/NET/Dispersal.origin.pdf", width=8, height=5)
ggsave(p1, filename="../Figures/NET/Dispersal.origin.png", width=8, height=5, bg="white")

ggsave(p2, filename="../Figures/NET/Net.Dispersal.continent.pdf", width=8, height=5)
ggsave(p2, filename="../Figures/NET/Net.Dispersal.continent.png", width=8, height=5, bg="white")

#GLM

rep.df.all.extinction
rep.df.all.speciation
rep.df.all.dispersal

rep.df.all.extinction[year_window==0 & seed_continent=="South American Origin" & species.type=="Immigrant" & rep==98]

rep.df.all.extinction_extinction<-rep.df.all.extinction[type=="Extinction"]
colnames(rep.df.all.extinction_extinction)[c(8, 11)]<-c("N_Extinction", "net_extinction")
rep.df.all.extinction_local_extinction<-rep.df.all.extinction[type=="Local Extinction"]
colnames(rep.df.all.extinction_local_extinction)[c(8, 11)]<-c("N_Local_Extinction", "net_local_extinction")

rep.df.all.extinction_local_extinction[year_window<0,
                                       .(N=sum(N_Local_Extinction)), 
                                       by=list(year_window, seed_continent, species.type, continent, other_continent)]
df_extinction<-merge(rep.df.all.extinction_extinction, rep.df.all.extinction_local_extinction, 
                     by=c("year_window", "seed_continent", "species.type", "rep", "continent", "other_continent", "N_SP", "net_div_rate"), all=T)

df1<-merge(df_extinction, rep.df.all.speciation, 
           by=c("year_window", "seed_continent", "species.type", "rep", "continent", "other_continent", "N_SP", "net_div_rate"),
           all=T)
df1$type.x<-NULL
df1$type.y<-NULL
colnames(df1)[c(9, 11)]<-c("N_Extinction", "N_Speciation")

rep.df.all.dispersal.primary<-rep.df.all.dispersal[type=="Primary Invader"]
colnames(rep.df.all.dispersal.primary)[c(7, 11)]<-c("N_Primary_Invader", "net_primary_invader")
rep.df.all.dispersal.secondary<-rep.df.all.dispersal[type=="Secondary Invader"]
colnames(rep.df.all.dispersal.secondary)[c(7, 11)]<-c("N_Secondary_Invader", "net_secondary_invader")
df_dispersal<-merge(rep.df.all.dispersal.secondary, rep.df.all.dispersal.primary, 
                    by=c("year_window", "seed_continent", "species.type", "rep", "continent", "other_continent", "N_SP", "net_div_rate"),
                    all=T)
df_dispersal$type.x<-NULL
df_dispersal$type.y<-NULL
df_final<-merge(df1, df_dispersal,
                by=c("year_window", "seed_continent", "species.type", "rep", "continent", "other_continent", "N_SP", "net_div_rate"),
                all=T)

df_final<-df_final[!is.na(net_secondary_invader )]
df_final_se<-df_final[between(year_window, -1500, -50), 
                      .(net_div_rate=mean(net_div_rate),
                          net_extinction=mean(net_extinction),
                          net_local_extinction=mean(net_local_extinction),
                          net_speciation=mean(net_speciation),
                          net_secondary_invader=mean(net_secondary_invader),
                          net_primary_invader=mean(net_primary_invader)),
                      by=list(year_window,seed_continent, species.type, continent)]
model<-glm(data=df_final_se, net_div_rate~net_extinction+net_local_extinction+net_speciation+net_secondary_invader+net_primary_invader)
summary(model)


model.df<-df_final[between(year_window, -1500, -50), 
                   c("year_window", "seed_continent", "species.type", "rep",
                     "continent", "net_div_rate", "net_extinction", "net_local_extinction",
                     "net_speciation", "net_secondary_invader", "net_primary_invader")]


# Fit a multiple linear regression for each continent
ggplot(model.df)+
  geom_point(aes(x=net_div_rate, y=net_extinction, color=seed_continent))+
  facet_grid(continent~year_window, scale="free")
lm_coefs <- model.df[, {
  fit <- lm(net_div_rate ~ net_extinction + 
              net_local_extinction + net_speciation + net_secondary_invader + net_primary_invader)
  coef_summary <- summary(fit)$coefficients
  
  list(
    term = rownames(coef_summary),
    estimate = coef_summary[, "Estimate"],
    std_error = coef_summary[, "Std. Error"],
    p_value = coef_summary[, "Pr(>|t|)"]
  )
}, by = continent]

significant_factors <- lm_coefs[p_value < 0.05 & term != "(Intercept)"]

ggplot(significant_factors, aes(x = estimate, y = term, color = estimate > 0)) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std_error, 
                     xmax = estimate + 1.96 * std_error), 
                 height = 0.2, linewidth = 0.8) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ continent, scales = "free_y") +
  scale_color_manual(values = c("TRUE" = "#2c7bb6", "FALSE" = "#d7191c"),
                     labels = c("TRUE" = "Positive (+)", "FALSE" = "Negative (-)"),
                     name = "Effect Direction") +
  theme_bw(base_size = 14) +
  labs(title = "Significant Drivers of Net Diversification Rate",
       subtitle = "Coefficient estimates with 95% confidence intervals",
       x = "Coefficient Estimate",
       y = "") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f0f0f0"))

