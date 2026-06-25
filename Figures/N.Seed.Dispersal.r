library(data.table)
library(ggplot2)
library(sf)
library(stringr)
setDTthreads(30)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  sp.with.bridge<-readRDS("../Data/Tables/sp_full_continents.rda")
  seeds<-sp.with.bridge[,.(N=.N), by=list(NB, DA, seed_id)]
  final<-list()
  for (i in c(1:nrow(seeds))){
    print(paste(i, nrow(seeds)))
    item<-seeds[i]
    sp.items<-sp.with.bridge[NB==item$NB & DA==item$DA & seed_id==item$seed_id]
    seed_continent<-unique(sp.items[year== -1899]$seed_continent)
    target_continent<-ifelse(seed_continent=="South America", "North America", "South America")
    target_continent<-c(target_continent, "Two continents")
    target_item<-sp.items[current_continent %in% target_continent]
    to_target_continent<-(nrow(target_item)>0)
    if (to_target_continent==T){
      to_year<-min(target_item$year)
    }else{
      to_year<-Inf
    }
    final_continent<-target_item[year==0]
    to_target_continent_final<-(nrow(final_continent)>0)
    item$seed_continent<-seed_continent
    item$to_target_continent<-to_target_continent
    item$to_target_continent_final<-to_target_continent_final
    item$to_year<-to_year
    final[[i]]<-item
  }
  final.df<-rbindlist(final)
  saveRDS(final.df, "../Data/Tables/N.with.bridge.simulation.rda")
}
source("Figures/common.r")
df<-readRDS("../Data/Tables/N.with.bridge.simulation.rda")
seeds<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")



df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
table(df$seed_continent)

cell.dist<-readRDS("../Data/Tables/cells.with.dist.rda")
if (F){
  ggplot(cell.dist)+geom_sf()
}

unique.seeds<-unique(seeds[, c("continent", "seed_id")])
unique.seeds<-merge(cell.dist, unique.seeds, by.x="seqnum",
                    by.y="seed_id")


df_map<-merge(cell.dist, df, by.x="seqnum",
              by.y="seed_id")

df_map$to_target_continent_final_label<-ifelse(df_map$to_target_continent_final,
                                               "Yes", "No")
df_map$to_target_continent_final_label<-factor(df_map$to_target_continent_final_label, 
                                               levels = c("Yes", "No"), 
                                               labels = c("Yes", "No"))


df_map$NB<- factor(df_map$NB, 
                   levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                   labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
df_map[is.na(df_map$DA),]

p<-ggplot()+
  geom_sf(data=cell.dist, fill=NA, color="lightgrey", alpha=0.3)+
  geom_sf(data=unique.seeds, 
          aes(fill = "Extinct during burn-in"),
           alpha=1, color=NA)+
  geom_sf(data=df_map, 
          aes(fill=to_target_continent_final_label), alpha=1, color=NA)+
  scale_fill_manual(values = c("Yes" = color_high, 
             "No" = color_low, 
             "Extinct during burn-in" = color_mid2))+
  labs(fill="Dispersal to the other continent")+
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  facet_grid(DA~NB)+
  theme_bw() +
  theme(
    legend.position = "bottom",
    #panel.grid = element_blank(),
    #axis.text = element_blank(),  
    axis.title = element_blank()
  )

p
ggsave(p, filename="../Figures/Seed.Dispersal/Seed.Dispersal.Map.pdf", width=10, height=5)
ggsave(p, filename="../Figures/Seed.Dispersal/Seed.Dispersal.Map.png", width=10, height=5, bg="white")

seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
seeds.all[seed_id=="5412"]

rep.list<-list()
rep.all.list<-list()

for (rrrr in c(1:100)){
  print(rrrr)
  seeds<-seeds.all[rep==rrrr]
  item<-df[label %in% seeds$label]
  rep.to_target_continent<-item[to_target_continent==T, .(N.to_target_continent=.N), 
                                by=list(NB, DA, seed_continent)]
  rep.to_target_continent_final<-item[to_target_continent_final==T, .(N.to_target_continent_final=.N), 
                                      by=list(NB, DA, seed_continent)]
  rep<-merge(rep.to_target_continent_final, 
             rep.to_target_continent, by=c("NB", "DA", "seed_continent"), all=T)
  rep$rep<-rrrr
  rep.list[[rrrr]]<-rep
  
  rep.to_target_continent<-item[to_target_continent==T, .(N.to_target_continent=.N), 
                                by=list(seed_continent)]
  rep.to_target_continent_final<-item[to_target_continent_final==T, .(N.to_target_continent_final=.N), 
                                      by=list(seed_continent)]
  rep<-merge(rep.to_target_continent_final, 
             rep.to_target_continent, by=c("seed_continent"), all=T)
  rep$rep<-rrrr
  rep.all.list[[rrrr]]<-rep
}
rep.df.seed<-rbindlist(rep.list)

rep.df.all.seed<-rbindlist(rep.all.list)
rep.df.seed<-rbindlist(list(rep.df.seed,
                            data.table(expand.grid(NB="NARROW", DA=c("GOOD", "POOR"),
                                                   seed_continent =c("North America", "South America"),
                                                   N.to_target_continent_final=0,
                                                   N.to_target_continent=0,
                                                   rep=c(1:100))),
                            data.table(expand.grid(NB="MODERATE", DA=c("POOR"),
                                                   seed_continent =c("North America", "South America"),
                                                   N.to_target_continent_final=0,
                                                   N.to_target_continent=0,
                                                   rep=c(1:100))),
                            data.table(expand.grid(NB="BIG", DA=c("POOR"),
                                                   seed_continent =c("North America"),
                                                   N.to_target_continent_final=0,
                                                   N.to_target_continent=0,
                                                   rep=c(1:100)))))
saveRDS(rep.df.seed, "../Data/Tables/N.Seed.Dispersal.rep.rda")
saveRDS(rep.df.all.seed, "../Data/Tables/N.Seed.Dispersal.all.rep.rda")
rep.df.seed$label<-sprintf("%s.%s", rep.df.seed$NB, rep.df.seed$DA)
rep.df.seed
custom_colors <- c(
  "N to S" = color_high,
  "S to N" = color_low
)

rep.df.seed$type<-ifelse(rep.df.seed$seed_continent=="North America", "N to S", "S to N")
rep.df.seed$NB<-factor(rep.df.seed$NB, 
                     levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                     labels = c("BROAD", "MODERATE", "NARROW", "TINY"))

p1<-ggplot(rep.df.seed, 
       aes(x=type, y=N.to_target_continent_final))+
  labs(y="Number of seeds to the other continent")+
  #scale_color_manual(values=custom_colors)+
  #geom_point()+
  geom_boxplot()+
  facet_grid(DA~NB, scale="free")+
  theme_bw()+
  theme(axis.title.x = element_blank())
p1
ggsave(p1, filename="../Figures/Seed.Dispersal/Seed.Dispersal.boxplot.pdf", width=10, height=5)
ggsave(p1, filename="../Figures/Seed.Dispersal/Seed.Dispersal.boxplot.png", width=10, height=5, bg="white")

fwrite(rep.df.seed, "../Figures/Seed.Dispersal/Seed.Dispersal.boxplot.csv")
rep.df.all.seed$type<-ifelse(rep.df.all.seed$seed_continent=="North America", "N to S", "S to N")

p2<-ggplot(rep.df.all.seed, 
          aes(x=type, y=N.to_target_continent_final))+
  labs(y="Number of seeds to the other continent")+
  geom_boxplot()+
  theme_bw()+
  theme(axis.title.x = element_blank())
p2
ggsave(p2, filename="../Figures/Seed.Dispersal/Seed.Dispersal.boxplot.all.pdf", width=5, height=4)
ggsave(p2, filename="../Figures/Seed.Dispersal/Seed.Dispersal.boxplot.all.png", width=5, height=4, bg="white")

p2+p1
summary_dt<-rep.df.seed[, .(mean=mean(N.to_target_continent_final),
               sd=sd(N.to_target_continent_final)),
            by=list(type, NB, DA)]

colnames(summary_dt)<-c("Type", "Niche Breadth", "Dispersal Ability", "Mean", "SD")
summary_dt$Value<-sprintf("%.2f±%.2f", summary_dt$Mean, summary_dt$SD)
summary_dt$Mean<-NULL
summary_dt$SD<-NULL
setorderv(summary_dt, c("Niche Breadth", "Dispersal Ability", "Type"))
to.doc(summary_dt, 
       "Mean seeds to the other continent", 
       "../Figures/Seed.Dispersal/seed.2.other.continent.docx")
