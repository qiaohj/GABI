library(data.table)
library(flextable)
library(officer)
library(ggplot2)
setDTthreads(30)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")

if (F){
  sp.with.bridge<-readRDS("../Data/Tables/sp_full_continents.rda")
  seeds<-sp.with.bridge[,.(N=.N), by=list(NB, DA, seed_id)]
  final<-list()
  for (i in c(1:nrow(seeds))){
    print(paste(i, nrow(seeds)))
    item<-seeds[i]
    sp.items<-sp.with.bridge[NB==item$NB & DA==item$DA & seed_id==item$seed_id]
    seed_continent<-sp.items[year== -1800]$seed_continent
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

df<-readRDS("../Data/Tables/N.with.bridge.simulation.rda")
df<-df[NB %in% c("BIG-BIG", "MODERATE-MODERATE")]
df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
table(df$seed_continent)

seeds.all<-readRDS("../Data/Tables/random.seeds.rda")
seeds.all[seed_id=="5412"]

rep.list<-list()
for (rrrr in c(1:10)){
  print(rrrr)
  seeds<-seeds.all[rep==rrrr]
  item<-df[label %in% seeds$label]
  rep.to_target_continent<-item[to_target_continent==T, .(N.to_target_continent=.N), 
                                by=list(NB, DA, seed_continent)]
  rep.to_target_continent_final<-item[to_target_continent_final==T, .(N.to_target_continent_final=.N), 
                                      by=list(NB, DA, seed_continent)]
  rep<-merge(rep.to_target_continent_final, rep.to_target_continent, by=c("NB", "DA", "seed_continent"), all=T)
  rep$rep<-rrrr
  rep.list[[rrrr]]<-rep
}
rep.df.seed<-rbindlist(rep.list)
rep.df.seed$NB.label<-factor(rep.df.seed$NB, 
                        levels=c("BIG-BIG", "MODERATE-MODERATE"),
                        labels=c("BROAD", "NARROW"))

saveRDS(rep.df.seed, "../Data/Tables/N.Seed.Dispersal.rep.rda")
rep.df.seed$label<-sprintf("%s.%s", rep.df.seed$NB.label, rep.df.seed$DA)
custom_colors <- c(
  "North America" = "#B2182B",
  "South America" = "#2166AC"
)

p<-ggplot(rep.df.seed, 
       aes(x=label, y=N.to_target_continent_final, color=seed_continent))+
  labs(color="Original continent", y="Number of seeds to the other continent")+
  scale_color_manual(values=custom_colors)+
  #geom_point()+
  geom_boxplot()+
  theme_bw()+
  theme(axis.title.x = element_blank())
p


summary_dt<-rep.df.seed[, .(mean=mean(N.to_target_continent_final),
               sd=sd(N.to_target_continent_final)),
            by=list(seed_continent, NB.label, DA)]

colnames(summary_dt)<-c("Original continent", "Niche Breadth", "Dispersal Ability", "Mean", "SD")
summary_dt$Value<-sprintf("%.2f±%.2f", summary_dt$Mean, summary_dt$SD)
summary_dt$Mean<-NULL
summary_dt$SD<-NULL
to.doc(summary_dt, 
       "Mean seeds to the other continent", 
       "../Table.Doc/seed.2.other.continent.docx")
to.doc<-function(summary_dt, title, output_file){
  ft_booktabs <- flextable(summary_dt) %>%
    theme_booktabs() %>%
    autofit() %>%
    set_caption(caption = title)
  
  doc <- read_docx()
  
  
  doc <- doc %>%
    body_add_flextable(value = ft_booktabs)
  
  print(doc, target = output_file)
  
  cat(paste("Saved the document to", output_file, "\n"))
}
