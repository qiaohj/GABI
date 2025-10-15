library(data.table)
library(ggplot2)
library(sf)
library(terra)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")

t<-"Loss_extinction"
figure_df<-list()
for (t in c("Loss_extinction", "N_Species",
            "Loss_local", "New_arrival", "New_speciation")){
  print(t)
  df<-readRDS(sprintf("../Data/Tables/100k.speciation.years/N_%s_yearly_bootstrap.rda", t))
  all2_se<-df[, .(N=mean(N), sd_N=sd(N)), by=list(continent_from_to, nb, Year)]
  all2_se$type<-t
  figure_df[[length(figure_df)+1]]<-all2_se
}
figure_df<-rbindlist(figure_df)
p<-ggplot(figure_df)+
  geom_ribbon(aes(x=Year, ymin=N-sd_N, ymax=N+sd_N, fill=continent_from_to), alpha=0.5)+
  geom_line(aes(x=Year, y=N, color=continent_from_to))+
  facet_grid(type~nb, scale="free")+
  xlim(-1200, 0)+
  theme_bw()
ggsave(p, filename="../Figures/overall.png", width=20, height=10)
