library(data.table)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
nb_df<-readRDS(sprintf("../Data/IUCN_NB/%s/Mammals.rda", "N_S_America"))
nb_df<-nb_df[N_CELLS>1]
nb_df$breadth_quantile<-nb_df$q99 - nb_df$q01
nb_df$breadth_minmax<-nb_df$max - nb_df$min
hist(nb_df$breadth_quantile)

quantiles_pr_quantile<-quantile(nb_df[var=="pr"]$breadth_quantile, c(0.25, 0.5, 0.75))
quantiles_pr_minmax<-quantile(nb_df[var=="pr"]$breadth_minmax, c(0.25, 0.5, 0.75))

tasmax<-nb_df[var=="tasmax"]
tasmin<-nb_df[var=="tasmin"]
tas<-merge(tasmax, tasmin, by=c("species"))
quantiles_tas_quantile<-quantile(tas$q99.x - tas$q01.y, c(0.25, 0.5, 0.75))
quantiles_tas_minmax<-quantile(tas$max.x - tas$min.y, c(0.25, 0.5, 0.75))

plot(tas$max.x - tas$min.y, nb_df[var=="pr"]$breadth_minmax)
nb_list<-data.table(nb=c(nb_df[var=="pr"]$breadth_minmax, tas$max.x - tas$min.y),
                    type=c(rep("pr", nrow(tas)), rep("tas", nrow(tas))))
                  
#nb_list<-data.table(nb=c(nb_df[var=="pr"]$breadth_minmax),
#                    type=c(rep("pr", nrow(nb_df))))

pr<-ceiling(quantiles_pr_minmax)
t<-ceiling(quantiles_tas_minmax)

saveRDS(list(pr=pr, t=t), "../Data/nb.rda")
lines<-data.table(v=c(pr, t), type=c(rep("pr", 3), rep("tas", 3)))
ggplot(nb_list)+
  geom_histogram(aes(x=nb), bins=50)+
  geom_vline(data=lines, aes(xintercept = v), linetype=2)+
  scale_x_log10()+
  facet_wrap(~type, nrow=1, scale="free")
