library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")

seeds<-readRDS("../Data/seeds.rda")

df<-readRDS("../Data/Tables/100k.speciation.years/N.Speciation.Extinction.rda")
nrow(df)
df$continent<-NULL
df<-merge(df, seeds, by.x="seed_id", by.y="global_id")

df<-df[between(lat, -22, 50)]
range(df$lat)
#burn in 3100/2
burn_in<-3100/2
df_after<-df[year<burn_in]



df_N<-df[, .(N_SPECIES=sum(N_SPECIES), 
             N_SPECIATION=sum(N_SPECIATION),
             N_EXTINCTION=sum(N_EXTINCTION),
             N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
             N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
             N_ALL_SPECIES=sum(N_ALL_SPECIES),
             N_SEED=length(unique(seed_id))),
         by=list(continent, year)]

df_N[year==1799]$N_ALL_SPECIES<-df_N[year==1799]$N_SEED


df_N_checked<-df[year==burn_in+1 & N_SPECIES>0, 
                 .(N=.N), by=list(seed_id)]

df_filtered_seeds<-df[seed_id %in% df_N_checked[N==8]$seed_id]

df_filtered_N<-df_filtered_seeds[, .(N_SPECIES=sum(N_SPECIES), 
             N_SPECIATION=sum(N_SPECIATION),
             N_EXTINCTION=sum(N_EXTINCTION),
             N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
             N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
             N_ALL_SPECIES=sum(N_ALL_SPECIES),
             N_SEED=length(unique(seed_id))),
         by=list(continent, year)]

table(df_N_checked$N)

#df_N$label<-paste(df_N$nb, df_N$da)

ggplot(df_filtered_N)+
  geom_line(aes(y=N_SPECIES, x=year * -1, color=continent))+
  geom_vline(xintercept = burn_in * -1, linetype=2)+
  geom_text(data=df_N[year==1799], 
            aes(x=-1900, y=c(2e5, 2.2e5), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)+
  geom_text(data=df_filtered_N[year==burn_in+1], 
            aes(x=burn_in * -1 + 10, y=c(2e5, 2.2e5), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)

#random seeds
seed_pool<-unique(df_filtered_seeds[, c("seed_id", "continent")])
ramdom_seeds<-seed_pool[,.SD[sample(.N, 1000)],by = "continent"]
table(ramdom_seeds$continent)

#
vs<-readRDS("../Data/Tables/100k.speciation.years/virtual.species.rda")

vs_filter<-vs[seed_id %in% ramdom_seeds$seed_id & nb!="HUGE-HUGE"]
seeds_t<-seeds
colnames(seeds_t)[1]<-"original_continent"
colnames(seeds_t)[2]<-"seed_id"
seeds_t$seed_id<-as.character(seeds_t$seed_id)
vs_filter<-vs_filter[year<burn_in]
vs_filter_with_original<-merge(vs_filter, seeds_t, by="seed_id")
vs_filter_with_original$same_origin<-vs_filter_with_original$continent==vs_filter_with_original$original_continent
table(vs_filter_with_original$continent)
vs_filter_with_original_no_bridge<-vs_filter_with_original[
  continent %in% c("North America", "South America")]
table(vs_filter_with_original_no_bridge$continent)
nrow(vs_filter_with_original_no_bridge)

vs_filter_with_original_no_bridge$label<-paste(vs_filter_with_original_no_bridge$sp_id,
                                               vs_filter_with_original_no_bridge$nb, 
                                               vs_filter_with_original_no_bridge$da)
vs_filter_with_original_no_bridge$seed_label<-paste(vs_filter_with_original_no_bridge$seed_id,
                                               vs_filter_with_original_no_bridge$nb, 
                                               vs_filter_with_original_no_bridge$da)
vs_N<-vs_filter_with_original_no_bridge[, .(N=length(label)), 
                                        by=list(original_continent, same_origin, year, nb, da)]


ggplot(vs_N[year<=1400])+geom_line(aes(x=year * -2, y=N, color=original_continent, linetype=same_origin))+
  facet_grid2(da~nb, scale="free_y", independent="y")

#remove outliers

N_species<-df[year==0 & N_SPECIES>0]
quantile(N_species$N_SPECIES, c(0, 1, 0.99, 0.95, 0.90))
table(N_species[N_SPECIES>100]$continent)
N_species$seed_label<-paste(N_species$seed_id, N_species$nb, N_species$da)
vs_filter_with_original_no_bridge_no_outliers<-
  vs_filter_with_original_no_bridge[
    !(seed_label %in% N_species[N_SPECIES>100]$seed_label)]

vs_N_no_outlier<-vs_filter_with_original_no_bridge_no_outliers[, .(N=length(label)), 
                                        by=list(original_continent, same_origin, year, nb, da)]


ggplot(vs_N_no_outlier[year<=1400])+
  geom_line(aes(x=year * -2, y=N, color=original_continent, linetype=same_origin))+
  facet_grid2(da~nb, scale="free_y", independent="y")

df_N[year==1799]


ggplot(df_filtered_seeds[year==0 & N_SPECIES>1])+
  geom_histogram(aes(x=N_SPECIES))+
  scale_x_log10()

ggplot(df_filtered_seeds[year==0 & N_SPECIES>1])+
  geom_point(aes(x=N_SPECIES, y=N_ALL_SPECIES))

scores<-df_filtered_seeds[year==0 & N_SPECIES>1]$N_SPECIES
q3 <- quantile(log(scores, base=10), 0.75)
iqr <- IQR(log(scores, base=10))
upper_bound <- q3 + 1.5*iqr
threshold_iqr<-min(scores[log(scores, base=10)>=upper_bound])

sd<-sd(log(scores, base=10))
upper_bound<-mean(log(scores, base=10)+3 * sd)
threshold_sd<-min(scores[log(scores, base=10)>upper_bound])

df_filtered_outliers<-df_filtered_seeds[year==0 & N_SPECIES>1]
seeds_outliers<-unique(df_filtered_outliers[N_SPECIES>threshold_sd]$seed_id)
df_filtered_outliers<-df_filtered_seeds[!(seed_id %in% df_filtered_outliers$seed_id)]
range(df_filtered_outliers$N_SPECIES)

table(seeds_N$N)
seeds_N[N==5]
df_filtered_outliers[seed_id==10710]
