library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
library(dplyr)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
seeds<-readRDS("../Data/Tables/seeds.rda")
seeds[global_id==9745]
#conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
#simulations<-data.table(dbReadTable(conn, "simulations"))
#dbDisconnect(conn)
#target.seeds<-simulations[continent_id<=100]$global_id
#seeds<-seeds[global_id %in% target.seeds]
#ggplot(seeds)+geom_sf(data=seed.dist)+geom_point(aes(x=lon, y=lat, color=continent))

seed.dist<-readRDS("../Data/Tables/cells.with.dist.rda")
seed.dist[which(seed.dist$seqnum==9745),]
df<-readRDS("../Data/Tables/N.Speciation.Extinction.All.NB.rda")
df[seed_id==9745]

table(df$nb)
df[,(N=length(unique(seed_id))), by="continent"]
nrow(df)
df$continent<-NULL
df.detail<-merge(df, seeds, by.x="seed_id", by.y="global_id")
df.detail[,.(N=length(unique(seed_id))), by=list(continent)]
df[!seed_id %in% df.detail$seed_id]

#df<-df[between(lat, -35, 45)]
range(df.detail$lat)

burn_in<-3200/2
unique(df.detail$nb)

df_N_checked<-df.detail[year==burn_in & N_SPECIES>0, 
                        .(N=.N), by=list(seed_id, nb)]

df_N_checked$label<-sprintf("%d.%s", df_N_checked$seed_id, df_N_checked$nb)
df.detail$label<-sprintf("%d.%s", df.detail$seed_id, df.detail$nb)
df_filtered_seeds<-df.detail[label %in% df_N_checked[N==2]$label]
table(df_N_checked$N)

df.detail[,.(N=length(unique(seed_id))), by=list(nb)]

df.detail$label2<-sprintf("%d.%s.%s", df.detail$seed_id, df.detail$nb, df.detail$da)

df_filtered_N<-df_filtered_seeds[, .(N_SPECIES=sum(N_SPECIES), 
                                     N_SPECIATION=sum(N_SPECIATION),
                                     N_EXTINCTION=sum(N_EXTINCTION),
                                     N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                                     N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                                     N_ALL_SPECIES=sum(N_ALL_SPECIES),
                                     N_SEED=length(unique(seed_id))),
                                 by=list(continent, year)]

df_filtered_N_detals<-df_filtered_seeds[, .(N_SPECIES=sum(N_SPECIES), 
                                            N_SPECIATION=sum(N_SPECIATION),
                                            N_EXTINCTION=sum(N_EXTINCTION),
                                            N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                                            N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                                            N_ALL_SPECIES=sum(N_ALL_SPECIES),
                                            N_SEED=length(unique(seed_id))),
                                        by=list(continent, year, nb, da)]
df_filtered_N_detals[year==burn_in+1, 
                     c("N_SEED", "continent", "nb", "da")]
df_filtered_N[year==burn_in]

table(df_N_checked$N)


#define outliers
N_species<-df_filtered_seeds[year==0 & N_SPECIES>0]
N_species.all<-df_filtered_seeds[year==0]
df_filtered_seeds[,.(N=length(unique(seed_id))), by="continent"]


length(unique(df.detail[! seed_id %in% df_filtered_seeds[continent=="South America"]$seed_id 
                        & continent=="South America"]$seed_id))

table(seeds$continent)

quantiles<-quantile(N_species$N_SPECIES, c(0, 1, 0.99, 0.98, 0.95, 0.90, 0.999))
names(quantiles)<-NULL
p<-ggplot(N_species.all)+
  geom_histogram(aes(x=N_SPECIES), binwidth=1)+
  geom_vline(aes(xintercept=quantiles[3]), linetype=2)+
  scale_x_sqrt(breaks=c(100, 1000, ceiling(quantiles[3]), 2500, 5000, 7500, 10000))+
  scale_y_sqrt()+
  theme_minimal() +
  labs(
    x = "Number of species",
    y = "Number of simulations"
  ) +
  theme(
    panel.grid = element_blank()
  )       
p
ggsave(p, filename="../Figures/Outliers/Outlier.distribution.pdf", width=6, height=3)
ggsave(p, filename="../Figures/Outliers/Outlier.distribution.png", width=6, height=3, bg="white")

N_species$seed_label<-paste(N_species$seed_id, N_species$nb, N_species$da)
outliers<-unique(N_species[(N_SPECIES>quantiles[3])])
dt.outliers<-outliers[,.(N=.N), by=list(nb, da, continent)]
dt.outliers$nb<-factor(dt.outliers$nb, 
                     levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                     labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
setorderv(dt.outliers, c("nb", "da", "continent"))

dt.outliers<-outliers[,.(N=.N), by=list(nb, da, continent)]
dt.outliers$nb<-factor(dt.outliers$nb, 
                       levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                       labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
setorderv(dt.outliers, c("nb", "da"))

dt.outliers

to.doc(dt.outliers, "Number of outliers per combination", "../Figures/Outliers/outliers.docx",
       digits = 0)
range(N_species$N_SPECIES)
ggplot(outliers)+geom_histogram(aes(x=N_SPECIES), bins=100)+
  facet_grid(nb~continent)

table(outliers$continent)
N_species_filter<-df_filtered_seeds[!seed_id %in% outliers$seed_id]
df_filtered_N_filter<-N_species_filter[, .(N_SPECIES=sum(N_SPECIES), 
                                           N_SPECIATION=sum(N_SPECIATION),
                                           N_EXTINCTION=sum(N_EXTINCTION),
                                           N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                                           N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                                           N_ALL_SPECIES=sum(N_ALL_SPECIES),
                                           N_SEED=length(unique(seed_id))),
                                       by=list(continent, year)]

ggplot(df_filtered_N_filter)+
  geom_line(aes(y=N_SPECIES, x=year * -1, color=continent))+
  geom_vline(xintercept = burn_in * -1, linetype=2)+
  geom_text(data=df_filtered_N[year==1504], 
            aes(x=-1100, y=c(2e3, 6e4), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)+
  geom_text(data=df_filtered_N[year==burn_in+1], 
            aes(x=burn_in * -1 + 10, y=c(1.7e3, 5.5e4), 
                label=paste(continent, N_SEED, sep=": ")),
            hjust = 0)


outliers_ID<-unique(outliers$seed_id)
unique(N_species$nb)
#random seeds
no.outliers<-df_filtered_seeds[(year==0 & !(seed_id %in% outliers_ID))]


seed_pool<-no.outliers[, .(N_SPECIES=sum(N_SPECIES)), 
                       by=list(seed_id, continent, nb, da, label)]
seed_pool[,.(N=.N), by=list(nb, da, continent)]
seed_pool$label2<-sprintf("%s.%s", seed_pool$label, seed_pool$da)
seed_pool$weight<-1

if (F){
  seed_pool.map<-merge(seed.dist, seed_pool, by.y="seed_id", by.x="seqnum")
  seed_pool[, .(N=.N), by=list(nb, continent)]
  ggplot(seed.dist)+
    geom_sf(fill="lightgrey", color=NA)+
    geom_sf(data=seed_pool.map, aes(fill=continent.x))+
    facet_wrap(~nb)+
    map_theme
  
  ggplot(seed.dist)+
    geom_sf(fill="lightgrey", color=NA)+
    geom_sf(data=seed_pool.map)+
    facet_grid(da~nb)+
    map_theme
}
df_filtered_seeds[, .(N=length(unique(seed_id))), by=list(nb, da, continent)]



seed_pool<-merge(seed_pool, seed.dist[, c("seqnum", "min.dist")], 
                 by.x="seed_id", by.y="seqnum")
if (F){
  ggplot(seed_pool)+geom_histogram(aes(x=min.dist), bins=10)+
    facet_grid(continent~nb)
}
n<-seed_pool[, .(N=.N), by=list(continent, min.dist, nb, da)]
n
setorderv(n, "min.dist")

seed_pool[,.(N=.N), by=list(nb, da, continent)]   
ggplot(seed.dist[which(seed.dist$continent %in% c("South America", "North America")),])+
  geom_histogram(aes(x=min.dist, fill=continent), binwidth = 1, position = "identity", 
                 alpha = 0.5, color="white")+
  geom_vline(xintercept = 70, linetype=2)+
  theme_bw()
seed_pool.df<-unique(seed_pool[, c("continent", "seed_id", "min.dist")])
ggplot(seed_pool.df)+geom_histogram(aes(x=min.dist), binwidth = 1)+
  facet_wrap(~continent)

coms<-unique(n[, c("nb", "da", "min.dist")])
bins<-list()
max_seeds<-5
for (i in c(1:nrow(coms))){
  com<-coms[i]
  item<-n[nb==com$nb & da==com$da & min.dist==com$min.dist]
  if (nrow(item)!=2){
    print(item)
    print("skip")
    next()
  }
  min.N<-min(item$N, max_seeds)
  bins[[length(bins)+1]]<-data.table(min.dist=com$min.dist,
                                     nb=com$nb,
                                     da=com$da,
                                     N=min.N)
}
bins<-rbindlist(bins)
table(bins$N)
all_ramdom_seeds<-list()
set.seed(1024)
for (rep in c(1:100)){
  print(rep)
  seed_pool.rand<-list()
  for (i in c(1:nrow(bins))){
    bin<-bins[i]
    if (nrow(bin[nb=="BROAD" & da=="POOR" & min.dist==35])==1){
      #asdf
    }
    seed.item<-seed_pool[nb==bin$nb & da==bin$da & min.dist==bin$min.dist]
    seed.item<-seed.item[,.SD[sample(.N, bin$N, prob=weight)],by = "continent"]
    seed_pool.rand[[length(seed_pool.rand)+1]]<-seed.item
  }
  seed_pool.rand<-rbindlist(seed_pool.rand)
  if (F){
    ggplot(seed_pool.rand)+geom_histogram(aes(x=min.dist, fill=continent), binwidth = 1)+
      facet_grid(nb~continent)
    
    seed_pool.rand[,.(N=.N), by=c("continent", "nb", "da")]
    ggplot(seed_pool.rand)+geom_boxplot(aes(x=continent, y=N_SPECIES))+
      facet_grid(nb~da)
    seed_pool.rand[,.(N=sum(N_SPECIES)), by=list(nb, continent)]
  }
  seed_pool.rand[, .(N=.N), by=c("continent", "nb", "da")]
  #ramdom_seeds<-seed_pool.rand[,.SD[sample(.N, 100)],
  #                             by = c("continent", "nb", "da")]
  
  ramdom_seeds<-seed_pool.rand
  if (F){
    ggplot(ramdom_seeds)+geom_histogram(aes(x=min.dist, fill=continent))+
      facet_grid(nb~continent)
  } 
  
  if (F){
    ramdom_seeds$geometry<-NULL
    ggplot(ramdom_seeds)+geom_histogram(aes(x=min.dist, fill=continent), binwidth = 2)+
      facet_wrap(~continent)
    ramdom_seeds.map<-merge(seed.dist, ramdom_seeds, by.x="seqnum", by.y="seed_id")
    ggplot(ramdom_seeds.map)+
      geom_sf(data=seed.dist, fill="lightgrey", color=NA)+geom_sf(aes(fill=continent.x))+
      facet_wrap(~nb)
  }
  ramdom_seeds$geometry<-NULL
  Ncheck<-ramdom_seeds[,.(N=length(unique(seed_id))), by=list(continent, nb, da)]
  print(Ncheck[nb=="NARROW" & da=="POOR"])
  all_seeds<-ramdom_seeds
  all_seeds$rep<-rep
  all_seeds$label<-sprintf("%d.%s.%s", all_seeds$seed_id, all_seeds$nb, all_seeds$da)
  #all_seeds<-all_seeds[!label %in% outliers]
  all_ramdom_seeds[[rep]]<-all_seeds
}

all_ramdom_seeds_df<-rbindlist(all_ramdom_seeds)


unique(all_ramdom_seeds_df[, .(N=.N), by=list(continent, rep, nb, da)]$N)

#saveRDS(all_ramdom_seeds_df, "../Data/Tables/random.seeds.threshold.by.nb.distance.rda")

all_ramdom_seeds_df<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
seeds<-copy(all_ramdom_seeds_df)
seeds$nb <- factor(
  seeds$nb,
  levels = c("BROAD", "BIG", "MODERATE", "NARROW"),
  labels = c("BROAD", "MODERATE", "NARROW", "TINY")
)

N <- seeds[, .(N_Seeds = length(unique(seed_id))), by = list(rep, nb, continent)]

N_ALL <- seeds[,
  .(N_ALL_Seeds = length(unique(seed_id))),
  by = list(nb, continent)
]
N<-merge(N, N_ALL, by=c("nb", "continent"))


N$Per<-N$N_Seeds/N$N_ALL_Seeds
N_se <- N[,
  .(
    N_Seeds = mean(N_Seeds),
    sd_N_Seeds = sd(N_Seeds),
    N_ALL_Seeds = mean(N_ALL_Seeds),
    sd_N_ALL_Seeds = sd(N_ALL_Seeds),
    Per = mean(Per)*100,
    sd_Per = sd(Per)*100
  ),
  by = list(nb, continent)
]
setorderv(N_se, "nb")

to.doc(N_se, "Proportion of seed usages", "../Figures/Seeds/propotion.of.seed.usage.docx", digits = 2)


N_times<-seeds[, .(N=.N), by=c("nb", "seed_id", "continent")]
N_times[N==200]
length(unique(N_times[N==200]$seed_id))

table(N_times[N==200]$continent)
