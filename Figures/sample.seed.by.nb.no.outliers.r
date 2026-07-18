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
N_species[N_SPECIES==max(N_species$N_SPECIES)]
N_species$seed_label<-paste(N_species$seed_id, N_species$nb, N_species$da)
no.outliers<-df_filtered_seeds[(year==0 )]


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

saveRDS(all_ramdom_seeds_df, "../Data/Tables/random.seeds.threshold.by.nb.distance.no.outliers.rda")

all_ramdom_seeds_df<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.no.outliers.rda")

check<-N_species[N_SPECIES>7000]
check$seed_label<-sprintf("%d.%s.%s", check$seed_id, check$nb, check$da)

N<-all_ramdom_seeds_df[label %in% check$seed_label]
N[,(N=.N), by=list(label)]

seed_pool[seed_id==9652 & nb=="MODERATE"]

xxx<-all_ramdom_seeds_df[label %in% 
                      seed_pool[min.dist==34  & 
                                  nb=="MODERATE" & da=="GOOD"]$label2]
xxx$rep<-NULL
unique(xxx)
seeds<-copy(all_ramdom_seeds_df)
seeds$nb <- factor(
  seeds$nb,
  levels = c("BROAD", "BIG", "MODERATE", "NARROW"),
  labels = c("BROAD", "MODERATE", "NARROW", "TINY")
)
seeds$label_nb<-sprintf("%d.%s", seeds$seed_id, seeds$nb)
N <- seeds[, .(N_Seeds = length(unique(label)),
               N=.N), by = list(rep, nb, continent)]
setorderv(N, c("rep", "nb", "continent"))

N

N_ALL <- seeds[,
               .(N_ALL_Seeds = length(unique(label))),
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

to.doc(N_se, "Proportion of seed usages", "../Figures/Seeds/propotion.of.seed.usage.no.outliers.docx", digits = 2)


N_times<-seeds[, .(N=.N), by=c("nb", "da", "label", "continent")]
N_times[N==100]
length(unique(N_times[N==100]$label     ))

table(N_times[N==200]$continent)

N_200<-N_times[N==100]
N_200_ST<-N_200[,.(N_Seed=length(unique(label))), by=list(nb, da, continent)]
setorderv(N_200_ST, c("nb", "da", "continent"))
N_200_ST
sum(N_200_ST$N_Seed)
