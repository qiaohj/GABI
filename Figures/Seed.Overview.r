library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
library(RSQLite)
library(DBI)
library(stringr)
library(patchwork)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")

conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
dist<-data.table(dbReadTable(conn, "distances"))
dbDisconnect(conn)

seeds<-readRDS("../Data/Tables/seeds.rda")
seeds[global_id==9745]

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

df_N_checked[N==1]

df.detail[seed_id==506 & nb=="BIG" & year>=burn_in]

df_N_checked$label<-sprintf("%d.%s", df_N_checked$seed_id, df_N_checked$nb)
df.detail$label<-sprintf("%d.%s", df.detail$seed_id, df.detail$nb)
df_filtered_seeds<-df.detail[label %in% df_N_checked[N==2]$label]
table(df_N_checked$N)

df.detail[,.(N=length(unique(seed_id))), by=list(nb)]

df.detail$label2<-sprintf("%d.%s.%s", df.detail$seed_id, df.detail$nb, df.detail$da)


all_ramdom_seeds_df<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")

no.boot.seeds.id<-df.detail[!seed_id %in% df_N_checked[N==2]$seed_id]
no.boot.seeds.id[,.(N=length(unique(seed_id))), by=list(continent)]
no.boot.seeds.id<-unique(no.boot.seeds.id$seed_id)
no.boot.seeds.id<-no.boot.seeds.id[sample(length(no.boot.seeds.id), 37)]

df_filtered_seeds[,.(N=length(unique(seed_id))), by=list(continent)]



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

N_species$seed_label<-paste(N_species$seed_id, N_species$nb, N_species$da)
outliers<-unique(N_species[(N_SPECIES>quantiles[3])])
dt.outliers<-outliers[,.(N=.N), by=list(nb, da, continent)]
dt.outliers$nb<-factor(dt.outliers$nb, 
                       levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                       labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
setorderv(dt.outliers, c("nb", "da", "continent"))

dt.outliers<-outliers[,.(N=.N), by=list(nb, da)]
dt.outliers$nb<-factor(dt.outliers$nb, 
                       levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                       labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
setorderv(dt.outliers, c("nb", "da"))

dt.outliers


no.boot.seeds.id
outliers<-outliers[, c("seed_id", "nb", "da", "continent")]
outliers$type<-"outliers"
outliers$label<-sprintf("%d.%s.%s", outliers$seed_id, outliers$nb, outliers$da)
no.boot.seeds.id<-data.table(expand.grid(seed_id=no.boot.seeds.id, 
                                         nb=unique(df.detail$nb),
                                         da=unique(df.detail$da),
                                         continent="North America"))
no.boot.seeds.id$type<-"no.survive"
no.boot.seeds.id[,.(N=.N), by=list(nb, da, continent, type)]

boot<-unique(all_ramdom_seeds_df[, c("seed_id", "nb", "da", "continent")])
boot$type<-"bootstrapping"
boot$label<-sprintf("%d.%s.%s", boot$seed_id, boot$nb, boot$da)

boot.pool<-unique(df_filtered_seeds[, c("seed_id", "nb", "da", "continent")])
boot.pool[, .(N=length(unique(seed_id))), by=list(continent)]
boot.pool$type<-"seed.pool"

all.seeds<-rbindlist(list(boot.pool, no.boot.seeds.id))
all.seeds$label<-sprintf("%d.%s.%s", all.seeds$seed_id, all.seeds$nb, all.seeds$da)
all.seeds[label %in% outliers$label, type:="outlier"]
all.seeds[label %in% boot$label, type:="bootstrapping"]
N.burn.in.failed<-all.seeds[da=="GOOD",.(N=length(unique(seed_id))), by=list(continent, type, nb)]
N.burn.in.failed<-N.burn.in.failed[type %in% c("seed.pool", "no.survive")]
N.burn.in.failed<-N.burn.in.failed[,.(N=sum(N)), by=list(continent, nb)]
setorderv(N.burn.in.failed, c("nb", "continent"))
all.seeds[type=="bootstrapping", type:="seed.pool"]
all.seeds[,.(N=length(unique(seed_id))), by=list(continent, type, nb)]
all.seeds[type %in% c("seed.pool", "no.survive"),
          .(N=length(unique(seed_id))), by=list(type, nb)]


all.seeds.unique<-all.seeds
all.seeds.unique$nb<-NULL
all.seeds.unique$da<-NULL
all.seeds.unique$label<-NULL
all.seeds.unique<-unique(all.seeds.unique)
all.seeds.unique[,.(N=length(unique(seed_id))), by=list(continent, type)]

cells<-read_sf("../Shape/isea3h8/N_S_America.shp")

all.seeds.shp<-merge(cells, all.seeds, by.x="seqnum", by.y="seed_id", all=T)
all.seeds.shp[is.na(all.seeds.shp$type), "type"]<-"Background"

all.seeds.shp$type<-factor(all.seeds.shp$type, 
                           levels=c("no.survive", "seed.pool", "outlier", "Background"),
                           labels=c("No Survive", "Seed Pool", "Outlier", "Background"))
cells$continent<-factor(cells$continent,
                        levels=c("North America", "South America", "bridge1", "bridge2"),
                        labels=c("North America", "South America", "Isthmus", "Caribbean"))
p<-ggplot()+
  geom_sf(data=cells, aes(fill=continent), color=NA, alpha=0.5)+
  geom_sf(data=all.seeds.shp[all.seeds.shp$type!="Background",], fill=color_high, color=NA)+
  geom_sf(data=all.seeds.shp[all.seeds.shp$type=="Outlier",], fill=color_high, color=NA)+
  theme_minimal()+
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE))+
  scale_fill_manual(values=c("South America"=color_sa,
                             "North America"=color_na,
                             "Isthmus"=color_2,
                             "Caribbean"=color_mid2))+
  labs(title="Seed cells")+
  theme(legend.position = c(0.05, 0.05), 
        legend.justification = c(0, 0),
        legend.title = element_blank())
p

pr_last<-pr[year==1800]
pr_last_cell<-merge(cells, pr_last, by.y="global_id", by.x="seqnum")

tasmax_last<-tasmax[year==1800]
tasmax_last_cell<-merge(cells, tasmax_last, by.y="global_id", by.x="seqnum")

tasmin_last<-tasmin[year==1800]
tasmin_last_cell<-merge(cells, tasmin_last, by.y="global_id", by.x="seqnum")


p_pr<-ggplot()+
  geom_sf(data=cells, fill=NA, color="grey90")+
  geom_sf(data=pr_last_cell, aes(fill=v), color=NA)+
  scale_fill_gradient2(low=color_low, high=color_high, mid = color_mid,
                       midpoint = mean(pr_last_cell$v))+
  theme_minimal()+
  labs(fill="PREC (mm/year)")+
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE))+
  scale_y_continuous(guide = guide_axis(check.overlap = TRUE))+
  
  theme(legend.position = "right",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  guides(
    fill = guide_colorbar(
      barwidth = unit(0.3, "cm"),
      barheight = unit(1.5, "cm"),
      label.hjust = 0.5,
      title.hjust = 0.5
    )
  )
p_pr
p_tasmax<-ggplot()+
  geom_sf(data=cells, fill=NA, color="grey90")+
  geom_sf(data=tasmax_last_cell, aes(fill=v), color=NA)+
  scale_fill_gradient2(low=color_low, high=color_high, mid = color_mid,
                       midpoint =0)+
  scale_y_continuous(guide = guide_axis(check.overlap = TRUE))+
  theme_minimal()+
  labs(fill="TMAX (°C)")+
  
  theme(legend.position = "right",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  guides(
    fill = guide_colorbar(
      barwidth = unit(0.3, "cm"),
      barheight = unit(1.5, "cm"),
      label.hjust = 0.5,
      title.hjust = 0.5
    )
  )

p_tasmin<-ggplot()+
  geom_sf(data=cells, fill=NA, color="grey90")+
  geom_sf(data=tasmin_last_cell, aes(fill=v), color=NA)+
  scale_fill_gradient2(low=color_low, high=color_high, mid = color_mid,
                       midpoint = 0)+
  scale_y_continuous(guide = guide_axis(check.overlap = TRUE))+
  theme_minimal()+
  labs(fill="TMIN (°C)")+
  
  theme(legend.position = "right",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  guides(
    fill = guide_colorbar(
      barwidth = unit(0.3, "cm"),
      barheight = unit(1.5, "cm"),
      label.hjust = 0.5,
      title.hjust = 0.5
    )
  )

p_env<-p_tasmax/p_tasmin/p_pr
p_env

p_env_curve<-readRDS("../Figures/Climate_curve/Climate_curve.rda")

p_final<-(p+p_env+ plot_layout(widths = c(3, 1)))/p_env_curve+ plot_layout(heights = c(2, 1))

p_final
ggsave(p_final, filename="../Figures/Figure.Overview/Overview.pdf", width=10, height=8)
ggsave(p_final, filename="../Figures/Figure.Overview/Overview.png", width=10, height=8, bg="white")

