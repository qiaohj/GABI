library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
iucn_map<-read_sf("/media/huijieqiao/Butterfly/GABI/Shape/IUCN/MAMMALS/MAMMALS_TERRESTRIAL_ONLY.shp")
sp<-data.table(species=iucn_map$binomial,
               class_=iucn_map$class,
               order_=iucn_map$order_,
               family_=iucn_map$family,
               genus_=iucn_map$genus)
sp<-unique(sp)
iucn<-readRDS("../Data/Tables/nb_range_mammals_iucn.rda")

iucn.full<-merge(iucn, sp, by="species")
iucn.full[order_==toupper("Chiroptera")]
iucn.full<-iucn.full[order_!=toupper("Chiroptera")]

unique(iucn$species)


pr_percentile<-quantile(iucn[var=="pr"]$range, 
                        c(0.2, 0.4, 0.6, 0.8))
tas_percentile<-quantile(iucn[var=="tas"]$range, 
                             c(0.2, 0.4, 0.6, 0.8))

pr_percentile_no_bat<-quantile(iucn.full[var=="pr"]$range, 
                        c(0.2, 0.4, 0.6, 0.8))
tas_percentile_no_bat<-quantile(iucn.full[var=="tas"]$range, 
                             c(0.2, 0.4, 0.6, 0.8))

vline<-data.table(v=c(ceiling(pr_percentile), ceiling(tas_percentile)),
                  v_no_bat=c(ceiling(pr_percentile_no_bat), ceiling(tas_percentile_no_bat)),
                  var=c(rep("pr", 4), rep("tas", 4)),
                  NB=rep(c("TINY", "NARROW", "MODERATE", "BROAD"), 2))
nb<-list(pr=ceiling(pr_percentile),
         t=ceiling(tasmean_percentile))

iucn<-iucn[(range<4000 & var=="pr") | var=="tas"]
iucn$var<-factor(iucn$var, levels=c("tas", "pr"), labels=c("Temperature", "Precipitation"))
vline$var<-factor(vline$var, levels=c("tas", "pr"), labels=c("Temperature", "Precipitation"))

p<-ggplot(iucn)+geom_histogram(aes(x=range, fill=var), bins = 50)+
  geom_vline(data=vline, aes(xintercept = v), linetype=2)+
  facet_wrap(~var, nrow=1, scale="free")+
  scale_fill_manual(values=c("Temperature"=color_high,
                             "Precipitation"=color_low))+
  theme_bw() +
  labs(
    title = "",
    #subtitle = "Pie area proportional to Total Population",
    fill = "Area"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank()
  )         
ggsave(p, filename="../Figures/NB/NB.pdf", width=8, height=4)
ggsave(p, filename="../Figures/NB/NB.png", width=8, height=3, bg="white")
to.doc(vline, "Niche Breadth", "../Figures/NB/NB.docx", digits = 0)


nb_full_df<-readRDS(sprintf("../Data/IUCN_NB/Mammals.%s.rda", "World"))
N<-nb_full_df[range>0,.(N_SP=length(unique(species))),
              by=list(is_na, is_sa)]
N.doc<-nb_full_df[range>0 & (is_na==T | is_sa==T)]
N.doc<-unique(N.doc)
N.doc$N_CELLS<-NULL
N.doc$min_3sd<-NULL
N.doc$max_3sd<-NULL
N.doc$range_3sd<-NULL
N.doc$continent<-NULL
colnames(N.doc)[2]<-"N_CELLS"
unique(N.doc$species)
dim(N.doc)
#N.doc[N_CELLS!=N_CELLS_ALL]
fwrite(N.doc, "../Figures/NB/IUCN.NB.csv")
