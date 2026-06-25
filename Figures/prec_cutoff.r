library(data.table)
library(ggplot2)
library(sf)
library(stringr)
library(RSQLite)
library(DBI)
setDTthreads(30)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  cell.dist<-readRDS("../Data/Tables/cells.with.dist.rda")
  base_db<-"../Configuration/conf.null.sqlite"
  mydb <- dbConnect(RSQLite::SQLite(), base_db)
  simulations<-dbReadTable(mydb, "simulations")
  dbDisconnect(mydb)
  simulations<-data.table(simulations)
  nrow(simulations)
  seeds<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  
  simulations.dt<-unique(data.table(seed_id=simulations$global_id, NB=simulations$nb,
                             nb_v=simulations$nb_v))
  new_cols<-c("tasmin_low", "tasmin_high",
              "tasmax_low", "tasmax_high",
              "pr_low", "pr_high")
  simulations.dt[, (new_cols) := tstrsplit(simulations.dt$nb_v, split = "[,|]", type.convert = TRUE)]
  simulations.dt[,.(N=.N), by=list(NB)]
  
  simulations.dt<-merge(cell.dist, simulations.dt, by.y="seed_id", by.x="seqnum")
  simulations.dt$NB<-factor(simulations.dt$NB, 
                       levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                       labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
  
  cutoff_sim<-simulations.dt[which(simulations.dt$pr_low<0),]
  
  simulated.seeds<-cutoff_sim[which(cutoff_sim$seqnum %in% seeds$seed_id),]
  
  p<-ggplot()+
    geom_sf(data=cell.dist, fill=NA, color="lightgrey", alpha=0.3)+
    geom_sf(data=cutoff_sim, aes(fill=abs(pr_low)), color=NA)+
    geom_sf(data=simulated.seeds, fill=NA, color=color_low, alpha=0.5)+
    facet_wrap(~NB, nrow=1)+
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    scale_fill_gradient(
      low = "lightgrey",
      high = color_high,
      na.value = "transparent",
      name = "Prec cutoff"
    ) + theme_bw() +
    theme(
      legend.position = "bottom",
      #panel.grid = element_blank(),
      #axis.text = element_blank(),  
      axis.title = element_blank()
    )
  
  ggsave(p, filename="../Figures/Prec_Cutoff/Prec_Cutoff.pdf", width=10, height=3)
  ggsave(p, filename="../Figures/Prec_Cutoff/Prec_Cutoff.png", width=10, height=3, bg="white")
  
  
  
}