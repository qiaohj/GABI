library(data.table)
library(ggplot2)
library(sf)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")
if (F){
  sp.with.bridge<-readRDS("../Data/Tables/sp_full_continents.rda")
  sp.with.bridge.new<-sp.with.bridge
  sp.with.bridge.new[current_continent=="bridge1", current_continent:="North America"]
  sp.with.bridge.new[previous_continent=="bridge1", previous_continent:="North America"]
  sp.with.bridge.new[parent_continent=="bridge1", parent_continent:="North America"]
  
  
  sp.with.bridge.new$N<-NULL
  sp.with.bridge.new$gain.continent<-NULL
  sp.with.bridge.new$loss.continent<-NULL
  sp.with.bridge.new$south.america<-NULL
  sp.with.bridge.new$north.america<-NULL
  sp.with.bridge.new$origin_continent<-NULL
  sp.with.bridge.new2<-sp.with.bridge.new[, c("year", "NB", "DA", "seed_id", "seed_continent",
                                              "current_continent", "previous_continent", "species.label",
                                              "type")]
  sp.with.bridge.new<-unique(sp.with.bridge.new)
  sp.with.bridge.new2<-unique(sp.with.bridge.new2)
}