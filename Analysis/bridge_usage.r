library(data.table)
library(ggplot2)
library(sf)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")

df<-readRDS("../Data/Tables/100.100.rda")

df$label<-sprintf("%s_%s_%s", df$global_id, df$sp_id, df$da)

labels<-unique(df$label)
for (i in c(1:length(labels))){
  l<-labels[i]
  item<-df[label==l]
  # Status
  #1 Extinct immediately
  #2 Extinct
  #3 Extant
  
}