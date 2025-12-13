library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)
library(ape)
library(phangorn)
library(phytools)
library(geiger)
library(stringr)
library(tidyverse)
#library(plotKML)
#library(ggtree)

setwd("/media/huijieqiao/Butterfly/GABI/GABI")
#source("commons/functions.r")
setDTthreads(1)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))

base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)
simulations<-data.table(simulations)
#simulations<-simulations[continent_id<=100]
base_db<-"../Configuration/configuration.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
tasmin<-dbReadTable(mydb, "tasmin")
tasmax<-dbReadTable(mydb, "tasmax")
pr<-dbReadTable(mydb, "pr")
dbDisconnect(mydb)
pr<-data.table(pr)
tasmin<-data.table(tasmin)
tasmax<-data.table(tasmax)


table(simulations$species_evo_type)

table(simulations[, c("nb", "da")])

simulations<-simulations[nb %in% c("BIG-BIG", "MODERATE-MODERATE", "NARROW-NARROW")]
template<-"%d.%s.%s"
all_df<-simulations
all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]
#item<-all_df[nb=="BROAD"&da=="GOOD"&global_id==1361&species_evo_type==2&directional_speed==0.1]
#item<-all_df[nb=="BROAD"&da=="GOOD"&global_id==1567&species_evo_type==2&directional_speed==0.1]
i=1
all_df<-all_df[which(all_df$species_evo_level==0),]
table(all_df$species_evo_type)
table(all_df$species_evo_level)

#all_df<-all_df[label=="9979.MODERATE-MODERATE.GOOD"]
for (i in c(1:nrow(all_df))){
  
  
  item<-all_df[i,]
  
  sp<-sprintf(template, item$global_id, item$nb, item$da)
  print(paste(i, nrow(all_df), sp))
  
  
  base<-"/media/huijieqiao/Butterfly/GABI/Results"
  #base<-"/media/huijieqiao/Butterfly/GABI/Results.NULL"
  
  ttt<-sprintf("%s/%s/%s.why.speciation.rda", base, sp, sp)
  
  if (file.exists(ttt)){
    print("skip")
    next()
    size<-file.size(ttt)
    if (size>100){
      
      #print(sprintf("rm -rf %s", ttt))
      #next()
    }
    print("redo")
    #next()
  }
  
  
  log<-sprintf("%s/%s/%s.sqlite", base, sp, sp)
  
  #saveRDS(NULL, ttt)
  
  mydb <- dbConnect(RSQLite::SQLite(), log)
  trees<-dbReadTable(mydb, "trees")
  dbDisconnect(mydb)
  if (nrow(trees)==0){
    text.string<-sprintf("SP%d @ 1799-0:1799;", item$global_id)
  }else{
    text.string<-trees[1,2]
    text.string<-gsub("\\]", "#", gsub("\\[", "#", text.string))
    if (!grepl("\\(", text.string)){
      text.string<-sprintf("(a:0)%s", text.string)
    }
  }
  vert.tree<-read.tree(text=text.string)
  #plot(vert.tree)
  #nodelabels()
  nodes<-data.table(label=c(vert.tree$node.label, vert.tree$tip.label),
                    type=c(rep("node", length(vert.tree$node.label)),
                           rep("leaf", length(vert.tree$tip.label)))
  )
  nodes<-nodes[label!="a"]
  if (nrow(nodes)==1){
    nodes$type<-"leaf"
  }
  nodes[, c("PX","PY") := data.table(str_split_fixed(label,"@",2))]
  nodes[, c("from","to") := data.table(str_split_fixed(PY,"-",2))]
  nodes$from<-as.numeric(nodes$from)
  nodes$to<-as.numeric(nodes$to)
  
  print(log)
  nodes[is.na(to)]$to<-nodes[is.na(to)]$from
  nodes$event<-"SPECIATION"
  nodes[(type=="leaf")&(to==0), "event"]<-"NONE"
  nodes[(type=="leaf")&(to!=0), "event"]<-"EXTINCTION"
  if (nrow(nodes[event=="SPECIATION"])>=10){
    asdf
    next()
  }
  
  #saveRDS(event_df, ttt)
}
