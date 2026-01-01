library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
seed_id<-10397
NB<-"NARROW"
DA<-"GOOD"
df<-fread(sprintf("/media/huijieqiao/Butterfly/GABI/Results/%d.%s.%s/%d.%s.%s.log",
                  seed_id, NB, DA, seed_id, NB, DA))
colnames(df)<-c("year", "global_id", "x", "xx", "xxx", "is_suitable")
df<-df[is_suitable==1]

shpfname = "../Shape/isea3h8/N_S_America.shp"
hexagon<-read_sf(shpfname)

for (y in seq(from=1604, to=0, by=-10)){
  my.name <- readline(prompt="X=exit: ")
  if (toupper(my.name)=="X"){
    break()
  }
  item<-df[year==y]
  hexagon_item<-hexagon[which(hexagon$seqnum %in% item$global_id),]
  print(ggplot(hexagon)+geom_sf()+geom_sf(data=hexagon_item, fill="red")+
    ggtitle(y))
}

conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmean<-data.table(dbReadTable(conn, "tasmean"))
dbDisconnect(conn)

conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
simulations<-data.table(dbReadTable(conn, "simulations"))
dbDisconnect(conn)

check<-simulations[global_id==seed_id & nb==NB & da==DA]
check<-df

#22.81,32.81|152.13,801.13

pr_item<-pr[global_id %in% check$global_id & year==1649]
tas_item<-tasmean[global_id %in% check$global_id & year==1649]
