library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
df<-fread("/media/huijieqiao/WD22T_11/GABI/Results/39578/39578_268_GOOD/39578_268_GOOD.log")
colnames(df)<-c("year", "glpbal_id", "x", "xx", "xxx", "is_suitable")
df<-df[is_suitable==1]

shpfname = "../Data/Shape/isea3h8/N_S_America.shp"
hexagon<-read_sf(shpfname)

for (y in c(1799:0)){
  my.name <- readline(prompt="X=exit: ")
  if (toupper(my.name)=="X"){
    break()
  }
  item<-df[year==y]
  hexagon_item<-hexagon[which(hexagon$seqnum %in% item$glpbal_id),]
  print(ggplot(hexagon)+geom_sf()+geom_sf(data=hexagon_item, fill="red")+
    ggtitle(y))
}
