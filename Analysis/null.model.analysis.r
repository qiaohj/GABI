library(data.table)
library(ggplot2)
library(sf)
library(patchwork)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")

log<-fread("/media/huijieqiao/Butterfly/GABI/Results_NULL/41655.NARROW.GOOD/41655.NARROW.GOOD.log")
colnames(log)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
table(log$suitable)
log<-log[suitable==1]
log.normal<-fread("/media/huijieqiao/Butterfly/GABI/Results/41655.NARROW.GOOD/41655.NARROW.GOOD.log")
colnames(log.normal)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")

table(log.normal$suitable)
log.normal<-log.normal[suitable==1]

log.no.sp<-fread("/media/huijieqiao/Butterfly/GABI/Results_NULL_NO_SPECIATION/41655.NARROW.GOOD/41655.NARROW.GOOD.log")
colnames(log.no.sp)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")

table(log.no.sp$suitable)
log.no.sp<-log.no.sp[suitable==1]

cells<-read_sf("../Shape/isea3h8/N_S_America.shp")

years<-unique(log$year)
y=1899
all.cells.id<-unique(log$global_id, log.normal$global_id)
cells_mask<-cells[(cells$seqnum %in% all.cells.id),]
p1<-ggplot()+geom_sf(data=cells, fill=NA, color="lightgrey") + 
  geom_sf(data=cells_mask, fill=NA, color="black")
ggsave(p1, filename="../Figures/Null.Example/41655.study.area.png", width=5, height=5)
for (y in years){
  print(y)
  item<-log[year==y]
  item<-item[,.(N_SP=length(unique(sp_id))), by=list(global_id)]
  item.sf<-merge(cells_mask, item, by.x="seqnum", by.y="global_id")
  
  item.normal<-log.normal[year==y]
  item.normal<-item.normal[,.(N_SP=length(unique(sp_id))), by=list(global_id)]
  item.normal.sf<-merge(cells_mask, item.normal, by.x="seqnum", by.y="global_id")
  
  item.no.sp<-log.no.sp[year==y]
  item.no.sp<-item.no.sp[,.(N_SP=length(unique(sp_id))), by=list(global_id)]
  item.no.sp.sf<-merge(cells_mask, item.no.sp, by.x="seqnum", by.y="global_id")
  
  
  p2<-ggplot()+
    geom_sf(data=cells_mask, fill=NA, color="lightgrey")+
    geom_sf(data=item.sf, , fill="red")+
    geom_sf_text(data = item.sf, aes(label = N_SP), size = 3, color = "white") +
    
    labs(title=sprintf("No Change @ %d", y))+
    theme_bw()+
    theme(legend.position = "none")
  p3<-ggplot()+
    geom_sf(data=cells_mask, fill=NA, color="lightgrey")+
    geom_sf(data=item.normal.sf, fill="red")+
    geom_sf_text(data = item.normal.sf, aes(label = N_SP), size = 3, color = "white") +
    labs(title=sprintf("Change @ %d", y))+
    theme_bw()+
    theme(legend.position = "none")
  
  p4<-ggplot()+
    geom_sf(data=cells_mask, fill=NA, color="lightgrey")+
    geom_sf(data=item.no.sp.sf, fill="red")+
    geom_sf_text(data = item.no.sp.sf, aes(label = N_SP), size = 3, color = "white") +
    labs(title=sprintf("No Speciation @ %d", y))+
    theme_bw()+
    theme(legend.position = "none")
  p<-p2+p3+p4
  ggsave(p, filename=sprintf("../Figures/Null.Example/By.year/%d.png", y), width=10, height=5)
  #0.376596738,0.724239343,0.976679685,0.9995805560000001,1.0
}
