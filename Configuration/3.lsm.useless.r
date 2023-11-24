library(terra)
library(ncdf4)
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/continental_movement/Script")
if (F){
  continents<-read_sf("../Data/Shape/continents/continent.shp")
  america<-continents[which(continents$id %in% c(2, 5)),]
  st_crs(america)<-st_crs(4326)
  all_age<-readRDS("../Data/meta_table.rda")
  variables<-unique(all_age[group=="variable", c("label", "name", "folder", "file", "age")])
  all_age[name=="lsm" & label=="tEVT0"]
  lsm<-variables[name=="lsm" & grepl("qrparm.mask.nc", file)]
  setorder(lsm, "age")
  pdf("../Figures/lsm/lsm.pdf", onefile = TRUE)
  lands<-list()
  for (i in c(1:nrow(lsm))){
    print(i)
    item1<-lsm[i]
    nc1<-rast(sprintf("%s/%s", item1$folder, item1$file))
    nc1<-nc1[[grepl("lsm", names(nc1))]]
    nc1<-rotate(nc1)
    df<-data.table(as.data.frame(nc1, xy=T))
    colnames(df)[3]<-"v"
    df<-df[v==1]
    df$age<-item1$age
    lands[[i]]<-df
    p<-ggplot(df)+geom_tile(aes(x=x, y=y), fill="lightgrey")+
      geom_sf(data=america, fill=NA, color="red")+
      coord_sf()+ggtitle(item1$age)+
      theme_bw()
    print(p)
    
  }
  dev.off()
  
  lands_df<-rbindlist(lands)
  
  lands_se<-lands_df[, .(N=.N), by=list(x, y)]
  lands_se$land<-ifelse(lands_se$N==919, 1, 0)
  p<-ggplot(lands_se)+geom_tile(aes(x=x, y=y, fill=factor(land)))+
    geom_sf(data=america, fill=NA, color="red")+
    coord_sf()+
    theme_bw()
  ggsave(p, filename="../Figures/lsm/lsm_overview.pdf")
  
  saveRDS(lands_df, "../Data/LSM/lsm.rda")
  
}
if (F){
  lsm<-readRDS("../Data/LSM/lsm.rda")
  tasmin<-rast("../Data/Raster/Rough.3.75x2.5/tempmonmin_abs.tif")
  mask<-tasmin[[1]]
  values(mask)<-c(1:length(values(mask)))
  index<-extract(mask, lsm[, c("x", "y")])$y0
  lsm$index<-index
  
  table(lsm$v)
  all_xy<-unique(lsm[, c("x", "y", "index")])
  ages<-seq(0, 3600, by=4)
  all_xys<-list()
  for (age in ages){
    all_xy_item<-all_xy
    all_xy_item$age<-age
    all_xys[[length(all_xys)+1]]<-all_xy_item
  }
  all_xys<-rbindlist(all_xys)
  
  ele<-rast("../Data/Raster/wc2.1_10m_elev.tif")
  if (F){
    ele<-rast("../Data/Raster/wc2.1_10m_elev.tif")
    fs<-list.files("../Data/Raster/gebco_2023_sub_ice_topo_geotiff/", pattern="\\.tif")
    all_ele<-list()
    for (f in fs){
      print(f)
      r<-rast(sprintf("../Data/Raster/gebco_2023_sub_ice_topo_geotiff/%s", f))
      r_resampled_r<-resample(r, ele)
      r_resampled<-as.data.table(r_resampled_r, xy=T)
      colnames(r_resampled)[3]<-"v"
      all_ele[[length(all_ele)+1]]<-r_resampled
    }
    all_ele<-rbindlist(all_ele)
    all_ele$index<-extract(mask, all_ele[, c("x", "y")])$y0
    
    all_ele<-data.table(all_ele)
    all_ele<-all_ele[!is.na(index)]
    ele_se<-all_ele[, .(min_ele=min(v), 
                        max_ele=max(v),
                        mean_ele=mean(v),
                        q99=quantile(v, 0.99),
                        q95=quantile(v, 0.95),
                        q05=quantile(v, 0.05),
                        q01=quantile(v, 0.01),
                        q75=quantile(v, 0.75),
                        q25=quantile(v, 0.25),
                        q50=quantile(v, 0.50)),
                    by=list(index)]
    saveRDS(ele_se, "../Data/LSM/ele_se.rda")
  }
  ele_se<-readRDS("../Data/LSM/ele_se.rda")
  lsm_ele<-merge(all_xys, ele_se, by=c("index"))
  y<-names(tasmin)[1]
  
  
  lsm_ele$tasmin<- -9999
  lsm_ele$tasmin_mean_global<- -9999
  lsm_ele$tasmin_mean_land<- -9999
  ages<-unique(lsm_ele$age)
  for (a in ages){
    print(a)
    year<-sprintf("y%d", a)
    r<-tasmin[[year]]
    lsm_ele[age==a]$tasmin<-extract(r, lsm_ele[age==a, c("x", "y")], ID=F)[,1]
  }
  if (F){
    xy<-unique(lsm_ele[, c("x", "y", "mean_ele")])
    ggplot(xy)+geom_tile(aes(x=x, y=y, fill=mean_ele))
  }
  lsm_ele_se<-lsm_ele[,.(tasmin_mean_land=mean(tasmin)), by=list(age)]
  lsm_ele_2<-merge(lsm_ele, lsm_ele_se, by=c("age"))
  tasmin<-rast("../Data/Raster/Rough.3.75x2.5/tempmonmin_abs.tif")
  v_mean<-global(tasmin, "mean", na.rm=TRUE)
  v_mean<-data.table(age=as.numeric(gsub("y", "", rownames(v_mean))),
                     tasmin_mean_global=v_mean[,1])
  lsm_ele_2<-merge(lsm_ele_2, v_mean, by=c("age"))
  saveRDS(lsm_ele_2, "../Data/LSM/lsm_ele_fullset.rda")
  
  lsm_ele_2[tasmin==-9999]
  #lsm_ele<-lsm_ele[tasmin!=-9999]
  table(lsm_ele_2$v)
  #lsm_ele$v<-NULL
  lsm$v<-1
  lsm_ele_v<-merge(lsm_ele_2, lsm, by=c("x", "y", "age", "index"), all=T)
  if (F){
    xy<-unique(lsm_ele_v[is.na(min_ele), c("x", "y")])
    ggplot(xy)+geom_tile(aes(x, y))
  }
  lsm_ele_v<-lsm_ele_v[!is.na(min_ele),]
  lsm_ele_v[is.na(v)]$v<-0
  table(lsm_ele_v$v)
  table(lsm_ele_v_2$v)
  
  
  lsm_ele_v_se<-lsm_ele_v[v==1, .(N=.N), by=list(index)]
  #lsm_ele_v_2<-lsm_ele_v[!index %in% lsm_ele_v_se[N==901]$index]
  lsm_ele_v_2<-lsm_ele_v
  setorderv(lsm_ele_v_2, c("age", "x", "y"))
  saveRDS(lsm_ele_v_2, "../Data/LSM/lsm_ele_trainset.rda")
}

lsm_ele_trainse<-readRDS("../Data/LSM/lsm_ele_trainset.rda")
lsm_ele_fullset<-readRDS("../Data/LSM/lsm_ele_fullset.rda")
if (F){
  model<-glm(data=lsm_ele_v_2, v~y+mean_ele+tasmin_mean_global+tasmin_mean_land)
  pred<-predict(model, lsm_ele_v_2)
  
  plot(lsm_ele_v_2$v, pred)
  summary(model)
  
  library(randomForest)
  
  rf<-randomForest(data=lsm_ele_v_2[between(y, -30, 30)], 
                   v_factor~y+mean_ele+tasmin_mean_global+tasmin_mean_land,
                   importance=TRUE)
}
table(lsm_ele_trainse[between(y, -30, 30)]$v)
lsm_ele_trainse$v_factor<-factor(lsm_ele_trainse$v)
rf<-randomForest(data=lsm_ele_trainse[between(y, -20, 20)], 
                 v_factor~mean_ele+tasmin_mean_global+tasmin,
                 importance=TRUE)
rf

#summary(rf)
importance(rf)
varImpPlot(rf)
lsm_ele_trainse[, .(mean_ele=max(mean_ele)), by=(v)]
lsm_ele_fullset$pred<-predict(rf, lsm_ele_fullset)

range(lsm_ele_trainse[between(y, -20, 20) & v==0]$mean_ele)
lsm_ele_fullset$pred_ele<-ifelse(lsm_ele_fullset$mean_ele>300, 1, 0)
ggplot(lsm_ele_trainse[age==3600])+geom_tile((aes(x, y, fill=v_factor)))+
  coord_equal()

ggplot(lsm_ele_fullset[between(y, -20, 20) & age==3600])+
  geom_tile((aes(x, y, fill=pred_ele)))+
  coord_equal()
