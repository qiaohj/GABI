library(terra)
library(ggplot2)
library(data.table)
library(ncdf4)
setwd("/media/huijieqiao/WD22T_11/continental_movement/Script")
all_age<-readRDS("../Data/meta_table.rda")

vars<-c("precipmon_av", "tempmonmax_abs", "tempmonmin_abs")
all_age<-all_age[name %in% vars]
all_age$comment<-NULL
all_age$att<-NULL
all_age<-unique(all_age)
ages<-seq(from=min(all_age$age), to=max(all_age$age), by=2)

ages<-data.table(previous_age=ages-2, age=ages, next_age=ages+2)

#1st round, get all layers that in the netcdf files
i=1
layers<-list()
layers_index<-list()
for (i in c(1:nrow(ages))){
  print(paste(i, nrow(ages)))
  items<-all_age[age==ages[i]$age]
  if (nrow(items)==0){
    next()
  }
  nc<-rast(sprintf("%s/%s", items[1]$folder, items[1]$file))
  nc_r<-rotate(nc)
  
  for (v in vars){
    r_sub<-nc_r[[grep(v, names(nc_r))]]
    rm("r_final")
    if (v=="precipmon_av"){
      r_final<-sum(r_sub)*30
    }
    if (v=="tempmonmax_abs"){
      r_final<-max(r_sub)
    }
    if (v=="tempmonmin_abs"){
      r_final<-min(r_sub)
    }
    index<-length(layers_index)+1
    layers_index[[index]]<-data.table(age=ages[i]$age, var=v, index=index)
    layers[[index]]<-r_final
    
  }
}

layers_index<-rbindlist(layers_index)
layers_index$type<-"raw"
#2nd round, if there is no date in it, we should recalculate it based on the 
#two layers near the missing date.
layers_index2<-list()
i=1
for (i in c(1:nrow(ages))){
  print(paste(i, nrow(ages)))
  items<-all_age[age==ages[i]$age]
  if (nrow(items)!=0){
    next()
  }
  
  source<-layers_index[age %in% c(ages[i]$previous_age, ages[i]$next_age)]
  
  for (v in vars){
    r_sub<-source[var==v]
    rm("r_final")
    r<-rast(layers[r_sub$index])
    r_final<-mean(r)
    index<-length(layers)+1
    layers_index2[[length(layers_index2)+1]]<-data.table(age=ages[i]$age, var=v, index=index,
                                                         type="mean")
    layers[[index]]<-r_final
    
  }
}
layers_index2<-rbindlist(layers_index2)
layers_index<-rbindlist(list(layers_index, layers_index2))
mask<-rast(nrows=180, ncols=360, xmin=-180, xmax=180, ymin=-90, ymax=90)
layers_index[, .(min(age), max(age)),
             by=list(type)]
for (v in vars){
  print(v)
  item_index<-layers_index[var==v]
  setorder(item_index, age)
  raster<-rast(layers[item_index$index])
  names(raster)<-paste("y", item_index$age, sep="")
  writeRaster(raster, sprintf("../Data/Raster/Rough.3.75x2.5/%s.tif", v), overwrite=T)
  raster2<-resample(raster, mask, method="bilinear")
  if (v=="precipmon_av"){
    v_name<-"pr"
  }
  if (v=="tempmonmax_abs"){
    v_name<-"tasmax"
  }
  if (v=="tempmonmin_abs"){
    v_name<-"tasmin"
  }
  writeRaster(raster2, sprintf("../Data/Raster/Fine.1x1/%s.tif", v_name), overwrite=T)
}

if (F){
  v<-vars[1]
  for (v in vars){
    raster<-rast(sprintf("../Data/Raster/Rough.3.75x2.5/%s.tif", v))
    mean<-data.table(age=as.numeric(gsub("y", "", names(raster))),
                     v=global(raster, "mean"))
    mean$y<-mean$age * -1
    if (F){
      ggplot(mean)+geom_line(aes(x=y, y=v.mean))
    }
  }
}