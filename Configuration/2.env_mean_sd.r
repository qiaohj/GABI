library(terra)
library(ncdf4)
library(data.table)
setwd("/media/huijieqiao/WD22T_11/continental_movement/Script")
all_age<-readRDS("../Data/meta_table.rda")
variables<-unique(all_age[group=="variable", c("label", "name", "folder", "file", "age")])

paste(unique(variables$name), collapse=", ")


#precipmon_av, precipmonmax_abs, precipmonmin_abs, tempmon_av, 
#tempmonmax_abs, tempmonmin_abs, cmm, drymon, wetmon, wmm, mixLyrDpth_ym_uo, 
#salinity_ym_dpth, temp_ym_dpth, lsm, field700, depthlevel, depthdepth, ht, 
#field150, field152, field153, field154, field174, 
#field175, precip_mm_srf, temp_mm_1_5m  
files<-unique(variables[, c("label", "folder", "file", "age")])
i=1
all_v<-list()
for (i in c(1:nrow(files))){
  print(paste(i, nrow(files)))
  nc<-nc_open(sprintf("%s/%s", files[i]$folder, files[i]$file))
  vars<-unique(variables[folder==files[i]$folder & file==files[i]$file]$name)
  for (v in vars){
    values<-ncvar_get(nc, v)
    
    if (length(dim(values))==3){
      v_mean<-apply(values, c(3), mean)
      v_sd<-apply(values, c(3), sd)
      v_min<-apply(values, c(3), min)
      v_max<-apply(values, c(3), max)
      items<-c(1:dim(values)[3])
      if (length(items)==12){
        item_type<-"month"
      }else{
        item_type<-"depth"
      }
    }else{
      v_mean<-mean(values)
      v_sd<-sd(values)
      v_min<-min(values)
      v_max<-max(values)
      items<-0
      item_type<-""
    }
    df_item<-data.table(items=items, 
                        item_type=item_type,
                        v_mean=v_mean,
                        v_sd=v_sd,
                        v_min=v_min,
                        v_max=v_max,
                        var=v, age=files[i]$age)
    all_v[[length(all_v)+1]]<-df_item
    
  }
  nc_close(nc)
}
all_v_df<-rbindlist(all_v)
saveRDS(all_v_df, "../Data/env_fullstat.rda")
