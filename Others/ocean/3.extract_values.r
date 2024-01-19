library(terra)
library(ncdf4)
library(data.table)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
all_age<-readRDS("../Data/3600Ma_simulations_ocean/meta_table.rda")
variables<-unique(all_age[group=="variable", c("label", "name", "folder", "file", "age")])
View(all_age[grepl("pf", file)])
View(all_age[grepl("pf", name) & label=="tEVT0"])


fff<-"ocean"
targets_vars<-variables
i=1
vars<-unique(targets_vars$name)
targets_files<-targets_vars
targets_files$name<-NULL
targets_files<-unique(targets_files)
v<-vars[2]
target_folder<-sprintf("../Data/Erin/%s", fff)
if (!dir.exists(target_folder)){
  dir.create(target_folder)
}
month_label<-c(sprintf("%s_", tolower(month.abb)), "ann_")
getMonth<-function(str){
  unlist(sapply(str, function(x) which(str_detect(x, month_label))))
  
}
targets_files$month<-getMonth(targets_files$file)
unique(variables$age)
for (i in c(1:nrow(targets_files))){
  print(paste(i, nrow(targets_files)))
  item<-targets_files[i]
  nc<-rast(sprintf("%s/%s", item$folder, item$file))
  nc_r<-rotate(nc)
  for (v in vars){
    folder<-sprintf("%s/%s", target_folder, v)
    if (!dir.exists(folder)){
      dir.create(folder)
    }
    r_sub<-nc_r[[grep(v, names(nc_r))]]
    writeRaster(r_sub, sprintf("%s/%d.%d.tif", folder, item$age, item$month), overwrite=T)
  }
}
