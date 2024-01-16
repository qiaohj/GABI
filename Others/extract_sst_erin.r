library(terra)
library(ncdf4)
library(data.table)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
all_age<-readRDS("../Data/meta_table.rda")
variables<-unique(all_age[group=="variable", c("label", "name", "folder", "file", "age")])
View(all_age[grepl("pf", file)])
View(all_age[grepl("pf", name) & label=="tEVT0"])


fff<-"pgclann_SST_Salinity_MLD"
targets_vars<-variables[grepl(fff, file)]
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
    writeRaster(r_sub, sprintf("%s/%d.tif", folder, item$age), overwrite=T)
  }
}
