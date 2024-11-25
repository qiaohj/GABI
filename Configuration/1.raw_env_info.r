library(terra)
library(ncdf4)
library(data.table)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
source<-"../Data/3600Ma_simulations"
folders<-list.dirs(source)
folders<-folders[-1]
f<-folders[1]
all<-list()
for (f in folders){
  files<-list.files(f, pattern="\\.nc")
  fil<-files[1]
  for (fil in files){
    print(paste(f, fil))
    nc<-nc_open(sprintf("%s/%s", f, fil))
    global_att <- ncatt_get(nc, 0)
    rr<-list()
    for (g in names(global_att)){
      rr[[length(rr)+1]]<-data.table(name=g, group="global", comment=global_att[[g]])
    }
    #rr<-rbindlist(rr)
    variables = names(nc[['var']])
    dims<-names(nc[['dim']])
    
    vars<-data.table(name=c(variables, dims), group=c(rep("variable", length(variables)),
                                                      rep("dim", length(dims))))
    for (i in c(1:nrow(vars))){
      name <- vars[i]$name
      
      var_dim<-paste(dim(ncvar_get(nc, name)), collapse=",")
      var_att<-ncatt_get(nc, name)
      for (at in names(var_att)){
        rr[[length(rr)+1]]<-data.table(name=name, group=vars[i]$group, att=at, 
                                       comment=ncatt_get(nc, name, at)$value,
                                       dim=var_dim)
      }
    }
    
    rr<-rbindlist(rr, fill=T)
    rr$folder<-f
    rr$file<-fil
    all[[length(all)+1]]<-rr
  }
  nc_close(nc)
}
all<-rbindlist(all)

year_labels<-fread("../Data/name_year.csv", header=T)
year_labels$V3<-NULL
year_labels$V4<-NULL
colnames(year_labels)<-c("label", "age")
year_labels<-year_labels[label!=""]
year_labels$age<-as.numeric(gsub("k", "", year_labels$age))
all$label<-gsub("../Data/3600Ma_simulations/", "", all$folder)
all_age<-merge(all, year_labels, by="label")
fwrite(all_age, "../Data/meta_table.csv")
saveRDS(all_age, "../Data/meta_table.rda")
all_age<-readRDS("../Data/meta_table.rda")


all_age[att=="long_name" & age==1872]$comment
