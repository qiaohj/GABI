library(terra)
library(ncdf4)
library(data.table)
setwd("/media/huijieqiao/WD22T_11/continental_movement/Script")
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
}
all<-rbindlist(all)
fwrite(all, "../Data/meta_table.csv")
