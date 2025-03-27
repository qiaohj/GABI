library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
library(ggh4x)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
target<-"/media/huijieqiao/WD22T_11/GABI/Results"
folders<-list.dirs(target, full.names=T)
length(folders)
ns<-read_sf("../Data/Shape/isea3h8/N_S_America.shp")
ns<-data.table(ns)
ns$geometry<-NULL
folders<-folders[sample(length(folders), length(folders))]
f<-folders[1]
folders[which(grepl("40435", folders))]

conn<-dbConnect(RSQLite::SQLite(), "../Configuration/configuration.sqlite")
pr<-data.table(dbReadTable(conn, "pr"))
tasmax<-data.table(dbReadTable(conn, "tasmax"))
tasmin<-data.table(dbReadTable(conn, "tasmin"))
dist<-data.table(dbReadTable(conn, "distances"))
dbDisconnect(conn)

colnames(pr)[2]<-"pr"
colnames(tasmax)[2]<-"tasmax"
colnames(tasmin)[2]<-"tasmin"

envs<-merge(merge(pr, tasmax, by=c("global_id", "year")),
            tasmin, by=c("global_id", "year"))
i=36084
for (i in c(2:length(folders))){
  f<-folders[i]
  files<-list.files(f)
  #print(length(files))
  if (length(files)>=4){
    print(paste(f, i, length(folders)))
    target<-sprintf("%s/nb.rda", f)
    if (file.exists(target)){
      next()
    }
    ff<-files[grepl("sqlite", files)]
    ff<-gsub("\\.sqlite", "", ff)
    saveRDS(NULL, target)
    log<-fread(sprintf("%s/%s.log", f, ff))
    colnames(log)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
    log<-log[suitable==1]
    if (nrow(log)==0){
      next()
    }
    
    log.full<-merge(log, envs, by=c("year", "global_id"))
    nb<-log.full[, .(min_pr=min(pr),
                     max_pr=max(pr),
                     min_tas=min(tasmax, tasmin),
                     max_tas=max(tasmax, tasmin)),
                 by=list(year, sp_id)]
    
    nb$pr<-nb$max_pr - nb$min_pr
    nb$tas<-nb$max_tas - nb$min_tas
    if (F){
      nb<-readRDS("/media/huijieqiao/WD22T_11/GABI/Results/40435.MODERATE-MODERATE.GOOD/nb.rda")
      ggplot(nb)+geom_line(aes(x=year*-2, y=pr, color=sp_id))+
        theme(legend.position = "none")
      ggplot(nb[year %in% c(1800, 1000, 500, 0)])+geom_histogram(aes(x=tas))+
        facet_wrap(~year)+
        theme(legend.position = "none")
    }
    
    saveRDS(nb, target)
  }
}

if (F){
  target<-"/media/huijieqiao/WD22T_11/GABI/Results"
  folders<-list.dirs(target, full.names=T)
  nblist<-list()
  for (i in c(2:length(folders))){
    print(i)
    f<-folders[i]
    files<-list.files(f)

    ff<-files[grepl("sqlite", files)]
    ff<-gsub("\\.sqlite", "", ff)
    infors<-strsplit(ff, "\\.")[[1]]
    target<-sprintf("%s/nb.rda", f)
    ddd<-readRDS(target)
    
    
    if (is.null(ddd)){
      next()
    }
    if (nrow(ddd)==0){
      next()
    }
    ddd$seed_id<-infors[1]
    ddd$nb<-infors[2]
    ddd$da<-infors[3]
    nblist[[length(nblist)+1]]<-ddd
  }
  nbdf<-rbindlist(nblist)
  saveRDS(nbdf, "../Data/Tables/100k.speciation.years/nb.rda")
  if (F){
    nbdfxx<-nbdf[year==0 & nb!="HUGE-HUGE"]
    iucn_quantile<-readRDS("../Data/nb_range_mammals_iucn.rda")
    iucn<-readRDS(sprintf("../Data/IUCN_NB/%s/%s.rda", "World",  "Mammals"))
    iucn$iqr<-iucn$q3 - iucn$q1
    iucn$range_iqr<-iucn$q3 + iucn$iqr*1.5 - (iucn$q1 - iucn$iqr*1.5)
    iucn$range_3sd<-iucn$sd * 6
    iucn$range_min_max<-iucn$max - iucn$min
    
    ggplot(nbdfxx)+geom_histogram(aes(x=pr))+
      facet_grid2(nb~da, scale="free", independent="all")
    ggplot(nbdfxx[sample(nrow(nbdfxx), nrow(iucn[var=="pr"]))])+
      geom_histogram(aes(x=pr))+
      geom_histogram(data=iucn[var=="pr"], aes(x=range_3sd), fill="red", alpha=0.3)
    
  }
}
