library(data.table)
library(RSQLite)
library(DBI)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results"
folders<-list.dirs(target, full.names=T)
length(folders)

folders<-folders[2:length(folders)]
N.unfinished<-0
for (f in folders){
  files<-list.files(f)
  if (("unfinished.txt" %in% files)){
    fff<-sprintf("%s/unfinished.txt", f)
    if (!file.exists(fff)){
      next()
    }
    
    x<-fread(fff, header = F)
    v<-x$V1
    vv<-as.numeric(strsplit(v, ":")[[1]])
    
    if (length(vv)==1){
      unlink(f, recursive=T)
      next()
    }
    print(sprintf("%s, N species: %d, Steps left: %d", f, vv[1], vv[2]))
    N<-vv[1]+vv[2]
    pass<-T
    if (vv[1]>=2000 & vv[2]>=300){
      #next()
    }
    if (vv[2]>=1000){
      #next()
    }
    if (N>3000){
      #next()
    }else{
      #var = readline(prompt = "Remove it? (Y: remove, Q: quit) ");
      if (var=="Y"){
        pass<-F
      }
      if (var=="Q"){
        break()
      }
    }
    
    if (pass==T){
      N.unfinished<-N.unfinished+1
    }else{
      print(sprintf("Removing %s", f))
      #unlink(f, recursive=T)
    }
    next()
  }
  #print(length(files))
  if (length(files)<4 & (!"unfinished.txt" %in% files)){
    #print(f)
    #unlink(f, recursive=T)
    
  }
}
N.unfinished

if (F){
  list2<-list.dirs("/media/huijieqiao/Butterfly/GABI/Results_Unfinished")
  list2<-list2[2:length(list2)]
  for (f in list2){
    ff<-gsub("Results_Unfinished", "Results", f)
    if (!dir.exists(ff)){
      print(ff)
      print(paste("removing ", f))
      unlink(f, recursive=T)
    }
  }
}
if (F){
  
  list2<-list.dirs("/media/huijieqiao/Butterfly/GABI/Results_Unfinished")
  list2<-list2[2:length(list2)]
  for (f in list2){
    ff<-gsub("Results_Unfinished", "Results", f)
    if (dir.exists(ff)){
      print(ff)
      unlink(ff, recursive = T)
    }
  }
}
