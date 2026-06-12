library(data.table)
library(RSQLite)
library(DBI)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results"
folders<-list.dirs(target, full.names=T)
length(folders)

folders<-folders[2:length(folders)]

for (f in folders){
  files<-list.files(f)
  if (length(files)<4 & (!"unfinished.txt" %in% files)){
    print(f)
    #unlink(f, recursive=T)
    
  }
}
