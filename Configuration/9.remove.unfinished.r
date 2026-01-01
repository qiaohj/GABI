library(data.table)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results"
folders<-list.dirs(target, full.names=T)
length(folders)

for (f in folders){
  
  files<-list.files(f)
  #print(length(files))
  if (length(files)<4){
    print(files)
    unlink(f, recursive=T)
    
  }
}
