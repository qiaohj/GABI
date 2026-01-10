library(data.table)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results"
folders<-list.dirs(target, full.names=T)
length(folders)
folders<-folders[!grepl("BROAD", folders)]
folders<-folders[!grepl("MODERATE", folders)]

for (f in folders){
  
  files<-list.files(f)
  #print(length(files))
  if (length(files)<4){
    
    
    if (length(files[grepl("too", files)])>0){
      next()
    }
    print(files)
    unlink(f, recursive=T)
    
  }
}
