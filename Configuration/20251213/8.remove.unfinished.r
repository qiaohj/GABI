library(data.table)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
target<-"/media/huijieqiao/WD22T_11/GABI/Results"
folders<-list.dirs(target, full.names=T)
length(folders)

for (f in folders){
  
  files<-list.files(f)
  #print(length(files))
  if (length(files)<4){
    print(files)
    #unlink(f, recursive=T)
    
  }
}
