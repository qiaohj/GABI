library(data.table)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results"
folders<-list.dirs(target, full.names=T)
length(folders)
folders<-folders[!folders %in% 
                   c("/media/huijieqiao/Butterfly/GABI/Results/4409.BROAD.GOOD",
                     "/media/huijieqiao/Butterfly/GABI/Results/12104.BROAD.GOOD",
                     "/media/huijieqiao/Butterfly/GABI/Results/5157.BROAD.GOOD",
                     "/media/huijieqiao/Butterfly/GABI/Results/40597.BIG.GOOD",
                     "/media/huijieqiao/Butterfly/GABI/Results/4085.BROAD.GOOD",
                     "/media/huijieqiao/Butterfly/GABI/Results/40528.BROAD.GOOD",
                     "/media/huijieqiao/Butterfly/GABI/Results/11279.BROAD.GOOD",
                     "/media/huijieqiao/Butterfly/GABI/Results/11298.BROAD.GOOD",
                     "/media/huijieqiao/Butterfly/GABI/Results/9901.BROAD.GOOD")]
folders<-folders[2:length(folders)]
N.unfinished<-0
for (f in folders){
  
  files<-list.files(f)
  if (("unfinished.txt" %in% files)){
    N.unfinished<-N.unfinished+1
    next()
  }
  #print(length(files))
  if (length(files)<4 & (!"unfinished.txt" %in% files)){
    print(f)
    unlink(f, recursive=T)
    
  }
  
  if (length(files[grepl("too", files)])>0){
    #print(files)
    #unlink(f, recursive=T)
    next()
  }
  
}
