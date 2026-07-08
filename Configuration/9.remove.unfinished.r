library(data.table)
library(RSQLite)
library(DBI)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results_NULL"
folders<-list.dirs(target, full.names=T)
length(folders)

folders<-folders[2:length(folders)]
N<-0

folders<-folders[!folders %in% c("/media/huijieqiao/Butterfly/GABI/Results_NULL/4767.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/36126.MODERATE.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/5078.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/5407.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/4446.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/39628.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/12822.BIG.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/5014.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/12842.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/32940.MODERATE.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/6039.MODERATE.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/5315.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/33102.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/39813.BIG.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/4164.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/5724.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/5069.BROAD.GOOD",
                                 "/media/huijieqiao/Butterfly/GABI/Results_NULL/5245.BROAD.GOOD")]
for (f in folders){
  files<-list.files(f)
  if (length(files)<4 & (!"unfinished.txt" %in% files)){
    print(f)
    unlink(f, recursive=T)
    N<-N+1
  }
}
N
