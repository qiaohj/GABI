library(jsonlite)
library(data.table)
library(curl)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")

if (T){
  #url_t<-"https://paleobiodb.org/data1.2/occs/list.json?datainfo&rowcount&max_ma=%d&min_ma=%d"
  #url_t<-"https://paleobiodb.org/data1.2/colls/list.json?datainfo&rowcount&max_ma=%d&min_ma=%d"
  #url_t<-"https://paleobiodb.org/data1.2/specs/measurements.json?datainfo&rowcount&max_ma=%d&min_ma=%d"
  #url_t<-"https://paleobiodb.org/data1.2/specs/list.json?datainfo&rowcount&max_ma=%d&min_ma=%d"
  #url_t<-"https://paleobiodb.org/data1.2/occs/taxa.json?datainfo&rowcount&max_ma=%d&min_ma=%d"
  url_t<-"https://paleobiodb.org/data1.2/occs/strata.json?datainfo&rowcount&max_ma=%d&min_ma=%d"
  for (i in seq(550, 10, by=-10)){
    f<-sprintf("../Data/PaleoDB/strata_json/%d-%d.rda", i, i-10)
    if (file.exists(f)){
      next()
    }
    print(i)
    url<-sprintf(url_t, i, i-10)  
    con <- curl(url)
    v<-readLines(con)
    saveRDS(v, f)
  }
  dflist<-list()
  folder<-"colls"
  for (i in seq(550, 10, by=-10)){
    print(i)
    f<-sprintf("../Data/PaleoDB/%s_json/%d-%d.rda", folder, i, i-10)
    v<-readRDS(f)
    v<-fromJSON(v)
    if (length(v$records)!=0){
      v<-data.table(v$records)
      dflist[[length(dflist)+1]]<-v
    }
  }
  dfdf<-rbindlist(dflist, fill=T)
  dfdf<-unique(dfdf)
  
  saveRDS(dfdf, sprintf("../Data/PaleoDB/%s_full.rda", folder))
  
  dfdf<-readRDS(sprintf("../Data/PaleoDB/%s_full.rda", folder))
}