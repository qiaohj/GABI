library(data.table)
library(ggplot2)
library(sf)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")

t_folder<-"500k.NB_BROAD.speciation.years/"
df<-readRDS(sprintf("../Data/Tables/%s/c100.virtual.species.rda", t_folder))

#df$label<-sprintf("%s_%s_%s", df$global_id, df$sp_id, df$da)

labels<-unique(df$label)
results<-list()
for (i in c(1:length(labels))){
  print(paste(i, paste(length(labels))))
  l<-labels[i]
  item<-df[label==l]
  conf<-item[1]
  conf$year<-NULL
  conf$N<-NULL
  conf$init_n<-ifelse(conf$continent=="North America", 1, 0)
  conf$init_s<-ifelse(conf$continent=="South America", 1, 0)
  conf$min_year<-min(item$year)
  # Status
  #1 Extinct immediately
  #2 Extinct
  #3 Extant
  if (nrow(item)==1 & item[1]$N==0){
    conf$status<-"Extinct immediately"
    conf$final_n<-0
    conf$middle_n<-0
    conf$final_s<-0
    conf$middle_s<-0
    conf$final_b1<-0
    conf$middle_b1<-0
    conf$final_b2<-0
    conf$middle_b2<-0
  }else{
    item<-item[year!=1800]
    continents<-unique(item$continent)
    conf$middle_n<-ifelse("North America" %in% continents, 1, 0)
    conf$middle_s<-ifelse("South America" %in% continents, 1, 0)
    conf$middle_b1<-ifelse("bridge1" %in% continents, 1, 0)
    conf$middle_b2<-ifelse("bridge2" %in% continents, 1, 0)
    
    if (conf$min_year==0){
      conf$status<-"Extant"
      item<-item[year==0]
      continents<-unique(item$continent)
      conf$final_n<-ifelse("North America" %in% continents, 1, 0)
      conf$final_s<-ifelse("South America" %in% continents, 1, 0)
      conf$final_b1<-ifelse("bridge1" %in% continents, 1, 0)
      conf$final_b2<-ifelse("bridge2" %in% continents, 1, 0)
    }else{
      conf$status<-ifelse(conf$min_year<1550, "Extinct after isthmus", "Extinct before isthmus")
      conf$final_n<-0
      conf$final_s<-0
      conf$final_b1<-0
      conf$final_b2<-0
    }
    
    
  }
  results[[i]]<-conf
}
result_df<-rbindlist(results, fill=T)
result_df$xx<-sprintf("%d.%d:%d.%d.%d.%d:%d.%d.%d.%d",
                      result_df$init_n, result_df$init_s,
                      result_df$final_n, result_df$final_s,
                      result_df$final_b1, result_df$final_b2,
                      result_df$middle_n, result_df$middle_s,
                      result_df$middle_b1, result_df$middle_b2)
table(result_df$xx)
result_df[is.na(min_year)]$min_year<-1800
xx<-result_df[, .(N=.N), by=list(continent, status)]
setorderv(xx, c("status", "continent"))
saveRDS(result_df, sprintf("../Data/Tables/%s/dispersal_result.rda", t_folder))

#North to South: 334
dim(result_df[init_n==1 & middle_s==1])
#North to South to the end: 320
dim(result_df[init_n==1 & final_s==1])

#Sorth to Nouth: 2787
dim(result_df[init_s==1 & middle_n==1])
#Sorth to Nouth to the end: 2428
dim(result_df[init_s==1 & final_n==1])

result_df[, .(N=.N), by=c("init_s", "init_n", "status")]

result_df[status=="Extant", .(N=.N), by=c("final_n", "final_s", 
                                          "init_n", "init_s",
                                          "status")]

result_df[status=="Extant", .(N=.N), by=c("final_n", "final_s", 
                                          "init_n", "init_s", 
                                          "final_b1", "final_b2",
                                          "status")]

x<-result_df[status=="Extant", .(N=.N), by=c("final_n", "final_s", 
                                             "init_n", "init_s", 
                                             "middle_b1", "middle_b2",
                                             "status")]
x[init_n==1 & final_s==0]

result_df[status=="Extinct", .(N=.N), by=c("middle_n", "middle_s", 
                                           "init_n", "init_s", 
                                           
                                           "status")]

y<-result_df[status=="Extinct", .(N=.N), by=c("middle_n", "middle_s", 
                                              "init_n", "init_s", 
                                              "middle_b1", "middle_b2",
                                              "status")]
y[init_s==1]

