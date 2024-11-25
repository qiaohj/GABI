library(data.table)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
taxa<-readRDS("../Data/PaleoDB/taxa_table.rda")
taxa<-taxa[class %in% c("Amphibia", "Aves", "Mammalia", "Reptilia")]
taxa[is.na(ext)]<-0
N_ROW1<-taxa[, .(N=.N), by=c("class", "ext")]

taxa_gbif_full<-fread("../Data/GBIF/taxa.gbif.csv")
taxa_gbif<-taxa_gbif_full[iucnRedListCategory %in% 
                       c("CR", "DD", "EN", "EX", "LC", "NE", "NT", "VU")]
taxa_gbif<-taxa_gbif[class %in% c("Amphibia", "Aves", "Mammalia", "Reptilia",
                                  "Testudines")]

exist_taxa<-taxa[nam %in% taxa_gbif$species]

taxa[class=="Reptilia" & ext==1 & !(nam %in% taxa_gbif$species)]
#Macrochelys temminckii
taxa_gbif_full[genus=="Macrochelys"]
taxa[genus=="Macrochelys"]


N_ROW2<-exist_taxa[, .(N_GBIF=.N), by=c("class", "ext")]


N_ROW<-merge(N_ROW1, N_ROW2, by=c("class", "ext"), all=T)
