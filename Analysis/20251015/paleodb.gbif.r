library(data.table)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
db<-fread("../Data/PaleoDB.GBIF/0008218-241024112534372.csv")

table(db$taxonRank)
occs<-fromJSON("../Data/PaleoDB/paleobiodb10ma.json")
collections<-fromJSON("../Data/PaleoDB/collections10ma.json")
occs<-data.table(occs$records)
collections<-data.table(collections$records)

occs[tid=="txn:374469"]
head(db)
View(db[occurrenceID=="pbdb:occ:4451"])
unique(db$phylum)
