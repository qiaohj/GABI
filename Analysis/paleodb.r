library(jsonlite)
library(data.table)
library(curl)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")

if (F){
  url_t<-"https://paleobiodb.org/data1.2/occs/taxa.json?datainfo&rowcount&max_ma=%d&min_ma=%d"
  for (i in seq(500, 10, by=-10)){
    f<-sprintf("../Data/PaleoDB/taxa_json/%d-%d.rda", i, i-10)
    if (file.exists(f)){
      next()
    }
    print(i)
    url<-sprintf(url_t, i, i-10)  
    con <- curl(url)
    v<-readLines(con)
    saveRDS(v, f)
  }
  
  con<-curl("https://paleobiodb.org/data1.2/occs/taxa.json?datainfo&rowcount&rank=min_genus&max_ma=540")
  v<-readLines(con)
  saveRDS(v, "../Data/PaleoDB/taxa_json_genus_above.rda")
  
  vvv<-fromJSON(v)
  genus<-data.table(vvv$records)
  saveRDS(genus, "../Data/PaleoDB/taxa_genus_above.rda")
  for (i in seq(2000, 10, by=-10)){
    f<-sprintf("../Data/PaleoDB/taxa_json/%d-%d.rda", i, i-10)
    v<-readRDS(f)
    v<-fromJSON(v)
    if (length(v$records)!=0){
      v<-data.table(v$records)
      taxa[[length(taxa)+1]]<-v
    }
  }
  taxadf<-rbindlist(taxa, fill=T)
  taxadf<-unique(taxadf)
  taxadf[nam=="Annelida"]
  taxadf$noc<-NULL
  taxadf<-unique(taxadf)
  saveRDS(taxadf, "../Data/PaleoDB/taxa_full.rda")
}
db<-fromJSON("../Data/PaleoDB/paleobiodb10ma.json")
#tna: accepted_name	: The value of this field will be the accepted taxonomic name corresponding to the identified name.
#rnk: accepted_rank : The taxonomic rank of the accepted name. This may be different from the identified rank if the identified name is a nomen dubium or otherwise invalid, or if the identified name has not been fully entered into the taxonomic hierarchy of this database.
#oei: early_interval: The specific geologic time range associated with this occurrence (not necessarily a standard interval), or the interval that begins the range if late_interval is also given
#oli: late_interval	: The interval that ends the specific geologic time range associated with this occurrence, if different from the value of early_interval
#eag: max_ma        : The early bound of the geologic time range associated with this occurrence (in Ma)
#lag: min_ma        : The late bound of the geologic time range associated with this occurrence (in Ma)
#idn: identified_name:The taxonomic name by which this occurrence was identified. This field will be omitted for responses in the compact voabulary if it is identical to the value of accepted_name.
#idr: identified_rank:The taxonomic rank of the identified name, if this can be determined. This field will be omitted for responses in the compact voabulary if it is identical to the value of accepted_rank.
#tdf: difference    : If the identified name is different from the accepted name, this field gives the reason why. This field will be present if, for example, the identified name is a junior synonym or nomen dubium, or if the species has been recombined, or if the identification is misspelled.



taxa<-readRDS("../Data/PaleoDB/taxa_full.rda")
taxa
#rnk: The rank of this taxon, ranging from subspecies up to kingdom


collections<-fromJSON("../Data/PaleoDB/collections10ma.json")
#sfm: The formation in which the collection was found


db<-data.table(db$records)

collections<-data.table(collections$records)

db[!cid %in% collections$oid]
collections[!oid %in% db$cid]

sp<-"Aturia coxi"
db[tna %in% sp][3]
collections[oid %in% db[tna %in% sp]$cid[3]]

db

#rnk: 20: phylum, 17: class, 13: order, 9: family, 5: genus, 3 species
#taxa[rnk %in% c(1, 2, 4)]: zero record in it, so species is trustable.
#ctn: container_no 
#par: The identifier of the parent taxon, or of its senior synonym if there is one. This field and those following are only available if the classification of this taxon is known to the database.
#ext: True if this taxon is extant on earth today, false if not, not present if unrecorded


species<-taxa[rnk==3]
species<-species[, c("oid", "nam", "par", "ctn")]
species$phylum<-""
species$phylum_id<-""
species$class<-""
species$class_id<-""
species$order<-""
species$order_id<-""
species$family<-""
species$family_id<-""
species$genus<-""
species$genus_id<-""
taxa[oid=="txn:455949"]

species[!is.na(ctn)]
i=1
for (i in c(1:nrow(species))){
  print(paste(i, nrow(species)))
  if (is.na(species[i]$ctn)){
    ids<-species[i]$par
  }else{
    #ids<-c(species[i]$par, species[i]$ctn)
    ids<-species[i]$par
  }
  phylum_item<-getRank(ids, 20)
  if (nrow(phylum_item)==1){
    species[i]$phylum<-phylum_item$nam
    species[i]$phylum_id<-phylum_item$oid
  }
  class_item<-getRank(ids, 17)
  if (nrow(class_item)==1){
    species[i]$class<-class_item$nam
    species[i]$class_id<-class_item$oid
  }
  order_item<-getRank(ids, 13)
  if (nrow(order_item)==1){
    species[i]$order<-order_item$nam
    species[i]$order_id<-order_item$oid
  }
  family_item<-getRank(ids, 9)
  if (nrow(family_item)==1){
    species[i]$family<-family_item$nam
    species[i]$family_id<-family_item$oid
  }
  genus_item<-getRank(ids, 5)
  if (nrow(genus_item)==1){
    species[i]$genus<-genus_item$nam
    species[i]$genus_id<-genus_item$oid
  }
  
}

saveRDS(species, "../Data/PaleoDB/taxa.rda")
pid<-"txn:148905"
taxa[oid=="txn:374469"]
getRank(c("txn:148905"), 20)
getRank(c("txn:148905"), 17)
getRank(c("txn:148905"), 13)
getRank(c("txn:148905"), 9)
getRank(c("txn:148905"), 5)


getRank<-function(pid, rank){
  item<-taxa[oid %in% pid]
  if (!is.na(item$ctn)){
    ids<-c(item$par, item$ctn)
  }else{
    ids<-item$par  
  }
  item<-item[!is.na(item$nam)]
  print(item)
  if (nrow(item)==0){
    data.table()
  }else{
    if (item$rnk==rank){
      item
    }else{
      
      getRank(ids, rank)
    }
  }
}

par_pairs<-c("txn:42622"="txn:87634")

table(species$class)

taxa[oid=="txn:374469"]
taxa[oid=="txn:374468"]
taxa[oid=="txn:42742"]
taxa[oid=="txn:42622"]


table(taxa[rnk==20]$nam)
