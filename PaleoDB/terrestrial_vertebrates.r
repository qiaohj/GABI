library(data.table)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
backbone<-fread("../Data/GBIF/backbone/Taxon.tsv", quote="")
taxa<-readRDS("../Data/PaleoDB/taxa_table.rda")
taxa<-unique(taxa)
taxa$acc_name<-""
taxa$syn_name<-""
reg_string <- "\\(.*?\\)"
taxa$fixed_name<-gsub(reg_string, "", taxa$nam)
taxa$fixed_name<-gsub("  ", " ", taxa$fixed_name)
taxa[fixed_name=="Katastrophomena (Katastrophomena) woodlandensis"]

backbone[canonicalName=="Spondylus marisrubri"]
backbone[taxonID==5729648]

taxa[fixed_name %in% backbone[taxonomicStatus=="accepted"]$canonicalName]$acc_name<-
  taxa[fixed_name %in% backbone[taxonomicStatus=="accepted"]$canonicalName]$fixed_name

taxa[fixed_name %in% backbone[taxonomicStatus!="accepted"]$canonicalName]$syn_name<-
  taxa[fixed_name %in% backbone[taxonomicStatus!="accepted"]$canonicalName]$fixed_name

#https://www.gbif.org/tools/species-lookup
missing_names<-taxa[acc_name=="" & syn_name==""]
missing_names<-missing_names[, c("fixed_name")]
colnames(missing_names)<-"scientificName"
fwrite(missing_names[1:5000], "../Data/GBIF/missing_names1.csv")
fwrite(missing_names[5001:nrow(missing_names)], "../Data/GBIF/missing_names2.csv")

normalized1<-fread("../Data/GBIF/normalized1.csv")
normalized2<-fread("../Data/GBIF/normalized2.csv")
normalized<-rbindlist(list(normalized1, normalized2))
normalized[status!="ACCEPTED" & matchType=="FUZZY"]
table(normalized$rank)
normalized<-normalized[, c("verbatimScientificName", "key", "matchType", "status", "rank",
                           "canonicalName", "usageKey", "acceptedUsageKey")]
normalized[is.na(acceptedUsageKey)]$acceptedUsageKey<-normalized[is.na(acceptedUsageKey)]$usageKey

normalizedx<-merge(normalized, backbone, by.x="acceptedUsageKey", by.y="taxonID", all.x=T, all.y=F)
normalizedx<-normalizedx[, c("verbatimScientificName", "matchType",
                             "status", "rank", "canonicalName.x",
                             "canonicalName.y")]
normalizedx<-unique(normalizedx)
normalizedx$match_type_2<-sprintf("%s_%s", normalizedx$matchType, normalizedx$status)
table(normalizedx$match_type_2)
table(normalizedx$status)
missing_names[!(scientificName %in% normalized$verbatimScientificName)]

taxa<-taxa[!is.na(nam)]
taxa$match_type<-""
taxa[acc_name!=""]$match_type<-"Accepted"
taxa[syn_name!=""]$match_type<-"Synonym"
taxa[match_type==""]

taxa_with_fuzzy<-merge(taxa, normalizedx, by.x="fixed_name", by.y="verbatimScientificName", all=T)
taxa_with_fuzzy[match_type==""]$match_type<-taxa_with_fuzzy[match_type==""]$match_type_2

table(taxa_with_fuzzy$match_type)
taxa_with_fuzzy[match_type=="HIGHERRANK_SYNONYM"]
taxa_with_fuzzy[status=="ACCEPTED"]$acc_name<-
  taxa_with_fuzzy[status=="ACCEPTED"]$canonicalName.y
taxa_with_fuzzy[status=="SYNONYM"]$syn_name<-
  taxa_with_fuzzy[status=="SYNONYM"]$canonicalName.y



table(taxa_with_fuzzy[acc_name=="" & matchType=="FUZZY" & status == "ACCEPTED"]$rank)
taxa_with_fuzzy[acc_name=="" & matchType=="FUZZY" & status == "ACCEPTED"]$match_type<-
  "Accepted_Fuzzy"
taxa_with_fuzzy[acc_name=="" & matchType=="FUZZY" & status == "ACCEPTED"]$acc_name<-
  taxa_with_fuzzy[acc_name=="" & matchType=="FUZZY" & status == "ACCEPTED"]$canonicalName.x
taxa_with_fuzzy[match_type=="Accepted_Fuzzy"]

table(taxa_with_fuzzy[acc_name=="" & matchType=="FUZZY" & status == "DOUBTFUL"]$status)

taxa_with_fuzzy[acc_name=="" & matchType=="FUZZY" & status == "ACCEPTED"]$match_type<-
  "Accepted_Fuzzy"
taxa_with_fuzzy[acc_name=="" & matchType=="FUZZY" & status == "ACCEPTED"]$acc_name<-
  taxa_with_fuzzy[acc_name=="" & matchType=="FUZZY" & status == "ACCEPTED"]$canonicalName.x
taxa_with_fuzzy[match_type=="Accepted_Fuzzy"]


N_taxa_with_fuzzy<-taxa_with_fuzzy[, .(N=.N), by="fixed_name"]

normalizedx[verbatimScientificName=="Achistrum brevis"]
taxa[fixed_name=="Achistrum brevis"]
N_taxa_with_fuzzy[N>1]
#



no_in_gbif_taxa<-taxa[!nam %in% backbone$canonicalName]
no_in_gbif_taxa[class=="Mammalia"]
table(no_in_gbif_taxa[class=="Mammalia"]$order)

table(taxa$phylum)
table(taxa[phylum=="Chordata"]$class)
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
backbone[genericName=="Emys" & specificEpithet=="marmorata"]
backbone[canonicalName=="Emys marmorata"]

#Macrochelys temminckii
taxa_gbif_full[genus=="Macrochelys"]
taxa[genus=="Macrochelys"]


N_ROW2<-exist_taxa[, .(N_GBIF=.N), by=c("class", "ext")]


N_ROW<-merge(N_ROW1, N_ROW2, by=c("class", "ext"), all=T)
