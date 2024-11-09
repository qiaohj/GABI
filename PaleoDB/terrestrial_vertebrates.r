library(data.table)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
backbone<-fread("../Data/GBIF/backbone/Taxon.tsv", quote="")
taxa<-readRDS("../Data/PaleoDB/taxa_table.rda")
taxa<-taxa[!is.na(nam)]
taxa<-unique(taxa)
taxa$acc_name<-""
taxa$syn_name<-""
reg_string <- "\\(.*?\\)"
taxa$fixed_name<-gsub(reg_string, "", taxa$nam)
taxa$fixed_name<-gsub("  ", " ", taxa$fixed_name)
taxa[fixed_name=="Katastrophomena (Katastrophomena) woodlandensis"]

backbone[canonicalName=="Spondylus marisrubri"]
backbone[taxonID==8936276]
backbone[canonicalName=="Abderospira aldrichi"]
taxa[syn_name=="Abderospira aldrichi"]

taxa$match_type<-""
taxa[fixed_name %in% backbone[taxonomicStatus %in% c("accepted", "doubtful")]$canonicalName]$acc_name<-
  taxa[fixed_name %in% backbone[taxonomicStatus %in% c("accepted", "doubtful")]$canonicalName]$fixed_name
taxa[fixed_name %in% backbone[taxonomicStatus=="accepted"]$canonicalName]$match_type<-
  "ACCEPTED"
taxa[fixed_name %in% backbone[taxonomicStatus=="doubtful"]$canonicalName]$match_type<-
  "DOUBTFUL"

taxa[fixed_name %in% backbone[taxonomicStatus!="accepted"]$canonicalName]$syn_name<-
  taxa[fixed_name %in% backbone[taxonomicStatus!="accepted"]$canonicalName]$fixed_name

table(backbone$taxonomicStatus)

table(backbone[is.na(acceptedNameUsageID)]$taxonomicStatus)

table(backbone[!is.na(acceptedNameUsageID)]$taxonomicStatus)


backbone_syn<-backbone[taxonomicStatus %in% c("heterotypic synonym",
                                              "homotypic synonym",
                                              "proparte synonym ",
                                              "synonym")]
backbone_acc<-backbone[taxonomicStatus %in% c("accepted", "doubtful")]

backbone_syn<-backbone_syn[, c("canonicalName", "acceptedNameUsageID", "taxonID",
                               "taxonomicStatus")]
backbone_acc<-backbone_acc[, c("canonicalName", "taxonID", "taxonomicStatus")]

backbone_syn_acc<-merge(backbone_syn, backbone_acc, by.x="acceptedNameUsageID", 
                        by.y="taxonID")
nrow(backbone_syn_acc)
nrow(backbone_syn)



colnames(backbone_syn_acc)<-c("acceptedNameUsageID",
                              "synonym",
                              "taxonID",
                              "synTaxonomicStatus",    
                              "acceptedName",
                              "accTaxonomicStatus")
table(backbone_syn_acc$accTaxonomicStatus)
backbone_syn[canonicalName=="Abderospira aldrichi"]
backbone_acc[taxonID=="8936276"]
backbone[taxonID=="8936276"]

table(taxa$match_type)
taxa_acc<-merge(taxa, backbone_syn_acc, by.x="fixed_name", by.y="synonym", all.x=T)
N_taxa_acc<-taxa_acc[, .(N=.N), by="fixed_name"]
N_taxa_acc[N>1]

taxa_acc[fixed_name=="Acer villosum"]

table(taxa_acc$match_type)

taxa_acc[match_type==""]$match_type<-taxa_acc[match_type==""]$synTaxonomicStatus

taxa_acc[match_type %in% c("heterotypic synonym", "homotypic synonym",
                           "proparte synonym", "synonym")]$acc_name<-
  taxa_acc[match_type %in% c("heterotypic synonym", "homotypic synonym",
                             "proparte synonym", "synonym")]$acceptedName

taxa_acc[is.na(match_type)]$match_type<-""
table(taxa_acc$match_type)

#https://www.gbif.org/tools/species-lookup
missing_names<-taxa_acc[acc_name=="" & syn_name==""]
missing_names<-missing_names[, c("fixed_name")]
colnames(missing_names)<-"scientificName"
fwrite(missing_names[1:5000], "../Data/GBIF/missing_names1.csv")
fwrite(missing_names[5001:nrow(missing_names)], "../Data/GBIF/missing_names2.csv")

normalized1<-fread("../Data/GBIF/normalized1.csv")
normalized2<-fread("../Data/GBIF/normalized2.csv")
normalized<-rbindlist(list(normalized1, normalized2))
normalized[status!="ACCEPTED" & matchType=="FUZZY"]
table(normalized$rank)
table(normalized$status)

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
#
#taxa[syn_name!=""]$match_type<-"Synonym"
taxa_acc[match_type==""]

taxa_with_fuzzy<-merge(taxa_acc, normalizedx, by.x="fixed_name", 
                       by.y="verbatimScientificName", all=T)
taxa_with_fuzzy[match_type==""]$match_type<-taxa_with_fuzzy[match_type==""]$match_type_2

table(taxa_with_fuzzy$match_type)
taxa_with_fuzzy$match_type<-gsub(" ", "_", toupper(taxa_with_fuzzy$match_type))


taxa_with_fuzzy[match_type=="NONE_"]

taxa_with_fuzzy[match_type=="HIGHERRANK_SYNONYM"]
taxa_with_fuzzy[match_type=="FUZZY_DOUBTFUL" & canonicalName.x!=canonicalName.y]

table(taxa_with_fuzzy[acc_name==""]$match_type)
taxa_with_fuzzy[match_type=="FUZZY_ACCEPTED"]$acc_name<-
  taxa_with_fuzzy[match_type=="FUZZY_ACCEPTED"]$canonicalName.x

taxa_with_fuzzy[match_type=="FUZZY_DOUBTFUL"]$acc_name<-
  taxa_with_fuzzy[match_type=="FUZZY_DOUBTFUL"]$canonicalName.x

taxa_with_fuzzy[match_type=="FUZZY_SYNONYM"]$acc_name<-
  taxa_with_fuzzy[match_type=="FUZZY_SYNONYM"]$canonicalName.y

taxa_with_fuzzy[match_type=="HIGHERRANK_ACCEPTED"]$acc_name<-
  taxa_with_fuzzy[match_type=="HIGHERRANK_ACCEPTED"]$canonicalName.x

taxa_with_fuzzy[match_type=="HIGHERRANK_DOUBTFUL"]$acc_name<-
  taxa_with_fuzzy[match_type=="HIGHERRANK_DOUBTFUL"]$canonicalName.x

taxa_with_fuzzy[match_type=="HIGHERRANK_SYNONYM"]$acc_name<-
  taxa_with_fuzzy[match_type=="HIGHERRANK_SYNONYM"]$canonicalName.y



N_taxa_with_fuzzy<-taxa_with_fuzzy[, .(N=.N), by="fixed_name"]

normalizedx[verbatimScientificName=="Cerithium gracile"]
taxa_with_fuzzy[fixed_name=="Cerithium gracile"]
N_taxa_with_fuzzy[N==5]

saveRDS(taxa_with_fuzzy, "../Data/PaleoDB/taxa_table_with_fuzzy.rda")

