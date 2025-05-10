library(data.table)
library(ape)
library(phytools)
library(ggtree)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
df<-readRDS("../Data/Tables/100k.speciation.years/virtual.species.rda")
df_N<-df[,.(N=.N), by=list(seed_id, nb, da)]
#View(df_N)
#head(df)
#seed_id<-"39581"
df$Parent<-sub("-[^-]*$", "", df$sp_id)
#df[sp_id==seed_id]$Parent<-""
#df[sp_id==seed_id]

labels<-unique(df$label)


for (l in labels){
  print(l)
  target<-sprintf("../Data/temp/%s.rda", l)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  d<-df[label==l]
  
  d$year<-d$year * -1
  #d<-d[continent %in% c("South America", "North America")]
  #View(d)
  
  species_withns<-d[,.(from=min(year), to=max(year)), by=list(sp_id, Parent, continent)]
  species_withns$sp_id<-as.factor(species_withns$sp_id)
  species_withns$Parent<-as.factor(species_withns$Parent)
  species_withns[Parent==""]$Parent<-"Seed"
  
  species<-d[, .(from=min(year), to=max(year)), by=list(sp_id, Parent)]
  species$sp_id<-as.factor(species$sp_id)
  species$Parent<-as.factor(species$Parent)
  species[Parent==""]$Parent<-"Seed"
  
  tree <- as.phylo(species)
  tree$edge.length <- species$to - species$from
  #node_times <- node.depth.edgelength(tree)
  #cutoff_time <- 2000
  #nodes_before <- which(node_times <= cutoff_time)
  #total_nodes <- length(nodes_before) 
  #is_leaf <- nodes_before <= Ntip(tree)
  #leaf_count <- sum(is_leaf)
  #node_count <- total_nodes - leaf_count
  
  species$is_leaf<-!(species$sp_id %in% species$Parent)
  species$type<-"Species"
  species[is_leaf==T & to!=0]$type<-"Extinction"
  species[is_leaf==F]$type<-"Speciation"
  #species[sp_id==seed_id]$type<-"Root"
  species$handle<-F
  setorderv(species, c("from", "to"), c(1, 1))
  
  item_prev<-NULL
  n<-list()
  for (y in c(min(species_withns$from):max(species_withns$to))){
    n_item<-list()
    item<-species_withns[from<=y & to>=y]
    item$L1<-"C"
    N_species<-item[continent %in% c("North America", "South America"), 
                    .(N=length(unique(sp_id))), by=list(continent)]
    N_species$Type<-"N Species"
    n_item[[length(n_item)+1]]<-N_species
    item_all<-species_withns[from<=y]
    N_species_all<-item_all[continent %in% c("North America", "South America"), 
                            .(N=length(unique(sp_id))), by=list(continent)]
    N_species_all$Type<-"N ALL Species"
    n_item[[length(n_item)+1]]<-N_species_all
    
    if (!is.null(item_prev)){
      item_prev$L2<-"P"
      merged_item<-merge(item_prev, item, by=c("sp_id", "Parent", "continent", "from", "to"), all=T)
      merged_item<-merged_item[continent %in% c("North America", "South America")]
      gain_item<-merged_item[is.na(L2)]
      loss_item<-merged_item[is.na(L1)]
      if (nrow(gain_item)>0){
        gain_item$Type<-"New speciation"
        gain_item[sp_id %in% item_prev$sp_id]$Type<-"New arrival"
        
        N_species_gain<-gain_item[Type=="New speciation", 
                                  .(N=.N), by=list(sp_id, Type)]
        N_gain<-list()
        for (parent in unique(gain_item[Type=="New speciation"]$Parent)){
          N_species_gain<-gain_item[Parent==parent, .(N=.N), by=list(sp_id, Type)]
          if (length(unique(N_species_gain$N))==1){
            if (N_species_gain[1]$N==1){
              Speciation_type<-"across continents"
            }
            if (N_species_gain[1]$N==2){
              Speciation_type<-"both continents"
            }
            N_gain_item<-data.table(continent=c("North America", "South America"),
                                    Type="New speciation", N=1, Speciation_type=Speciation_type)
            
          }else{
            single_species<-N_species_gain[N==1]
            single_species<-species_withns[continent %in% c("North America", "South America") & 
                                             from==y & sp_id %in% single_species$sp_id]
            N_gain_item<-data.table(continent=single_species$continent,
                                    Type="New speciation", N=1, Speciation_type="single continent")
          }
          N_gain[[length(N_gain)+1]]<-N_gain_item
        }
        
        N_gain_item<-gain_item[Type=="New arrival", 
                               .(N=.N), by=list(continent, Type)]
        N_gain_item$Speciation_type<-NA
        if (nrow(N_gain_item)>0){
          N_gain[[length(N_gain)+1]]<-N_gain_item
        }
        N_gain<-rbindlist(N_gain)
        n_item[[length(n_item)+1]]<-N_gain
      }
      if (nrow(loss_item)>0){
        loss_item$Type<-"Unknown"
        check<-species[to==(y-1) & type=="Speciation" & sp_id %in% loss_item$sp_id]
        loss_item[sp_id %in% check$sp_id]$Type<-"Loss speciation"
        
        check<-species[to==(y-1) & type=="Extinction" & sp_id %in% loss_item$sp_id]
        loss_item[sp_id %in% check$sp_id]$Type<-"Loss extinction"
        
        loss_item[Type=="Unknown"]$Type<-"Loss local"
        N_loss<-loss_item[,.(N=.N), by=list(continent, Type)]
        n_item[[length(n_item)+1]]<-N_loss
      }
    }
    n_item<-rbindlist(n_item, use.names=T, fill=T)
    n_item$Year<-y
    n[[length(n)+1]]<-n_item
    item_prev<-item
    item_prev$L1<-NULL
  }
  n_df<-rbindlist(n, use.names=T, fill=T)
  
  saveRDS(n_df, target)
}

if (F){
  all<-list()
  for (l in labels){
    print(l)
    target<-sprintf("../Data/temp/%s.rda", l)
    ddddd<-readRDS(target)
    ddddd$Label<-l
    all[[length(all)+1]]<-ddddd
  }
  all_df<-rbindlist(all, fill=TRUE)
  saveRDS(all_df, "../Data/Tables/100k.speciation.years/N_full.rda")
  types<-unique(all_df$Type)
  for (t in types){
    print(t)
    item<-all_df[Type==t]
    saveRDS(item, sprintf("../Data/Tables/100k.speciation.years/N_%s.rda", gsub(" ", "_", t)))
  }
  for (t in c("Loss_speciation", "Loss_extinction", "Loss_local", "New_arrival", "New_speciation")){
    print(t)
    ddd<-readRDS(sprintf("../Data/Tables/100k.speciation.years/N_%s.rda", t))
    setorderv(ddd, c("Label", "Year"))
    ddd[, cumulative_N := cumsum(N), by = Label]
    saveRDS(ddd, sprintf("../Data/Tables/100k.speciation.years/N_%s.rda", t))
    
  }
  
  for (t in c("N_ALL_Species", "N_Species", "Loss_speciation", "Loss_extinction", 
              "Loss_local", "New_arrival", "New_speciation")){
    print(t)
    ddd<-readRDS(sprintf("../Data/Tables/100k.speciation.years/N_%s.rda", t))
    ddd[, c("seed_id", "nb", "da") := tstrsplit(Label, "\\.")]
    
    saveRDS(ddd, sprintf("../Data/Tables/100k.speciation.years/N_%s.rda", t))
    table(ddd$Speciation_type)
  }
  
}

if (F){

  
}
n_df[Type=="New speciation"]
n_df[, .(N=sum(N)), by=list(continent, Type)]
n_df[Year==0]
table(species$type)

xxx<-d[continent %in% c("North America", "South America") & year==0]
setorder(xxx, "sp_id")
na<-unique(xxx[continent=="North America"]$sp_id)
sa<-unique(xxx[continent=="South America"]$sp_id)
na[na=="39581-1-1-1-2"]
sa[sa=="39581-1-1-1-2"]
na[!na %in% sa]
sa[!sa %in% na]

table(d[sp_id=="39581-1-1-1-2"]$continent)
length(unique(c(na, sa)))

species_withns[!sp_id %in% species$sp_id]

tree$Nnode

Ntip(tree)

xxxx<-species_withns[sp_id %in% species[type=="Speciation"]$sp_id]
setorder(xxxx, "sp_id")

species[sp_id=="39581-1-2-2-1"]
species[Parent=="39581-1-2-2-1"]


ggtree(tree) +
  geom_nodelab(hjust=-.1)+
  geom_tiplab() +
  scale_x_continuous(limits=c(0, 2500), breaks = seq(0, 2500, by = 100))


N.Sp.Ex<-readRDS("../Data/Tables/100k.speciation.years/N.Speciation.Extinction.rda")
d_N.Sp.Ex<-N.Sp.Ex[nb=="MODERATE-MODERATE" & da=="GOOD" & seed_id==39581]
View(d_N.Sp.Ex)
