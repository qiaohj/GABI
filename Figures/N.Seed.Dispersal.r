library(data.table)
library(ggplot2)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
if (F){
  sp.without.bridge<-readRDS("../Data/Tables/100k.speciation.years/sp_full_continents.without.bridge.rda")
  sp.with.bridge<-readRDS("../Data/Tables/100k.speciation.years/sp_full_continents.rda")
  seeds.all<-readRDS("../Data/Tables/100k.speciation.years/random.seeds.rda")
  all.with.bridge.item<-list()
  all.with.bridge.seed<-list()
  all.without.bridge.item<-list()
  all.without.bridge.seed<-list()
  
  for (rrrr in c(1:10)){
    print(rrrr)
    seeds<-seeds.all[rep==rrrr]
    sp_filter.without.bridge<-sp.without.bridge[label %in% seeds$label]
    sp_filter.with.bridge<-sp.with.bridge[label %in% seeds$label]
    
    #without.bridge
    sp_item.without.bridge<-sp_filter.without.bridge
    sp_item.without.bridge<-sp_item.without.bridge[, c("year", "sp_id", "NB", "DA", "seed_id", "Parent", 
                         "origin_continent", "seed_continent",
                         "current_continent")]
    sp_item.without.bridge<-unique(sp_item.without.bridge)
    sp_item.N.without.bridge<-sp_item.without.bridge[, .(N=length(unique(sp_id))), 
                                  by=list(year, 
                                          seed_id,
                                          NB,
                                          origin_continent, current_continent)]
    sp_item.N.without.bridge$rep<-rrrr
    all.without.bridge.item[[rrrr]]<-sp_item.N.without.bridge
    
    sp_seed.without.bridge<-sp_item.without.bridge
    sp_seed.without.bridge<-sp_seed.without.bridge[, c("year", "NB", "DA", "seed_id", "seed_continent", "current_continent")]
    sp_seed.without.bridge<-unique(sp_seed.without.bridge)
    
    sp_seed.N.without.bridge<-sp_item.without.bridge[, .(N=length(unique(seed_id))), 
                                by=list(year, 
                                        seed_id,
                                        NB,
                                        seed_continent, current_continent)]
    sp_seed.N.without.bridge$rep<-rrrr
    all.without.bridge.seed[[rrrr]]<-sp_seed.N.without.bridge
    
    #with.bridge
    sp_item.with.bridge<-sp_filter.with.bridge
    sp_item.with.bridge<-sp_item.with.bridge[, c("year", "sp_id", "NB", "DA", "seed_id", "Parent", 
                                                       "origin_continent", "seed_continent",
                                                       "current_continent")]
    sp_item.with.bridge<-unique(sp_item.with.bridge)
    sp_item.N.with.bridge<-sp_item.with.bridge[, .(N=length(unique(sp_id))), 
                                                     by=list(year, 
                                                             seed_id,
                                                             NB,
                                                             origin_continent, current_continent)]
    
    sp_item.N.with.bridge$rep<-rrrr
    all.with.bridge.item[[rrrr]]<-sp_item.N.with.bridge
    
    sp_seed.with.bridge<-sp_item.with.bridge
    sp_seed.with.bridge<-sp_seed.with.bridge[, c("year", "NB", "DA", "seed_id", "seed_continent", "current_continent")]
    sp_seed.with.bridge<-unique(sp_seed.with.bridge)
    
    sp_seed.N.with.bridge<-sp_item.with.bridge[, .(N=length(unique(seed_id))), 
                                                     by=list(year,
                                                             seed_id,
                                                             NB,
                                                             seed_continent, 
                                                             current_continent)]
    sp_seed.N.with.bridge$rep<-rrrr
    all.with.bridge.seed[[rrrr]]<-sp_seed.N.with.bridge
  }
  
  all.with.bridge.item.df<-rbindlist(all.with.bridge.item)
  all.with.bridge.seed.df<-rbindlist(all.with.bridge.seed)
  all.without.bridge.item.df<-rbindlist(all.without.bridge.item)
  all.without.bridge.seed.df<-rbindlist(all.without.bridge.seed)
  saveRDS(all.with.bridge.item.df, "../Data/Tables/100k.speciation.years/N.with.bridge.continent.rda")
  saveRDS(all.with.bridge.seed.df, "../Data/Tables/100k.speciation.years/N.with.bridge.seed.rda")
  saveRDS(all.without.bridge.item.df, "../Data/Tables/100k.speciation.years/N.without.bridge.continent.rda")
  saveRDS(all.without.bridge.seed.df, "../Data/Tables/100k.speciation.years/N.without.bridge.seed.rda")
  
  
}
if (F){
  seeds<-sp.with.bridge[,.(N=.N), by=list(NB, DA, seed_id)]
  final<-list()
  for (i in c(1:nrow(seeds))){
    print(paste(i, nrow(seeds)))
    item<-seeds[i]
    sp.items<-sp.with.bridge[NB==item$NB & DA==item$DA & seed_id==item$seed_id]
    seed_continent<-sp.items[year== -1800]$seed_continent
    target_continent<-ifelse(seed_continent=="South America", "North America", "South America")
    target_continent<-c(target_continent, "Two continents")
    target_item<-sp.items[current_continent %in% target_continent]
    to_target_continent<-(nrow(target_item)>0)
    if (to_target_continent==T){
      to_year<-min(target_item$year)
    }else{
      to_year<-Inf
    }
    final_continent<-target_item[year==0]
    to_target_continent_final<-(nrow(final_continent)>0)
    item$seed_continent<-seed_continent
    item$to_target_continent<-to_target_continent
    item$to_target_continent_final<-to_target_continent_final
    item$to_year<-to_year
    final[[i]]<-item
  }
  final.df<-rbindlist(final)
}

#all.with.bridge.item.df<-readRDS("../Data/Tables/100k.speciation.years/N.with.bridge.continent.rda")
all.with.bridge.seed.df<-readRDS("../Data/Tables/100k.speciation.years/N.with.bridge.seed.rda")
#all.without.bridge.item.df<-readRDS("../Data/Tables/100k.speciation.years/N.without.bridge.continent.rda")
#all.without.bridge.seed.df<-readRDS("../Data/Tables/100k.speciation.years/N.without.bridge.seed.rda")

setorderv(all.with.bridge.seed.df, "year", 1)

df.seed.with.bridge<-all.with.bridge.seed.df[current_continent %in% c("South America", "North America")]
df.seed.with.bridge<-df.seed.with.bridge[current_continent!=seed_continent]
df.seed.with.bridge$current_continent<-NULL

min_year <- min(df.seed.with.bridge$year)
max_year <- max(df.seed.with.bridge$year)


cutoff_years <- data.table(Cutoff_Year = seq(min_year, max_year + 1, by = 1))


group_keys <- unique(df.seed.with.bridge[, .(NB, seed_continent, rep)])

group_keys[, temp_key := 1]
cutoff_years[, temp_key := 1]
dt_combinations <- merge(group_keys, cutoff_years, by = "temp_key", allow.cartesian = TRUE)
dt_combinations[, temp_key := NULL]


dt_final_result <- df.seed.with.bridge[
  # 引用 x (dt_combinations) 表，并使用 .() 语法选择和重命名列
  dt_combinations, 
  
  # 在 j 中：将原始表的 N 和 x 表的 Cutoff_Year 取出
  .(N = N, 
    Cutoff_Year = x.Cutoff_Year), 
  # 注意：i.N 是原始 N 值，x.Cutoff_Year 是截止年份
  
  on = .(
    NB = NB, 
    seed_continent = seed_continent, 
    rep = rep,
    year <= Cutoff_Year 
  ),
  allow.cartesian = TRUE
]

# 6. 分组累计 N
# 现在 dt_final_result 已经包含了 N 和 Cutoff_Year，可以直接按名称分组
dt_final_result <- dt_final_result[, 
                                   .(Cumulative_N = sum(N, na.rm = TRUE)),
                                   by = .(NB, seed_continent, rep, Cutoff_Year)
]
# 7. 排序和展示结果
setorder(dt_final_result, NB, seed_continent, rep, Cutoff_Year)