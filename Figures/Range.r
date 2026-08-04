library(data.table)
library(ggplot2)
library(sf)
library(stringr)
library(flextable)
library(officer)
library(RSQLite)
library(DBI)
library(patchwork)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
if (F){
  range<-readRDS("../Data/Tables/Species.Range.rda")
  seeds.all<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.rda")
  range<-range[seed_id %in% seeds.all$seed_id]
  
  range.null<-readRDS("../Data/Tables/Species.Range.NULL.rda")
  range.null<-range.null[seed_id %in% seeds.all$seed_id]
  
  
  hist(range$N.Cells)
  iucn<-readRDS(sprintf("../Data/IUCN_NB/Mammals.%s.rda", "World"))
  iucn<-iucn[continent=="America"]
  iucn<-iucn[range>0]
  iucn<-unique(iucn[, c("species", "N_CELLS")])
  hist(iucn$N_CELLS)
  
  df <- data.table(
    value = c(range$N.Cells, range.null$N.Cells, iucn$N_CELLS),
    group = c(rep("Simulation", length(range$N.Cells)),
              rep("Null model", length(range.null$N.Cells)), 
              rep("IUCN", length(iucn$N_CELLS)))
  )
  
  p_density <- ggplot(df, aes(x = value, fill = group, color = group)) +
    geom_density(alpha = 0.5) +
    theme_minimal() +
    labs(title = "Density curves", x = "Range", y = "Density")
  
  
  
  p_ecdf <- ggplot(df, aes(x = value, color = group)) +
    stat_ecdf(linewidth = 1) +
    theme_minimal() +
    labs(title = "Cumulative probability distribution", x = "Range", y = "Cumulative probability")
  
  p<-p_density+p_ecdf
  ggsave(p, filename="../Figures/Figure.Range/Range.pdf", width=12, height=5)
  ggsave(p, filename="../Figures/Figure.Range/Range.png", width=12, height=5)
  
  ks_result <- ks.test(range$N.Cells, iucn$N_CELLS)
  print(ks_result)
  
  df <- data.frame(
    value = c(range.null$N.Cells, iucn$N_CELLS),
    group = c(rep("Null model", length(range.null$N.Cells)), rep("IUCN", length(iucn$N_CELLS)))
  )
  
  p_ecdf <- ggplot(df, aes(x = value, color = group)) +
    stat_ecdf(linewidth = 1) +
    theme_minimal() +
    labs(title = "2. 累积概率分布图 (eCDF)", x = "数值", y = "累积概率")
  
  ks_result <- ks.test(range.null$N.Cells, iucn$N_CELLS)
  print(ks_result)
  
}