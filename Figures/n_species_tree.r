library(tidyverse)
library(igraph)
library(showtext)
library(rcartocolor)
library(data.table)
library(ggpubr)
library(ggrepel)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
t_folder<-"500k.NB_BROAD.speciation.years"
dispersal_result<-readRDS(sprintf("../Data/Tables/%s/dispersal_result.rda", t_folder))
extinct<-c("Extinct after isthmus", "Extinct before isthmus", "Extinct immediately")
extant<-c("Extant")

N.Speciation.Extinction<-readRDS(sprintf("../Data/Tables/%s/N.Speciation.Extinction.rda", t_folder))
table(N.Speciation.Extinction[year==0]$N_SPECIATION)
N.Speciation.Extinction[N_SPECIATION>=50]
table(dispersal_result$status)
S_N_Extant<-dispersal_result[continent=="South America" & 
                               status %in% extant &
                               init_s==1 & final_n==1]
S_N_Extant$xLabel<-"S_N_Extant"
N_S_Extant<-dispersal_result[continent=="North America" & 
                               status %in% extant &
                               init_n==1 & final_s==1]
N_S_Extant$xLabel<-"N_S_Extant"
S_N_Extinct<-dispersal_result[continent=="South America" & 
                                status %in% extinct &
                                init_s==1 & middle_s==1]
S_N_Extinct$xLabel<-"S_N_Extinct"
N_S_Extinct<-dispersal_result[continent=="North America" & 
                                status %in% extinct &
                                init_n==1 & middle_s==1]
N_S_Extinct$xLabel<-"N_S_Extinct"

#N_S_Table<-rbindlist(list(S_N_Extant, N_S_Extant, S_N_Extinct, N_S_Extinct))
N_S_Table<-rbindlist(list(S_N_Extant, N_S_Extant))
N_S_Table$seed_id<-as.numeric(N_S_Table$seed_id)
N_S_Table<-merge(N_S_Table, N.Speciation.Extinction[year==0], by=c("seed_id", "nb", "da"))

p1<-ggplot(N_S_Table[N_SPECIATION<=100])+geom_boxplot(aes(x=xLabel, y=N_SPECIATION))
p2<-ggplot(N_S_Table[N_SPECIATION<=100])+geom_boxplot(aes(x=xLabel, y=N_EXTINCTION))

ggarrange(p1, p2, nrow=1)

#t.test(N_S_Table[N_SPECIATION<=100 & xLabel=="N_S_Extant"]$N_SPECIATION,
#       N_S_Table[N_SPECIATION<=100 & xLabel=="S_N_Extant"]$N_SPECIATION)
i=1
nbs<-dispersal_result[,.(N=.N), by=list(nb, da)]
all_r<-list()
for (i in c(1:nrow(nbs))){
  print(i)
  sub_result<-dispersal_result[nb==nbs[i]$nb & da==nbs[i]$da]
  
  table(sub_result$status)
  nnn<-strsplit(nbs[i]$nb, "-")[[1]]
  item<-data.table(nb_prcp=nnn[1],
                   nb_temp=nnn[2],
                   da=nbs[i]$da,
                   N_Simulations=
                     nrow(sub_result),
                   N_North_America=
                     nrow(sub_result[continent=="North America"]),
                   N_South_America=
                     nrow(sub_result[continent=="South America"]),
                   N_North_America_Extant=
                     nrow(sub_result[continent=="North America" & status %in% extant]),
                   N_North_America_Extinct=
                     nrow(sub_result[continent=="North America" & status %in% extinct]),
                   N_South_America_Extant=
                     nrow(sub_result[continent=="South America" & status %in% extant]),
                   N_South_America_Extinct=
                     nrow(sub_result[continent=="South America" & status %in% extinct]),
                   N_North_America_Extant_to_South=
                     nrow(sub_result[continent=="North America" & status %in% extant & init_n==1 & final_s==1]),
                   N_North_America_Extant_not_to_South=
                     nrow(sub_result[continent=="North America" & status %in% extant & init_n==1 & final_s==0]),
                   N_South_America_Extant_to_North=
                     nrow(sub_result[continent=="South America" & status %in% extant & init_s==1 & final_n==1]),
                   N_South_America_Extant_not_to_North=
                     nrow(sub_result[continent=="South America" & status %in% extant & init_s==1 & final_n==0]),
                   N_North_America_Extinct_to_South=
                     nrow(sub_result[continent=="North America" & status %in% extinct & init_n==1 & middle_s==1]),
                   N_North_America_Extinct_not_to_South=
                     nrow(sub_result[continent=="North America" & status %in% extinct & init_n==1 & middle_s==0]),
                   N_South_America_Extinct_to_North=
                     nrow(sub_result[continent=="South America" & status %in% extinct & init_s==1 & middle_n==1]),
                   N_South_America_Extinct_not_to_North=
                     nrow(sub_result[continent=="South America" & status %in% extinct & init_s==1 & middle_n==0])
  )
  all_r[[i]]<-item
  N_Simulations<-sprintf("Simulations\n%s", format(nrow(sub_result), big.mark=","))
  N_North_America<-sprintf("North America\n%s", format(nrow(sub_result[continent=="North America"]), big.mark=","))
  N_South_America<-sprintf("South America\n%s", format(nrow(sub_result[continent=="South America"]), big.mark=","))
  N_North_America_Extant<-sprintf("Extant\n%s", format(nrow(sub_result[continent=="North America" & status %in% extant]), 
                                                       big.mark=","))
  N_North_America_Extinct<-sprintf("Extinct\n%s", 
                                   format(nrow(sub_result[
                                     continent=="North America" & 
                                       status %in% extinct]), 
                                     big.mark=","))
  N_South_America_Extant<-sprintf("Extant\n%s", format(nrow(sub_result[continent=="South America" & status %in% extant]), 
                                                       big.mark=","))
  N_South_America_Extinct<-sprintf("Extinct\n%s", 
                                   format(nrow(sub_result[
                                     continent=="South America" & 
                                       status %in% extinct]), 
                                     big.mark=","))
  N_North_America_Extant_to_South<-sprintf("to South\n%s", 
                                           format(nrow(sub_result[continent=="North America" & 
                                                                    status %in% extant &
                                                                    init_n==1 & final_s==1]), 
                                                  big.mark=","))
  N_North_America_Extant_not_to_South<-sprintf("not to South\n%s", 
                                               format(nrow(sub_result[continent=="North America" & 
                                                                        status %in% extant &
                                                                        init_n==1 & final_s==0]), 
                                                      big.mark=","))
  
  N_South_America_Extant_to_North<-sprintf("to North\n%s", 
                                           format(nrow(sub_result[continent=="South America" & 
                                                                    status %in% extant &
                                                                    init_s==1 & final_n==1]), 
                                                  big.mark=","))
  N_South_America_Extant_not_to_North<-sprintf("not to North\n%s", 
                                               format(nrow(sub_result[continent=="South America" & 
                                                                        status %in% extant &
                                                                        init_s==1 & final_n==0]), 
                                                      big.mark=","))
  
  
  
  N_North_America_Extinct_to_South<-sprintf("to South\n%s", 
                                            format(nrow(sub_result[continent=="North America" & 
                                                                     status %in% extinct &
                                                                     init_n==1 & middle_s==1]), 
                                                   big.mark=","))
  N_North_America_Extinct_not_to_South<-sprintf("not to South\n%s", 
                                                format(nrow(sub_result[continent=="North America" & 
                                                                         status %in% extinct &
                                                                         init_n==1 & middle_s==0]), 
                                                       big.mark=","))
  
  N_South_America_Extinct_to_North<-sprintf("to North\n%s", 
                                            format(nrow(sub_result[continent=="South America" & 
                                                                     status %in% extinct &
                                                                     init_s==1 & middle_n==1]), 
                                                   big.mark=","))
  N_South_America_Extinct_not_to_North<-sprintf("not to North\n%s", 
                                                format(nrow(sub_result[continent=="South America" & 
                                                                         status %in% extinct &
                                                                         init_s==1 & middle_n==0]), 
                                                       big.mark=","))
  
  
  tree <- tibble(from = c(N_Simulations,
                          N_Simulations,
                          N_North_America,
                          N_North_America,
                          N_South_America,
                          N_South_America,
                          N_North_America_Extant,
                          N_North_America_Extant,
                          N_North_America_Extinct,
                          N_North_America_Extinct,
                          N_South_America_Extant,
                          N_South_America_Extant,
                          N_South_America_Extinct,
                          N_South_America_Extinct
  ),
  to = c(N_North_America,
         N_South_America,
         N_North_America_Extant,
         N_North_America_Extinct,
         N_South_America_Extant,
         N_South_America_Extinct,
         N_North_America_Extant_to_South,
         N_North_America_Extant_not_to_South,
         N_North_America_Extinct_to_South,
         N_North_America_Extinct_not_to_South,
         N_South_America_Extant_to_North,
         N_South_America_Extant_not_to_North,
         N_South_America_Extinct_to_North,
         N_South_America_Extinct_not_to_North
  ))
  
  g = graph_from_data_frame(tree, directed = TRUE)
  coords = layout_as_tree(g)
  colnames(coords) = c("x", "y")
  
  output_df = as_tibble(coords) %>%
    mutate(step = vertex_attr(g, "name"),
           label = step,
           x = x*-1)
  
  plot_nodes = output_df %>%
    mutate(xmin = x - 0.45,
           xmax = x + 0.45,
           ymin = y - 0.3,
           ymax = y + 0.3)
  
  plot_edges = tree %>%
    mutate(id = row_number()) %>%
    pivot_longer(cols = c("from", "to"),
                 names_to = "s_e",
                 values_to = "step") %>%
    left_join(plot_nodes, by = "step") %>%
    select(-c(label, y, xmin, xmax)) %>%
    mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
    select(-c(ymin, ymax))
  
  p = ggplot() +
    geom_rect(data = plot_nodes,
              mapping = aes(xmin = xmin, ymin = ymin, 
                            xmax = xmax, ymax = ymax),
              alpha = 0.5) 
  
  p = p + 
    geom_text(data = plot_nodes,
              mapping = aes(x = x, y = y, label = label),
              color = "#585c45") 
  p = p + 
    geom_path(data = plot_edges,
              mapping = aes(x = x, y = y, group = id),
              colour = "#585c45",
              arrow = arrow(length = unit(0.3, "cm"), type = "open"))
  p<-p + labs(x=paste("PRCP.TEMP", nbs[i]$nb, nbs[i]$da))
  
  ggsave(p, filename=sprintf("../Figures/TREES/%s.%s.png", nbs[i]$nb, nbs[i]$da), width=10, height=4)
  
}
all_r<-rbindlist(all_r)
all_r
saveRDS(all_r, "../Data/result_500k.rda")

all_r$N_2_S<-all_r$N_North_America_Extant_to_South+all_r$N_North_America_Extinct_to_South
all_r$S_2_N<-all_r$N_South_America_Extant_to_North+all_r$N_South_America_Extinct_to_North
all_r$N_2_S_Extant_Per<-all_r$N_North_America_Extant_to_South/all_r$N_North_America_Extant
all_r$S_2_N_Extant_Per<-all_r$N_South_America_Extant_to_North/all_r$N_South_America_Extant

all_r$N_2_S_Per<-all_r$N_2_S/all_r$N_North_America
all_r$S_2_N_Per<-all_r$S_2_N/all_r$N_South_America


all_r$label<-sprintf("PR:%s,TP:%s,DA:%s", 
                     substr(all_r$nb_prcp,1,1), 
                     substr(all_r$nb_temp,1,1), 
                     substr(all_r$da,1,1))
ggplot(all_r, aes(x=N_2_S_Extant_Per, y=S_2_N_Extant_Per))+
  geom_point(aes(color=label))+
  geom_text_repel(aes(label=label))+
  geom_point(aes(x=N_2_S_Per, y=S_2_N_Per, color=label), shape=2)+
  geom_abline(linetype=2)
