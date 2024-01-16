library(tidyverse)
library(igraph)
library(showtext)
library(rcartocolor)
library(data.table)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")
dispersal_result<-readRDS("../Data/Tables/dispersal_result.rda")

sub_result<-dispersal_result

table(sub_result$status)
extinct<-c("Extinct after isthmus", "Extinct before isthmus", "Extinct immediately")
extant<-c("Extant")

N_Simulations<-sprintf("Simulations\n%s", format(nrow(sub_result), big.mark=","))
N_North_America<-sprintf("North America\n%s", format(nrow(sub_result[continent=="North America"]), big.mark=","))
N_South_America<-sprintf("South America\n%s", format(nrow(sub_result[continent=="South America"]), big.mark=","))
N_North_America_Extant<-sprintf("Extant\n%s", format(nrow(sub_result[continent=="North America" & status %in% extant]), 
                                                     big.mark=","))
N_North_America_Extinct<-sprintf("Extant\n%s", 
                                 format(nrow(sub_result[
                                   continent=="North America" & 
                                     status %in% extinct]), 
                                   big.mark=","))
N_South_America_Extant<-sprintf("Extant\n%s", format(nrow(sub_result[continent=="South America" & status %in% extant]), 
                                                     big.mark=","))
N_South_America_Extinct<-sprintf("Extant\n%s", 
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
                                                                  init_s==1 & middle_s==1]), 
                                                big.mark=","))
N_South_America_Extinct_not_to_North<-sprintf("not to North\n%s", 
                                             format(nrow(sub_result[continent=="South America" & 
                                                                      status %in% extinct &
                                                                      init_s==1 & middle_s==0]), 
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
p
