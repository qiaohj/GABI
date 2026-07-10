library(data.table)
library(ggplot2)
library(sf)
setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
target<-"/media/huijieqiao/Butterfly/GABI/Results"

cells<-readRDS("../Data/Tables/cells.with.dist.rda")
cells$continent<-factor(cells$continent, 
                        levels=c("North America", "South America", "bridge1", "bridge2"),
                        labels=c("North America", "South America", "Isthmus", "Caribbean"))
p<-ggplot(cells)+geom_sf(aes(fill=continent), linewidth = 0.2, color="lightgrey")+
  scale_fill_manual(values=c("South America"=color_high,
                             "North America"=color_low,
                             "Isthmus"=color_2,
                             "Caribbean"=color_mid2))+
  coord_sf()+
  theme_bw() +
  labs(
    title = "",
    #subtitle = "Pie area proportional to Total Population",
    fill = "Area"
  ) +
  theme(
    legend.position = "bottom",
    axis.title = element_blank()
  )      
p
ggsave(p, filename="../Figures/Mask/Figure.Mask.pdf", width=6, height=6)
ggsave(p, filename="../Figures/Mask/Figure.Mask.png", width=6, height=6, bg="white")

cells.dt<-data.table(seqnum=cells$seqnum, continent=cells$continent)

cells.dt.N<-cells.dt[, .(N=.N), by=list(continent)]
to.doc(cells.dt.N, "Number of grids per area", "../Figures/Mask/Figure.Mask.docx", digits = 0)

