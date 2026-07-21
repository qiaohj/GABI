library(data.table)
library(patchwork)
library(ggplot2)
library(ggeffects)

setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
df<-readRDS("../Data/Tables/N.with.bridge.simulation.rda")
df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
table(df$seed_continent)
cells<-readRDS("../Data/Tables/cells.with.dist.rda")
cells<-data.table(seed_id=cells$seqnum, min.dist=cells$min.dist)

df<-merge(df, cells, by="seed_id")
df$to_target_continent_final_factor<-as.factor(df$to_target_continent_final)
df$NB_factor<-factor(df$NB, 
                     levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                     labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
df$DA_factor<-as.factor(df$DA)
df<-df[seed_id!="9745"]



#model<-glm(to_target_continent_final_factor~seed_continent +min.dist+NB_factor+DA_factor, 
#           data=df, family = binomial)
model<-glm(to_target_continent_final_factor~min.dist+NB_factor+DA_factor, 
           data=df, family = binomial)

summary(model)


pred_min_dist <- ggpredict(model, terms = "min.dist")

p1<-plot(pred_min_dist) +
  labs(
    y = "Predicted probability of successful dispersal",
    x = "Minimum distance to the other continent",
    title = "(a) Effect of minimum distance on dispersal probability"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    #axis.title = element_text(face = "bold")
  )

pred_NB <- ggpredict(model, terms = "NB_factor")

p2<-plot(pred_NB)+labs(x="Niche breadth",
                       y="Predicted probability of successful dispersal",
                       title="(b) Effect of niche breadth on dispersal probability")+
  theme_classic() +
  theme(
    legend.position = "right",
    #axis.title = element_text(face = "bold"),
    axis.title.y=element_blank()
  )

pred_DA <- ggpredict(model, terms = "DA_factor")
p3<-plot(pred_DA)+labs(x="Dispersal ability",
                       y="Predicted probability of successful dispersal",
                       title="(c) Effect of dispersal ability on dispersal probability")+
  theme_classic() +
  theme(
    legend.position = "right",
    #axis.title = element_text(face = "bold"),
    axis.title.y=element_blank()
  )

p<-(p1 | (p2 / p3)) + 
  plot_layout(widths = c(1.5, 1))

ggsave(p, filename="../Figures/Seed.Dispersal/Seed.Dispersal.GLM.pdf", width=12, height=5)
ggsave(p, filename="../Figures/Seed.Dispersal/Seed.Dispersal.GLM.png", width=12, height=5)
