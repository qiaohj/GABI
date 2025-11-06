library(data.table)
library(flextable)
library(officer)
library(ggplot2)
library(ggeffects)

setDTthreads(30)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
df<-readRDS("../Data/Tables/N.with.bridge.simulation.rda")
df<-df[NB %in% c("BIG-BIG", "MODERATE-MODERATE")]
df$label<-sprintf("%d.%s.%s", df$seed_id, df$NB, df$DA)
table(df$seed_continent)
cells<-readRDS("../Data/cells.with.dist.rda")
cells<-data.table(seed_id=cells$seqnum, min.dist=cells$min.dist)

df<-merge(df, cells, by="seed_id")
df$to_target_continent_final_factor<-as.factor(df$to_target_continent_final)
df$NB_factor<-as.factor(df$NB)
df$DA_factor<-as.factor(df$DA)

model<-glm(to_target_continent_final_factor~min.dist+NB_factor+DA_factor, 
           data=df, family = binomial)
summary(model)


pred_min_dist <- ggpredict(model, terms = "min.dist")

p1<-plot(pred_min_dist) +
  labs(
    y = "Predicted Probability of Successful Dispersal",
    x = "Minimum Distance to Target Continent (min.dist)",
    title = "Effect of Minimum Distance on Dispersal Probability"
  ) +
  theme_bw()

pred_NB <- ggpredict(model, terms = "NB_factor")
p2<-plot(pred_NB)+labs(title="Predicted Probability to other continent")

pred_DA <- ggpredict(model, terms = "DA_factor")
p3<-plot(pred_DA)+labs(title="Predicted Probability to other continent")

(p1 | (p2 / p3)) + 
  plot_layout(widths = c(2, 1))
