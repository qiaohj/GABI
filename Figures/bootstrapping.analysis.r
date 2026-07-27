library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(sf)
library(dplyr)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
source("Figures/common.r")



seed.95<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.95.rda")
seed.96<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.96.rda")

seed.99<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.99.rda")
seed.995<-readRDS("../Data/Tables/random.seeds.threshold.by.nb.distance.995.rda")

N95<-seed.95[,.(N.Usaged.95=.N), by=list(continent, seed_id, nb, da, seed_label, N_SPECIES, min.dist)]
N96<-seed.96[,.(N.Usaged.96=.N), by=list(continent, seed_id, nb, da, seed_label, N_SPECIES, min.dist)]

N99<-seed.99[,.(N.Usaged.99=.N), by=list(continent, seed_id, nb, da, seed_label, N_SPECIES, min.dist)]
N995<-seed.995[,.(N.Usaged.995=.N), by=list(continent, seed_id, nb, da, seed_label, N_SPECIES, min.dist)]

N.ALL<-merge(merge(N95, N96, by=c("continent", "seed_id", "nb", "da", "seed_label", "N_SPECIES", "min.dist"), all=T),
             merge(N99, N995, by=c("continent", "seed_id", "nb", "da", "seed_label", "N_SPECIES", "min.dist"), all=T),
             by=c("continent", "seed_id", "nb", "da", "seed_label", "N_SPECIES", "min.dist"), all=T)


N.ALL[is.na(N.Usaged.95), N.Usaged.95:=0]
N.ALL[is.na(N.Usaged.96), N.Usaged.96:=0]
N.ALL[is.na(N.Usaged.99), N.Usaged.99:=0]
N.ALL[is.na(N.Usaged.995), N.Usaged.995:=0]
N.ALL[N.Usaged.95==0]
N.ALL[N.Usaged.96==0]
N.ALL[N.Usaged.99==0]
N.ALL[N.Usaged.995==0]

N.SP.95<-N.ALL[N.Usaged.95>0, .(N_SPECIES=sum(N_SPECIES)), by=.(continent)]
N.SP.96<-N.ALL[N.Usaged.96>0, .(N_SPECIES=sum(N_SPECIES)), by=.(continent)]

N.ALL[N.Usaged.95>0,.(N_Simulations=.N), by=.(continent, nb, da)]

N.SP.99<-N.ALL[N.Usaged.99>0, .(N_SPECIES=sum(N_SPECIES)), by=.(continent)]
N.SP.995<-N.ALL[N.Usaged.995>0, .(N_SPECIES=sum(N_SPECIES)), by=.(continent)]

richness.df<-readRDS("../Data/Tables/N.with.bridge.seed.continent.rda")
richness.df$label<-sprintf("%d.%s.%s", richness.df$seed_id, richness.df$NB, richness.df$DA)

N.SP.Type.95<-richness.df[label %in% N95$seed_label, 
                          .(Native=sum(in_source_continent),
                            Immigrant=sum(to_target_continent)),
                          by=.(seed_continent)]
N.SP.Type.95$Immigrant_Per<-N.SP.Type.95$Immigrant/(N.SP.Type.95$Native+N.SP.Type.95$Immigrant)

N.SP.Type.96<-richness.df[label %in% N96$seed_label, 
                          .(Native=sum(in_source_continent),
                            Immigrant=sum(to_target_continent)),
                          by=.(seed_continent)]
N.SP.Type.96$Immigrant_Per<-N.SP.Type.96$Immigrant/(N.SP.Type.96$Native+N.SP.Type.96$Immigrant)

N.SP.Type.99<-richness.df[label %in% N99$seed_label, 
                          .(Native=sum(in_source_continent),
                            Immigrant=sum(to_target_continent)),
                          by=.(seed_continent)]
N.SP.Type.99$Immigrant_Per<-N.SP.Type.99$Immigrant/(N.SP.Type.99$Native+N.SP.Type.99$Immigrant)

N.SP.Type.995<-richness.df[label %in% N995$seed_label, 
                          .(Native=sum(in_source_continent),
                            Immigrant=sum(to_target_continent)),
                          by=.(seed_continent)]
N.SP.Type.995$Immigrant_Per<-N.SP.Type.995$Immigrant/(N.SP.Type.995$Native+N.SP.Type.995$Immigrant)


N.ALL$Per.95.96<-N.ALL$N.Usaged.95/N.ALL$N.Usaged.96
N.ALL$Per.99.995<-N.ALL$N.Usaged.99/N.ALL$N.Usaged.995
hist(N.ALL$Per.99.995)

richness.df<-readRDS("../Data/Tables/N.with.bridge.seed.continent.rda")
richness.df$label<-sprintf("%d.%s.%s", richness.df$seed_id, richness.df$NB, richness.df$DA)
colnames(richness.df)
colnames(N.ALL)[c(1, 3, 4)]<-c("seed_continent", "NB", "DA")
richness<-merge(richness.df, N.ALL, by=c("seed_continent", "NB", "DA", "seed_id"))
richness$in.95<-richness$N.Usaged.95!=0
richness$in.96<-richness$N.Usaged.96!=0
richness$in.99<-richness$N.Usaged.99!=0
richness$in.995<-richness$N.Usaged.995!=0

richness$label.95.96<-sprintf("95:%s & 96:%s", richness$in.95, richness$in.96)
richness$label.99.995<-sprintf("99:%s & 995:%s", richness$in.99, richness$in.995)

table(richness$label.95.96)
table(richness$label.99.995)


richness$N_SP_95<-richness$N_SPECIES * richness$N.Usaged.95
richness$N_SP_96<-richness$N_SPECIES * richness$N.Usaged.96
richness$N_SP_99<-richness$N_SPECIES * richness$N.Usaged.99
richness$N_SP_995<-richness$N_SPECIES * richness$N.Usaged.995

richness$N_Native_95<-richness$in_source_continent * richness$N.Usaged.95
richness$N_Native_96<-richness$in_source_continent * richness$N.Usaged.96
richness$N_Native_99<-richness$in_source_continent * richness$N.Usaged.99
richness$N_Native_995<-richness$in_source_continent * richness$N.Usaged.995

richness$N_Immigrant_95<-richness$to_target_continent * richness$N.Usaged.95
richness$N_Immigrant_96<-richness$to_target_continent * richness$N.Usaged.96
richness$N_Immigrant_99<-richness$to_target_continent * richness$N.Usaged.99
richness$N_Immigrant_995<-richness$to_target_continent * richness$N.Usaged.995

richness[,c("seed_id", "NB", "DA", "seed_continent", "N_Native_99", "N_Native_995", "N_Immigrant_99", "N_Immigrant_995")]

richness.N<-richness[, .(N_Native_95=sum(N_Native_95),
                         N_Native_96=sum(N_Native_96),
                         N_Native_99=sum(N_Native_99),
                         N_Native_995=sum(N_Native_995),
                         N_Immigrant_95=sum(N_Immigrant_95),
                         N_Immigrant_96=sum(N_Immigrant_96),
                         N_Immigrant_99=sum(N_Immigrant_99),
                         N_Immigrant_995=sum(N_Immigrant_995)),
                     by=.(seed_continent, label.99.995)]

new_simulations_99_995<-richness[label.99.995=="99:FALSE & 995:TRUE"]
p_99_995_N_SPECIES<-ggplot(new_simulations_99_995[N_SPECIES>1000])+
  geom_boxplot(aes(y=N_SPECIES, x=seed_continent))
p_99_995_N_USAGE<-ggplot(new_simulations_99_995[N_SPECIES>1000])+
  geom_boxplot(aes(y=N.Usaged.995, x=seed_continent))

richness.N<-richness[, .(N_Native_95=sum(N_Native_95),
                         N_Native_96=sum(N_Native_96),
                         N_Native_99=sum(N_Native_99),
                         N_Native_995=sum(N_Native_995),
                         N_Immigrant_95=sum(N_Immigrant_95),
                         N_Immigrant_96=sum(N_Immigrant_96),
                         N_Immigrant_99=sum(N_Immigrant_99),
                         N_Immigrant_995=sum(N_Immigrant_995)),
                     by=.(seed_continent, label.95.96)]

new_simulations_95_96<-richness[label.95.96=="95:FALSE & 96:TRUE"]
p_95_96_N_SPECIES<-ggplot(new_simulations_95_96[N_SPECIES>150])+
  geom_bar(aes(y=N_SPECIES, x=seed_continent, fill=seed_label), stat = "identity", color="grey")+
  theme(legend.position = "none")

p_95_96_N_USAGE<-ggplot(new_simulations_95_96[N_SPECIES>100])+
  geom_boxplot(aes(y=N.Usaged.96, x=seed_continent))


quantile(richness$N_SPECIES, seq(0.9, 1, by=0.005))

(p_95_96_N_SPECIES+p_99_995_N_SPECIES)/
  (p_95_96_N_USAGE+p_99_995_N_USAGE)


richness.df<-readRDS("../Data/Tables/N.with.bridge.seed.continent.rda")
richness.df$label<-sprintf("%d.%s.%s", richness.df$seed_id, richness.df$NB, richness.df$DA)
colnames(richness.df)

N_species<-readRDS("../Data/Tables/N_species.quantile.rda")

full.quantile<-readRDS("../Data/Tables/full.quantile.rda")
full.quantile<-c(-Inf, full.quantile, Inf)

group_labels <- c("<95%", "95%-95.5%", "95.5%-96%", "96%-96.5%", 
                  "96.5%-97%", "97%-97.5%", "97.5%-98%", "98%-98.5%", 
                  "98.5%-99%", "99%-99.5%", "99.5%-100%", ">100%")

N_species[, Group := cut(N_SPECIES, 
                          breaks = full.quantile, 
                          labels = group_labels, 
                          include.lowest = TRUE,
                          right = TRUE)]

table(N_species$Group)
N_species$label<-sprintf("%d.%s.%s", N_species$seed_id, N_species$nb, N_species$da)
N_species_sub<-N_species[, c("label", "Group")]
richness.group<-merge(richness.df, N_species_sub, by=c("label"))


richness.group_N<-richness.group[, .(N_Native=sum(in_source_continent),
                                     N_Immigrant=sum(to_target_continent)),
                                 by=.(Group, seed_continent)]

N_Native<-richness.group_N[, c("Group", "seed_continent", "N_Native")]
N_Native$continent<-N_Native$seed_continent

N_Immigrant<-richness.group_N[, c("Group", "seed_continent", "N_Immigrant")]
N_Immigrant$continent<-
  ifelse(N_Immigrant$seed_continent=="South America", "North America", "South America")

N_2<-merge(N_Immigrant, N_Native, by=c("Group", "continent"))
N_2$Immigrant_Per<-N_2$N_Immigrant/(N_2$N_Immigrant+N_2$N_Native)


ggplot(N_2[Group!="<95%"])+
  geom_boxplot(aes(x=Group, 
                   y=Immigrant_Per, 
                   color=continent))+
  geom_hline(yintercept = 0.5, linetype=2)


richness.group_N<-richness.group[, .(N_Native=sum(in_source_continent),
                   N_Immigrant=sum(to_target_continent)),
               by=.(Group, seed_continent, NB, DA)]

N_Native<-richness.group_N[, c("Group", "seed_continent", "N_Native", "NB", "DA")]
N_Native$continent<-N_Native$seed_continent

N_Immigrant<-richness.group_N[, c("Group", "seed_continent", "N_Immigrant", "NB", "DA")]
N_Immigrant$continent<-
  ifelse(N_Immigrant$seed_continent=="South America", "North America", "South America")

N_2<-merge(N_Immigrant, N_Native, by=c("Group", "continent", "NB", "DA"))
N_2$Immigrant_Per<-N_2$N_Immigrant/(N_2$N_Immigrant+N_2$N_Native)


ggplot(N_2[NB %in% c("BIG", "BROAD")])+
  geom_boxplot(aes(x=Group, 
                   y=Immigrant_Per, 
                   color=continent))+
  geom_hline(yintercept = 0.5, linetype=2)+
  facet_grid(DA~NB)

ggplot(N_2)+
  geom_line(aes(x=Group, 
                   y=N_Native, 
                   color=continent,
                group=continent))+
  geom_line(aes(x=Group, 
                y=N_Immigrant, 
                color=continent,
                group=continent), linetype=2)



if (F){
  base<-"/media/huijieqiao/Butterfly/GABI/Results"
  sp<-"11871.BIG.GOOD"
  
  
  readRDS(sprintf("%s/%s/%s.N.speciation.extinction.rda", base, sp, sp))
  
  richness.df[seed_id==11871 & NB=="BIG" & DA=="GOOD"]
  N_species[seed_id==11871 & nb=="MODERATE" & da=="GOOD"]
  richness.df[to_target_continent==max(richness.df$to_target_continent)]
  
  N_species[seed_id==11871 & nb=="BIG" & da=="GOOD"]
  
}

N.raw.Native<-richness.group[, c("label", "NB", "DA", "seed_id",
                                 "seed_continent", "in_source_continent",
                                 "Group")]

richness.group$per<-richness.group$to_target_continent/(richness.group$to_target_continent+richness.group$in_source_continent)

richness.group.N<-richness.group[,.(N=.N), by=c("Group", "seed_continent", "NB", "DA")]

ggplot(richness.group.N[!Group %in% c("<95%", ">100%")])+
  geom_point(aes(x=Group, y=N, color=seed_continent))+
  geom_text(aes(x=Group, y=N, label=N, color=seed_continent),
            vjust = -0.8, show.legend = FALSE)+
  facet_grid(DA~NB)


ggplot(richness.group[NB %in% c("BROAD", "BIG") & DA=="GOOD" & Group %in% c("95%-95.5%", "99%-99.5%")])+
  geom_hline(yintercept = 0.5, linetype=2)+
  geom_boxplot(aes(x=Group, y=per, color=seed_continent))+
  facet_grid(DA~NB)


richness.group[NB %in% c("BROAD", "BIG") & DA=="GOOD" & Group %in% c("99%-99.5%")]

diff.99.995<-richness[N.Usaged.99!=N.Usaged.995]

ggplot(diff.99.995)+geom_point(aes(x=N.Usaged.99, y=N.Usaged.995))

richness[between(N.Usaged.99,1,99) & N.Usaged.995==100 ]


View(richness[min.dist==63 & NB=="BROAD" & DA=="GOOD", 
         c("seed_continent", "seed_id", "NB", "DA", "N_SPECIES", 
           "N.Usaged.99", "N.Usaged.995",
           "to_target_continent", "in_source_continent")])


diff.99.995$contribution_target<-diff.99.995$to_target_continent*diff.99.995$N.Usaged.995 -
  diff.99.995$to_target_continent*diff.99.995$N.Usaged.99 

diff.99.995$contribution_source<-diff.99.995$in_source_continent*diff.99.995$N.Usaged.995 -
  diff.99.995$in_source_continent*diff.99.995$N.Usaged.99 

summary(diff.99.995$contribution_source)

range(diff.99.995$contribution_source)
range(diff.99.995$contribution_target)
diff.99.995[contribution_source>=100000]
diff.99.995[contribution_target>=100000]
