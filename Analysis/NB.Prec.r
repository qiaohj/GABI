library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
simulations<-data.table(dbReadTable(conn, "simulations"))
dbDisconnect(conn)

df<-simulations[is_run==1]
df[, .(N=length(unique(global_id))), by=list(nb, da, continent)]

df<-df[, c("global_id", "nb", "da", "continent", "environments", "nb_v")]
df$nb_v<-gsub("\\|", ",", df$nb_v)
target_cols <- c("tmin", "tmax", "pmin", "pmax")
df[, (target_cols):=tstrsplit(df$nb_v, ",", fixed = TRUE, type.convert = T,
                             keep = c(1, 2, 5, 6))]
hexagon_ns<-read_sf("../Shape/isea3h8/N_S_America.shp")
area<-st_area(hexagon_ns)
df_prec<-merge(hexagon_ns, df[], by.x="seqnum", by.y="global_id")
ggplot()+ 
  geom_sf(data=hexagon_ns,  aes(fill=continent),
          color=NA, linewidth=0.1) +
  geom_sf(data=lines, linetype=2)+
  coord_sf(crs = st_crs(crs_america))+
  #geom_hline(yintercept = c(-40, 40), linetype=2)+
  #geom_hline(yintercept = c(-22, 14, 50), linetype=2)+
  scale_fill_manual (breaks=c("North America", "South America",
                              "bridge1", "bridge2"),
                     labels=c("North America", "South America",
                              "bridge1", "bridge2"),
                     values=c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f"))+
  theme(
    axis.line = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
    #panel.grid.minor = element_line(color="black"),
    plot.background = element_rect(fill="#fde7c0"),
    panel.background = element_rect(fill="#fde7c0"),
    legend.background = element_rect(fill = "#fde7c0", color = NA),
    legend.title = element_blank(),
    #panel.border = element_blank(),
    legend.position="bottom",
    #legend.key.width=unit(0.8,"in"),
    #strip.background.x = element_blank(),
    #strip.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


df$folder<-sprintf("/media/huijieqiao/Butterfly/GABI/Results/%d.%s.%s",
                   df$global_id, df$nb, df$da)
df$folder2<-sprintf("/media/huijieqiao/Butterfly/GABI/Results/%d.%s.%s/unfinished.txt",
                   df$global_id, df$nb, df$da)

df$finished<-ifelse(dir.exists(df$folder), "Finished", "Queue")
df[finished=="Finished", 
   finished:=ifelse(file.exists(df[finished=="Finished"]$folder2), "Unfinished", "Finished")]

table(df$finished)


df.sub<-df[finished=="Finished"]
df.sub.N<-df.sub[,.(N=.N), by=list(global_id, continent)]

NNN<-df.sub.N[N>=7]
table(NNN$continent)

seed_id1<-df.sub.N[N==8 | continent=="South America"]
seed_id2<-df.sub.N[N==7 & continent=="North America"]
seed_id2<-seed_id2[sample(seed_id2), ]