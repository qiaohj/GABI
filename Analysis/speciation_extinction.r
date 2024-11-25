library(data.table)
library(ggplot2)
library(sf)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/GABI/Script")

t_folder<-"500k.NB_BROAD.speciation.years/"
df<-readRDS(sprintf("../Data/Tables/%s/c100.virtual.species.rda", t_folder))
df[year==1800, .(N=.N), by=list(seed_id, nb, da)]
df[year==1000]
df$sp_id_length<-str_length(df$sp_id)
df$sp_label<-sprintf("%s.%s.%s", df$sp_id, df$nb, df$da)
df[sp_id_length==max(df$sp_id_length)]

seeds<-unique(df$label)

seed<-"12844.NARROW-NARROW.GOOD"
first_year<-df[, .(first_year=max(year)), by=c("sp_label")]
df_with_first_year<-merge(df, first_year, by="sp_label")
df_with_first_year[sp_label=="6203-2.NARROW-NARROW.POOR"]

item<-df_with_first_year[label==seed]
setorderv(item, "year", -1)
item<-df_with_first_year
item[year==1800]$N<-1
orgin<-item[year==1800]
orgin<-unique(orgin[, c("seed_id", "continent")])
colnames(orgin)<-c("seed_id", "origin_continent")
item<-merge(item, orgin, by="seed_id")
#item<-item[N>0]
N_event_list<-list()
details<-list()
for (y in seq(1799, 0)){
  print(y)
  item_present<-item[year==y]
  item_previous<-item[year==(y+1)]
  
  item_present<-item_present[,c("continent", "sp_label", "N", "first_year", "nb", "da", "origin_continent")]
  colnames(item_present)<-c("continent", "sp_label", "N_present", "first_year", "nb", "da", "origin_continent")
  
  item_previous<-item_previous[,c("continent", "sp_label", "N", "first_year", "nb", "da", "origin_continent")]
  colnames(item_previous)<-c("continent", "sp_label", "N_previous", "first_year", "nb", "da", "origin_continent")
  
  m_item<-merge(item_previous, item_present, by=c("continent", "sp_label", "first_year", "nb", "da", "origin_continent"), all=T)
  m_item<-m_item[continent %in% c("North America", "South America")]
  m_item$speciation<-m_item$first_year==y
  m_item$previous<-!is.na(m_item$N_previous)
  m_item$present<-!is.na(m_item$N_present)
  m_item<-m_item[!(previous & present)]
  #m_item[previous==F & present==T & continent!=origin_continent]
  
  if (nrow(m_item)==0){
    next()
  }
  details[[length(details)+1]]<-m_item
  N_event<-m_item[, .(N=.N, present_year=y), 
                  by=list(continent, speciation, previous, present, nb, da, origin_continent)]
  N_event_list[[length(N_event_list)+1]]<-N_event
}
details<-rbindlist(details)
N_event_df<-rbindlist(N_event_list)

#if N_previous is NA, and present_year==first_year, it is a new species driven by speciation
#if present_year!=first_year, it is a new species dispersal from another continent.

N_event_df[continent==origin_continent]

#previous==T & present==F EXTINCT (continent==origin_continent: Local Extinction/Allochthonous Extinction)
#previous==F & present==T EXTINCT (continent==origin_continent: Local Speciation/Dispersal)

N_event_df$Type<-"NONE"

N_event_df[previous==T & present==F & continent==origin_continent]$Type<-"Local Extinction"
N_event_df[previous==T & present==F & continent!=origin_continent]$Type<-"Allochthonous Extinction"
N_event_df[previous==F & present==T & continent==origin_continent]$Type<-"Local Speciation"
N_event_df[previous==F & present==T & speciation==T & continent!=origin_continent]$Type<-"Allochthonous Speciation"
N_event_df[previous==F & present==T & speciation==F & continent!=origin_continent]$Type<-"Dispersal"

N<-N_event_df[, .(N=sum(N)), by=list(Type, origin_continent, nb, continent)]
N_SP<-details[,.(N_SP=length(unique(sp_label))), by=list(continent, nb, origin_continent)]

N<-merge(N_SP, N, by=c("continent", "origin_continent", "nb"))

N$Per<-N$N/N$N_SP
N[Per>1]
item<-details[previous==T & present==F & 
          origin_continent=="North America" & continent=="North America" &
          nb=="NARROW-NARROW"]
details[sp_label=="6203-2.NARROW-NARROW.POOR"]
N_sp_label<-item[, .(N_splabel=.N), by=list(sp_label)]
range(N$Per)

N$Label<-sprintf("%s->%s", N$origin_continent, N$continent)

p<-ggplot(N)+geom_point(aes(x=nb, y=Per, color=Label))+
  facet_wrap(~Type)+
  scale_y_log10()+
  theme(axis.text.x=element_text(angle = -90, hjust = 0),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        color = "lightgrey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        color="lightgrey"))
p
ggsave(p, filename="../Figures/event_per.png", width=15, height=6)

p<-ggplot(N)+geom_point(aes(x=nb, y=N, color=Label))+
  facet_wrap(~Type)+
  scale_y_log10()+
  theme(axis.text.x=element_text(angle = -90, hjust = 0),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        color = "lightgrey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        color="lightgrey"))
p
ggsave(p, filename="../Figures/event_N.png", width=15, height=6)
