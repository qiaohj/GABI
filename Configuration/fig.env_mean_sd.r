library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD22T_11/continental_movement/Script")
all_v_df<-readRDS("../Data/env_fullstat.rda")
#precipmon_av, precipmonmax_abs, precipmonmin_abs, tempmon_av, 
#tempmonmax_abs, tempmonmin_abs, cmm, drymon, wetmon, wmm, mixLyrDpth_ym_uo, 
#salinity_ym_dpth, temp_ym_dpth, lsm, field700, depthlevel, depthdepth, ht, 
#field150, field152, field153, field154, field174, 
#field175, precip_mm_srf, temp_mm_1_5m  

v<-c("precipmon_av", "tempmonmax_abs", "tempmonmin_abs")
item<-all_v_df[var %in% v]
item<-item[, .(v_mean=mean(v_mean)),
           by=list(age, var)]
item$y<-item$age * -1
setorderv(item, c("var", "y"), c(1, 1))
item$span<-0
item[c(2:(nrow(item)))]$span<-abs(item[c(1:(nrow(item)-1))]$y - item[c(2:(nrow(item)))]$y)
item[span>4 | span==0]$span<-4
max(item[span==1]$age)
table(item$span)/3
ggplot(item)+
  geom_vline(aes(xintercept=y, color=factor(span)), linetype=1, alpha=0.3)+
  #geom_point(aes(x=y, y=3))+
  geom_line(aes(x=y, y=v_mean))+
  facet_wrap(~var, nrow=3, scale="free")+
  theme_bw()

