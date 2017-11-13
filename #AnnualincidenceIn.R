#### checking vaccines by year ###

setwd(vaccin)
yrinc_D2PPI<-matrix(0, 31, rrun)
yrinc_D3PPI<-matrix(0, 31, rrun)
yrinc_D5PPI<-matrix(0, 31, rrun)
yrinc_D7PPI<-matrix(0, 31, rrun)
yrinc_D10PPI<-matrix(0, 31, rrun)

yrinc_D2PPI5<-matrix(0, 31, rrun)
yrinc_D3PPI5<-matrix(0, 31, rrun)
yrinc_D5PPI5<-matrix(0, 31, rrun)
yrinc_D7PPI5<-matrix(0, 31, rrun)
yrinc_D10PPI5<-matrix(0, 31, rrun)
  

 # will be the 1000 selected runs
for (kkk in 1:rrun){
  print(kkk)
  #kkk<-1
  
  inc_yr<-read.csv(paste("inc_rates_Yr_",kkk,".csv",sep=""),sep=",",header=T)
  inc_yr<-as.data.frame(inc_yr)
  
  
  ##call in the annual incidenc for 2025-2050 for PPI VE for each = 100%
  yrinc_D2PPI[,kkk]<-inc_yr[1:31,1082]
  yrinc_D3PPI[,kkk]<-inc_yr[1:31,1083]
  yrinc_D5PPI[,kkk]<-inc_yr[1:31,1084]
  yrinc_D7PPI[,kkk]<-inc_yr[1:31,1085]
  yrinc_D10PPI[,kkk]<-inc_yr[1:31,1086]
  
  ##call in the annual incidenc for 2025-2050 for PPI VE for each = 50%
  yrinc_D2PPI5[,kkk]<-inc_yr[1:31,542]
  yrinc_D3PPI5[,kkk]<-inc_yr[1:31,543]
  yrinc_D5PPI5[,kkk]<-inc_yr[1:31,544]
  yrinc_D7PPI5[,kkk]<-inc_yr[1:31,545]
  yrinc_D10PPI5[,kkk]<-inc_yr[1:31,546]
  

  
}

#calculate medians and range
med_yrinc100<-matrix(0,31,15)
med_yrinc50<-matrix(0,31,15)



for (jj in 1:31){

# TBN
med_yrinc100[jj,c(1:3)]<-c(median(yrinc_D2PPI[jj,]),min(yrinc_D2PPI[jj,]),max(yrinc_D2PPI[jj,]))
med_yrinc100[jj,c(4:6)]<-c(median(yrinc_D3PPI[jj,]),min(yrinc_D3PPI[jj,]),max(yrinc_D3PPI[jj,]))
med_yrinc100[jj,c(7:9)]<-c(median(yrinc_D5PPI[jj,]),min(yrinc_D5PPI[jj,]),max(yrinc_D5PPI[jj,]))
med_yrinc100[jj,c(10:12)]<-c(median(yrinc_D7PPI[jj,]),min(yrinc_D7PPI[jj,]),max(yrinc_D7PPI[jj,]))
med_yrinc100[jj,c(13:15)]<-c(median(yrinc_D10PPI[jj,]),min(yrinc_D10PPI[jj,]),max(yrinc_D10PPI[jj,]))

med_yrinc50[jj,c(1:3)]<-c(median(yrinc_D2PPI5[jj,]),min(yrinc_D2PPI5[jj,]),max(yrinc_D2PPI5[jj,]))
med_yrinc50[jj,c(4:6)]<-c(median(yrinc_D3PPI5[jj,]),min(yrinc_D3PPI5[jj,]),max(yrinc_D3PPI5[jj,]))
med_yrinc50[jj,c(7:9)]<-c(median(yrinc_D5PPI5[jj,]),min(yrinc_D5PPI5[jj,]),max(yrinc_D5PPI5[jj,]))
med_yrinc50[jj,c(10:12)]<-c(median(yrinc_D7PPI5[jj,]),min(yrinc_D7PPI5[jj,]),max(yrinc_D7PPI5[jj,]))
med_yrinc50[jj,c(13:15)]<-c(median(yrinc_D10PPI5[jj,]),min(yrinc_D10PPI5[jj,]),max(yrinc_D10PPI5[jj,]))

}



Year<-seq(2020,2050,1)
med_yrinc100<-as.data.frame(cbind(Year,med_yrinc100))
med_yrinc50<-as.data.frame(cbind(Year,med_yrinc50))

write.table(med_yrinc100,"med_yrinc100.csv",sep=",",row.names=F)
write.table(med_yrinc50,"med_yrinc50.csv",sep=",",row.names=F)


###notification plots  ####

ggYrinc<-ggplot(med_yrinc100, aes(x=Year, y=med_yrinc100[,2])) +
  theme_classic() +
  geom_ribbon(aes(ymin = med_yrinc100[,3], ymax = med_yrinc100[,4], width=3), fill = "grey30") +
  geom_line(aes(y = med_yrinc100[,2]), colour="black") +
  geom_ribbon(aes(ymin = med_yrinc100[,6], ymax = med_yrinc100[,7], width=3), fill = "yellow", alpha=0.3) +
  geom_line(aes(y = med_yrinc100[,5]), colour="yellow") +
  geom_ribbon(aes(ymin = med_yrinc100[,9], ymax = med_yrinc100[,10], width=3), fill = "blue", alpha=0.3) +
  geom_line(aes(y = med_yrinc100[,8]), colour="blue") +
  geom_ribbon(aes(ymin = med_yrinc100[,12], ymax = med_yrinc100[,13], width=3), fill = "red", alpha=0.3) +
  geom_line(aes(y = med_yrinc100[,11]), colour="red") +
  geom_ribbon(aes(ymin = med_yrinc100[,15], ymax = med_yrinc100[,16], width=3), fill = "green", alpha=0.3) +
  geom_line(aes(y = med_yrinc100[,14]), colour="green") +
  scale_y_continuous(expand=c(0,0)) +
#   scale_x_continuous(expand=c(0,0)) +
  coord_cartesian(ylim = c(0, 70)) +  
#   coord_cartesian(xlim = c(2020, 2050)) +  
  labs(x="Year",y="All-age incidence rate (/100,000 population)") +
  labs(x="Year",y="All-age incidence rate (/100,000 population)") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black", linetype = "solid")) +
  theme(axis.line.y = element_line(size = 0.5, colour = "black", linetype = "solid")) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank()) +
  theme(legend.position="right")


ggsave("ggYrinc.pdf", plot = ggYrinc, width = 16, height = 9, dpi = 120)



##work in progress:
# library(reshape)
# dd_sub = med_yrinc100[,c(1,2,5,8,11,14)]
# dd_subrib = med_yrinc100[,c(1,3,4,6,7,9,10,12,13,15,16)]
# colnames(dd_sub)<-c("Year","2 years","3 years","5 years","7 years","10 years")
# colnames(dd_subrib)<-c("Year","2 yearsl","2 yearsu","3 yearsl","3 yearsu","5 yearsl","5 yearsu","7 yearsl","7 yearsu","10 yearsl","10 yearsu")
# ##Then rearrange your data frame
# dd = melt(dd_sub, id=c("Year"))
# ddrib = melt(dd_subrib, id=c("Year"))
# ggYrinc2<-ggplot(dd) + geom_line(aes(x=Year, y=value, colour=variable)) +
#   scale_colour_manual(values=c("red","green","blue","yellow","black")) +
#   geom_ribbon(aes(ymin = ddrib[,3], ymax = med_yrinc100[,4], width=3), fill = "grey30")
# 
# ggYrinc2



