### Heatmaps ####

## have already calculated the % reduction in incidence and mortality for each run (all age and by age) and stored in 2050_reduction_incidence_kkk etc. Is stored for 2050 as primary outcome, and 2035 as secondary outcome. ##

##need to import these csvs and pull out the all-age data ##

C=0
#input
if (C == 0){vaccin<-"/Users/Rebecca/GatesVx/Vxoutput"}
if (C == 1){vaccin<-"/home/lsh355020/China_Gates/Vxoutput"}

#output 
if (C == 0){vaccout<-"/Users/Rebecca/GatesVx/Vxoutput/heatmap"}
if (C == 1){vaccout<-"/home/lsh355020/China_Gates/Vxoutput/heatmap"}

setwd(vaccin)

#number runs
rrun<-2

#source vx characteristics so know # of vaccine types
source('#vx.R')
numvx<-combn*typen

RImatrix<-c()
RImatrix35<-c()
RImatrixM<-c()

## call in matrices for reduction in incidence and mortality

for (xx in 1:rrun){
  
RIcall<-t(read.csv(paste("2050_reduction_incidence_",xx,".csv", sep='')))
RIcall<-cbind(RIcall,rep(xx,nrow(RIcall)))
RImatrix<-rbind(RImatrix,RIcall[,c(1,10:15)])

RIcall35<-t(read.csv(paste("2035_reduction_incidence_",xx,".csv", sep='')))
RIcall35<-cbind(RIcall35,rep(xx,nrow(RIcall35)))
RImatrix35<-rbind(RImatrix35,RIcall35[,c(1,10:15)])
  
RIcallM<-t(read.csv(paste("2050_reduction_mortality_",xx,".csv", sep='')))
RIcallM<-cbind(RIcallM,rep(xx,nrow(RIcallM)))
RImatrixM<-rbind(RImatrixM,RIcallM[,c(1,10:15)])

}

RImatrix<-round(RImatrix,2)
RImatrix35<-round(RImatrix35,2)
RImatrixM<-round(RImatrixM,2)

colnames(RImatrix)<-c("redu","type","VE_I","VE_D","dur","count","run")
colnames(RImatrix35)<-c("redu","type","VE_I","VE_D","dur","count","run")
colnames(RImatrixM)<-c("redu","type","VE_I","VE_D","dur","count","run")

write.table(RImatrix,"redu_inc_alldata.csv",sep=",",row.names = F)
write.table(RImatrixM,"redu_mort_alldata.csv",sep=",",row.names = F)



