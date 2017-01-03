### calc of average and ranges for NNV from cluster ###
library(ggplot2)

C=0
#input
if (C == 0){vaccin<-"/Users/Rebecca/GatesVx/Vxoutput"}
if (C == 1){vaccin<-"/home/lsh355020/China_Gates/Vxoutput"}

#output 
if (C == 0){vaccout<-"/Users/Rebecca/GatesVx/Vxoutput/NNV"}
if (C == 1){vaccout<-"/home/lsh355020/China_Gates/Vxoutput/NNV"}

setwd(vaccin)
rrun<-1000
period_NNV_CA<-matrix(0,rrun,96)
period_NNV_DA<-matrix(0,rrun,96)

vaccna<-read.csv('period_NNV_case_1.csv',check.names=T)
vaccnames<-colnames(vaccna)

colnames(period_NNV_CA)<-vaccnames
colnames(period_NNV_DA)<-vaccnames



for (ii in 1:rrun){
  
  period_NNV_CA[ii,]<-as.matrix(read.csv(paste('period_NNV_case_',ii,'.csv', sep=''),check.names=F))
  period_NNV_DA[ii,]<-as.matrix(read.csv(paste('period_NNV_death_',ii,'.csv', sep=''),check.names=F))
  
#   NNV_CA[ii,]<-as.matrix(read.csv(paste('annual_NNV_case_',ii,'.csv', sep=''),check.names=F))
#   Num_CA[ii,]<-as.matrix(read.csv(paste('annual_cases_averted_',ii,'.csv',sep=''),check.names=F))
#   Age_CA[ii,]<-as.matrix(read.csv(paste('age_cases_averted_',ii,'.csv',sep=''),check.names=F))

}


period_NNV_CA_range<-matrix(0,3,96)
period_NNV_DA_range<-matrix(0,3,96)

for (jj in 1:96){
  
  period_NNV_CA_range[,jj]<-c(median(period_NNV_CA[,jj]),min(period_NNV_CA[,jj]),max(period_NNV_CA[,jj]))
  period_NNV_DA_range[,jj]<-c(median(period_NNV_DA[,jj]),min(period_NNV_DA[,jj]),max(period_NNV_DA[,jj]))
   
}


colnames(period_NNV_CA_range)<-vaccnames
colnames(period_NNV_DA_range)<-vaccnames

period_NNV_CA_r_ord<-cbind(period_NNV_CA_range[,61:72],period_NNV_CA_range[,25:36],period_NNV_CA_range[,37:48], period_NNV_CA_range[,1:12], period_NNV_CA_range[,73:84], period_NNV_CA_range[,85:96], period_NNV_CA_range[,49:60], period_NNV_CA_range[,13:24])

period_NNV_DA_r_ord<-cbind(period_NNV_DA_range[,61:72],period_NNV_DA_range[,25:36],period_NNV_DA_range[,37:48], period_NNV_DA_range[,1:12], period_NNV_DA_range[,73:84], period_NNV_DA_range[,85:96], period_NNV_DA_range[,49:60], period_NNV_DA_range[,13:24])


setwd(vaccout)
write.table(period_NNV_CA_r_ord,'period_NNV_CA_range_ordered.csv',sep=",",row.names=FALSE)
write.table(period_NNV_DA_r_ord,'period_NNV_DA_range_ordered.csv',sep=",",row.names=FALSE)




