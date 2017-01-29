#### calculate median characteristics to achieve given impact.

##Follows on from #heatmap

C=0
#input
if (C == 0){vaccin<-"/Users/Rebecca/GatesVx/Vxoutput"}
if (C == 1){vaccin<-"/home/lsh355020/China_Gates/Vxoutput"}

#output 
if (C == 0){vaccout<-"/Users/Rebecca/GatesVx/Vxoutput/heatmap"}
if (C == 1){vaccout<-"/home/lsh355020/China_Gates/Vxoutput/heatmap"}

#home
if (C == 0){home<-"/Users/Rebecca/GatesVx"}
if (C == 1){home<-"/home/lsh355020/China_Gates/"}

#want to keep from original data set, not from medians, as dont know how many points of the 1000 runs are in or out of the range if use median (ie.. could unfairly weight those one the edges of the boundaries), so subset from the RImatrix data set (NOT the med_RI set)

RImatrix<-as.data.frame(RImatrix)
chars2029<-subset(RImatrix,redu>=20 & redu<30)
chars3039<-subset(RImatrix,redu>=30 & redu<40)
chars4049<-subset(RImatrix,redu>=40 & redu<50)
chars5059<-subset(RImatrix,redu>=50 & redu<60)
chars6069<-subset(RImatrix,redu>=60 & redu<70)
chars7079<-subset(RImatrix,redu>=70 & redu<80)
chars8089<-subset(RImatrix,redu>=80 & redu<90)
chars90100<-subset(RImatrix,redu>=90 & redu<=100)

#need to subset by type

#loop for vaccine type and calc median and range of VE_I, VE_D and dur

avchars2029<-matrix(0,3,typen*3)
avchars3039<-matrix(0,3,typen*3)
avchars4049<-matrix(0,3,typen*3)
avchars5059<-matrix(0,3,typen*3)
avchars6069<-matrix(0,3,typen*3)
avchars7079<-matrix(0,3,typen*3)
avchars8089<-matrix(0,3,typen*3)
avchars90100<-matrix(0,3,typen*3)

#need to calc separately for PPI, PSI,PRI
for (kk in 1:typen){
# there are 3 characteristics need to average - VE_I, VE_D and dur
for (jj in 1:3){

subt2029<-subset(chars2029,type==kk)
avchars2029[1,(jj+(3*kk)-3)]<-median(subt2029[,(jj+2)])
avchars2029[2,(jj+(3*kk)-3)]<-min(subt2029[,(jj+2)])
avchars2029[3,(jj+(3*kk)-3)]<-max(subt2029[,(jj+2)])

subt3039<-subset(chars3039,type==kk)
avchars3039[1,(jj+(3*kk)-3)]<-median(subt3039[,(jj+2)])
avchars3039[2,(jj+(3*kk)-3)]<-min(subt3039[,(jj+2)])
avchars3039[3,(jj+(3*kk)-3)]<-max(subt3039[,(jj+2)])

subt4049<-subset(chars4049,type==kk)
avchars4049[1,(jj+(3*kk)-3)]<-median(subt4049[,(jj+2)])
avchars4049[2,(jj+(3*kk)-3)]<-min(subt4049[,(jj+2)])
avchars4049[3,(jj+(3*kk)-3)]<-max(subt4049[,(jj+2)])

subt5059<-subset(chars5059,type==kk)
avchars5059[1,(jj+(3*kk)-3)]<-median(subt5059[,(jj+2)])
avchars5059[2,(jj+(3*kk)-3)]<-min(subt5059[,(jj+2)])
avchars5059[3,(jj+(3*kk)-3)]<-max(subt5059[,(jj+2)])

subt6069<-subset(chars6069,type==kk)
avchars6069[1,(jj+(3*kk)-3)]<-median(subt6069[,(jj+2)])
avchars6069[2,(jj+(3*kk)-3)]<-min(subt6069[,(jj+2)])
avchars6069[3,(jj+(3*kk)-3)]<-max(subt6069[,(jj+2)])

subt7079<-subset(chars7079,type==kk)
avchars7079[1,(jj+(3*kk)-3)]<-median(subt7079[,(jj+2)])
avchars7079[2,(jj+(3*kk)-3)]<-min(subt7079[,(jj+2)])
avchars7079[3,(jj+(3*kk)-3)]<-max(subt7079[,(jj+2)])

subt8089<-subset(chars8089,type==kk)
avchars8089[1,(jj+(3*kk)-3)]<-median(subt8089[,(jj+2)])
avchars8089[2,(jj+(3*kk)-3)]<-min(subt8089[,(jj+2)])
avchars8089[3,(jj+(3*kk)-3)]<-max(subt8089[,(jj+2)])

subt90100<-subset(chars90100,type==kk)
avchars90100[1,(jj+(3*kk)-3)]<-median(subt90100[,(jj+2)])
avchars90100[2,(jj+(3*kk)-3)]<-min(subt90100[,(jj+2)])
avchars90100[3,(jj+(3*kk)-3)]<-max(subt90100[,(jj+2)])

}
}

avchars<-rbind(avchars2029,avchars3039,avchars4049,avchars5059,avchars6069,avchars7079,avchars8089,avchars90100)
avchars<-cbind(avchars,rep((seq(20,90,10)),each=3),rep(c("med","min","max"),times=8))
avchars<-as.data.frame(avchars)

setwd(vaccout)
write.table(avchars,"vx_characteristics_avg.csv",sep=",",row.names = F)
write.table(chars2029,"vx_characteristics_2029.csv",sep=",",row.names = F)
write.table(chars3039,"vx_characteristics_3039.csv",sep=",",row.names = F)
write.table(chars4049,"vx_characteristics_4049.csv",sep=",",row.names = F)
write.table(chars5059,"vx_characteristics_5059.csv",sep=",",row.names = F)
write.table(chars6069,"vx_characteristics_6069.csv",sep=",",row.names = F)
write.table(chars7079,"vx_characteristics_7079.csv",sep=",",row.names = F)
write.table(chars8089,"vx_characteristics_8089.csv",sep=",",row.names = F)
write.table(chars90100,"vx_characteristics_90100.csv",sep=",",row.names = F)


# for (ff in 2:8){
# colnames(paste("avchars",ff,"0",ff,"9",sep=''))<-c('type','VE_I','VE_D','type','VE_I','VE_D')
# }

colnames(avchars2029)<-rep(c('VE_I','VE_D','Dur'),typen)  

