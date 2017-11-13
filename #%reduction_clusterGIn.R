#### Calculate % reduction in I and M in 2050 ####

#set up how to find first row of each run
lastcol<-(((typen)*(combn))+1)*steps
y1<-seq(1,lastcol,steps)

pcreduI<-matrix(0,14,length(vacnames))
pcreduI35<-matrix(0,14,length(vacnames))
#pcreduI2<-matrix(0,9,length(vacnames))
pcreduM<-matrix(0,14,length(vacnames))
pcreduM35<-matrix(0,14,length(vacnames))

#calc the difference between baseline and 2050 result for each vaccine, and calc percentage reduction. y1 is minus 1 to get rid of the baseline number
pcredrow=c("All","0-14years","15-54years","55-64years","≥65years", "<55 years","55+ years","15-24years","25-54years","type","VE_I","VE_D","dur","count")
agecols<-c("black","red","blue","purple","green")

#2050
pcreduI[1,]<-100*((rrun_dfvx[151,15]-rrun_dfvx[(y1[-1]+150),15]))/(rrun_dfvx[151,15])
pcreduI[2,]<-100*((rrun_dfvx[151,16]-rrun_dfvx[(y1[-1]+150),16]))/(rrun_dfvx[151,16])
pcreduI[3,]<-100*((rrun_dfvx[151,17]-rrun_dfvx[(y1[-1]+150),17]))/(rrun_dfvx[151,17])
pcreduI[4,]<-100*((rrun_dfvx[151,18]-rrun_dfvx[(y1[-1]+150),18]))/(rrun_dfvx[151,18])
pcreduI[5,]<-100*((rrun_dfvx[151,19]-rrun_dfvx[(y1[-1]+150),19]))/(rrun_dfvx[151,19])
pcreduI[6,]<-100*((rrun_dfvx[151,21]-rrun_dfvx[(y1[-1]+150),21]))/(rrun_dfvx[151,21])
pcreduI[7,]<-100*((rrun_dfvx[151,20]-rrun_dfvx[(y1[-1]+150),20]))/(rrun_dfvx[151,20])
pcreduI[8,]<-100*((rrun_dfvx[151,89]-rrun_dfvx[(y1[-1]+150),89]))/(rrun_dfvx[151,89])
pcreduI[9,]<-100*((rrun_dfvx[151,90]-rrun_dfvx[(y1[-1]+150),90]))/(rrun_dfvx[151,90])
pcreduI[10:14,]<-t(rrun_dfvx[y1[-1],c("type","VE_I","VE_D","dur","count")])

#2035
pcreduI35[1,]<-100*((rrun_dfvx[136,15]-rrun_dfvx[(y1[-1]+135),15]))/(rrun_dfvx[136,15])
pcreduI35[2,]<-100*((rrun_dfvx[136,16]-rrun_dfvx[(y1[-1]+135),16]))/(rrun_dfvx[136,16])
pcreduI35[3,]<-100*((rrun_dfvx[136,17]-rrun_dfvx[(y1[-1]+135),17]))/(rrun_dfvx[136,17])
pcreduI35[4,]<-100*((rrun_dfvx[136,18]-rrun_dfvx[(y1[-1]+135),18]))/(rrun_dfvx[136,18])
pcreduI35[5,]<-100*((rrun_dfvx[136,19]-rrun_dfvx[(y1[-1]+135),19]))/(rrun_dfvx[136,19])
pcreduI35[6,]<-100*((rrun_dfvx[136,21]-rrun_dfvx[(y1[-1]+135),21]))/(rrun_dfvx[136,21])
pcreduI35[7,]<-100*((rrun_dfvx[136,20]-rrun_dfvx[(y1[-1]+135),20]))/(rrun_dfvx[136,20])
pcreduI35[8,]<-100*((rrun_dfvx[136,89]-rrun_dfvx[(y1[-1]+135),89]))/(rrun_dfvx[136,89])
pcreduI35[9,]<-100*((rrun_dfvx[136,90]-rrun_dfvx[(y1[-1]+135),90]))/(rrun_dfvx[136,90])
pcreduI35[10:14,]<-t(rrun_dfvx[y1[-1],c("type","VE_I","VE_D","dur","count")])


colnames(pcreduI)<-vacnames
rownames(pcreduI)<-pcredrow
colnames(pcreduI35)<-vacnames
rownames(pcreduI35)<-pcredrow


#pcreduI2[1,]<-100*((inc2050[1,1]-inc2050[2:nrow(inc2050),1])/inc2050[1,1])


###chk numberd
#2050
pcreduM[1,]<-100*((rrun_dfvx[151,29]-rrun_dfvx[(y1[-1]+150),29]))/(rrun_dfvx[151,29])
pcreduM[2,]<-100*((rrun_dfvx[151,30]-rrun_dfvx[(y1[-1]+150),30]))/(rrun_dfvx[151,30])
pcreduM[3,]<-100*((rrun_dfvx[151,31]-rrun_dfvx[(y1[-1]+150),31]))/(rrun_dfvx[151,31])
pcreduM[4,]<-100*((rrun_dfvx[151,32]-rrun_dfvx[(y1[-1]+150),32]))/(rrun_dfvx[151,32])
pcreduM[5,]<-100*((rrun_dfvx[151,33]-rrun_dfvx[(y1[-1]+150),33]))/(rrun_dfvx[151,33])
pcreduM[6,]<-100*((rrun_dfvx[151,37]-rrun_dfvx[(y1[-1]+150),37]))/(rrun_dfvx[151,37])
pcreduM[7,]<-100*((rrun_dfvx[151,36]-rrun_dfvx[(y1[-1]+150),36]))/(rrun_dfvx[151,36])
pcreduM[8,]<-100*((rrun_dfvx[151,91]-rrun_dfvx[(y1[-1]+150),91]))/(rrun_dfvx[151,91])
pcreduM[9,]<-100*((rrun_dfvx[151,92]-rrun_dfvx[(y1[-1]+150),92]))/(rrun_dfvx[151,92])
pcreduM[10:14,]<-t(rrun_dfvx[y1[-1],c("type","VE_I","VE_D","dur","count")])


#2035
pcreduM35[1,]<-100*((rrun_dfvx[136,29]-rrun_dfvx[(y1[-1]+135),29]))/(rrun_dfvx[136,29])
pcreduM35[2,]<-100*((rrun_dfvx[136,30]-rrun_dfvx[(y1[-1]+135),30]))/(rrun_dfvx[136,30])
pcreduM35[3,]<-100*((rrun_dfvx[136,31]-rrun_dfvx[(y1[-1]+135),31]))/(rrun_dfvx[136,31])
pcreduM35[4,]<-100*((rrun_dfvx[136,32]-rrun_dfvx[(y1[-1]+135),32]))/(rrun_dfvx[136,32])
pcreduM35[5,]<-100*((rrun_dfvx[136,33]-rrun_dfvx[(y1[-1]+135),33]))/(rrun_dfvx[136,33])
pcreduM35[6,]<-100*((rrun_dfvx[136,37]-rrun_dfvx[(y1[-1]+135),37]))/(rrun_dfvx[136,37])
pcreduM35[7,]<-100*((rrun_dfvx[136,36]-rrun_dfvx[(y1[-1]+135),36]))/(rrun_dfvx[136,36])
pcreduM35[8,]<-100*((rrun_dfvx[136,91]-rrun_dfvx[(y1[-1]+135),91]))/(rrun_dfvx[136,91])
pcreduM35[9,]<-100*((rrun_dfvx[136,92]-rrun_dfvx[(y1[-1]+135),92]))/(rrun_dfvx[136,92])
pcreduM35[10:14,]<-t(rrun_dfvx[y1[-1],c("type","VE_I","VE_D","dur","count")])


colnames(pcreduM)<-vacnames
rownames(pcreduM)<-pcredrow
colnames(pcreduM35)<-vacnames
rownames(pcreduM35)<-pcredrow

setwd("Vxoutput")
write.table(pcreduI,paste('2050_reduction_incidence_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
write.table(pcreduI35,paste('2035_reduction_incidence_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
write.table(pcreduM,paste('2050_reduction_mortality_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
write.table(pcreduM35,paste('2035_reduction_mortality_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
setwd(home)
