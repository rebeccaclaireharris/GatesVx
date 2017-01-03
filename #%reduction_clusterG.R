#### Calculate % reduction in I and M in 2050 ####

#set up how to find first row of each run
#typen is -1 as dont use the first one at the moment
lastcol<-(((typen-1)*(combn))+1)*steps
y1<-seq(1,lastcol,steps)

pcreduI<-matrix(0,9,length(vacnames))
pcreduM<-matrix(0,9,length(vacnames))

#calc the difference between baseline and 2050 result for each vaccine, and calc percentage reduction. y1 is minus 1 to get rid of the baseline number
pcredrow=c("All","0-14years","15-54years","55-64years","≥65years", "<55 years","55+ years","15-24years","25-54years")
agecols<-c("black","red","blue","purple","green")
pcreduI[1,]<-100*((rrun_dfvx[151,15]-rrun_dfvx[(y1[-1]+150),15]))/(rrun_dfvx[151,15])
pcreduI[2,]<-100*((rrun_dfvx[151,16]-rrun_dfvx[(y1[-1]+150),16]))/(rrun_dfvx[151,16])
pcreduI[3,]<-100*((rrun_dfvx[151,17]-rrun_dfvx[(y1[-1]+150),17]))/(rrun_dfvx[151,17])
pcreduI[4,]<-100*((rrun_dfvx[151,18]-rrun_dfvx[(y1[-1]+150),18]))/(rrun_dfvx[151,18])
pcreduI[5,]<-100*((rrun_dfvx[151,19]-rrun_dfvx[(y1[-1]+150),19]))/(rrun_dfvx[151,19])
pcreduI[6,]<-100*((rrun_dfvx[151,21]-rrun_dfvx[(y1[-1]+150),21]))/(rrun_dfvx[151,21])
pcreduI[7,]<-100*((rrun_dfvx[151,20]-rrun_dfvx[(y1[-1]+150),20]))/(rrun_dfvx[151,20])
pcreduI[8,]<-100*((rrun_dfvx[151,89]-rrun_dfvx[(y1[-1]+150),89]))/(rrun_dfvx[151,89])
pcreduI[9,]<-100*((rrun_dfvx[151,90]-rrun_dfvx[(y1[-1]+150),90]))/(rrun_dfvx[151,90])

colnames(pcreduI)<-vacnames
rownames(pcreduI)<-pcredrow

###chk numberd
pcreduM[1,]<-100*((rrun_dfvx[151,29]-rrun_dfvx[(y1[-1]+150),29]))/(rrun_dfvx[151,29])
pcreduM[2,]<-100*((rrun_dfvx[151,30]-rrun_dfvx[(y1[-1]+150),30]))/(rrun_dfvx[151,30])
pcreduM[3,]<-100*((rrun_dfvx[151,31]-rrun_dfvx[(y1[-1]+150),31]))/(rrun_dfvx[151,31])
pcreduM[4,]<-100*((rrun_dfvx[151,32]-rrun_dfvx[(y1[-1]+150),32]))/(rrun_dfvx[151,32])
pcreduM[5,]<-100*((rrun_dfvx[151,33]-rrun_dfvx[(y1[-1]+150),33]))/(rrun_dfvx[151,33])
pcreduM[6,]<-100*((rrun_dfvx[151,37]-rrun_dfvx[(y1[-1]+150),37]))/(rrun_dfvx[151,37])
pcreduM[7,]<-100*((rrun_dfvx[151,36]-rrun_dfvx[(y1[-1]+150),36]))/(rrun_dfvx[151,36])
pcreduM[8,]<-100*((rrun_dfvx[151,91]-rrun_dfvx[(y1[-1]+150),91]))/(rrun_dfvx[151,91])
pcreduM[9,]<-100*((rrun_dfvx[151,92]-rrun_dfvx[(y1[-1]+150),92]))/(rrun_dfvx[151,92])

colnames(pcreduM)<-vacnames
rownames(pcreduM)<-pcredrow

setwd("Vaccoutput")
write.table(pcreduI,paste('2050_reduction_incidence_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
write.table(pcreduM,paste('2050_reduction_mortality_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
setwd(home)
