#### PAF calc ####

library(plyr)

C=1
if (C == 0){home<-"/Users/Rebecca/Vaccine_china"}
if (C == 1){home<-"/home/lsh355020/China_VX/"}

if (C == 0){input<-"/Users/Rebecca/Vaccine_china/Data"}
if (C == 1){input<-"/home/lsh355020/China_VX/Data"}

if (C == 0){output<-"/Users/Rebecca/Vaccine_china/Outputs"}
if (C == 1){output<-"/home/lsh355020/China_VX/PAF"}


setwd(home)

source('#DataGrab.R')
setwd(home)
source('CFunctions_PAF.R')

setwd(home);setwd(input)
para<-as.matrix(drop.levels(read.csv('para1000fit.csv',header=TRUE,check.names=F)))

nm<-c(pararange[,1],"p0") # The parameter ranges


# What country? (For you this will always be China for now)
cntry<-"China"

setwd(home)

#PAF
rrun<-1000 # will be the 1000 selected runs
xout<-c(); eee<-c(); # Initialise all vectors to be empty

PAF<-matrix(0,15,rrun)


for (yyy in c(2000,2025,2050)){

for (aaa in 1:5){  
  print(c(yyy,aaa))
  
for (kkk in 1:rrun){
  print(kkk)
  for (i in 1:length(nm)){assign(nm[i],as.numeric(para[kkk,i]))} # Assign the parameters to the correct values
  neta2<-neta # this parameter needs extra assigning for some annoying reason! 
  
  # Run the model with these parameters  
  #4th setof terms, second entry of that term is the timestep
  system.time(Xn<-FitGo(cntry,1,c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE,alpha),c(2,(1/2),c(0.02,0.02,0.8,0.07)),c(1900,2050),0,0))   
  
}
#end of age loop
}
#end of years loop
}


PAFpc<-matrix(0,12,rrun)
#2000
PAFpc[1,]<-100*((PAF[13,]-PAF[1,])/PAF[13,])
PAFpc[2,]<-100*((PAF[13,]-PAF[2,])/PAF[13,])
PAFpc[3,]<-100*((PAF[13,]-PAF[3,])/PAF[13,])
PAFpc[4,]<-100*((PAF[13,]-PAF[4,])/PAF[13,])
#2025
PAFpc[5,]<-100*((PAF[14,]-PAF[5,])/PAF[14,])
PAFpc[6,]<-100*((PAF[14,]-PAF[6,])/PAF[14,])
PAFpc[7,]<-100*((PAF[14,]-PAF[7,])/PAF[14,])
PAFpc[8,]<-100*((PAF[14,]-PAF[8,])/PAF[14,])
#2050
PAFpc[9,]<-100*((PAF[15,]-PAF[9,])/PAF[15,])  
PAFpc[10,]<-100*((PAF[15,]-PAF[10,])/PAF[15,])
PAFpc[11,]<-100*((PAF[15,]-PAF[11,])/PAF[15,])
PAFpc[12,]<-100*((PAF[15,]-PAF[12,])/PAF[15,])
setwd(output)
write.table(PAF,'PAF.csv',sep=",",row.names=FALSE)
write.table(PAFpc,'PAF_percent.csv',sep=",",row.names=FALSE)

PAF_range<-matrix(0,12,3)
#2000
PAF_range[1,]<-c(median(PAFpc[1,]),min(PAFpc[1,]),max(PAFpc[1,]))
PAF_range[2,]<-c(median(PAFpc[2,]),min(PAFpc[2,]),max(PAFpc[2,]))
PAF_range[3,]<-c(median(PAFpc[3,]),min(PAFpc[3,]),max(PAFpc[3,]))
PAF_range[4,]<-c(median(PAFpc[4,]),min(PAFpc[4,]),max(PAFpc[4,]))
#2025
PAF_range[5,]<-c(median(PAFpc[5,]),min(PAFpc[5,]),max(PAFpc[5,]))
PAF_range[6,]<-c(median(PAFpc[6,]),min(PAFpc[6,]),max(PAFpc[6,]))
PAF_range[7,]<-c(median(PAFpc[7,]),min(PAFpc[7,]),max(PAFpc[7,]))
PAF_range[8,]<-c(median(PAFpc[8,]),min(PAFpc[8,]),max(PAFpc[8,]))
#2050
PAF_range[9,]<-c(median(PAFpc[9,]),min(PAFpc[9,]),max(PAFpc[9,]))
PAF_range[10,]<-c(median(PAFpc[10,]),min(PAFpc[10,]),max(PAFpc[10,]))
PAF_range[11,]<-c(median(PAFpc[11,]),min(PAFpc[11,]),max(PAFpc[11,]))                                
PAF_range[12,]<-c(median(PAFpc[12,]),min(PAFpc[12,]),max(PAFpc[12,]))

setwd(output)
write.table(PAF_range,'PAFpc_range.csv',sep=",",row.names=FALSE)



