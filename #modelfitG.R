#### FInal plots of model fit


# install.packages('leaflet')
# install.packages('dplyr')
library(plyr)
library(grid)
library(ggplot2)
library(leaflet)
library(gtable)  # ver 0.2.0
library(grid)    # ver 3.3.1
library(cowplot)
#library(gridExtra) # for using gri arrange
# library(dplyr)
# install.package(grDevices)
# install.packages(colorRamps)

C=1
if (C == 0){home<-"/Users/Rebecca/Vaccine_china"}
if (C == 1){home<-"/home/lsh355020/China_VX/"}

if (C == 0){input<-"/Users/Rebecca/Vaccine_china/Vaccoutput/160913"}
if (C == 1){input<-"/home/lsh355020/China_VX/Data"}

if (C == 0){output<-"/Users/Rebecca/Vaccine_china/Vaccoutput"}
if (C == 1){output<-"/home/lsh355020/China_VX/Epi"}


### Data fitted to ###



population2010<- c(1359822,246707,863710,135859,113546)
population2050<- c(1384976,204187,623982,225492,331315)
population2025<-195605                                           #just elderly
population2050u<-c(3867612,300552,722200,248041,364447)
population2050l<-c(1153148,124968,527054,202943,298184)

incidence2010<- c(78)
incidence2010u<- c(83)
incidence2010l<- c(72)

#noitfication lower limit is the reported notifications, notification upper limit assumes that all from private hospitals are missed from notification reporting (80% of cases reported to CDC, 20% private hospital)  
notif2010 <- c(63.91,2.72,64.62,104.36,143.07)
notif2010u<- c(63.91,2.72,64.62,104.36,143.07)*1.2
#notif2010l<- c(63.91,2.72,64.62,104.36,143.07)
notif2010l<- c(63.91,2.72,64.62,104.36,143.07)*0.8


prevalence2000<- c(178,92,155,596)
prevalence2000u<- c(195,116,189,698)
prevalence2000l<- c(163,72,126,510)

prevalence2010<- c(116,59,99,346)
prevalence2010u<- c(132,86,129,407)
prevalence2010l<- c(101,40,77,294)

mortality2010<- c(3.37,0.29,1.91,15.69)
mortality2010u<- mortality2010*1.5
mortality2010l<- mortality2010*0.5



setwd(home)

source('#DataGrab.R')
setwd(home)
source('CFunctions_final.R')

setwd(home);setwd(input)
para<-as.matrix(drop.levels(read.csv('para1000fit.csv',header=TRUE,check.names=F)))

nm<-c(pararange[,1],"p0") # The parameter ranges


# What country? (For you this will always be China for now)
cntry<-"China"

setwd(home)

rrun<-4

# initiate matrices
yrTBNtot <- matrix(0, 51, rrun)
yrTBN_014 <- matrix(0, 51, rrun)
yrTBN_1554 <- matrix(0, 51, rrun)
yrTBN_5564 <- matrix(0, 51, rrun)
yrTBN_65 <- matrix(0, 51, rrun)

  yrTBMtot<- matrix(0, 51, rrun)
  yrTBM_014<- matrix(0, 51, rrun)
  yrTBM_1559<- matrix(0, 51, rrun)
  yrTBM_60<- matrix(0, 51, rrun)
  
  yrTBPbtot<- matrix(0, 51, rrun)
  yrTBPb_1529<- matrix(0, 51, rrun)
  yrTBPb_3059<- matrix(0, 51, rrun)
  yrTBPb_60<- matrix(0, 51, rrun)


  yrTBItot<- matrix(0, 51, rrun)



 # will be the 1000 selected runs
xout<-c(); eee<-c(); # Initialise all vectors to be empty
for (kkk in 1:rrun){
  print(kkk)
  #kkk<-1
  for (i in 1:length(nm)){assign(nm[i],as.numeric(para[kkk,i]))} # Assign the parameters to the correct values
  neta2<-neta # this parameter needs extra assigning for some annoying reason! 
  
  # Run the model with these parameters  
  #4th setof terms, second entry of that term is the timestep
  system.time(Xn<-FitGo(cntry,1,c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE,alpha),c(2,(1/2),c(0.02,0.02,0.8,0.07)),c(1900,2050),0,0))   
  
  Xn<-as.data.frame(Xn)
  
  ### by year ###
  
  ##calc annual 
  yrTBNtot[,kkk]<-Xn$TBNtot[101:151]
  yrTBN_014[,kkk]<-Xn$"TBN0-14"[101:151]
  yrTBN_1554[,kkk]<-Xn$"TBN15-54"[101:151]
  yrTBN_5564[,kkk]<-Xn$"TBN55-64"[101:151]
  yrTBN_65[,kkk]<-Xn$"TBN65+"[101:151]
  
  yrTBMtot[,kkk]<-Xn$TBMtot[101:151]
  yrTBM_014[,kkk]<-Xn$"TBM0-14"[101:151]
  yrTBM_1559[,kkk]<-Xn$"TBM15-59"[101:151]
  yrTBM_60[,kkk]<-Xn$"TBM60+"[101:151]
 
  yrTBPbtot[,kkk]<-Xn$"TBPb15+"[101:151]
  yrTBPb_1529[,kkk]<-Xn$"TBPb15-29"[101:151]
  yrTBPb_3059[,kkk]<-Xn$"TBPb30-59"[101:151]
  yrTBPb_60[,kkk]<-Xn$"TBPb60+"[101:151]
  
  yrTBItot[,kkk]<-Xn$"TBItot"[101:151]
  
}

#calculate medians and range
med_TBN<-matrix(0,51,15)
med_TBM<-matrix(0,51,12)
med_TBPb<-matrix(0,51,12)
med_TBI<-matrix(0,51,3)


for (jj in 1:51){

# TBN
med_TBN[jj,c(1:3)]<-c(median(yrTBNtot[jj,]),min(yrTBNtot[jj,]),max(yrTBNtot[jj,]))
med_TBN[jj,c(4:6)]<-c(median(yrTBN_014[jj,]),min(yrTBN_014[jj,]),max(yrTBN_014[jj,]))
med_TBN[jj,c(7:9)]<-c(median(yrTBN_1554[jj,]),min(yrTBN_1554[jj,]),max(yrTBN_1554[jj,]))
med_TBN[jj,c(10:12)]<-c(median(yrTBN_5564[jj,]),min(yrTBN_5564[jj,]),max(yrTBN_5564[jj,]))
med_TBN[jj,c(13:15)]<-c(median(yrTBN_65[jj,]),min(yrTBN_65[jj,]),max(yrTBN_65[jj,]))

med_TBM[jj,c(1:3)]<-c(median(yrTBMtot[jj,]),min(yrTBMtot[jj,]),max(yrTBMtot[jj,]))
med_TBM[jj,c(4:6)]<-c(median(yrTBM_014[jj,]),min(yrTBM_014[jj,]),max(yrTBM_014[jj,]))
med_TBM[jj,c(7:9)]<-c(median(yrTBM_1559[jj,]),min(yrTBM_1559[jj,]),max(yrTBM_1559[jj,]))
med_TBM[jj,c(10:12)]<-c(median(yrTBM_60[jj,]),min(yrTBM_60[jj,]),max(yrTBM_60[jj,]))

med_TBPb[jj,c(1:3)]<-c(median(yrTBPbtot[jj,]),min(yrTBPbtot[jj,]),max(yrTBPbtot[jj,]))
med_TBPb[jj,c(4:6)]<-c(median(yrTBPb_1529[jj,]),min(yrTBPb_1529[jj,]),max(yrTBPb_1529[jj,]))
med_TBPb[jj,c(7:9)]<-c(median(yrTBPb_3059[jj,]),min(yrTBPb_3059[jj,]),max(yrTBPb_3059[jj,]))
med_TBPb[jj,c(10:12)]<-c(median(yrTBPb_60[jj,]),min(yrTBPb_60[jj,]),max(yrTBPb_60[jj,]))

med_TBI[jj,c(1:3)]<-c(median(yrTBItot[jj,]),min(yrTBItot[jj,]),max(yrTBItot[jj,]))

}



Year<-seq(2000,2050,1)
med_TBN<-as.data.frame(cbind(Year,med_TBN))
med_TBM<-as.data.frame(cbind(Year,med_TBM))
med_TBPb<-as.data.frame(cbind(Year,med_TBPb))
med_TBI<-as.data.frame(cbind(Year,med_TBI))




ggTBN<-ggplot(med_TBN, aes(x=Year, y=med_TBN[,2])) +
  theme_classic() +
  geom_ribbon(aes(ymin = med_TBN[,3], ymax = med_TBN[,4], width=3), fill = "grey70") +
  geom_line(aes(y = med_TBN[,2]), colour="black", width=3) +
  #scale_y_continuous(expand=c(0,0)) +
  #coord_cartesian(ylim = c(0, 100)) +  
  labs(x="Year",y="All-age notification rate (/100,000population)") +
  geom_point(aes(x=2010,y=notif2010[1]), colour="black") +
  geom_segment(aes(x=2010,y=notif2010l[1],xend=2010,yend=notif2010u[1]))


ggTBN

ggTBN014<-ggplot(med_TBN, aes(x=Year, y=med_TBN[,5])) +
  theme_classic() +
  geom_ribbon(aes(ymin = med_TBN[,6], ymax = med_TBN[,7], width=3), fill = "red", alpha=0.3) +
  geom_line(aes(y = med_TBN[,5]), colour="black", width=3) +
  #scale_y_continuous(expand=c(0,0)) +
  #coord_cartesian(ylim = c(0, 100)) +  
  labs(x="Year",y="0-14 years notification rate (/100,000population)") +
  geom_point(aes(x=2010,y=notif2010[2]), colour="red") +
  geom_segment(aes(x=2010,y=notif2010l[2],xend=2010,yend=notif2010u[2]))


ggTBN014


ggTBN1554<-ggplot(med_TBN, aes(x=Year, y=med_TBN[,8])) +
  theme_classic() +
  geom_ribbon(aes(ymin = med_TBN[,9], ymax = med_TBN[,10], width=3), fill = "orange", alpha=0.3) +
  geom_line(aes(y = med_TBN[,8]), colour="orange", width=3) +
  #scale_y_continuous(expand=c(0,0)) +
  #coord_cartesian(ylim = c(0, 100)) +  
  labs(x="Year",y="15-54 years notification rate (/100,000population)") +
  geom_point(aes(x=2010,y=notif2010[3]), colour="red") +
  geom_segment(aes(x=2010,y=notif2010l[3],xend=2010,yend=notif2010u[3]))


ggTBN1554

plot_grid(ggTBN,ggTBN014,ggTBN1554, ncol=2)

#ggTBNall<-grid.arrange(ggTBN,ggTBN014,ggTBN1554, ncol=3, top = "Notification Rates")

#ggsave("ggTBNall.pdf", plot = ggTBNall, width = 16, height = 9, dpi = 120)

