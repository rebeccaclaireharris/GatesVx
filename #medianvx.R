##### CALCULATING MEDIAN CHARACTERISTICS TO ACHIEVE GIVEN LEVEL OF IMPACT ####

## this code will output the median efficacy POI, POD and duration to achieve e.g. 20-30% reduction in TB incidence rate in 2050 compared to the no new vaccine baseline ##

C=0
#input
if (C == 0){vaccin<-"/Users/Rebecca/GatesVx/Vxoutput"}
if (C == 1){vaccin<-"/home/lsh355020/China_Gates/Vxoutput"}

#output 
if (C == 0){vaccout<-"/Users/Rebecca/GatesVx/Vxoutput/medianvx"}
if (C == 1){vaccout<-"/home/lsh355020/China_Gates/Vxoutput/medianvx"}

setwd(vaccin)

#number runs
rrun<-1000
baseinc<-matrix(0,rrun,2)

colnames(baseinc)<-c(2035,2050)

#call in baseline incidence data from 2035 and 2050  from all runs
##call in other inc data here too to reduce number of loops???

for (ii in 1:rrun){
  
  baseinc[ii,]<-as.matrix(read.csv(paste('base_inc_2035_2050_',ii,'.csv', sep=''),check.names=F))
  
}


baseI<-  xxxxxxxxxx


## calculate the incidence ranges that would be equivalent to the xx-yy% reductions in incidence rate ##
#### NEED TO CALC FOR EACH RUN !!!‹‹‹


imp<-seq(20,90,10)/100
aim<-matrix(0,(length(imp)-1),3)
aim<-as.data.frame(aim)
colnames(aim)<-c("lowI","highI","range")

for (ii in 1:(length(imp)-1)){
  
  aim[ii,1:2]<-c(baseI*(1-imp[ii+1]),baseI*(1-imp[ii]))
  aim[ii,3]<-paste(imp[ii]*100,'-',imp[ii+1]*100,sep='')

}

aim<-as.data.frame(aim)

##




