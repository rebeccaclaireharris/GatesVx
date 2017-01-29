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

#home
if (C == 0){home<-"/Users/Rebecca/GatesVx"}
if (C == 1){home<-"/home/lsh355020/China_Gates/"}

setwd(vaccin)

#number runs
rrun<-11

#source vx characteristics so know # of vaccine types
setwd(home)
source('#vx.R')
numvx<-combn*typen

setwd(vaccin)


RImatrix<-c()
RImatrix35<-c()
RImatrixM<-c()

## call in matrices for reduction in incidence and mortality

for (xx in 10:rrun){
  
  print(xx)
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

setwd(vaccout)
write.table(RImatrix,"redu_inc_alldata.csv",sep=",",row.names = F)
write.table(RImatrixM,"redu_mort_alldata.csv",sep=",",row.names = F)


RImatrix<-as.matrix(RImatrix)
RImatrix35<-as.matrix(RImatrix35)
RImatrixM<-as.matrix(RImatrixM)

#matrix for calculating median min and max for each vax
med_RI<-matrix(0,numvx,3)

#calc med/max/min
for (jj in 1:numvx){
  
  seq1<-seq(1,(rrun*numvx),numvx)
  med_RI[jj,1]<-median(RImatrix[seq1+jj-1,1])
  med_RI[jj,2]<-min(RImatrix[seq1+jj-1,1])
  med_RI[jj,3]<-max(RImatrix[seq1+jj-1,1])

}


colnames(med_RI)<-c('median','min','max')
med_RI<-cbind(med_RI,RImatrix[1:numvx,2:6])


write.table(med_RI,"redu_inc_median.csv",sep=",",row.names = F)

## run code for calculating characteristics to provide a given level of impact
setwd(home)
source("#char4impact.R")


# ### USE Col 1 of med_RI to plot heat maps, selecting all of one dur and plotting. 
# ### will do a cross section plot to show min/max - 50% VE against each and 10yrs duration?
# STILL WORKING ON THE BELOW
#first tried with lattice package, but gives pixelated levels rather than smooth plots.

library(ggplot2)
library(RColorBrewer)

my_pal<-brewer.pal(11,"Spectral")
use_pal<-colorRampPalette(my_pal)


chars2029<-subset(RImatrix,redu>=20 & redu<30)



groups<-c("PPI","PRI","PSI")


#groups (PPI etc) loop
for (ig in 1:3){
  
  #durations loop
    
  #subset data on type of vaccine ppi/psi/pri. If statements for if not all types are being run
  
  med_RI<-as.data.frame(med_RI)
  if (typen>=1) {RI50_PPI<-subset(med_RI,type==1)}
  if (typen>=2) {RI50_PRI<-subset(med_RI,type==2)}
  if (typen>=3) {RI50_PSI<-subset(med_RI,type==3)}
  
  #set lists of efficacy against inf and disease
  y<-effDis
  x<-effInf 
  
  #subset the above 3 data frames to give data set by duration of protection (as each will be plotted separately, so no need to have them in the same ata frame)
  for (hh in 1:length(durs)){
    
    if (typen>=1) assign(paste0("RI50_PPI_D", durs[hh]), subset(RI50_PPI,dur==durs[hh]))    
    if (typen>=2) assign(paste0("RI50_PRI_D", durs[hh]), subset(RI50_PRI,dur==durs[hh]))    
    if (typen>=3) assign(paste0("RI50_PSI_D", durs[hh]), subset(RI50_PSI,dur==durs[hh]))    
  
    
    
     png(paste(vaccout,"/A.plot/",groups[ig],"_",durs[hh],"_heatmap.png",sep=""), width = 6, height = 6, units = 'in', res = 600)

  #call in each duration and get %reduction data amd greate a matrix of [VED,VEI]
  z<- get(paste0("RI50_PPI_D",durs[hh]))
  z<-matrix(z[,1],  nrow=length(effInf),ncol=length(effDis))
      
  ctlns <- contourLines(x, y, z, levels=c(0))
  print(filled.contour(x,y,z,xlab="Efficacy against Disease",ylab="Efficacy against Infection",levels=seq(0,100,by=10),color=use_pal,main=paste(groups[ig]," vaccine providing ",durs[hh],"years of protection",sep="")))
  dev.off()   
    
  }
}
  

 plot.axes={axis(1); axis(2);sapply(1, function(x) lines(ctlns[[x]][[2]], ctlns[[x]][[3]], lwd=1,lty=2))} 

  png(paste(start_dir,"/Results/All/A.plot/",groups[ig],"_CE_Adult.png",sep=""), width = 6, height = 6, units = 'in', res = 600)
  z<-matrix(dadu$CEvx,nrow=5,ncol=6)
  ctlns <- contourLines(x, y, z, levels=c(0))
  print(filled.contour(x,y,z,xlab="efficacy",ylab="duration",levels=seq(-1,40,by=0.41),color=use_pal,main=paste(groups[ig]," Adult",sep=""),
                       plot.axes={axis(1); axis(2);sapply(1, function(x) lines(ctlns[[x]][[2]], ctlns[[x]][[3]], lwd=1,lty=2))}))
  dev.off()

}





#groups (PPI etc) loop
for (ig in 1:3){
  
  #durations loop
    
  #subset data on type of vaccine ppi/psi/pri. If statements for if not all types are being run
  
  med_RI<-as.data.frame(med_RI)
  if (typen>=1) {RI50_PPI<-subset(med_RI,type==1)}
  if (typen>=2) {RI50_PRI<-subset(med_RI,type==2)}
  if (typen>=3) {RI50_PSI<-subset(med_RI,type==3)}
  
  #set lists of efficacy against inf and disease
  y<-effDis
  x<-effInf 
  
  #subset the above 3 data frames to give data set by duration of protection (as each will be plotted separately, so no need to have them in the same ata frame)
  for (hh in 1:length(durs)){
    
    if (typen>=1) assign(paste0("RI50_PPI_D", durs[hh]), subset(RI50_PPI,dur==durs[hh]))    
    if (typen>=2) assign(paste0("RI50_PRI_D", durs[hh]), subset(RI50_PRI,dur==durs[hh]))    
    if (typen>=3) assign(paste0("RI50_PSI_D", durs[hh]), subset(RI50_PSI,dur==durs[hh]))    
  
    
    
     png(paste(vaccout,"/A.plot/",groups[ig],"_",durs[hh],"_heatmap.png",sep=""), width = 6, height = 6, units = 'in', res = 600)

  #call in each duration and get %reduction data amd greate a matrix of [VED,VEI]
  z<- get(paste0("RI50_PPI_D",durs[hh]))
  z<-matrix(z[,1],  nrow=length(effInf),ncol=length(effDis))
      
  ctlns <- contourLines(x, y, z, levels=c(0))
  print(filled.contour(x,y,z,xlab="Efficacy against Disease",ylab="Efficacy against Infection",levels=seq(0,100,by=10),color=use_pal,main=paste(groups[ig]," vaccine providing ",durs[hh],"years of protection",sep="")))
  dev.off()   
    
  }
}
  














# 
# #down select to the durations of interest
# 
# #2,5, 10, 25
# medRI_2<-subset(med_RI, med_RI[,7]==2)
# medRI_5<-subset(med_RI, med_RI[,7]==5)
# medRI_10<-subset(med_RI, med_RI[,7]==10)
# medRI_25<-subset(med_RI, med_RI[,7]==25)
# 
# median<-medRI_5[,1]
# VEI<-medRI_5[,5]
# VED<-medRI_5[,6]
# 
# grid2<-cbind(VEI,VED,median)
#   
# ### Lattice package ###
# 
# library(lattice)
# library(grid)
# 
# ## contour plot is just lines, whereas level plot is a coloured in contour plot.
# 
# ## S3 method for class 'matrix'
#     levelplot(x, data = NULL, aspect = "iso",
#               ..., xlim, ylim,
#               row.values = seq_len(nrow(x)),
#               column.values = seq_len(ncol(x)))
# 
# #would be better plot as continuous?? at moment is as grid. Also not sufficient resolution
# 
#     medRI_5<-as.data.frame(medRI_5)
#     
#     levelplot(median~VE_I*VE_D, data=medRI_5,contour=TRUE, cuts=40)
#     levelplot(median~VE_I*VE_D, data=medRI_5,cuts=40)
#     contourplot(median~VE_I*VE_D, data=medRI_5,contour=TRUE, cuts=5)
# 
# 
# levelplot(z~x*y, grid, cuts = 50,scales=list(log="e"), xlab="",
#               ylab="", main="Weird Function", sub="with log scales",
#               colorkey = FALSE, region = TRUE)
