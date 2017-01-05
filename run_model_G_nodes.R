###### ** Run vaccine scenarios - one param set on each on cluster node  ** #######

library(plyr)

# On cluster (C=1) or no (C=0)?
C=0
if (C == 0){home<-"/Users/Rebecca/GatesVx"}
if (C == 1){home<-"/home/lsh355020/China_Gates/"}

if (C == 0){input<-"/Users/Rebecca/GatesVx/Data"}
if (C == 1){input<-"/home/lsh355020/China_Gates/Data"}

setwd(home)

#read in data and function script
source('#DataGrabG.R')
setwd(home)
source('CFunctions_G.R')

setwd(home);setwd(input)
#read in 1000 fit parameter sets
para<-as.matrix(drop.levels(read.csv('para1000fit.csv',header=TRUE,check.names=F)))

setwd(home)

nm<-c(pararange[,1],"p0") # The parameter ranges

# What country? (For you this will always be China for now)
cntry<-"China"


#### Vaccine interventions  ####
typen<-1 ## Number of vaccine types (PPI, PRI, PSI_LR)
effInf<-seq(0,100,10)/100   #efficacy for POI
effDis<-seq(0,100,10)/100   #efficacy for POD
durs<-c(2,3,5,7,10,15,20,25,100) #duration of protection (yrs)
cover<-0.8  #routine coverage
coverM<-0.7 #mass campaign coverage
vage<-12   #age at which the routine/first vaccination occurs
fms<-10   #shortest frequency of mass campaigns (years)
combn<-length(effInf)*length(effDis)*length(durs) ## Number of efficacy and duration combinations



# Run Vaccines and where to store
setwd(home);

# Storage data frame
#dfvx<-c()
cumulvx<-c()
vaxgive<-c()
vaxgiveyr<-c()
cumulvxyrM<-c()
cumulvxyrI<-c()
NumV<-c()
inc2050<-c()
mort2050<-c()
inc2035<-c()
mort2035<-c()
rrun_dfvx<-c()
vacnames<-c()

#### Run through all vaccines for one fit ####
if (C==0){kkk<-1}
if (C==1){kkk<-as.numeric(Sys.getenv("SGE_TASK_ID"))}

  print(kkk)
  
  for (i in 1:length(nm)){assign(nm[i],as.numeric(para[kkk,i]))}
  neta2<-neta
  
  # Run the model with these parameters 
  # Second input of length 1 so "no vaccine" scenario
  Xn<-FitGo(cntry,1,c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE, alpha),c(2,0.5,c(0.02,0.02,0.8,0.07)),c(1900,2050),0,C)  
  # save in big df for plot - original one
  
  #source('#BasicPlot.R')
  setwd("Vxoutput")
  write.table(Xn,paste('Xnbaseline',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  setwd(home)
  
  new_active<-cbind(TBAc,0,0)
  new_ac_age<-cbind(TBAc_age,0,0)
  new_mort<-cbind(TBMo,0,0)
  inc2050<-rbind(TBI[151,])
  mort2050<-rbind(TBM[151,])
  inc2035<-rbind(TBI[136,])
  mort2035<-rbind(TBM[136,])
  
  eee<-cbind(Xn,0,0,0,0,0); colnames(eee)<-c(colnames(Xn),"type","VE_I","VE_D","dur","count")
  #dfvx<-rbind(dfvx,eee)
  rrun_dfvx<-eee
  
  # For each type of vaccine
  #3 vax types and only delivering to one age group for now.
  for (nn in 1:typen){
    
    count<-0;coms<-matrix(0,combn,3);
    #for each POI efficacy
    for (vv in 1:length(effInf)){
      # For each POD efficacy
      for (zz in 1:length(effDis)){
        # For each duration
        for (xx in 1:length(durs)){
          count<-count+1  
          coms[count,]<-c(effInf[vv],effDis[zz],durs[xx])
          
          ticI <- effInf[vv]; ticD <- effDis[zz];   toc <- durs[xx];   print(c(nn,ticI,ticD,toc))
        
          # Length of second input > 1 so triggers FitGo to do a vaccine scenario
          X<-FitGo(cntry,c(nn,cover,coverM,ticI,ticD,toc,fms,vage),c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE,alpha),c(2,0.5,c(0.02,0.02,0.8,0.07)),c(1900,2050),0,C)  
          
          
          if (nn == 1){vtp<-"PPI"
          } else if (nn == 2){
            vtp<-"PRI"
          } else if (nn == 3){
            vtp<-"PSI_LR"
          } 
          
          vxtyp<-paste(vtp,"_",ticI,"_",ticD,"_",toc)
          vacnames<-c(vacnames,vxtyp)
          
          #         # save in countries VXout for DALY calc       
          #         # save in big df for plot
          eee<-cbind(X,nn,ticI,ticD,toc,count); colnames(eee)<-c(colnames(X),"type","VE_I","VE_D","dur","count")
          #dfvx<-rbind(dfvx,eee)
          #dfvx<-as.matrix(dfvx)
          rrun_dfvx<-rbind(rrun_dfvx,eee)
          
          #calcs needed for NNV
          new_active<-cbind(new_active,TBAc,nn,count)
          new_ac_age<-cbind(new_ac_age,TBAc_age,nn,count)
          new_mort<-cbind(new_mort,TBMo,nn,count)
          NumV<-cbind(NumV,NV,nn,count)
          
          #outputting 2050 incidence rate
          inc2050<-rbind(inc2050,TBI[151,])
          mort2050<-rbind(mort2050,TBM[151,])
          
          #outputting 2035 incidence rate
          inc2035<-rbind(inc2035,TBI[136,])
          mort2035<-rbind(mort2035,TBM[136,])
          
        }}}}
  #assign('dfvx',dfvx,envir=.GlobalEnv)
  assign('new_active',new_active,envir=.GlobalEnv)
  assign('new_ac_age',new_ac_age,envir=.GlobalEnv)
  assign('new_mort',new_mort,envir=.GlobalEnv)
  assign('NumV',NumV,envir=.GlobalEnv)
  assign('inc2050',inc2050,envir=.GlobalEnv)
  assign('mort2050',mort2050,envir=.GlobalEnv)
  assign('inc2035',inc2035,envir=.GlobalEnv)
  assign('mort2035',mort2035,envir=.GlobalEnv)
  assign('rrun_dfvx',rrun_dfvx,envir=.GlobalEnv)
  assign('vacnames',vacnames,envir=.GlobalEnv)
  assign('kkk',kkk,envir=.GlobalEnv)


  
  setwd("Vxoutput")
  write.table(new_active,paste('new_active_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(new_mort,paste('new_mort_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(NumV,paste('number_vaccinated_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(inc2050,paste('inc_rates_2050_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(mort2050,paste('mort_rates_2050_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(inc2035,paste('inc_rates_20350',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(mort2035,paste('mort_rates_2035_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(rrun_dfvx,paste('rrun_dfvx_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  
  print(count)
  print(vacnames)
  setwd(home)
  source('#NNV_clusterG.R')
  setwd(home)
  source('#%reduction_clusterG.R')


