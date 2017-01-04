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
typen<-3 ## Number of vaccine types (PPI, PRI, PSI_LR)
effI<-seq(0,100,10)/100   #efficacy for POI
effD<-seq(0,100,10)/100   #efficacy for POD
durs<-c(2,3,5,7,10,15,20,30,100) #duration of protection (yrs)
cover<-0.8  #routine coverage
coverM<-0.7 #mass campaign coverage
boost<-10   #spacing of booster campaigns
combn<-length(effI)*length(effD)*length(durs) ## Number of efficacy and duration combinations

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
  
  
  eee<-cbind(Xn,0,0,0,0,0); colnames(eee)<-c(colnames(Xn),"type","VE_I","VE_D","dur","count")
  #dfvx<-rbind(dfvx,eee)
  rrun_dfvx<-eee
  
  # For each type of vaccine
  #3 vax types and only delivering to one age group for now.
  for (nn in 1:typen){
    
    count<-0;coms<-matrix(0,combn,3);
    #for each POI efficacy
    for (vv in 1:length(effI)){
      # For each POD efficacy
      for (zz in 1:length(effD)){
        # For each duration
        for (xx in 1:length(durs)){
          count<-count+1  
          coms[count,]<-c(effI[vv],effD[zz],durs[xx])
          ticI <- effI[vv]; ticD <- effD[zz];   toc <- durs[xx];   print(c(nn,cov,ticI,ticD,toc))
        
          # Length of second input > 1 so triggers FitGo to do a vaccine scenario
          X<-FitGo(cntry,c(nn,cover,coverM,ticI,ticD,toc,boost),c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE,alpha),c(2,0.5,c(0.02,0.02,0.8,0.07)),c(1900,2050),0,C)  
          
          
          if (nn == 2){vtp<-"OA_PRI"
          } else if (nn == 3){
            vtp<-"OA_PSI"
          } else if (nn == 4){
            vtp<-"OA_PPI"
          } else if (nn == 5){
            vtp<-"Ado_PRI"
          } else if (nn == 6){
            vtp<-"Ado_PSI"
          } else if (nn == 7){
            vtp<-"Ado_PPI"
          } else if (nn == 8){
            vtp<-"OA_PSIL"
          } else if (nn == 9){
            vtp<-"Ado_PSIL"
          }
          
          vxtyp<-paste(vtp,"_",cov,"_",tic,"_",toc)
          vacnames<-c(vacnames,vxtyp)
          
          #         # save in countries VXout for DALY calc       
          #         # save in big df for plot
          eee<-cbind(X,nn,cov,tic,toc,count); colnames(eee)<-c(colnames(X),"type","cov","VE","dur","count")
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
          
          
        }}}}
  #assign('dfvx',dfvx,envir=.GlobalEnv)
  assign('new_active',new_active,envir=.GlobalEnv)
  assign('new_ac_age',new_ac_age,envir=.GlobalEnv)
  assign('new_mort',new_mort,envir=.GlobalEnv)
  assign('NumV',NumV,envir=.GlobalEnv)
  assign('inc2050',inc2050,envir=.GlobalEnv)
  assign('mort2050',mort2050,envir=.GlobalEnv)
  assign('rrun_dfvx',rrun_dfvx,envir=.GlobalEnv)
  assign('vacnames',vacnames,envir=.GlobalEnv)
  assign('kkk',kkk,envir=.GlobalEnv)


  
  setwd("Vaccoutput")
  write.table(new_active,paste('new_active_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(new_mort,paste('new_mort_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(NumV,paste('number_vaccinated_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(inc2050,paste('inc_rates_2050_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(mort2050,paste('mort_rates_2050_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  write.table(rrun_dfvx,paste('rrun_dfvx_',kkk,'.csv',sep=""),sep=",",row.names=FALSE)
  
  print(count)
  print(vacnames)
  setwd(home)
  source('#NNV_clusterG.R')
  setwd(home)
  source('#%reduction_clusterG.R')


