### VACCINE CHARACTERISTICS to be called in to the model ###


#short run checks
typen<-1 ## Number of vaccine types (PPI, PRI, PSI_LR)
effInf<-seq(0,100,50)/100   #efficacy for POI
effDis<-seq(0,100,50)/100   #efficacy for POD
# effInf<-0 #efficacy for POI
# effDis<-0   #efficacy for POD
#durs<-c(5,10,100) #duration of protection (yrs)

# typen<-3 ## Number of vaccine types (PPI, PRI, PSI_LR)
# effInf<-seq(0,100,10)/100   #efficacy for POI
# effDis<-seq(0,100,10)/100   #efficacy for POD
durs<-c(2,3,5,7,10,15,20,25,100) #duration of protection (yrs)
cover<-0.8  #routine coverage
coverM<-0.7 #mass campaign coverage
vage<-9   #age at which the routine/first vaccination occurs
fms<-10   #shortest frequency of mass campaigns (years)
combn<-length(effInf)*length(effDis)*length(durs) ## Number of efficacy and duration combinations


