## Vaccine matrix generation
# Vaccine type is an input: Vx1 = PPI, Vx2 = PRI, Vx3 = PSI_LR
# Efficacy (eff) and Duration (D) are separate inputs
# Gives theta = proportion per time step of that age that move into the vaccine category (=cov)
# And d = matrix of ages vs times, 1 at (i,j) if at time j all those of age i have to leave the vaccine category


# Year of introduction
yrintro = 2025

# Mid point of year - for infant 6month old vaccine (need to have some death before vaccine)
# dont need midyear for adult/elderly
#midyear <- round((1/dt)/2)

times<-seq(year1,(yearend+(1-dt)),dt)
steps<-length(times)

# Generate matrix of exiting vaccine strata
dV2<-matrix(0,steps,Mnage); 


# Generate matrix of entering vaccine strata
thetaV2<-matrix(0,steps,Mnage); 
thetaV2a<-matrix(0,steps,Mnage); 
thetaV2m<-matrix(0,steps,Mnage);
thetablank<-matrix(0,steps,Mnage);


# If introduce before the end of the simulation
if (yrintro < yearend){
 
  # index for the start of the year timestep and halfway
  startyr <- seq((yrintro-year1)*(1/dt)+1,(yearend-year1)*(1/dt)+1,(1/dt))
  
  
  # MASS CAMPAIGNS of all ages above the routine vaccination age:  //Every fmass years from intro (likely 10 yrs), unless duration (D) longer//
  if (effI!=0 | effD!=0) {
  
  if (D < fmass){spacing<-fmass} else {spacing<-D}
  
  massvxyr<-seq((yrintro-year1)*(1/dt)+1,(yearend-year1)*(1/dt)+1,spacing*(1/dt))  

### THETA: vaccination ages and years ####
  # routine vaccination
  thetaV2[startyr,(vxage+1)]<-coverage
  thetaV2a[startyr,(vxage+1)]<-coverage
  
  # vaccination in mass campaigns
  thetaV2[massvxyr,(vxage+2):Mnage]<-coverageM
  thetaV2m[massvxyr,(vxage+2):Mnage]<-coverageM
  

### D (WANING): waning from vaccine strata by ages and years ####

if (D <= (yearend-yrintro)){ ## D <- 100 if lifelong protection. If D is greater than yearend-yrintro then wouldnt wane out before end of model. None leave vaccine category. 
  
  ##calc as vaccine yrs+timesteps for dur protection+1 step so that is removed at beginning timestep
  massexityr<-massvxyr+(D*(1/dt))
  massexityr<-massexityr[massexityr<=(yearend-year1+1)*(1/dt)]
  
  dV2[massexityr,(vxage+2+D):Mnage]<-1
  
  dV2[(startyr[-1:-D]),(vxage+1+D)]<-1

  
 }
}
}


### setting theta ad d fordepending on vaccine tye modelled - theta is theta# (coverage), d is dv#  ###
#vacc1=PPI
#vacc2=PRI
#vacc3=PSI_LR



if (vaccine == 1){
  thetaS<-thetaV2
  thetaL<-thetaV2
  thetaR<-thetaV2
  d<-dV2 
} else if (vaccine == 2){
  thetaS<-thetaV2
  thetaL<-thetablank
  thetaR<-thetablank
  d<-dV2
} else if (vaccine == 3){
  thetaS<-thetablank
  thetaL<-thetaV2
  thetaR<-thetaV2
  d<-dV2
} 


# For checking output
assign('thetaV2a',thetaV2a,envir=.GlobalEnv);
assign('thetaV2m',thetaV2m,envir=.GlobalEnv);


#added
assign('thetaV2',thetaV2,envir=.GlobalEnv);
#assign('thetaV4',thetaV4,envir=.GlobalEnv);
assign('d', d,envir=.GlobalEnv);




