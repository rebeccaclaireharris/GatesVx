## Grabs the needed parameters from data files and does initial manipulation (of ART coverage etc)
C=0
## Needs to be the address of the data file, in cluster or computer
if (C == 0){input<-"/Users/Rebecca/GatesVx_india/Data"}
if (C == 1){input<-"/home/lsh355020/India_Gates/Data"}


setwd(input)

## ******* READ IN DATA
## Read in data from csv files (Stored in Data folder)
# (header for names, check.names stops X appearing)
suppressWarnings(library('gdata')) ## For drop.levels function
#setwd("Data")

# # Country lists (all, 22HBC, income groups)
countries <- as.matrix(read.csv('CountryList_22HBC.csv',check.names=F))     ## change this to actual list later

#import contact matrix
myneta<- suppressWarnings(as.matrix(read.csv('myneta.csv',header=FALSE,check.names=F)[,-1])) #reads in cij

# Births, mortality, contact number, Popsize in 2009 and 2050, Pop structure in 2009, in 2050
births <- drop.levels(read.csv('Births.csv',header=TRUE,check.names=F,stringsAsFactors = FALSE))      # countries as column titles, number of contacts/life expectancy/
#mortm<-read.csv('MortAge.csv',header=TRUE,check.names=F) # Columns = countries, rows = mortality rate per age... But by 5 year gaps... 
mortage<-read.csv('LEtables.csv',header=TRUE,check.names=F) # Columns = countries, rows = mortality rate per age... But by 5 year gaps... 
# sing <- suppressWarnings(read.csv('Singular.csv',header=TRUE,check.names=F))      # countries as column titles, number of contacts(neta) 
# M<-suppressWarnings(read.csv('Outputdata.csv',header=TRUE,check.names=F))[,-1]
Popsize <- suppressWarnings(read.csv('PSize(0950).csv',header=TRUE,check.names=F))      # countries as column titles, Population size in 2010, 2050 sep rows
pstruc <- suppressWarnings(read.csv('AgeStruc09.csv',header=TRUE,check.names=F))      # countries as column titles, Rows are ages
pstruc50 <- suppressWarnings(drop.levels(read.csv('AgeStruc50.csv',header=TRUE,check.names=F)))      # countries as column titles, Rows are ages. For checking
# Case detection, treatment success, natural history parameters, ranges for previous
cdrv <- suppressWarnings(read.csv('CDR.csv',header=TRUE,check.names=F))[1:24,]        # Number of years, cols = countries)
suctv <- suppressWarnings(read.csv('SucT.csv',header=TRUE,check.names=F))[1:19,]        # Number of years, cols = countries)
para <- as.matrix(drop.levels(read.csv('para.csv',header=TRUE,check.names=F)))       # Number of parameters same for all countries - CHECK with new model
pararange <- as.matrix(drop.levels(read.csv('pararanges.csv',header=TRUE,check.names=F)))       # # 



# # Calculate Case detection rate increase (sigmoidal as above)
# cdrm<-matrix(0,length(countries),(2050-2010+1))
# for (i in 1:length(countries)){
#   hh<-cdrv[,countries[i]] # HIV incidence in 15-49yos, cdr 2010, cdr 2025
#   cdr2010<-hh[1]; cdr2025<-hh[2];
#   a<-cdr2010;b<-cdr2025-cdr2010;
#   for (j in 1:(2025-2010+1)){
#     ###what does this mean???
#     cdrm[i,j]<-a + b/(1+ 1.1*exp(-0.8*(j-7)))^(2) 
#   }
#   #cdr remains constant after 2025
#   cdrm[i,(2026-2010+1):(2050-2010+1)]<-cdrm[i,(2025-2010+1)]
# }
# cdrm<-cdrm/100
# rownames(cdrm)<-countries

#pessimistic
# cdrm<-matrix(0,length(countries),(2050-1990+1))
# for (i in 1:length(countries)){
#   cdrm[i,(1:(2012-1990+1))]<- t(cdrv[1:23,countries[i]])
#   #cdr remains constant after 2012
#   cdrm[i,(2013-1990+1):(2050-1990+1)]<-cdrm[i,(2012-1990+1)]
# }
# cdrm<-cdrm/100
# rownames(cdrm)<-countries

# ##CDRM using generalised logistic function  - deals with variation in the data and can decrease the jump likely to have been artificial

### THESE NEED UPDATING
cdrm<-matrix(0,length(countries),(2050-1990+1))
for (i in 1:length(countries)){
  K<-cdrv[23,countries[i]]
  A<-cdrv[1,countries[i]]
  Q<-0.2
  Qv<-0.22
  slope<-0.065
  inflect<-8

#   slope<-0.1
#   inflect<-7
  cdrm[i,(1:(2050-1990+1))]<- A+((K-A)/((1+(Q*exp(-slope*(((1:(2050-1990+1))-inflect)))))^(1/Qv)))
  
}
cdrm<-cdrm/100
rownames(cdrm)<-countries


# #optimistic
# cdrm2<-<-matrix(0,length(countries),(2050-1990+1))
# for (i in 1:length(countries)){
#   a<-cdrv[1,countries[i]] # cdr 2010,
#   ### this needs to change as need to be the level that would deliver 2025 goals
#   c<-cdrv[1,countries[i]] # cdr 2025
#   b<-c-a
#   for (j in 1:(2025-2010+1)){
#     ###what does this mean??? sigmoidal increase to 2025 level??
#     cdrm[i,j]<-a + b/(1+ 1.1*exp(-0.8*(j-7)))^(2) 
#   }
#   #cdr remains constant after 2025
#   cdrm[i,(2026-2010+1):(2050-2010+1)]<-cdrm[i,(2025-2010+1)]
# }
# cdrm<-cdrm/100
# rownames(cdrm)<-countries

# Treatment success 
#pessimistic
suctm<-matrix(0,length(countries),(2050-1994+1))
rownames(suctm)<-countries
for (i in 1:length(countries)){
  suctm[i,(1:(2011-1994+1))]<- t(suctv[(1:18),countries[i]])
  #suctm remains constant after 2011
  suctm[i,(2012-1994+1):(2050-1994+1)]<-suctm[i,(2011-1994+1)]
}


## AGES - how wide are the age classes? 
widthage <- 1    ## Take individual age classes
mm <- 101         # Maximum age 
Mnage <- ceiling(mm/widthage) # Number of age classes for matrix

## ********* PARAMETERS
# Assign out parameters - same for all countries
for (i in 1:length(para[,1])){assign(para[i,1],as.numeric(para[i,2]))}

# Reset input 
setwd(input)

# plot ART, cdr and suct increase
# plot(0,0,xlim=c(2010,2050),ylim=c(0,1),xlab="Years",ylab="ART coverage (proportion)")
# for (i in 1:length(countries)){
#   lines(seq(2009,2050,1),artm[i,])
#   points(c(2009,2025),c(art2009,art2025),pch=4)
# }
# plot(0,0,xlim=c(2009,2050),ylim=c(0,100),xlab="Years",ylab="Case Detection Rate (%)")
# for (i in 1:length(countries)){
# lines(seq(2009,2050,1),cdrm[i,])
# points(c(2009,2025),c(cdr2009,cdr2025),pch=4)
# }
# plot(0,0,xlim=c(2009,2050),ylim=c(0,1),xlab="Years",ylab="Treatment Success (proportion)")
# for (i in 1:length(countries)){
#   lines(seq(2009,2050,1),suctm[i,])
#   points(c(2009,2025),c(cdr2009,cdr2025),pch=4)
# }
