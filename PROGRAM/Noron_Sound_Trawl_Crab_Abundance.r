################################################################################
#   Norton Sound Trawl Survey Harvest Estimation Program in R
#   Version 1.1   02/07/2017
#   This program esitmate abundance of
#   Legal (CW>120mm)
#   Pre-recruit 1 and 2 
#   By Toshihide "Hamachan" Hamazaki
################################################################################

#-------------------------------------------------------------------------------
#  0.0  Initialize working Environment                                          
#-------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
library(reshape2)
#-------------------------------------------------------------------------------
#  0.1  Define Folder Directories:                                                    
#-------------------------------------------------------------------------------
# Data folder location 
base_dir <- file.path('C:','Projects','Norton_Sound','NSCrab','Trawl_data','NS_Trawl_Survey')
setwd(base_dir)
data_dir <- file.path(base_dir,'DATA')
# Output table folder location 
out_data_dir <- file.path(base_dir,'PROGRAM')
# Source R-code folder location 
source_dir <- file.path(out_data_dir,'R-code')
#-------------------------------------------------------------------------------
#  0.2 Read haul data  
#-------------------------------------------------------------------------------
source(file.path(source_dir,'Noron_Sound_Read_tow_data.r'))
		
#===============================================================================
#  0.3 Define crab data   
#===============================================================================
# Set classification  'ADFG' or 'CPT'
classification <- 'CPT' 
# Use retow TRUE or FALSE 
retow <- FALSE
# Ignore unknown male size-shell crab? 
unknown <- FALSE

#===============================================================================
# functions
# Crab Classificaton 
#===============================================================================
# ADFG--------------------------------------------------------------------------
adfg.class <- function(crabdata){
  crab.class <- with(crabdata,
		ifelse(Sex==2,'female',
		ifelse(Legal==2,'Legal',
		ifelse(Size_mm>89,'Pre1', 
		ifelse(Size_mm>75,'Pre2','Pre3')))))
  return(crab.class)
}

# CPT---------------------------------------------------------------------------
cpt.class <- function(crabdata){
  crab.class <-with(crabdata,
	ifelse(Sex==2,'female',
	ifelse(Size_mm>=64,'Total','other')))
	return(crab.class)
}
# Standard Output --------------------------------------------------------------
st <- c('Year','Agent','Haul','Sex','Size_mm','Legal','Sampling.Factor')
#===============================================================================
# 1.0  Read ADFG Crab data  
#===============================================================================
# Find the file names of ADFG Crab data in the Crab folder
crab.file.list <- list.files(file.path(data_dir,'ADFG','Crab'))
# n: number of filses 
n <- length(crab.file.list)
# read each file and combine to make a single ADFG.haul data 
ADFG.crab <- data.frame()
for(i in 1:n){
 temp <- read.csv(file.path(data_dir,'ADFG','Crab',crab.file.list[i]), skip = 6, na='',header=TRUE)
 ADFG.crab <- rbind(ADFG.crab,temp)
}
# Change format (Station is numeric)

# Add Sampling factor  
ADFG.crab$Sampling.Factor <- 1
ADFG.crab$Size_mm[ADFG.crab$Size_mm==0] <- NA  
# Add Agent name 
ADFG.crab$Agent <- 'ADFG'
ADFG.crab <- ADFG.crab[,st]

#===============================================================================
# 2.0  Read NOAA Crab data  
#===============================================================================
# Data file name:  Keep in the data directory
noaa <- read.csv(file.path(data_dir,'NMFS_76_91','NOAA_1976_1991_Crab.csv'),skip=6,header=TRUE)
# 1: sublegal or female, 2: Legal
noaa$Legal <- ifelse(noaa$Size_mm >104 & noaa$Sex ==1,2,1)
noaa$Size_mm[noaa$Size_mm==0] <- NA  
# Add Agenget name 
noaa$Agent <- 'NOAA'
# Remove unnecessary data 
noaa <- noaa[,st]

#===============================================================================
# 3.0  Read NOAA NBS Crab data  
#===============================================================================
# Find the file names of ADFG Crab data in the Crab folder
crab.file.list <- list.files(file.path(data_dir,'NOAA_NBS','Crab'))
# n: number of filses 
n <- length(crab.file.list)
# read each file and combine to make a single ADFG.haul data 
NBS.crab <- data.frame()
for(i in 1:n){
 temp <- read.csv(file.path(data_dir,'NOAA_NBS','Crab',crab.file.list[i]), skip = 6, na='',header=TRUE)
NBS.crab <- rbind(NBS.crab,temp)
}
# Change format (Station is numeric)

# 1: sublegal or female, 2: Legal
NBS.crab$Legal <- ifelse(NBS.crab$Size_mm >104 & NBS.crab$Sex ==1,2,1)
NBS.crab$Size_mm[NBS.crab$Size_mm==0] <- NA  
# Renumber Hauls 
NBS.crab$Haul <- with(NBS.crab, Haul + 100*Vessel)
# Add Agent name
NBS.crab$Agent <- 'NBS'
# Remove unnecessary data 
NBS.crab <- NBS.crab[,st]

#===============================================================================
# 4.0  Create Data for Abundance Analyses   
#===============================================================================
# Combine all data 
crabdata <- rbind(noaa,ADFG.crab,NBS.crab)
# Remove data with No sex info 
crabdata <- crabdata[!is.na(crabdata$Sex),]
# Classify Crab based 
if (classification == 'CPT'){crabdata$class <- cpt.class(crabdata)}
if (classification == 'ADFG'){crabdata$class <- adfg.class(crabdata)}
# Add NA class 
crabdata$class[is.na(crabdata$class)] <- 'UNK'

############################################################################
# 5.0  Calculate total number of crab by class and combine with haul data
############################################################################
ncrab <- aggregate(Sampling.Factor ~ Year+Agent+Haul+class, FUN=sum, data=crabdata)
#ncrab <- aggregate(Sampling.Factor ~ Year+Agent+Haul, FUN=sum, data=crabdata)
names(ncrab)[5] <- 'n'
# merge ncrab data with haul data 
crabdata <- merge(haul,ncrab, by = c('Year','Agent','Haul'),all=TRUE)
crabdata$class[is.na(crabdata$class)] <- 'NOCrab'
crabdata$n[is.na(crabdata$n)] <- 0 
#write.csv(crabdata.w,paste(data_dir,'crabdata_vast.csv',sep=''),row.names=T) 
# calculate abundance 

crabdata$estkm <- with(crabdata,n*Area_Nm2*(1.852^2)/Swept_km2)
crabdata$estnm <- with(crabdata,n*Area_Nm2/Swept_NM2)
crabdata$cpuenm <- with(crabdata,n/Swept_NM2)
# Remove data that does not have any Area_Nm info (out of ADFG survey area)
crabdata <- crabdata[which(!is.na(crabdata$estnm)),]
# Remove bad tow data (Keep retow data)
crabdata.r <- crabdata[which(is.na(crabdata$Haul_rate)|crabdata$Haul_rate != 'd'),]
# Remove retow data
if(retow == FALSE){ 
crabdata.r <- crabdata[which(is.na(crabdata$Haul_rate)),]
}

# Average out retow data 
crabdata.est <- aggregate(estnm ~ Year+Agent+ADFG_Station+ADFG_tier+CPT_STD+class, FUN=mean, data=crabdata.r)
crabdata.n <- aggregate(n ~ Year+Agent+ADFG_Station+ADFG_tier+CPT_STD+class, FUN=sum, data=crabdata.r)
crabdata.nw <- dcast(crabdata.n, Year+ADFG_Station+ADFG_tier+CPT_STD+Agent~class)
write.csv(crabdata.nw,'ADFG_NBS_crab_n.csv')
crabdata.cpue <- aggregate(cpuenm ~ Year+Agent+ADFG_Station+ADFG_tier+CPT_STD+class, FUN=mean, data=crabdata.r)
ADFG_NBS <- crabdata.cpue[with(crabdata.cpue,(Year %in% c(2017,2019,2021,2023)) & (ADFG_Station %in% c(79,81,121,123,125,127,129,131,133,135,176,180,186))),]
ADFG_NBS.w <- dcast(ADFG_NBS, Year+ADFG_Station+ADFG_tier+CPT_STD~class+Agent)
write.csv(ADFG_NBS.w,'ADFG_NBS.csv')

############################################################################
# 6.0  Remove resample and no ADF&G station data       
############################################################################

############################################################################
# 7.0  Calculate Abundance by station
############################################################################
# Remove unnecessary data 
crabdata.est <- crabdata.est[,c('Year','Agent','ADFG_Station','ADFG_tier','CPT_STD','class','estnm')]

# Change data from long to wide format 
crabdata.est.w <- dcast(crabdata.est, Year+Agent+ADFG_Station+ADFG_tier+CPT_STD~class)
crabdata.est.w$Total[crabdata.est.w$NOCrab ==0] <- 0
crabdata.est.w$Total<- ifelse(!is.na(crabdata.est.w$female)&is.na(crabdata.est.w$Total),
                             crabdata.est.w$female,crabdata.est.w$Total)
							 

crabdata.n.w <- dcast(crabdata.n, Year+Agent+ADFG_Station+ADFG_tier+CPT_STD~class)
crabdata.cpue.w <- dcast(crabdata.r, Year+Agent+ADFG_Station+Haul+Month+Day+Latitude+Longitude+Swept_NM2+Depth_m+Bottom_Temp+Surface_Temp+ADFG_tier~class, value.var='cpuenm')

write.csv(crabdata.cpue.w,'cpue.csv')
temp <- aggregate(Total ~ Year+Agent+ADFG_Station,mean, data=crabdata.est.w)
crabdata.est.w <- dcast(temp, ADFG_Station~Year+Agent,fun.aggregate = mean,value.var='Total')
temp <- merge(station, crabdata.est.w,by='ADFG_Station')
write.csv(temp,'VAST.csv')
 
head(temp)


# Add 0 to NA
crabdata.est.w[is.na(crabdata.est.w)] <- 0
crabdata.n.w[is.na(crabdata.n.w)] <- 0
# Extract class name 
if(unknown == FALSE){
classes <- names(crabdata.est.w)[-c(1:5)]
sclass <- classes[!classes %in% c('female','NOCrab','UNK')]
unkadd <- (crabdata.est.w$UNK*crabdata.est.w[,sclass]/ifelse(rowSums(crabdata.est.w[,sclass])==0,1,rowSums(crabdata.est.w[,sclass])))
head(crabdata.est.w)
crabdata.est.w[,sclass] <- crabdata.est.w[,sclass]+unkadd
}
   

#crabdata.est.w <- reshape(crabdata.est, timevar='class',idvar=c('Year','Agent','ADFG_Station','ADFG_tier','CPT_STD'),direction='wide' )

#crabdata.est.w[with(crabdata.est.w,est.female+est.Total+est.other ==0),] 


############################################################################
# 8.0  ADFG Estimates:  Run this if class is ADFG classification   
############################################################################
if (classification == 'ADFG'){
crabsum <- aggregate(cbind(female,Legal,Pre1,Pre2,Pre3) ~ Year+Agent,FUN=sum,data=crabdata.est.w)
crabsd <- aggregate(cbind(female,Legal,Pre1,Pre2,Pre3) ~ Year+Agent,FUN=sd,data=crabdata.est.w)
crabn <- aggregate(cbind(female,Legal,Pre1,Pre2,Pre3) ~ Year+Agent+ADFG_tier,FUN=sum,data=crabdata.n.w)
crabsum[order(crabsum$Year,crabsum$Agent),]
crabn[order(crabn$Year,crabn$Agent,crabn$ADFG_tier),]

by.tier <- aggregate(cbind(female,Legal,Pre1,Pre2,Pre3) ~ Year+Agent+ADFG_tier,FUN=sum,data=crabdata.est.w)
by.tier[order(by.tier$Year,by.tier$Agent,by.tier$ADFG_tier),]
}

############################################################################
# 9.0  CPT Estimates: Run this if class is CPT classification   
############################################################################
# limit data to CPT_STD area 
if (classification == 'CPT'){
# Limit data to standardized area 
crabdata.est.c <- crabdata.est.w[crabdata.est.w$CPT_STD =='S',] 
# Expand area to entier NS. 
#crabdata.est.c <- crabdata.est.w[crabdata.est.w$ADFG_tier !='ONS',] 
crabsum <- aggregate(Total ~ Year+Agent,data=crabdata.est.c,FUN = function(x) c(sum = sum(x),mean = mean(x),sd = sd(x),n = length(x)))
crabsum <- do.call(data.frame, crabsum)
 
crabsum$cv <- with(crabsum,sqrt(Total.n)*Total.sd/Total.sum)
print(crabsum)
crabsum.f <- aggregate(female ~ Year+Agent,data=crabdata.est.c,FUN = function(x) c(sum = sum(x),mean = mean(x),sd = sd(x),n = length(x)))
crabsum.f <- do.call(data.frame, crabsum.f)
crabsum.f$cv <- with(crabsum.f,sqrt(female.n)*female.sd/female.sum)
print(crabsum.f)
}





