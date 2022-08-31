###########################################################################
#   Norton Sound Trawl Survey Harvest Estimation Program in R
#   Version 1.1   02/07/2017
#   This program esitmate abundance of
#   Legal (CW>120mm)
#   Pre-recruit 1 and 2 
#   By Toshihide "Hamachan" Hamazaki
############################################################################

#-------------------------------------------------------------------------------
#  0.0  Initialize working Environment                                          
#-------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
library(doBy)
library(reshape2)
#-------------------------------------------------------------------------------
#  0.1  Define Folder Directories:                                                    
#-------------------------------------------------------------------------------
# Data folder location 
data_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/NS_Trawl_Survey/DATA/'
# Output table folder location 
out_data_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/NS_Trawl_Survey/PROGRAM/'
# Source R-code folder location 
source_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/NS_Trawl_Survey/PROGRAM/R-code/'
#-------------------------------------------------------------------------------
#  0.2 Read haul data  
#-------------------------------------------------------------------------------
source(paste0(source_dir,'Noron_Sound_Read_tow_data.r'))
		
#===============================================================================
#  0.3 Define crab data   
#===============================================================================
# Data file name:  Keep in the data directory
data_file1 <- 'ADFG/ADFG_Crab.csv'
data_file2 <- 'NMFS_76_91/NOAA_Crab.csv'
data_file3 <- 'NOAA_NBS/NOAA_RKC_NBS.csv'
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
# Data file name:  Keep in the data directory
adfg <- read.csv(paste0(data_dir,data_file1),na='.',header=TRUE)
# Add Sampling factor  
adfg$Sampling.Factor <- 1
adfg$Size_mm[adfg$Size_mm==0] <- NA  
# Add Agent name 
adfg$Agent <- 'ADFG'
adfg <- adfg[,st]

#===============================================================================
# 2.0  Read NOAA Crab data  
#===============================================================================
# Data file name:  Keep in the data directory
noaa <- read.csv(paste0(data_dir,data_file2),na='.',header=TRUE)
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
# Data file name:  Keep in the data directory
noaaNBS <- read.csv(paste0(data_dir,data_file3),na='',header=TRUE)
# 1: sublegal or female, 2: Legal
noaaNBS$Legal <- ifelse(noaaNBS$Size_mm >104 & noaaNBS$Sex ==1,2,1)
noaaNBS$Size_mm[noaaNBS$Size_mm==0] <- NA  
# Renumber Hauls 
noaaNBS$Haul <- with(noaaNBS, Haul + 10*Vessel - min(Vessel))
# Add Agent name
noaaNBS$Agent <- 'NBS'
# Remove unnecessary data 
noaaNBS <- noaaNBS[,st]

#===============================================================================
# 4.0  Create Data for Abundance Analyses   
#===============================================================================
# Combine all data 
crabdata <- rbind(noaa,adfg,noaaNBS)
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
# Add 0 to NA
crabdata.est.w[is.na(crabdata.est.w)] <- 0
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
crabn <- aggregate(cbind(female,Legal,Pre1,Pre2,Pre3) ~ Year+Agent,FUN=length,data=crabdata.est.w)
crabsum[order(crabsum$Year,crabsum$Agent),]
by.tier <- aggregate(cbind(female,Legal,Pre1,Pre2,Pre3) ~ Year+Agent+ADFG_tier,FUN=sum,data=crabdata.est.w)
by.tier[order(by.tier$ADFG_tier,by.tier$Year,by.tier$Agent),]
}

############################################################################
# 9.0  CPT Estimates: Run this if class is CPT classification   
############################################################################
# limit data to CPT_STD area 
if (classification == 'CPT'){
# Limit data to standardized area 
#crabdata.est.c <- crabdata.est.w[crabdata.est.w$CPT_STD =='S',] 
# Expand area to entier NS. 
crabdata.est.c <- crabdata.est.w[crabdata.est.w$ADFG_tier !='ONS',] 
crabsum <- summaryBy(Total ~ Year+Agent,FUN=c(sum,mean,sd,length),data=crabdata.est.c)
#crabsum$estsum <-  76*crabsum$Total.mean/1000
crabsum$cv <- with(crabsum,sqrt(Total.length)*Total.sd/Total.sum)
print(crabsum)
crabsum.f <- summaryBy(female ~ Year+Agent,FUN=c(sum,sd,length),data=crabdata.est.c)
crabsum.f$cv <- with(crabsum.f,sqrt(female.length)*female.sd/female.sum)
#print(crabsum.f)
}





