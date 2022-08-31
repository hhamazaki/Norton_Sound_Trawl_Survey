###########################################################################
#   Norton Sound Trawl Survey Harvest Estimation Program in R
#   Version 2.0   12/03/2019
#   This program esitmate abundance of
#   Legal Pre-recruit 1 2 3 and female for ADFG 
#   Total male crab (> 63mm) for CPT 
#   By Toshihide "Hamachan" Hamazaki
############################################################################

############################################################################
#  0.0  Initialize working Environment                                          
############################################################################
rm(list=ls(all=TRUE))
library(doBy)
library(reshape2)

############################################################################

############################################################################
#  0.1  Define Folder Directories:                                                    
############################################################################
# Data folder location 
data_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/Data_Analyses/DATA/'
# Output table folder location 
out_data_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/Data_Analyses/PROGRAM/'
# Source R-code folder location 
source_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/Data_Analyses/PROGRAM/R-code/'

############################################################################
#  0.2 Define crab data   
############################################################################
# Data file name:  Keep in the data directory
crabdata_file1 <- 'ADFG_Crab.csv'
crabdata_file2 <- 'NOAA_1976_1991_Crab.csv'
crabdata_file3 <- 'NOAA_RKC_NBS.csv'

############################################################################
#  0.3 Define abundnace estimate  
############################################################################
estimate <- 'ADFG' 
#estimate <- 'CPT' 

############################################################################
#  0.3 Read haul data  
############################################################################
source(paste0(source_dir,'Noron_Sound_Read_tow_data.r'))

############################################################################
# functiopn  Crab Classificaiton  ADFG and CPT
############################################################################
adfg.class <- function(crabdata){
  crab.class <- with(crabdata,
		ifelse(Sex==2,'female',
		ifelse(Legal==2,'Legal',
		ifelse(Size_mm>89,'Pre1', 
		ifelse(Size_mm>75,'Pre2','Pre3')))))
  return(crab.class)
}

cpt.class <- function(crabdata){
  crab.class <-with(crabdata,
	ifelse(Sex==2,'female',
	ifelse(Size_mm>=64,'Total','other')))
	return(crab.class)
}


############################################################################
# 1.0  Read ADFG Crab data  
############################################################################
# Data file name:  Keep in the data directory
adfg <- read.csv(paste0(data_dir,crabdata_file1),header=TRUE)
# Add Sampling factor  
adfg$Sampling.Factor <- 1
# Remove unnecessary data 
adfg$Vessel <- 1
adfg <- adfg[,c('Year','Vessel','Haul','Sex','Size_mm','Legal','Sampling.Factor')]
adfg$Agent <- 'ADFG'

############################################################################
# 2.0  Read NOAA Crab data  
############################################################################
# Data file name:  Keep in the data directory
noaa <- read.csv(paste0(data_dir,crabdata_file2),header=TRUE)
# 1: sublegal or female, 2: Legal
noaa$Legal <- ifelse(noaa$Size_mm >104 & noaa$Sex ==1,2,1)
# Remove unnecessary data 
noaa$Vessel <- 1
noaa <- noaa[,c('Year','Vessel','Haul','Sex','Size_mm','Legal','Sampling.Factor')]
noaa$Agent <- 'NOAA'

############################################################################
# 3.0  Read NOAA NBS Crab data  
############################################################################
# Data file name:  Keep in the data directory
nbs <- read.csv(paste0(data_dir,crabdata_file3),header=TRUE)
# 1: sublegal or female, 2: Legal
nbs$Legal <- ifelse(nbs$Size_mm >104 & nbs$Sex ==1,2,1)
# Remove unnecessary data 
nbs <- nbs[,c('Year','Vessel','Haul','Sex','Size_mm','Legal','Sampling.Factor')]
nbs$Agent <- 'NBS'

############################################################################
# 4.0  Combine all data and Classify crab based on classification
############################################################################
# Combine all data:
crabdata <- rbind(noaa,adfg,nbs)
# Classify each crab
if (estimate=='ADFG'){
crabdata$class <- adfg.class(crabdata)
}
if (estimate=='CPT'){
crabdata$class <- cpt.class(crabdata)
}
# sum each crab based on classficaiton long format
ncrab <- aggregate(Sampling.Factor ~ Year+Agent+Vessel+Haul+class, FUN=sum, data=crabdata)
# Change to wide format 
ncrab.w <- dcast(ncrab,Year+Agent+Vessel+Haul ~ class, values.var='Sampling.Factor')
#merge with haul data 
crabdata.w <- merge(haul,ncrab.w, by = c('Year','Agent','Vessel','Haul'),all=TRUE)
# Add zero to stations with no crab 
#Cnage back to long format  
crabdata <- melt(crabdata.w, id.vars = names(haul),
  variable.name = "class", 
  value.name = "n")
#Change NA to zero
crabdata$n <- ifelse(is.na(crabdata$n),0,crabdata$n)
crabdata$Area_Nm2 <- with(crabdata, ifelse(is.na(Area_Nm2),0,Area_Nm2)) 
# calculate abundance by station 
# Kilometers 
crabdata$estkm <- with(crabdata,ifelse(is.na(Swept_km2),0,n*Area_Nm2*(1.852^2)/Swept_km2))
# Nautical miles
crabdata$estnm <- with(crabdata,ifelse(is.na(Swept_NM2),0,n*Area_Nm2/Swept_NM2))
# CPUE
############################################################################
# 6.0  Remove resample and no ADF&G station data            
############################################################################
crabdata <- crabdata[is.na(crabdata$Haul_rate),]

############################################################################
# 8.0  ADFG Estimates:  
############################################################################
if(estimate =='ADFG'){
crabsum <- summaryBy(estnm ~ Year+Agent+class,FUN=c(sum,sd,length),data=crabdata)
crabsum$cv <- with(crabsum,sqrt(estnm.length)*estnm.sd/estnm.sum)
crabn <- summaryBy(estnm ~ Year+Agent+class,FUN=length,data=crabdata[crabdata$estnm>0,])
crabsum.w <- dcast(crabsum[,c('Year','Agent','class','estnm.sum')], Year+Agent~class)
crabcv.w <- dcast(crabsum[,c('Year','Agent','class','cv')], Year+Agent~class)
crabn.w <- dcast(crabn, Year+Agent~class)
print(crabsum.w)
print(crabcv.w)
print(crabn.w)
}

############################################################################
# 8.0  CPT classification   
############################################################################
if(estimate =='CPT'){
# limit data to standardized stations and Total 
crabdata.c <- crabdata[!is.na(crabdata$CPT_STD) & crabdata$CPT_STD =='S' & crabdata$class=='Total',] 
crabsum <- summaryBy(estnm ~ Year+Agent,FUN=c(sum,sd,length),data=crabdata.c)
crabn <- summaryBy(estnm ~ Year+Agent,FUN=length,data=crabdata.c[crabdata.c$estnm>0,])
names(crabn)<- c('Year','Agent','n')
crabsum <- merge(crabsum, crabn,by=c('Year','Agent'))
crabsum$cv <- with(crabsum,sqrt(estnm.length)*estnm.sd/estnm.sum)
print(crabsum)
}







