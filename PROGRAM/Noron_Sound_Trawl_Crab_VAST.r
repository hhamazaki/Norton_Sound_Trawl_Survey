###########################################################################
#   Norton Sound Trawl Survey Harvest Estimation Program in R
#   Version 1.1   02/07/2017
#   This program esitmate abundance of
#   Legal (CW>120mm)
#   Pre-recruit 1 and 2 
#   By Toshihide "Hamachan" Hamazaki
############################################################################

############################################################################
#  0.0  Initialize working Environment                                          
############################################################################
rm(list=ls(all=TRUE))
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
#  0.2 Read haul data  
############################################################################
source(paste0(source_dir,'Noron_Sound_Read_tow_data.r'))

############################################################################
# functiopn 
############################################################################
adfg.class <- function(crabdata){
  crab.class <- with(crabdata,
		ifelse(Sex==2,'female',
		ifelse(Legal==2,'Legal',
		ifelse(Size_mm>89,'Pre1', 
		ifelse(Size_mm>75,'Pre2','Pre3')))))
  return(crab.class)
}

############################################################################
# 4.1  For Assessment model 
############################################################################
cpt.class <- function(crabdata){
  crab.class <-with(crabdata,
	ifelse(Sex==2,'female',
	ifelse(Size_mm>=64,'Totalmale','Juvemile')))
	return(crab.class)
}

		
############################################################################
#  0.3 Define crab data   
############################################################################
# Data file name:  Keep in the data directory
data_file1 <- 'ADFG_Crab.csv'
data_file2 <- 'NOAA_1976_1991_Crab.csv'
data_file3 <- 'NOAA_RKC_NBS.csv'

############################################################################
# 1.0  Read ADFG Crab data  
############################################################################
# Data file name:  Keep in the data directory
adfg <- read.csv(paste0(data_dir,data_file1),na='.',header=TRUE)
# Add Sampling factor  
adfg$Sampling.Factor <- 1
# Remove unnecessary data 
adfg$Agent <- 'ADFG'
adfg <- adfg[,c('Year','Haul','Sex','Size_mm','Legal','Sampling.Factor')]
adfg$Agent <- 'ADFG'
############################################################################
# 2.0  Read NOAA Crab data  
############################################################################
# Data file name:  Keep in the data directory
noaa <- read.csv(paste0(data_dir,data_file2),na='.',header=TRUE)
# 1: sublegal or female, 2: Legal
noaa$Legal <- ifelse(noaa$Size_mm >104 & noaa$Sex ==1,2,1)
# Remove unnecessary data 
noaa <- noaa[,c('Year','Haul','Sex','Size_mm','Legal','Sampling.Factor')]
noaa$Agent <- 'NOAA'
############################################################################
# 3.0  Read NOAA 2010 Crab data  
############################################################################
# Data file name:  Keep in the data directory
nbs <- read.csv(paste0(data_dir,data_file3),na='',header=TRUE)
# 1: sublegal or female, 2: Legal
nbs$Legal <- ifelse(nbs$Size_mm >104 & nbs$Sex ==1,2,1)
# NBS survey haul number is by Vessel, Change haul number so that each haul has unique number 
nbs$Haul <- nbs$Haul + 10*(nbs$Vessel - min(nbs$Vessel))
# Remove unnecessary data 
nbs <- nbs[,c('Year','Haul','Sex','Size_mm','Legal','Sampling.Factor')]
nbs$Agent <- 'NBS'


############################################################################
# 5.0  Calculate total number of crab by class  and combine with haul data
############################################################################
crabdata <- rbind(noaa,adfg,nbs)
# Add size classes 
#crabdata$class <- cpt.class(crabdata)
crabdata$class <- adfg.class(crabdata)
# summarize by size class 
ncrab <- aggregate(Sampling.Factor ~ Year+Agent+Haul+class, FUN=sum, data=crabdata)
# Add haul data 
# Change from long to wide 
ncrab.w <- reshape(ncrab[,c('Year','Agent','Haul','class','Sampling.Factor')],timevar='class',idvar=c('Year','Agent','Haul'),direction='wide')
# Drop the last one 
ncrab.w <- ncrab.w[,-7]
# Rename 
names(ncrab.w) <- c('Year','Agent','Haul','Totalmale','Female','Juvenile')
# Remov bad, repeat, or no Swept info hauls  
haul.c <- haul[is.na(haul$Haul_rate) & (!(is.na(haul$Swept_NM2))),]
# merge ncrab data with haul data 
crabdata.w <- merge(haul.c,ncrab.w, by = c('Year','Agent','Haul'),all.x=TRUE)
# put 0 to missing data 
crabdata.w[is.na(crabdata.w)]<-0
write.csv(ncrab.w,paste(data_dir,'crabdata_vast_3.csv',sep=''),row.names=T) 


