###########################################################################
#   Norton Sound Trawl Survey Length summary Program in R
#   This program summarizes crab samples obtained from trawl survey
#   The data will be used for 
#   By Toshihide "Hamachan" Hamazaki
############################################################################

############################################################################
#  0.0  Initialize working Environment                                          
############################################################################
#rm(list=ls(all=TRUE))
#library(doBy)
############################################################################

############################################################################
#  0.1  Define Folder Directories:                                                    
############################################################################
# Data folder location 
data_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/NS_Trawl_Survey/DATA/'
# Output table folder location 
out_data_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/NS_Trawl_Survey/PROGRAM/'
# Source R-code folder location 
source_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/NS_Trawl_Survey/PROGRAM/R-code/'

############################################################################
#  0.3 Define crab data   
############################################################################
# Data file name:  Keep in the data directory
data_file1 <- 'ADFG/ADFG_Crab.csv'
data_file2 <- 'NMFS_76_91/NOAA_1976_1991_Crab.csv'
data_file3 <- 'NOAA_NBS/NOAA_RKC_NBS.csv'

############################################################################
# 1.0  Read ADFG Crab data  
############################################################################
# Data file name:  Keep in the data directory
adfg.o<- read.csv(paste0(data_dir,data_file1),na='.',header=TRUE)
# Add Sampling factor  
adfg.o$Sampling.Factor <- 1
# Remove unnecessary data 
adfg <- adfg.o[,c('Year','Sex','Shell','Size_mm','Sampling.Factor')]
adfg$Agent <- 'ADFG'
############################################################################
# 2.0  Read NOAA Crab data  
############################################################################
# Data file name:  Keep in the data directory
noaa <- read.csv(paste0(data_dir,data_file2),na='.',header=TRUE)
# Remove unnecessary data 
noaa <- noaa[,c('Year','Sex','Shell','Size_mm','Sampling.Factor')]
noaa$Agent <- 'NOAA'
############################################################################
# 3.0  Read NOAA 2010 Crab data  
############################################################################
# Data file name:  Keep in the data directory
noaa2010 <- read.csv(paste0(data_dir,data_file3),na='',header=TRUE)
# Remove 2018
noaa2010 <- noaa2010[which(noaa2010$Year != 2018),]
# Remove unnecessary data 
noaa2010 <- noaa2010[,c('Year','Sex','Shell','Size_mm','Sampling.Factor')]
noaa2010$Agent <- 'NOAA'

############################################################################
# 4.0  Combine all Crab data and reclassify 
############################################################################
crabs <- rbind(noaa,adfg,noaa2010)
# Limit data to male (Sex=1) with Shell info (Shell > 0)
crabs <- with(crabs,crabs[Sex == 1 & Shell > 0, ])
# Change Sehll to NewShell (1) and OldShell (2)
crabs$Shell <- with(crabs,ifelse(Shell <=2,1,2))

############################################################################
# 5.0  Calculate total number of crab by class  and combine with haul data
############################################################################
ncrab <- aggregate(Sampling.Factor ~ Year+Agent+Size_mm+Shell, FUN=sum, data=crabs)
