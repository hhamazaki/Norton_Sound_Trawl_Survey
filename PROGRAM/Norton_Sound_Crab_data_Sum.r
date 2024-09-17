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
base_dir <- file.path('C:','Projects','Norton_Sound','NSCrab','Trawl_data','NS_Trawl_Survey')
data_dir <- file.path(base_dir,'DATA')
# Output table folder location 
#out_data_dir <- file.path(base_dir,'PROGRAM')
# Source R-code folder location 
#source_dir <- file.path(out_data_dir, 'R-code')
st <- c('Year','Agent','Sex','Shell','Size_mm','Sampling.Factor')
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
noaa <- read.csv(file.path(data_dir,'NMFS_76_91','NMFS_1976_1991_Crab.csv'),skip=6,header=TRUE)
noaa$Size_mm[noaa$Size_mm==0] <- NA  
# Add Agenget name 
noaa$Agent <- 'NOAA'
Trawl76 <- c(1:26,136:234,243,252:258)  
noaa76 <- noaa[which(noaa$Year==1976 & noaa$Haul %in% Trawl76),]  # Remove crab taken out of NS. 
noaa <- rbind(noaa76,noaa[which(noaa$Year !=1976),])
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
NBS.crab$Size_mm[NBS.crab$Size_mm==0] <- NA  
# Add Agent name
NBS.crab$Agent <- 'NBS'
# Remove unnecessary data 
NBS.crab <- NBS.crab[which(NBS.crab$Year != 2018),]  # 2018 was experimental year: remove
NBS.crab <- NBS.crab[,st]



############################################################################
# 4.0  Combine all Crab data and reclassify 
############################################################################
crabs <- rbind(noaa,ADFG.crab,NBS.crab)
crabs <- with(crabs,crabs[which(Sex == 1 & !is.na(Shell)), ])
# Change Sehll to NewShell (1) and OldShell (2)
# Shell= 0 means that shell is molting, and we put them into NewShell 
crabs$Shell <- with(crabs,ifelse(Shell <=2,1,2))

############################################################################
# 5.0  Calculate total number of crab by class  and combine with haul data
############################################################################
ncrab <- aggregate(Sampling.Factor ~ Year+Agent+Size_mm+Shell, FUN=sum, data=crabs)
