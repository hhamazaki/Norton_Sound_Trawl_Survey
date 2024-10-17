################################################################################
#   Norton Sound Trawl Survey Harvest Estimation Program in R
#   Version 1.1   02/07/2017
#   By Toshihide "Hamachan" Hamazaki
################################################################################

################################################################################
#  0.0  Initialize working Environment                                          
################################################################################
rm(list=ls(all=TRUE))
library(reshape2)
#-------------------------------------------------------------------------------
#  0.1  Define Folder Directories:                                                    
#-------------------------------------------------------------------------------
# Data folder location 
base_dir <- file.path('C:','Projects','Norton_Sound','NSCrab','Trawl_data','NS_Trawl_Survey')
data_dir <- file.path(base_dir,'DATA')
# Output table folder location 
out_data_dir <- file.path(base_dir,'PROGRAM')
# Source R-code folder location 
source_dir <- file.path(base_dir,'PROGRAM','R-code')
#-------------------------------------------------------------------------------
#  0.2 Read haul data  
#-------------------------------------------------------------------------------
source(file.path(base_dir,'PROGRAM','R-code','Noron_Sound_Read_tow_data_rev.r'))
# remove retow data 
data_file1 <- 'Species76-91.csv'
data_file2 <- 'ADFG_Species.csv'
data_file3 <- 'NB_SP_DATA.csv'
data_file4 <- 'Spcode.csv'
data_file5 <- 'Spcode_Error.csv'
data_file6 <- 'Station.csv'
#-------------------------------------------------------------------------------
spn  <- read.csv(file.path(data_dir,data_file4),na='',header=TRUE)
spn_error <- read.csv(file.path(data_dir,data_file5),na='',header=TRUE)

# Include NOAA survey 
noaasp <- read.csv(file.path(data_dir,'NMFS_76_91',data_file1),na='',header=TRUE)
noaasp$Agent <- 'NOAA'
noaasp <- noaasp[which(noaasp$Weight_kg>0),]
# Data file name:  Keep in the data directory
catch.file.list <- list.files(file.path(data_dir,'ADFG','Species','Catch'))
n <- length(catch.file.list)
ADFG.catch <- data.frame()
for(i in 1:n){
 temp <- read.csv(file.path(data_dir,'ADFG','Species','Catch',catch.file.list[i])) 
 temp <- temp[,c('Year', 'Haul', 'Spcode', 'Sample_kg', 'Sample_n', 'Whole_kg', 'Whole_n')] 
 ADFG.catch <- rbind(ADFG.catch,temp)
}
ADFG.catch$Agent <- 'ADFG'
ADFG.catch[is.na(ADFG.catch)] <-0
ADFG.catch <- merge(haul,ADFG.catch,by=c('Agent','Haul','Year'))
ADFG.catch$rf[is.na(ADFG.catch$rf)]<-1
ADFG.catch$Weight_kg <- with(ADFG.catch,rf*Sample_kg+Whole_kg)
ADFG.catch$Total_n <- with(ADFG.catch,rf*Sample_n+Whole_n)
ADFG.catch<- ADFG.catch[which(ADFG.catch$Weight_kg>0),]

#===============================================================================
# 3.0  Read NOAA NBS Crab data  
#===============================================================================
# Data file name:  Keep in the data directory
noaaNBS <- read.csv(file.path(data_dir,'NOAA_NBS',data_file3),na='',header=TRUE)
# Renumber Hauls 
noaaNBS$Haul <- with(noaaNBS, Haul + 100*Vessel_Id)
# Add Agent name
noaaNBS$Agent <- 'NBS'
noaaNBS <- noaaNBS[which(noaaNBS$Weight_kg>0),]
#-------------------------------------------------------------------------------
# 0.3  Clean data 
#-------------------------------------------------------------------------------
# Standard columns
stclm <- c('Year','Haul','Spcode','Total_n','Weight_kg','Agent')
spp <- rbind(noaasp,ADFG.catch[,stclm],noaaNBS[,stclm])
# Error check 
# Merge error data 
spp <- merge(spp, spn_error,by=c('Spcode'), all.x = TRUE)
# Replace Spcode with error corrected
spp$Spcode <- with(spp,ifelse(!is.na(Spcode.n), Spcode.n,Spcode))
# Drop unnecessary columns
spp <- spp[,stclm]

# Change Egss shell case to eggs
spp$Spcode[spp$Spcode %in% c(401,436,473,474)] <- 1
# Combine Juveniles-adults 
# Pacific Cod
spp$Spcode[spp$Spcode %in% c(21720,21721,21722)] <- 21720
# Walleye Pollock 
spp$Spcode[spp$Spcode %in% c(21740,21741,21742)] <- 21740
# Keep only animals 
spp <- spp[spp$Spcode < 99990 & spp$Spcode > 2,] 

############################################################################
# Combine minor species to taxon level   
############################################################################
# sum by revised spcode
spp.s <- aggregate(Weight_kg~ Year+Agent+Haul+Spcode,FUN=sum,data=spp)
spp.YF <- spp.s[which(spp.s$Spcode == 10210),]
spp.YF <- merge(haul[which(haul$Agent=='ADFG'),],spp.YF[which(spp.YF$Agent=='ADFG'),],by=c('Year','Agent','Haul'),all=TRUE)
write.csv(spp.YF,'Yellowfin.csv',na='')

