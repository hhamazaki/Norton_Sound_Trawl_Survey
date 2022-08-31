###########################################################################
#   Norton Sound Trawl Survey Harvest Estimation Program in R
#   Version 1.1   02/07/2017
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
# 0.2  Read tow related data
############################################################################
# Data file name:  Keep in the data directory
data_file1 <- 'NOAA_1976_1991_Haul_data.csv'
data_file2 <- 'ADFG_Haul_data.csv'
data_file3 <- 'raise_data2.csv'
data_file4 <- 'NOAA_NBS_Haul.csv'
data_file5 <- 'NOAA_NBS_Station.csv'
data_file6 <- 'Station.csv'

#---------------------------------------------------------------------------
# Conversion factors 
ft.to.nm <- 0.000164579
ft.to.km <- 0.0003048
nm.km <- 1.852
#---------------------------------------------------------------------------

############################################################################
# 1.0  Read Haul data    
############################################################################
# Data file name:  Keep in the data directory
NOAA.haul <- read.table(paste0(data_dir,'NMFS_76_91/',data_file1), na = '', 
	header = TRUE,sep = ',', dec='.')
# NOAA DATA: calculated as net width(50ft)*(0003048: convert to Km)*tow distance(NM)*(1.852: convert to km)
NOAA.haul$Swept_km2 <- with(NOAA.haul,50*ft.to.km*tow_distance_NM*nm.km)
NOAA.haul$Swept_NM2 <- with(NOAA.haul,50*ft.to.nm*tow_distance_NM)
NOAA.haul$Agent <- 'NOAA'
NOAA.haul$Vessel <- 1
NOAA.haul <- NOAA.haul[,c('Year','Agent','ADFG_Station','Vessel','Haul','Haul_rate','Month','Day','Latitude','Longitude','Swept_km2','Swept_NM2','Depth_m','Bottom_Temp','Surface_Temp')]
  
# Data file name:  Keep in the data directory
ADFG.haul <- read.table(paste0(data_dir,'ADFG/', data_file2), na = '', 
	header = TRUE,sep = ',', dec='.')
# ADFG DATA converstion*: ADF&G DATA: calculated as net width(40ft)*tow distance(NM)
ADFG.haul$Swept_km2 <- with(ADFG.haul,40*ft.to.km*tow_distance_NM*nm.km)
ADFG.haul$Swept_NM2 <- with(ADFG.haul, 40*ft.to.nm*tow_distance_NM)
ADFG.haul$Agent <- 'ADFG'
ADFG.haul$Vessel <- 1
ADFG.haul <- ADFG.haul[,c('Year','Agent','ADFG_Station','Vessel','Haul','Haul_rate','Month','Day','Latitude','Longitude','Swept_km2','Swept_NM2','Depth_m','Bottom_Temp','Surface_Temp')]

# Combine data  
haul <- rbind(NOAA.haul,ADFG.haul)

############################################################################
# 5.0  Read Station data    
############################################################################
station <- read.csv(paste0(data_dir,data_file6),na='.',header=TRUE)
haul <- merge(haul,station[,c('ADFG_Station','Area_Nm2','ADFG_tier','CPT_STD')], by=c('ADFG_Station'),all.x = TRUE)
############################################################################
# 2.0 Read raise data (ADFG survey)  
############################################################################
# Data file name:  Keep in the data directory
raise <- read.csv(paste0(data_dir,'ADFG/',data_file3), na='', header=TRUE)
# Calculate rasie factor 
raise$Adjusted_Catch_kg <- with(raise, Gross_Catch_Weight_kg - Tare_kg - Large_fish.Debris_kg - RKC_kg)
raise$rf <- with(raise, as.numeric(Adjusted_Catch_kg)/as.numeric(SubSample_Weight_kg))
# If raise factor is less than 1 change to 1  
raise[which(raise$SubSample_Weight_kg==0|raise$rf<1),'rf'] <- 1
raise <- raise[,c('Haul','Year','rf')]
raise$Agent <-'ADFG'
############################################################################
#  3.0 Read 2010 NOAA survey data      
############################################################################
nbs.haul <- read.csv(paste0(data_dir,'NOAA_NBS/',data_file4),na='',header=TRUE) 
nbs.station <- read.csv(paste0(data_dir,'NOAA_NBS/',data_file5),na='',header=TRUE) 
nbs <- merge(nbs.haul,nbs.station, by = c('NOAA_Station'),all=TRUE)
nbs$Haul<- with(nbs, Haul + 10*Vessel - min(Vessel))
# Calculatate tow area (km^2)
nbs$Swept_km2 <- nbs$Area_Swept_Nm2*(nm.km^2)
nbs$Swept_NM2 <- nbs$Area_Swept_Nm2
nbs$Agent <- 'NBS'
nbs <- nbs[,c('Year','Agent','ADFG_Station','Vessel','Haul','Haul_rate','Month','Day','Latitude','Longitude','Swept_km2','Swept_NM2','Depth_m','Bottom_Temp','Surface_Temp','Area_Nm2')]
nbs <- merge(nbs,station[,c('ADFG_Station','ADFG_tier','CPT_STD')], by=c('ADFG_Station'),all.x = TRUE)
############################################################################
#  7.0 Combine Haul and raise data     
############################################################################
haul <- rbind(haul, nbs)
haul <- merge(haul,raise, by=c('Year','Agent','Haul'),all=TRUE)


