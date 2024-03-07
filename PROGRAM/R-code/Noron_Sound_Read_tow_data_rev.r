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
base_dir <- file.path('C:','Projects','Norton_Sound','NSCrab','Trawl_data','NS_Trawl_Survey')
data_dir <- file.path(base_dir,'DATA')
# Output table folder location 
out_data_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/NS_Trawl_Survey/PROGRAM/'
# Source R-code folder location 
source_dir <- file.path(base_dir,'PROGRAM','R-code')

############################################################################
# 0.2  Read tow related data
############################################################################
# Data file name:  Keep in the data directory
data_file1 <- 'NOAA_1976_1991_Haul_data.csv'
data_file2 <- 'ADFG_Haul_data.csv'
data_file3 <- 'raise_data2.csv'
data_file4 <- 'NBS_Haul_DATA.csv'
data_file5 <- 'NOAA_NBS_Station.csv'
data_file6 <- 'Station.csv'

#---------------------------------------------------------------------------
# Conversion factors 
ft.to.nm <- 0.000164579
ft.to.km <- 0.0003048
nm.km <- 1.852
fathom.to.m <- 1.8288
#---------------------------------------------------------------------------

############################################################################
# 1.0  Read Haul data    
############################################################################
# Data file name:  Keep in the data directory
NOAA.haul <- read.csv(file.path(data_dir,'NMFS_76_91',data_file1), na ='', header = TRUE)
# NOAA DATA: calculated as net width(50ft)*(0003048: convert to Km)*tow distance(NM)*(1.852: convert to km)
NOAA.haul$Swept_km2 <- with(NOAA.haul,50*ft.to.km*tow_distance_NM*nm.km)
NOAA.haul$Swept_NM2 <- with(NOAA.haul,50*ft.to.nm*tow_distance_NM)
NOAA.haul$Agent <- 'NOAA'
NOAA.haul$Vessel <- 1
NOAA.haul <- NOAA.haul[,c('Year','Agent','ADFG_Station','Vessel','Haul','Haul_rate','Month','Day','Latitude','Longitude','Swept_km2','Swept_NM2','Depth_m','Bottom_Temp','Surface_Temp')]

#---------------------------------------------------------------------------
#  Read ADF&G Data
#---------------------------------------------------------------------------
haul.file.list <- list.files(file.path(data_dir,'ADFG','Haul','Haul_data'))
n <- length(haul.file.list)
ADFG.haul <- data.frame()
for(i in 1:n){
 temp <- read.csv(file.path(data_dir,'ADFG','Haul','Haul_data',haul.file.list[i]),na='',header=TRUE)
 ADFG.haul <- rbind(ADFG.haul,temp)
}
# Change format 
ADFG.haul$Station <- as.numeric(ADFG.haul$Station)
# rename Staion to ADFG_Station
names(ADFG.haul)[names(ADFG.haul)=='Station'] <- 'ADFG_Station'
ADFG.haul$Date <- as.Date(ADFG.haul$Date,format='%m/%d/%Y')
ADFG.haul$Month <- as.integer(format(ADFG.haul$Date, "%m"))
ADFG.haul$Day <- as.integer(format(ADFG.haul$Date, "%d"))
# Change Deg min to Deg.dec 
ADFG.haul$Lat_Start <- with(ADFG.haul,Lat_Deg_Start+Lat_Min_Start/60)
ADFG.haul$Long_Start <- with(ADFG.haul,-1*(Long_Deg_Start+Long_Min_Start/60))
ADFG.haul$Lat_End <- with(ADFG.haul,Lat_Deg_End+Lat_Min_End/60)
ADFG.haul$Long_End <- with(ADFG.haul,-1*(Long_Deg_End+Long_Min_End/60))
# Change Depth fathom to meter
ADFG.haul$Depth_Min_m <- with(ADFG.haul,Depth_Min_fm*fathom.to.m)
ADFG.haul$Depth_Max_m <- with(ADFG.haul,Depth_Max_fm*fathom.to.m)
ADFG.haul$Depth_Ave_m <- with(ADFG.haul,Depth_Ave_fm*fathom.to.m)
# Calculate Tow_minutes
ADFG.haul$Tow_min <- with(ADFG.haul,
          strptime(Time_End,format='%H:%M')-strptime(Time_Start,format='%H:%M'))
# ADFG DATA converstion*: ADF&G DATA: calculated as net width(40ft)*tow distance(NM)
ADFG.haul$Tow_dist_km <- with(ADFG.haul,Tow_dist_NM*nm.km)
ADFG.haul$Swept_km2 <- with(ADFG.haul,40*ft.to.km*Tow_dist_NM*nm.km)
ADFG.haul$Swept_NM2 <- with(ADFG.haul, 40*ft.to.nm*Tow_dist_NM)
nssp.ADFG <- ADFG.haul[,c('Year','Haul','Haul_rate','Lat_Start','Long_Start','Tow_dist_km','Tow_minutes')]
write.csv(nssp.ADFG, file.path(data_dir,'ADFG_Tows.csv'))
ADFG.haul$Agent <- 'ADFG'
ADFG.haul$Vessel <- 1
names(ADFG.haul)[names(ADFG.haul)=='Depth_Ave_m'] <- 'Depth_m' 
names(ADFG.haul)[names(ADFG.haul)=='Temp_Surf_C'] <- 'Surface_Temp'
names(ADFG.haul)[names(ADFG.haul)=='Temp_Bottom_C'] <- 'Bottom_Temp' 
names(ADFG.haul)[names(ADFG.haul)=='Lat_Start'] <- 'Latitude' 
names(ADFG.haul)[names(ADFG.haul)=='Long_Start'] <- 'Longitude'

ADFG.haul <- ADFG.haul[,c('Year','Agent','ADFG_Station','Vessel','Haul','Haul_rate','Month','Day','Latitude','Longitude','Swept_km2','Swept_NM2','Depth_m','Bottom_Temp','Surface_Temp')]

# Combine data  
haul <- rbind(NOAA.haul,ADFG.haul)




############################################################################
# 5.0  Read Station data    
############################################################################
station <- read.csv(file.path(data_dir,data_file6),header=TRUE)
haul <- merge(haul,station[,c('ADFG_Station','Area_Nm2','ADFG_tier','CPT_STD')], by=c('ADFG_Station'),all.x = TRUE)
############################################################################
# 2.0 Read raise data (ADFG survey)  
############################################################################
# Data file name:  Keep in the data directory
raise.file.list <- list.files(file.path(data_dir,'ADFG','Haul','Subsample_data'))
n <- length(raise.file.list)
ADFG.raise <- data.frame()
for(i in 1:n){
 temp <- read.csv(file.path(data_dir,'ADFG','Haul','Subsample_data',raise.file.list[i])) 
 temp[is.na(temp)] <- 0
 if(!'SubSample_Weight_kg' %in%(names(temp))){
 temp$SubSample_Weight_kg = with(temp,SubSample_Weight_kg_1+SubSample_Weight_kg_2+SubSample_Weight_kg_3)
 }
 temp <- temp[,c("Year","Haul","ADFG_Station","Date","Gross_Catch_Weight_kg","Tare_kg","Large_fish_Debris_kg",
          "RKC_kg", "SubSample_Weight_kg","Remarks")] 
 ADFG.raise <- rbind(ADFG.raise,temp)
}

# Adjusted cath is Gross cath minus tare, large fish, debris, and RKC
ADFG.raise$Adjusted_Catch_kg <- with(ADFG.raise, Gross_Catch_Weight_kg - Tare_kg - Large_fish_Debris_kg - RKC_kg)
# raise factor is adjusted catch devided by subsample weight
ADFG.raise$rf <- with(ADFG.raise, as.numeric(Adjusted_Catch_kg)/as.numeric(SubSample_Weight_kg))
# If raise factor is less than 1 change to 1  
ADFG.raise[which(ADFG.raise$SubSample_Weight_kg==0|ADFG.raise$rf<1),'rf'] <- 1
ADFG.raise <- ADFG.raise[,c('Haul','Year','rf')]
ADFG.raise$Agent <-'ADFG'

############################################################################
#  3.0 Read 2010 NOAA survey data      
############################################################################
nbs.haul <- read.csv(file.path(data_dir,'NOAA_NBS',data_file4)) 
nbs.station <- read.csv(file.path(data_dir,'NOAA_NBS',data_file5)) 
nbs <- merge(nbs.haul,nbs.station, by = c('NOAA_Station'),all.x=TRUE)
nbs$Haul<- with(nbs, Haul + 100*Vessel)
# Calculatate tow area (km^2)
nbs$Swept_km2 <- with(nbs,Distance_Fished_Km*Net_Width_M/1000) 
nbs$Swept_NM2 <- nbs$Swept_km2/(nm.km^2)
nbs$Agent <- 'NBS'
nbs$Haul_rate <- NA
nbs$Month <- format(as.Date(nbs$Date.Time,'%m/%d/%Y'),"%m")
nbs$Day <- format(as.Date(nbs$Date.Time,'%m/%d/%Y'),"%d")
nbs <- nbs[,c('Year','Agent','ADFG_Station','Vessel','Haul','Haul_rate','Month','Day','Latitude','Longitude','Swept_km2','Swept_NM2','Depth_m','Bottom_Temp','Surface_Temp','Area_Nm2')]
nbs <- merge(nbs,station[,c('ADFG_Station','ADFG_tier','CPT_STD')], by=c('ADFG_Station'),all.x = TRUE)
############################################################################
#  7.0 Combine Haul and raise data     
############################################################################
haul <- rbind(haul, nbs)
haul <- merge(haul,ADFG.raise, by=c('Year','Agent','Haul'),all=TRUE)


