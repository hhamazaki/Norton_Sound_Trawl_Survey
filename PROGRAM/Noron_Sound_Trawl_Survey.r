###########################################################################
#   Norton Sound Trawl Survey Harvest Estimation Program in R
#   Version 1.1   02/07/2017
#   By Toshihide "Hamachan" Hamazaki
############################################################################

############################################################################
#  0.0  Initialize working Environment                                          
############################################################################
rm(list=ls(all=TRUE))
library(doBy)

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
# 0.2  Define Analyses Environment   
############################################################################
# Data file name:  Keep in the data directory
data_file1 <- 'Haul_data.csv'
data_file2 <- 'raise_data.csv'
data_file3 <- 'Species76-96.csv'
data_file4 <- 'Species99-17.csv'
data_file6 <- 'Spcode.csv'
data_file7 <- 'Station.csv'
data_file8 <- 'NOAA_2010.csv'
data_file9 <- 'NOAA_2010_station.csv'


############################################################################
# 0.3  Define Analyses Environment   
############################################################################
# Data file name:  Keep in the data directory
haul <- read.csv(paste0(data_dir,data_file1),na='',header=TRUE)
# Remove bad haul data (Haul_rate = d) 
haul <- haul[which(is.na(haul$Haul_rate)| haul$Haul_rate!= 'd'),]
# NOAA DATA converstion: 
# NOAA DATA: calculated as net width(50ft)*(0003048: convert to Km)*tow distance(NM)*(1.852: convert to km)
# ADFG DATA converstion*: ADF&G DATA: calculated as net width(40ft)*tow distance(NM)*(1.852: convert to km)
haul$towarea <- with(haul,ifelse(Year <= 1991,50*(0.0003048)*tow_distance_NM*1.852,
  40*(0.0003048)*tow_distance_NM*1.852))
# Remove unnecessary data 
haul <- haul[,c('Year','ADFG_Station','Haul','Month','Day','towarea','Depth_m','Bottom_Temp','Surface_Temp')]
############################################################################
# 0.3  Define Analyses Environment   
############################################################################
# Data file name:  Keep in the data directory
station <- read.csv(paste0(data_dir,data_file7),na='',header=TRUE)
station <- station[,c('ADFG_Station','ADFG_tier')]


############################################################################
# 0.3  Define Analyses Environment   
############################################################################
# Data file name:  Keep in the data directory
raise <- read.csv(paste0(data_dir,data_file2),na='',header=TRUE)
# Calculate rasie factor 
raise$rf <- with(raise,Adjusted_Net_Catch_kg/SubSample_Weight_kg)
# If raise factor is less than 1 change to 1  
raise[which(raise$SubSample_Weight_kg==0|raise$rf<1),'rf'] <- 1
raise <- raise[,c('Haul','Year','rf')]


############################################################################
# 0.3  Define Analyses Environment   
############################################################################
# Data file name:  Keep in the data directory
adfgsp <- read.csv(paste0(data_dir,data_file4),na='',header=TRUE)
adfgsp[is.na(adfgsp)] <- 0
adfgsp <- merge(raise[,c('Haul','Year','rf')],adfgsp,by=c('Haul','Year'),all=TRUE)
adfgsp$Weight <- with(adfgsp,rf*as.numeric(subsample_w_kg)+as.numeric(wholesample_w_kg))
adfgsp$Total_n <- with(adfgsp,rf*as.numeric(subsample_n)+as.numeric(wholesample_n))


############################################################################
# 0.3  Define Analyses Environment   
############################################################################
# Data file name:  Keep in the data directory
noaasp <- read.csv(paste0(data_dir,data_file3),na='',header=TRUE)

spp <- rbind(noaasp,adfgsp[,c('Year','Haul','Spcode','Weight','Total_n')])



############################################################################
# 0.3  Spcode corrections   
############################################################################
spp$Spcode[spp$Spcode==10226] <- 10262
spp$Spcode[spp$Spcode==12388] <- 21388
spp$Spcode[spp$Spcode==43001] <- 43010
spp$Spcode[spp$Spcode==40080] <- 40050
spp$Spcode[spp$Spcode==44580] <- 41580
spp$Spcode[spp$Spcode==48577] <- 68577
spp$Spcode[spp$Spcode==48580] <- 68580
spp$Spcode[spp$Spcode==56130] <- 56310
spp$Spcode[spp$Spcode==66111] <- 66611
spp$Spcode[spp$Spcode==66610] <- 66601
spp$Spcode[spp$Spcode==69091] <- 69061
spp$Spcode[spp$Spcode==69616] <- 69316
spp$Spcode[spp$Spcode==69781] <- 68781
spp$Spcode[spp$Spcode==72065] <- 72060
spp$Spcode[spp$Spcode==80100] <- 80200
spp$Spcode[spp$Spcode==80206] <- 80200
spp$Spcode[spp$Spcode==80450] <- 80540
spp$Spcode[spp$Spcode==82150] <- 82510
spp$Spcode[spp$Spcode==82501] <- 82510
spp$Spcode[spp$Spcode==89025] <- 98205
spp$Spcode[spp$Spcode==89082] <- 98082
spp$Spcode[spp$Spcode==98025] <- 98205
spp$Spcode[spp$Spcode==99800] <- 98000
spp$Spcode[spp$Spcode==21379] <- 21378 
spp$Spcode[spp$Spcode==24983] <- 74983 
spp$Spcode[spp$Spcode==48082] <- 43082 
spp$Spcode[spp$Spcode==50001] <- 50010 
spp$Spcode[spp$Spcode==51355] <- 21355 
spp$Spcode[spp$Spcode==60061] <- 66601 
spp$Spcode[spp$Spcode==66664] <- 66614 
spp$Spcode[spp$Spcode==68527] <- 68577
spp$Spcode[spp$Spcode==71526] <- 71525
spp$Spcode[spp$Spcode==71842] <- 81742
spp$Spcode[spp$Spcode==72757] <- 72752
spp$Spcode[spp$Spcode==72775] <- 72755
spp$Spcode[spp$Spcode==75752] <- 72752
spp$Spcode[spp$Spcode==82736] <- 82730
spp$Spcode[spp$Spcode==85170] <- 85180
spp$Spcode[spp$Spcode==85286] <- 75286
spp$Spcode[spp$Spcode==88594] <- 80594

#Remove non animals ########################################################
spp <- spp[which(spp$Spcode <= 99993),] 

############################################################################
# Combine minor species    
############################################################################
#combine sp:10260 and 10261 to 10260
spp$Spcode[spp$Spcode==10260] <- 10261
#combine sp:21313,21314,21315 to 21313
spp$Spcode[spp$Spcode %in% c(21314,21315)] <- 21313
#combine sp:21368,21370,21371,21375,21377,21378 to 21375
spp$Spcode[spp$Spcode %in% c(21368,21370,21371,21377,21378)] <- 21375
#combine sp:41201,41221 to 41221
spp$Spcode[spp$Spcode==41201] <- 41221
#combine sp:43000-43090 to 43000
spp$Spcode[spp$Spcode>43000&spp$Spcode<=43090] <- 43000
#combine sp:68577, 68578 to 68577
spp$Spcode[spp$Spcode==68578] <- 68577
#combine sp:68580, 68590 to 68580
spp$Spcode[spp$Spcode==68590] <- 68580
#combine sp:69010-69123 to 69010
spp$Spcode[spp$Spcode>69010&spp$Spcode<=69123] <- 69010
#combine sp:71750,71753 to 71753
spp$Spcode[spp$Spcode==71750] <- 71753
#combine sp:75284,75285, 75286 to 75284*/
spp$Spcode[spp$Spcode %in% c(75285,75286)] <- 75284
#combine sp:80590,80594,80595, 80596 to 80595;
spp$Spcode[spp$Spcode %in% c(80590,80594,80596)] <- 80595
#combine sp:81741,81742 to 81742
spp$Spcode[spp$Spcode==81741] <- 81742
#consolidate sp:82500,82510 to 82510
spp$Spcode[spp$Spcode==82500] <- 82510
#combine sp:83010,83020 to 83020
spp$Spcode[spp$Spcode==83010] <- 83020
#consolidate sp:98000-98200*/
spp$Spcode[spp$Spcode>98000&spp$Spcode<=98300] <- 98000

############################################################################
# Combine tow and speciess data     
############################################################################
nssp <- merge(haul,spp,by=c('Year','Haul'),all=TRUE)
# remove weight is na or 0
nssp <- nssp[which(nssp$Weight>0),]

############################################################################
#  Summarize data by station      
############################################################################
nsspw <- aggregate(Weight~Year+ADFG_Station+Spcode,FUN=sum,data=nssp)
nstow <- aggregate(towarea~Year+ADFG_Station,FUN=sum,data=haul)

############################################################################
#  Calculate CPUE       
############################################################################
nssp1 <- merge(nsspw,nstow,by=c('Year','ADFG_Station'))
nssp1$CPUE <- with(nssp1,Weight/towarea)
year <- aggregate(CPUE~Year+ADFG_Station,length,data=nssp1)
year_station <- aggregate(ADFG_Station~Year,length,data=year)



############################################################################
# NOAA 2010 data     
############################################################################
############################################################################
#  Read 20120 NOAA survey data      
############################################################################
ns2010 <- read.csv(paste0(data_dir,data_file8),na='',header=TRUE) 
# change names
names(ns2010) <- c('lat', 'long', 'NMFS_Station', 'Year', 'Month', 'Day', 'wcpue_kgha', 'ncpue_nha', 'cname','sname','Spcode', 'depth', 'Bottom_Temp','Surface_Temp','Haul')
st2010 <- read.csv(paste0(data_dir,data_file9),na='',header=TRUE) 


############################################################################
# Combine minor species    
############################################################################
#combine sp:10260 and 10261 to 10260
ns2010$Spcode[ns2010$Spcode==10260] <- 10261
#combine sp:21313,21314,21315 to 21313
ns2010$Spcode[ns2010$Spcode %in% c(21314,21315)] <- 21313
#combine sp:21368,21370,21371,21375,21377,21378 to 21375
ns2010$Spcode[ns2010$Spcode %in% c(21368,21370,21371,21377,21378)] <- 21375
#combine sp:41201,41221 to 41221
ns2010$Spcode[ns2010$Spcode==41201] <- 41221
#combine sp:43000-43090 to 43000
ns2010$Spcode[ns2010$Spcode>43000&ns2010$Spcode<=43090] <- 43000
#combine sp:68577, 68578 to 68577
ns2010$Spcode[ns2010$Spcode==68578] <- 68577
#combine sp:68580, 68590 to 68580
ns2010$Spcode[ns2010$Spcode==68590] <- 68580
#combine sp:69010-69123 to 69010
ns2010$Spcode[ns2010$Spcode>69010&ns2010$Spcode<=69123] <- 69010
#combine sp:71750,71753 to 71753
ns2010$Spcode[ns2010$Spcode==71750] <- 71753
#combine sp:75284,75285, 75286 to 75284*/
ns2010$Spcode[ns2010$Spcode %in% c(75285,75286)] <- 75284
#combine sp:80590,80594,80595, 80596 to 80595;
ns2010$Spcode[ns2010$Spcode %in% c(80590,80594,80596)] <- 80595
#combine sp:81741,81742 to 81742
ns2010$Spcode[ns2010$Spcode==81741] <- 81742
#consolidate sp:82500,82510 to 82510
ns2010$Spcode[ns2010$Spcode==82500] <- 82510
#combine sp:83010,83020 to 83020
ns2010$Spcode[ns2010$Spcode==83010] <- 83020
#consolidate sp:98000-98200*/
ns2010$Spcode[ns2010$Spcode>98000&ns2010$Spcode<=98300] <- 98000

ns2010 <- aggregate(wcpue_kgha~Haul+Spcode,FUN=sum,data=ns2010)
ns2010 <- merge(ns2010,st2010,by=c('Haul'))
# Convert to kg/km^2
ns2010$CPUE <- with(ns2010, wcpue_kgha*100)
# Convert toware Nm^2 to km^2
ns2010$towarea <- with(ns2010,Area_Swept_Nm2*(1.852*1.852))
ns2010$Weight <- with(ns2010,CPUE*towarea)
nssp2 <- ns2010[!is.na(ns2010$towarea),names(nssp1)]

# combine two data
nssp <- rbind(nssp1,nssp2)
# Add station information 
nssp <- rbind(nssp1,nssp2)
nssp <- merge(nssp,station,by='ADFG_Station')
#nssp <- nssp[nssp$ADFG_tier !='O',]
# find the number of station surveyed
temp <- aggregate(ADFG_tier~Year+ADFG_Station+Spcode,length,data=nssp)
temp <- aggregate(ADFG_Station~Year+Spcode,length,data=temp)

nstation <- aggregate(ADFG_Station~Year,max,data=temp)

# Extract data 
pacific.cod <- nssp[nssp$Spcode==21720,]


