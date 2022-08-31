################################################################################
#   Norton Sound Trawl Survey Harvest Estimation Program in R
#   Version 1.1   02/07/2017
#   By Toshihide "Hamachan" Hamazaki
################################################################################

################################################################################
#  0.0  Initialize working Environment                                          
################################################################################
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
# remove retow data 

data_file1 <- 'NMFS_76_91/Species76-96.csv'
data_file2 <- 'ADFG/ADFG_Species.csv'
data_file3 <- 'NOAA_NBS/NOAA_NBS_Species.csv'
data_file4 <- 'Spcode2.csv'
data_file5 <- 'Station.csv'

#-------------------------------------------------------------------------------
# Include NOAA survey 
noaasp <- read.csv(paste0(data_dir,data_file1),na='',header=TRUE)
noaasp$Agent <- ifelse(noaasp$Year < 1996,'NOAA','ADFG')
# Data file name:  Keep in the data directory
adfgsp <- read.csv(paste0(data_dir,data_file2),na='',header=TRUE)
adfgsp[is.na(adfgsp)] <- 0
adfgsp$Agent <- 'ADFG'
adfgsp <- merge(haul[c('Agent','Haul','Year','rf')],adfgsp,by=c('Agent','Haul','Year'))
adfgsp$Weight_kg <- with(adfgsp,rf*as.numeric(subsample_w_kg)+as.numeric(wholesample_w_kg))
adfgsp$Total_n <- with(adfgsp,rf*as.numeric(subsample_n)+as.numeric(wholesample_n))

#===============================================================================
# 3.0  Read NOAA NBS Crab data  
#===============================================================================
# Data file name:  Keep in the data directory
noaaNBS <- read.csv(paste0(data_dir,data_file3),na='',header=TRUE)
# Renumber Hauls 
noaaNBS$Haul <- with(noaaNBS, HAUL + 10*VESSEL - min(VESSEL))
# Add Agent name
noaaNBS$Agent <- 'NBS'


############################################################################
# 0.3  Define Analyses Environment   
############################################################################
spp <- rbind(noaasp,adfgsp[,c('Year','Haul','Spcode','Total_n','Weight_kg','Agent')])
############################################################################
# 0.3  Spcode corrections   
############################################################################
spp$Spcode[spp$Spcode==10226] <- 10220
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
spp <- spp[spp$Spcode < 99990 & spp$Spcode > 2,] 

############################################################################
# Combine minor species to taxon level   
############################################################################
#combine sp:10260 and 10261 to 10260
spp$Spcode[spp$Spcode==10260] <- 10261
#combine sp:21313,21314,21315 to 21313
spp$Spcode[spp$Spcode %in% c(21314,21315)] <- 21313
#combine sp:21368,21370,21371,21375,21377,21378 to 21375
spp$Spcode[spp$Spcode %in% c(21368,21370,21371,21377,21378)] <- 21375
#combine sp:21721,21722 to 21720: Combine Pacific Cod juv adult
spp$Spcode[spp$Spcode %in% c(21721,21722)] <- 21720
#combine sp:21741,21742 to 21740: Combine Polluck juv adult
spp$Spcode[spp$Spcode %in% c(21741,21742)] <- 21740
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

# sum by revised spcode
spp.s <- aggregate(cbind(Total_n, Weight_kg)~ Year+Agent+Haul+Spcode,FUN=sum,data=spp)


#-------------------------------------------------------------------------------
# Combine tow and speciess data     
#-------------------------------------------------------------------------------
nssp <- merge(haul,spp.s,by=c('Year','Agent','Haul'))
nssp$CPUE <- with(nssp,Weight_kg/Swept_km2)
# nssp is the base species conde
# Remove bad haul data 
nssp <- nssp[which(is.na(nssp$Haul_rate)),]

nssp.w <- dcast(nssp,Year+Agent+Haul~Spcode)
nssp.w[nssp.w$Year==1996,1:5]

#-------------------------------------------------------------------------------
# Calculate CPUE Index:  geometric mean CPUE*(proportion of stations the species is present)
#-------------------------------------------------------------------------------
 # Calculate mean log CPUE for each species 
nssp.i <- aggregate(log(CPUE) ~ Year+Agent+Spcode,FUN=mean,data=nssp[which(nssp$CPUE>0),])
# Find the number of stations with each species present 
nssp.n <- aggregate(CPUE ~ Year+Agent+Spcode,FUN=length,data=nssp[which(nssp$CPUE>0),])
nssp.s <- merge(nssp.i,nssp.n,by=c('Year','Agent','Spcode'))
# Find the number of stations with Trawled  
nssp.st <-aggregate(Haul~Agent+Year,FUN=length,data=nssp.w)
# Combined the data and rename 
nssp.s <- merge(nssp.s,nssp.st,by=c('Year','Agent'))
names(nssp.s)[4:6] <- c('ml.cpue','ns','nt')
# I.CPUE is geometric mean CPUE Index. 
nssp.s$I.CPUE <- with(nssp.s,exp(ml.cpue)*ns/nt)
#Add spcode 
spn  <- read.csv(paste0(data_dir,data_file4),na='',header=TRUE)
nssp.s <- merge(nssp.s,spn,by=c('Spcode'))

write.csv(nssp.s, paste0(data_dir,'CPUE_Index.csv'))

