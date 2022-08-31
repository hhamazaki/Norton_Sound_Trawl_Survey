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
data_file1 <- 'ADFG_Crab.csv'
data_file2 <- 'NOAA_Crab.csv'
data_file3 <- 'NOAA_RKC_NBS.csv'

############################################################################
# 1.0  Read ADFG Crab data  
############################################################################
# Data file name:  Keep in the data directory
adfg.o <- read.csv(paste0(data_dir,'ADFG/',data_file1),na='.',header=TRUE)
# Add Sampling factor  
adfg.o$Sampling.Factor <- 1
# Remove unnecessary data 
adfg.o$Agent <- 'ADFG'
adfg <- adfg.o[,c('Year', 'Agent','Haul','Sex', 'Size_mm','Shell','Clutch', 'Egg.condition', 'Egg.color','Sampling.Factor')]

############################################################################
# 2.0  Read NOAA Crab data  
############################################################################
# Data file name:  Keep in the data directory
noaa <- read.csv(paste0(data_dir,'NMFS_76_91/',data_file2),na='.',header=TRUE)
# Remove unnecessary data 
noaa$Agent <- 'NOAA'
noaa <- noaa[,c('Year', 'Agent','Haul','Sex', 'Size_mm','Shell','Clutch', 'Egg.condition', 'Egg.color','Sampling.Factor')]

############################################################################
# 3.0  Read NOAA 2010 Crab data  
############################################################################
# Data file name:  Keep in the data directory
noaa2010 <- read.csv(paste0(data_dir,'NOAA_NBS/',data_file3),na='',header=TRUE)
# Remove unnecessary data 
noaa2010$Agent <- 'NOAA'
noaa2010 <- noaa2010[,c('Year', 'Agent','Haul','Sex', 'Size_mm','Shell','Clutch', 'Egg.condition', 'Egg.color','Sampling.Factor')]

############################################################################
# 4.0  Combine all Crab data and reclassify 
############################################################################
crabs <- rbind(noaa,adfg,noaa2010)
# Change Sehll to NewShell (1) and OldShell (2)
crabs$Shell <- with(crabs,ifelse(Shell <=2,1,2))
crabs$e <- with(crabs, ifelse(Clutch>1,1,0))

############################################################################
# 5.0  Extract Females
############################################################################
fcrab <- crabs[crabs$Sex ==2,]
model <- glm(e ~ Size_mm, data = fcrab, family = "binomial")
library(MASS)
dose.p(model,p=c(0.5,0.9,0.95))


aggregate(Size_mm ~ Year+Agent+Clutch, FUN=max, data=fcrab)
aggregate(Size_mm ~ Year+Agent+Clutch, FUN=min, data=fcrab)

fcrab.m <- fcrab[fcrab$juv==0,]
ncrab <- aggregate(Sampling.Factor ~ Year+Agent+Clutch, FUN=sum, data=fcrab.m)
names(ncrab)[4] <-'nc'
scrab <- aggregate(nc ~ Year+Agent, FUN=sum, data=ncrab[ncrab$Clutch > 0 & ncrab$Clutch < 9,]) 
names(scrab)[3] <-'N'
f <- merge(ncrab[ncrab$Clutch==1,], scrab,by=c('Year','Agent'),all=TRUE)
fp <- merge(ncrab[ncrab$Clutch==1,], jcrab[jcrab$Clutch==1,],by=c('Year','Agent','Clutch'),all=TRUE)
f$p <- with(f,nc/N)
f[is.na(f)] <- 0
plot(p~Year, data=f,pch=19, col=2,ylim=c(0,1))
points(p~Year, data=f[f$Agent=='NOAA',],pch=19, col=4)
text(p~Year, data=f, label=f$N,pos=3)

