##########################################################################
#   Norton Sound Trawl Survey Female data analysea
#   This program summarizes crab samples obtained from trawl survey
#   The data will be used for 
#   By Toshihide "Hamachan" Hamazaki
############################################################################
# Definition Female Maturity:  ADFG
# < 2006: Immmature  < 72mm CL no egg,  Marure: > 71 mm CL 
# >= 2006: Immature  abdominal flap undevelopped,  Mature abdominal flap developped 
# Clutch Size:  ADFG
# 1: Barren clean pleopod,2: Barren mattede pleopods, 
# 3: 1-29% full, 4: 30-59% full, 5: 60-89% full, 6: 90-100% full
# Clutch Size:  NOAA
# 0: Immmature, 1: Barren clean pleopod,  
# 2: 1-12.5% full, 3: 12.5-25% full, 4: 25-50% full, 5: 50-75% full
# 6: 75-100% full, 7: > 100%, 9: Missing data 
# Clutch Condition: ADFG
# 1: All alive,  2: Dead egg < 20%, 3: Dead egg > 20%
# Clutch Condition: NOAA
# 
# Egg Condition: ADFG
# 1: Uneyed, 2: Eyed
# Egg Condition: NOAA
# 0: No eggs, 1: Uneyed, 2:Eyed,  3: Dead, 4: Empty
################################################################################
#  0.0  Initialize working Environment                                          
################################################################################
rm(list=ls(all=TRUE))
library(reshape2)
# lenclass function determine length class based on minimum length and increment.
lenclass <- function(len,minlen,inc)
{
lenclass1 <- floor((len - minlen)/inc)
lenclass <- minlen+lenclass1*inc
return(lenclass)
}
min.s <- 64
max.s <- 134
inc.s <- 10

#-------------------------------------------------------------------------------
#  0.1  Define Folder Directories:                                                    
#-------------------------------------------------------------------------------
# Main directory
main_dir <- file.path('C:','Projects','Norton_Sound','NSCrab','Trawl_data','NS_Trawl_Survey')
# Data folder location 
data_dir <- file.path(main_dir,'DATA')
# Output table folder location 
out_data_dir <- file.path(main_dir,'PROGRAM')
# Source R-code folder location 
source_dir <- file.path(main_dir,'PROGRAM','R-code')

#-------------------------------------------------------------------------------
#  0.2 Define crab data   
#-------------------------------------------------------------------------------
# standard output
st <- c('Year', 'Agent','Haul','Sex', 'Size_mm','Shell','Clutch', 'full','Egg.condition', 'Egg.color','Sampling.Factor')

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
# Assign clutch fullness % 
ADFG.crab$full <- with(ADFG.crab,ifelse(Clutch <= 2,0,
	ifelse(Clutch ==3, 15, 
    ifelse(Clutch ==4, 45,
    ifelse(Clutch ==5, 75, 95)))))
# Change Juvenile clutch size to  0 
ADFG.crab[which(ADFG.crab$JM=='J'),'Clutch'] <- 0
ADFG.crab$Agent <- 'ADFG'
# limit data 
ADFG.crab <- ADFG.crab[,st]


#===============================================================================
# 2.0  Read NOAA Crab data  
#===============================================================================
# Data file name:  Keep in the data directory
noaa <- read.csv(file.path(data_dir,'NMFS_76_91','NMFS_1976_1991_Crab.csv'),skip=6,header=TRUE)
# 1: sublegal or female, 2: Legal
noaa[which(noaa$Clutch == 9),'Clutch'] <- NA
noaa$full <- with(noaa,ifelse(Clutch <= 1,0,
	ifelse(Clutch ==2, 6.25, 
    ifelse(Clutch ==3, 18.75,
    ifelse(Clutch ==4, 27.5,
    ifelse(Clutch ==5, 62.5,
	ifelse(Clutch ==6, 87.5,100)))))))
noaa$Agent <- 'NOAA'
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

NBS.crab$Agent <- 'NOAA'
# Change cluch 9 to NA
NBS.crab[which(NBS.crab$Clutch == 9),'Clutch'] <- NA
NBS.crab$full <- with(NBS.crab,ifelse(Clutch <= 1,0,
	ifelse(Clutch ==2, 6.25, 
    ifelse(Clutch ==3, 18.75,
    ifelse(Clutch ==4, 27.5,
    ifelse(Clutch ==5, 62.5,
	ifelse(Clutch ==6, 87.5,100)))))))
NBS.crab <- NBS.crab[,st]

#===============================================================================
# 4.0  Create Data for Abundance Analyses   
#===============================================================================
# Combine all data 
crabs <- rbind(noaa,ADFG.crab,NBS.crab)

# Assign length class 
crabs$lenclass <- lenclass(crabs$Size_mm, min.s,inc.s)
# Change Sehll to NewShell (1) and OldShell (2)
crabs$Shell <- with(crabs,ifelse(Shell <=2,1,2))
# Get only females 
fcrab <- crabs[which(crabs$Sex ==2),]
aggregate(Sampling.Factor ~ Agent+Year,length, data =fcrab)
# Extract Mature female
fmcrab <- fcrab[which(fcrab$Clutch>0),]

#-------------------------------------------------------------------------------
# 4.1  Caclculate Mean clutch fullness and CI (of females that have eggs)
#-------------------------------------------------------------------------------
fullness <- aggregate(full ~ Year+Agent, FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)), data=fmcrab[which(fmcrab$Clutch>1),])
fullness <- do.call(data.frame,fullness)
fullness$l.CI <- with(fullness, full.mean-1.96*full.sd/sqrt(full.n))
fullness$u.CI <- with(fullness, full.mean+1.96*full.sd/sqrt(full.n))
fullness[order(fullness$Year,fullness$Agent),]


#-------------------------------------------------------------------------------
# 5.0  Calculate % of barren mature female
#-------------------------------------------------------------------------------
# Summarize by clutch
nsrkc.fe <- aggregate(Sampling.Factor ~ Year+Agent+Clutch, FUN=sum, data=fmcrab)
# Change to long to wide
nsrkc.few <- dcast(nsrkc.fe, Year+Agent~Clutch,value.var="Sampling.Factor")
# Change NA to 0 
nsrkc.few[is.na(nsrkc.few)]<- 0
nsrkc.few$barren <- 100*nsrkc.few[,3]/rowSums(nsrkc.few[,c(3:9)])
nsrkc.few




