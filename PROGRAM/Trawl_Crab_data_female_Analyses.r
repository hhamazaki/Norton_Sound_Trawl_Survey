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
############################################################################
#  0.0  Initialize working Environment                                          
############################################################################
rm(list=ls(all=TRUE))
library(reshape2)
############################################################################
# Read read.R for program use
################################################################################
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
############################################################################
#  0.1  Define Folder Directories:                                                    
############################################################################
# Data folder location 
data_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/NS_Trawl_Survey/DATA/'
# Output table folder location 
out_data_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/NS_Trawl_Survey/PROGRAM/'
# Source R-code folder location 
source_dir <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/NS_Trawl_Survey/PROGRAM/R-code/'

#  0.3 Define crab data   
#===============================================================================
# Data file name:  Keep in the data directory
data_file1 <- 'ADFG/ADFG_Crab.csv'
data_file2 <- 'NMFS_76_91/NOAA_Crab.csv'
data_file3 <- 'NOAA_NBS/NOAA_RKC_NBS.csv'
# standard output
st <- c('Year', 'Agent','Haul','Sex', 'Size_mm','Shell','Clutch', 'full','Egg.condition', 'Egg.color','Sampling.Factor')
#########################################################################
# 1.0  Read ADFG Crab data  
############################################################################
# Data file name:  Keep in the data directory
adfg <- read.csv(paste0(data_dir,data_file1),na='',header=TRUE)
# Add Sampling factor  
adfg$Sampling.Factor <- 1
# Remove unnecessary data 
adfg$Agent <- 'ADFG'
# Assign clutch fullness % 
adfg$full <- with(adfg,ifelse(Clutch <= 2,0,
	ifelse(Clutch ==3, 15, 
    ifelse(Clutch ==4, 45,
    ifelse(Clutch ==5, 75, 95)))))
# Change Juvenile clutch size to  0 
adfg[which(adfg$JM=='J'),'Clutch'] <- 0
# limit data 
adfg <- adfg[,st]

############################################################################
# 2.0  Read NOAA Crab data  
############################################################################
# Data file name:  Keep in the data directory
noaa <- read.csv(paste0(data_dir,data_file2),na='',header=TRUE)
# Remove unnecessary data 
noaa$Agent <- 'NOAA'
# Change cluch 9 to NA
noaa[which(noaa$Clutch == 9),'Clutch'] <- NA
noaa$full <- with(noaa,ifelse(Clutch <= 1,0,
	ifelse(Clutch ==2, 6.25, 
    ifelse(Clutch ==3, 18.75,
    ifelse(Clutch ==4, 27.5,
    ifelse(Clutch ==5, 62.5,
	ifelse(Clutch ==6, 87.5,100)))))))
noaa <- noaa[,st]

############################################################################
# 3.0  Read NOAA 2010 Crab data  
############################################################################
# Data file name:  Keep in the data directory
noaa2010 <- read.csv(paste0(data_dir,data_file3),na='',header=TRUE)
# Remove unnecessary data 
noaa2010$Agent <- 'NOAA'
# Change cluch 9 to NA
noaa2010[which(noaa2010$Clutch == 9),'Clutch'] <- NA
noaa2010$full <- with(noaa2010,ifelse(Clutch <= 1,0,
	ifelse(Clutch ==2, 6.25, 
    ifelse(Clutch ==3, 18.75,
    ifelse(Clutch ==4, 27.5,
    ifelse(Clutch ==5, 62.5,
	ifelse(Clutch ==6, 87.5,100)))))))

noaa2010 <- noaa2010[,st]
############################################################################
# 4.0  Combine all Crab data and clean 
############################################################################
crabs <- rbind(noaa,adfg,noaa2010)
# Assign length class 
crabs$lenclass <- lenclass(crabs$Size_mm, min.s,inc.s)
# Change Sehll to NewShell (1) and OldShell (2)
crabs$Shell <- with(crabs,ifelse(Shell <=2,1,2))
# Get only females 
fcrab <- crabs[which(crabs$Sex ==2),]
# Extract Mature female
fmcrab <- fcrab[which(fcrab$Clutch>0),]
# find # 
# mean cutuch fullness and CI 
fullness <- aggregate(full ~ Year+Agent, FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)), data=fmcrab[which(fmcrab$Clutch>1),])
fullness <- do.call(data.frame,fullness)
fullness$l.CI <- with(fullness, full.mean-1.96*full.sd/sqrt(full.n))
fullness$u.CI <- with(fullness, full.mean+1.96*full.sd/sqrt(full.n))
fullness[order(fullness$Year,fullness$Agent),]


clutch.full <- aggregate(Sampling.Factor ~ Year+Agent+Clutch+lenclass, FUN=sum, data=fmcrab)
clutch.full.w <- dcast(clutch.full, Year+Agent+lenclass ~ Clutch)
clutch.full.w[is.na(clutch.full.w)] <- 0
clutch.full.w$P <- round(100*clutch.full.w[,4]/rowSums(clutch.full.w[,4:7]),1)
clutch.full.p <- dcast(clutch.full.w, Year+Agent ~ lenclass)

############################################################################
# 5.0  Calculate total number of crab by class  and combine with haul data
############################################################################
# Summarize by clutch
nsrkc.fe <- aggregate(Sampling.Factor ~ Year+Agent+Clutch, FUN=sum, data=fcrab)
# Change to long to wide
nsrkc.few <- dcast(nsrkc.fe, Year+Agent~Clutch,value.var="Sampling.Factor")
# Change NA to 0 
nsrkc.few[is.na(nsrkc.few)]<- 0
barren <- 100*nsrkc.few[,4]/rowSums(nsrkc.few[,c(4:10)])
nsrkc.fp <- cbind(nsrkc.few[,1:2],nsrkc.few[,3:10]/rowSums(nsrkc.few[,c(3:10)]))
nsrkc.fp$name <- with(nsrkc.fp,paste(Year,Agent))

par(mfrow=c(1,1), mar=c(3,3,3,3),oma=c(3,1,2,1),xpd = TRUE)
barplot(t(nsrkc.fp[,-c(1:2)]),names.arg = nsrkc.fp$name, las = 3, col=c(2:6),)
legend('topright',
inset=c(-0.12,0),
legend=c('Immature','Barren','< 30%', '<75','>75%'),
afill=c(2:6),bty='n',
title='Clutch Size')




#===============================================================================
# 6.0  Read BBRKC and STMTRKC 
#===============================================================================
data_dir2 <- 'C:/Projects/Norton_Sound/NSCrab/Trawl_data/Data_Analyses/CRABHAUL/'
fcradata_file4 <- 'CRABHAUL_BBRKC_DATA_TABLE.csv'
data_file5 <- 'CRABHAUL_STMATTBKC_DATA_TABLE.csv'
bbrkc <- read.csv(paste0(data_dir2,data_file4),na='',header=TRUE)
stmtrkc <- read.csv(paste0(data_dir2,data_file5),na='',header=TRUE)
bbrkc$Year <- with(bbrkc,floor(CRUISE/100))
stmtrkc$Year <- with(stmtrkc,floor(CRUISE/100))
# Extract only Females 
bbrkc.f <- bbrkc[bbrkc$SEX==2,]
stmtrkc.f <- stmtrkc[stmtrkc$SEX==2,]
bbrkc.fe <- aggregate(SAMPLING_FACTOR ~ Year+CLUTCH_SIZE, FUN=sum, data=bbrkc.f[bbrkc.f$SHELL_CONDITION <3,])
stmtrkc.fe <- aggregate(SAMPLING_FACTOR ~ Year+CLUTCH_SIZE, FUN=sum, data=stmtrkc.f[stmtrkc.f$SHELL_CONDITION <3,])
stmtrkc.fes <- dcast(stmtrkc.fe, Year~CLUTCH_SIZE,value.var="SAMPLING_FACTOR")
stmtrkc.fes[is.na(stmtrkc.fes)]<- 0
stmtrkc.fes$p <- stmtrkc.fes[,3]/rowSums(stmtrkc.fes[,-c(1:2)])
bbrkc.fes <- dcast(bbrkc.fe, Year~CLUTCH_SIZE,value.var="SAMPLING_FACTOR")
bbrkc.fes[is.na(bbrkc.fes)]<- 0
bbrkc.fes$p <- bbrkc.fes[,3]/rowSums(bbrkc.fes[,-c(1:2)])


plot(p~Year, data=bbrkc.fes,type='o',pch=19, col=2,ylim=c(0,1),xlim=c(1975,2020),
     ylab ='Proportion of empty cluch mautre female',bty='l',yaxs='i')			 
lines(p~Year, data=nsrkc.fes,type='o',pch=19, col=4)
legend('topright',c('BBRKC','NSRKC'),pch=19,col=c(2,4),lty=1,bty='n')


