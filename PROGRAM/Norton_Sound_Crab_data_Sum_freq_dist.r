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
library(doBy)
library(reshape2)
############################################################################
################################################################################
# length classification function                                                    
################################################################################
# lenclass function determine length class based on minimum length and increment.
# class 1 is the minimum class 
lenclass <- function(len,minlen,inc)
{
lenclass1 <- floor((len - minlen)/inc)
lenclass <- minlen+lenclass1*inc
return(lenclass)
}

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
adfg <- read.csv(paste0(data_dir,data_file1),na='.',header=TRUE)
# Add Sampling factor  
adfg$Sampling.Factor <- 1
# Remove unnecessary data 
adfg$Agent <- 'ADFG'
adfg <- adfg[,c('Year','Sex','Shell','Size_mm','Sampling.Factor')]
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
# Remove unnecessary data 
noaa2010 <- noaa2010[,c('Year','Sex','Shell','Size_mm','Sampling.Factor')]
noaa2010$Agent <- 'NOAA'

############################################################################
# 4.0  Combine all Crab data and reclassify 
############################################################################
crabs <- rbind(noaa,adfg,noaa2010)
# Change Sehll to NewShell (1) and OldShell (2)
crabs$Shell <- with(crabs,ifelse(Shell <=2,1,2))
crabs$Shell <- with(crabs,ifelse(Sex==2,3,Shell))
# Add length class 
crabs$lenclass <- lenclass(crabs$Size_mm,0,5)
# Remove size ==0, 
crabs <- crabs[crabs$lenclass>0,]
############################################################################
# 5.0  Calculate total number of crab by class  and combine with haul data
############################################################################
ncrab <- aggregate(Sampling.Factor ~ Year+Agent+Sex+Shell+lenclass, FUN=sum, data=crabs)

############################################################################
# 6.0  Add lengCalculate total number of crab by class  and combine with haul data
############################################################################
# Transpose data
ncrab.w <- dcast(ncrab,Year+Agent+Sex+Shell ~ lenclass, values.var='Sampling.Factor')
# Replace NA to 0
ncrab.w[is.na(ncrab.w)] <- 0
cname <- aggregate(Sampling.Factor ~ Year+Agent, FUN=sum, data=crabs)
cname <- with(cname,cname[order(Year,Agent),1:2])

windows(record=TRUE)

par(mfrow=c(4,5), mar=c(2,2,2,1),oma = c(3,3,3,3),cex=0.8,mgp = c(3,.3,0))

for (i in 1:19){
foo <-with(ncrab.w,ncrab.w[Year==cname$Year[i]&Agent==cname$Agent[i]&Sex==1,-c(1:4)])
barplot(as.matrix(foo),col =rainbow(2),main=paste(cname[i,1],cname[i,2]), border=NA)
}
plot.new()
legend('topleft',fill=rainbow(2),legend=c('NewShell','Oldshell','Female'))





