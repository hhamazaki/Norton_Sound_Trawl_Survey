################################################################################
#   Norton Sound Trawl Survey Harvest Estimation Program in R
#   Version 1.1   02/07/2017
#   Version 2.0   03/31/2024
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
#      This will create haul data 
#-------------------------------------------------------------------------------
source(file.path(base_dir,'PROGRAM','R-code','Noron_Sound_Read_tow_data.r'))


data_file1 <- 'NMFS_1976_1991_Catch.csv'
data_file3 <- 'NB_SP_DATA.csv'
data_file4 <- 'Spcode.csv'
data_file5 <- 'Spcode_Error.csv'

#-------------------------------------------------------------------------------
spn  <- read.csv(file.path(data_dir,data_file4),na='',skip=6,header=TRUE)
spn_error <- read.csv(file.path(data_dir,data_file5),na='',skip=6,header=TRUE)

# Include NOAA survey 
noaasp <- read.csv(file.path(data_dir,'NMFS_76_91',data_file1),na='',skip=6,header=TRUE)
noaasp$Agent <- 'NOAA'
noaasp <- noaasp[which(noaasp$Weight_kg>0),]

################################################################################
#  2.0  Read ADF&G Catch data                                          
################################################################################
# Data file name:  Keep in the Catch  directory
catch.file.list <- list.files(file.path(data_dir,'ADFG','Species','Catch'))
n <- length(catch.file.list)
ADFG.catch <- data.frame()
# Combine them into a single Catch data 
for(i in 1:n){
 temp <- read.csv(file.path(data_dir,'ADFG','Species','Catch',catch.file.list[i]),skip=6, header=TRUE) 
 temp <- temp[,c('Year', 'Haul', 'Spcode', 'Sample_kg', 'Sample_n', 'Whole_kg', 'Whole_n')] 
 ADFG.catch <- rbind(ADFG.catch,temp)
}
# Add Agent name 
ADFG.catch$Agent <- 'ADFG'
# change  na catch is 
ADFG.catch[is.na(ADFG.catch)] <-0
# merge with haul data (this will add rasing factor) 
ADFG.catch <- merge(haul,ADFG.catch,by=c('Agent','Haul','Year'))
ADFG.catch$rf[is.na(ADFG.catch$rf)]<-1
# Calculate catch weight. Weight in data are eigher sample or whole 
ADFG.catch$Weight_kg <- with(ADFG.catch,rf*Sample_kg+Whole_kg)
ADFG.catch$Total_n <- with(ADFG.catch,rf*Sample_n+Whole_n)
# Limit data only catch weight is > 0 
ADFG.catch<- ADFG.catch[which(ADFG.catch$Weight_kg>0),]

#===============================================================================
# 3.0  Read NOAA NBS Catch data  
#===============================================================================
catch.file.list <- list.files(file.path(data_dir,'NOAA_NBS','Species'))
n <- length(catch.file.list)
noaaNBS <- data.frame()
# Combine them into a single Catch data 
for(i in 1:n){
 temp <- read.csv(file.path(data_dir,'NOAA_NBS','Species',catch.file.list[i]),skip=6, header=TRUE) 
 noaaNBS <- rbind(noaaNBS,temp)
}

# Renumber Hauls (because they use more than one vessels and each vessel haul starts from 1)
noaaNBS$Haul <- with(noaaNBS, Haul + 100*Vessel_Id)
# Add Agent name
noaaNBS$Agent <- 'NBS'
noaaNBS <- noaaNBS[which(noaaNBS$Weight_kg>0),]

#-------------------------------------------------------------------------------
# 4.0  Combine and Clean data 
#-------------------------------------------------------------------------------
# Extract only data of standard data 
stclm <- c('Year','Haul','Spcode','Total_n','Weight_kg','Agent')
spp <- rbind(noaasp,ADFG.catch[,stclm],noaaNBS[,stclm])
# Error check 

# Merge Spcode error data 
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
# Keep only animals Drop debries and eggs
spp <- spp[spp$Spcode < 99990 & spp$Spcode > 2,] 

#-------------------------------------------------------------------------------
# 4.1 Combine minor species to taxon level   
#-------------------------------------------------------------------------------
# sum by revised spcode
spp.s <- aggregate(Weight_kg~ Year+Agent+Haul+Spcode,FUN=sum,data=spp)

#-------------------------------------------------------------------------------
# Combine tow data     
#-------------------------------------------------------------------------------
nssp <- merge(haul,spp.s,by=c('Year','Agent','Haul'))
nssp$CPUE <- with(nssp,Weight_kg/Swept_km2)
# nssp is the base species conde
# Remove bad haul data 
nssp <- nssp[which(is.na(nssp$Haul_rate)),]
# Remove non-ADFG_stations 
nssp <- nssp[which(!is.na(nssp$ADFG_Station)),]
# Standardize Area (Limt data to NS ADFG survey Area for consistency.)
nssp <- nssp[which(nssp$ADFG_tier %in% c('c','t1','t2','t3')),]

#-------------------------------------------------------------------------------
# Calculate CPUE Index:  geometric mean CPUE*(proportion of stations the species is present)
#-------------------------------------------------------------------------------
 # Calculate mean log CPUE for each species 
nssp.i <- aggregate(log(CPUE) ~ Year+Agent+Spcode,FUN=mean,data=nssp)
# Find the number of stations with each species present 
nssp.n <- aggregate(CPUE ~ Year+Agent+Spcode,FUN=length,data=nssp)
nssp.s <- merge(nssp.i,nssp.n,by=c('Year','Agent','Spcode'))
# Find the number of stations with Trawled  
nssp.st <-aggregate(ADFG_Station~Agent+Year+Haul,FUN=min,data=nssp)
nssp.st <-aggregate(Haul~Agent+Year,FUN=length,data=nssp.st)

# Combine the data and rename 
nssp.s <- merge(nssp.s,nssp.st,by=c('Year','Agent'))
names(nssp.s)[4:6] <- c('ml.cpue','ns','nt')
# I.CPUE is geometric mean CPUE Index. 
nssp.s$I.CPUE <- with(nssp.s,exp(ml.cpue)*ns/nt)
#Add spcode 
nssp.s <- merge(nssp.s,spn,by=c('Spcode'))
#write.csv(nssp.syw, file.path(data_dir,'CPUE_Index2.csv'),row.names = FALSE,na = '',)

nssp.sy <- aggregate(I.CPUE ~ Spcode+Cme+Year,FUN=mean,data=nssp.s)
nssp.syw <- dcast(nssp.sy, Spcode+Cme ~ Year)
nssp.syw$n <- apply(nssp.syw[,-c(1:2)],1,function(x) length(x)-sum(is.na(x)))
nssp.syw <- nssp.syw[order(nssp.syw$n,decreasing = FALSE),]
foo <- as.matrix(nssp.syw[,-c(1:2,dim(nssp.syw)[2])])
foo[is.na(foo)] <- 0
rownames(foo) <- nssp.syw$Cme

heatmap(foo[nssp.syw$n>10,],Colv=NA,Rowv=NA,col = cm.colors(256),scale='row')



#-------------------------------------------------------------------------------
# Calculate CPUE Index: for larger categories geometric mean CPUE*(proportion of stations the species is present)
#-------------------------------------------------------------------------------
# Reaad socode:

# Combine nssp with spcode
nssp.sp <- merge(nssp, spn, by=c('Spcode'))
nssp.taxon <- aggregate(CPUE ~ Year+Agent+ADFG_Station+Taxon2,FUN=sum,data=nssp.sp)
nssp.taxon.i <- aggregate(log(CPUE) ~ Year+Agent+Taxon2,FUN=mean,data=nssp.taxon[nssp.taxon$CPUE>0,])
# Find the number of stations with each species present 
nssp.taxon.n <- aggregate(CPUE ~ Year+Agent+Taxon2,FUN=length,data=nssp.taxon[nssp.taxon$CPUE>0,])
nssp.taxon.s <- merge(nssp.taxon.i,nssp.taxon.n,by=c('Year','Agent','Taxon2'))

# Combined the data and rename 
nssp.taxon.s <- merge(nssp.taxon.s,nssp.st,by=c('Year','Agent'))
names(nssp.taxon.s)[4:6] <- c('ml.cpue','ns','nt')
# I.CPUE is geometric mean CPUE Index. 
nssp.taxon.s$I.CPUE <- with(nssp.taxon.s,exp(ml.cpue)*ns/nt)
nssp.taxon.w <- dcast(nssp.taxon.s,Taxon2~Year+Agent,value.var = 'I.CPUE')
nssp.taxon.w[is.na(nssp.taxon.w)] <- 0
barplot(as.matrix(nssp.taxon.w[,-1]),legend.text = nssp.taxon.w[,1],args.legend=list(x = 'topleft'),col=rainbow(16))



#-------------------------------------------------------------------------------
# CPUE comparision: ADFG vs. NBS
#-------------------------------------------------------------------------------
# Extract years when both surveys were conducted 
ADFG_NBS_haul <- haul[haul$Year %in% c(2017,2019,2021,2023),]
# Remove stations with no ADFG_station number 
ADFG_NBS_haul <- ADFG_NBS_haul[!is.na(ADFG_NBS_haul$ADFG_Station),]
# Change long to wide:  This will put hauled station side by side 
ADFG_NBS_haul$Date <- with(ADFG_NBS_haul,paste0(Month,'/',Day))
ADFG_NBS_haul <- dcast(ADFG_NBS_haul,Year+ADFG_Station~Agent,value.var='Date')
ADFG_NBS_haul$Dif <-  with(ADFG_NBS_haul,as.Date(NBS,"%m/%d")-as.Date(ADFG,"%m/%d"))
# Remove stations that both surveys did not occur 
ADFG_NBS_haul <-  ADFG_NBS_haul[complete.cases(ADFG_NBS_haul),]
# Limit data to survey occured within 7 days
ADFG_NBS_haul <-  ADFG_NBS_haul[ADFG_NBS_haul$Dif < 8,]

# Go back to long form. 
#ADFG_NBS_haul <-  melt(ADFG_NBS_haul,id.vars=c('Year','ADFG_Station'),variable.name = 'Agent')
# Merge with CPUE data 
ADFG_NBS <- merge(nssp,ADFG_NBS_haul[,c('Year','ADFG_Station')], by=c('Year','ADFG_Station'))
# Back to side by side by Stations and Species code 
ADFG_NBS.w  <- dcast(ADFG_NBS,Year+ADFG_Station+Spcode~Agent,value.var='CPUE')
# NA indicate the species were not caught by one survey.  Change to zero
ADFG_NBS.w[is.na(ADFG_NBS.w)] <- 0
# Remove if both satations did not catch the species 
ADFG_NBS.w <- with(ADFG_NBS.w,ADFG_NBS.w[which((ADFG+NBS) !=0,),])
# Extract unique Spcode 
Sp <- unique(ADFG_NBS.w$Spcode)
test <- data.frame()
for(i in 1:length(Sp)){
temp <- ADFG_NBS.w[which(ADFG_NBS.w$Spcode ==Sp[i]),]
#print(Sp[i])
#print(with(temp,wilcox.test(ADFG, NBS, paired = TRUE, alternative = "two.sided", conf.int = TRUE)))
tests <- with(temp,wilcox.test(ADFG, NBS, paired = TRUE, alternative = 'two.sided', conf.int = TRUE))
temps <- data.frame(Spcode = Sp[i], p.value = tests$p.value)
test <- rbind(test,temps)
}
ADFG_NBS.comp <- aggregate(cbind(ADFG,NBS)~Spcode,mean, data=ADFG_NBS.w)
ADFG_NBS.comp <- merge(ADFG_NBS.comp,test,by='Spcode')

ADFG_NBS.comp.s <- merge(spn[,c('Spcode','me','Taxon.Confidence')],ADFG_NBS.comp,by = 'Spcode')



ADFG_NBS.comp <- merge(ADFG_NBS.comp, test, by = 'Spcode')
ADFG_NBS.comp.s<-ADFG_NBS.comp[ADFG_NBS.comp$p.value<0.05,]
ADFG_NBS.comp.s <- merge(ADFG_NBS.comp.s, spn[,c('Spcode','Cme')], by = 'Spcode')
write.csv(ADFG_NBS.comp.s, file.path(data_dir,'CPUE_Index.csv'),row.names = FALSE,na = '',)

