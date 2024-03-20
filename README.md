# Norton_Sound_Trawl_Survey DATA and Analyses
This project holds Trawl survey data conducted in Norton Sound 
The project consists of 2 Major folders 
* DATA 
  * ADFG
    * Crab: RKC size shell sex data by year
    * Haul 
      * Haul_data: Haul data by year
      * Subsample_data: Subsample data to calculate raise factor by year 
    * Species 
      * Catch: Susbsample and whole seight and number of catch by species 
      * Length:  Length and weight of large fish species 
  * NMFS_76_91:  Historical NMFS trawl survey data 1976 - 1991
      * NMFS_1976_1991_Haul_data.csv
      * NMFS_1976_1991_Crab.csv
      * NMFS_1976_1991_Catch.csv
  * NOAA_NBS: NOAA NBS trawl survey: data downloaded from FOSS website
      * Crab: RKC size shell sex data by year
      * Haul: NBS Haul data 
      * Species:  NBS expanded weight and number of catch by year
      * NOAA_NBS_Station.csv:  NBS station lat long 
* PROGRAM  R code for estimating RKC abundance, Species CPUE Index 
    * Norton_Sound_Trawl_CPUE_Index.r  : Create annual trawl survey species index
    * Norton_Sound_Crab_Abundance.r: Create annual trawl survey crab abundance 
    * R-code:  Contains Source file that read and haul data 
    
      
     

