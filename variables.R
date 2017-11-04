# set working directory
# update with D's code to pull from box server directly, not local path
setwd("C:/Users/allth/Box Sync/StatsProject")

# import manually created csv file
mpdata <- read.csv("DEQ_Stats_Data.csv", header = TRUE)

# rename relevant headers to better format
colnames(mpdata)[colnames(mpdata)=="Total.Well.depth..ft."] <- "well.depth.ft"
colnames(mpdata)[colnames(mpdata)=="ORP..mv."] <- "orp.mv"
colnames(mpdata)[colnames(mpdata)=="Cr6..ppb."] <- "cr6.ppb"
  
  # alternative way to change name based on column number
  # names(mpdata)[8] <- "well.depth.ft"

# list of all column names
colnames(mpdata)

# need to deal with bdl's in cr6.ppb (factor -> numeric variable)
  # use criteria to create replacement value for bdl
  LOD <- 0.004
  LOQ <- 0.012
  bdl.crit <- LOD/sqrt(2)
  
  # replace "bdl" in 'cr6.ppb' with "bdl.crit"
  # attach to dataframe as new column
   
  
  


#####################################  SCRATCH  #########################################
# vectors of variables
  # orp <- mpdata$orp.mv
  #depth <- mpdata$well.depth.ft
  #cr6 <- mpdata$cr6.ppb
    # bdl string values mean this variable is a factor when it should be numeric
  
# lm(cr6 ~ orp)