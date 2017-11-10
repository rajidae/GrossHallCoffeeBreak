# set working directory
# update with D's code to pull from box server directly, not local path
### Since we decided not to pull from the URL, do we want to just update the path with our
### respective username every time we work in this document?
setwd("C:/Users/delan/Box Sync/StatsProject")

# import manually created csv file
mpdata <- na.omit(read.csv("DEQ_Stats_Data.csv", header = TRUE, stringsAsFactors = FALSE))

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
  
mpdata$cr6.ppb[mpdata$cr6.ppb=="bdl"] <- bdl.crit

par(mfrow=c(1,2))
plot(mpdata$well.depth.ft,mpdata$cr6.ppb, xlab="Well depth (ft)", 
     ylab= "Hex Cr (ppb)", col="darkblue")
plot(mpdata$orp.mv, mpdata$cr6.ppb, xlab="Orp (mV)", 
     ylab="LogHex Cr (ppb)", col="darkblue")

par(mfrow=c(1,2))
plot(mpdata$well.depth.ft, log(as.numeric(mpdata$cr6.ppb)), xlab="Well depth (ft)", 
     ylab= "Log Hex Cr (ppb)", col="darkblue")
plot(mpdata$orp.mv, log(as.numeric(mpdata$cr6.ppb)), xlab="Orp (mV)", 
     ylab="Log Hex Cr (ppb)", col="darkblue")

plot(mpdata$well.depth.ft, mpdata$orp.mv, xlab="Well depth (ft)", 
     ylab = "Orp (mV)", col="darkblue")
hist(as.numeric(mpdata$cr6.ppb), col="darkblue")
hist(log(as.numeric(mpdata$cr6.ppb)),col="darkblue")

##We still have sooo many values (I think they're the BDL values) that are skewing the
##data even after log transformed. I'm not sure how to handle those values.

   
  
  


#####################################  SCRATCH  #########################################
# vectors of variables
  # orp <- mpdata$orp.mv
  #depth <- mpdata$well.depth.ft
  #cr6 <- mpdata$cr6.ppb
    # bdl string values mean this variable is a factor when it should be numeric
  
# lm(cr6 ~ orp)