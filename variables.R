##### WORKSPACE SETUP #####

# set working directory
## Update with respective path - comment out the other's path
setwd("C:/Users/allth/Box Sync/StatsProject") # Kristen's Path
# setwd("") # Delaney's path

# import manually created csv file
mpdata <- na.omit(read.csv("DEQ_Stats_Data.csv", header = TRUE, stringsAsFactors = FALSE))
  ## does not seem to be omitting na rows?

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
  mpdata$cr6.ppb[mpdata$cr6.ppb=="bdl"] <- bdl.crit
  
# typeof(cr6.ppb) = "character" -> need to correct for this
storage.mode(mpdata$cr6.ppb) <- "double"

  
  
##### Exploration of Full Dataset ####  
  
# plot of raw variables
par(mfrow=c(1,2))
plot(mpdata$well.depth.ft, mpdata$cr6.ppb, xlab="Well depth (ft)", 
     ylab= "Hex Cr (ppb)", col="darkblue")
plot(mpdata$orp.mv, mpdata$cr6.ppb, xlab="Orp (mV)", 
     ylab="Hex Cr (ppb)", col="darkblue")

# plot of log transformed dependent variable Cr6
par(mfrow=c(1,2))
plot(mpdata$well.depth.ft, log(mpdata$cr6.ppb), xlab="Well depth (ft)", 
     ylab= "Log Hex Cr (ppb)", col="darkblue")
plot(mpdata$orp.mv, log(mpdata$cr6.ppb), xlab="Orp (mV)", 
     ylab="Log Hex Cr (ppb)", col="darkblue")

# plot of independent variables
par(mfrow=c(1,1))
plot(mpdata$well.depth.ft, mpdata$orp.mv, xlab="Well depth (ft)", 
     ylab = "Orp (mV)", col="darkblue")

# histogram of raw and log-transformed Cr6
par(mfrow=c(1,2))
hist(mpdata$cr6.ppb, col="darkblue", 
     main = "Hex Cr Histogram", xlab = "Hex Cr (ppb)")
hist(log(mpdata$cr6.ppb),col="darkblue", 
     main = "logHex Cr Histogram", xlab = "logHex Cr (ppb)")

# Pair-wise
source("pair.fun.R")
pairs(mpdata$cr6.ppb ~ mpdata$well.depth.ft + mpdata$orp.mv,
      lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist)

## We still have sooo many values (I think they're the BDL values) that are skewing the
## data even after log transformed. I'm not sure how to handle those values.



##### Exploration of Short Dataset #####

# Removing data points below LOD
mpdata.short <- mpdata[mpdata$cr6.ppb >= LOD,]

# Pair-wise
source("pair.fun.R")
pairs(mpdata.short$cr6.ppb ~ mpdata.short$well.depth.ft + mpdata.short$orp.mv,
      lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist)

# plot of independent variables Well Depth and ORP
par(mfrow=c(1,1))
plot(mpdata.short$well.depth.ft, mpdata.short$orp.mv, xlab="Well depth (ft)", 
     ylab = "Orp (mV)", col="darkgreen")

  # correlation test on Well Depth and ORP
  cor1 <- cor.test(mpdata.short$well.depth.ft, mpdata.short$orp.mv)
    cor1
    ## non-sig but close?
    
  cor1.full <- cor.test(mpdata$well.depth.ft, mpdata$orp.mv)
    cor1.full
    ## signficant correlation?

  # correlation test on pH and ORP for funsies
  par(mfrow=c(1,1))
  plot(mpdata.short$pH, mpdata.short$orp.mv, xlab="pH", 
         ylab = "Orp (mV)", col="darkgreen")
  cor2 <- cor.test(mpdata.short$pH, mpdata.short$orp.mv)    
    cor2
    ## significant correlation?
    
# plot of raw variables from short (top) compared to full (bottom)
par(mfrow=c(2,2))
plot(mpdata.short$well.depth.ft, mpdata.short$cr6.ppb, xlab="Well depth (ft)", 
     ylab= "Hex Cr (ppb)", col="darkgreen")
plot(mpdata.short$orp.mv, mpdata.short$cr6.ppb, xlab="Orp (mV)", 
     ylab="Hex Cr (ppb)", col="darkgreen")
plot(mpdata$well.depth.ft, mpdata$cr6.ppb, xlab="Well depth (ft)", 
     ylab= "Hex Cr (ppb)", col="darkblue")
plot(mpdata$orp.mv, mpdata$cr6.ppb, xlab="Orp (mV)", 
     ylab="Hex Cr (ppb)", col="darkblue")


# plot of log transformed Cr6 from short (top) compared to full (bottom)
par(mfrow=c(2,2))
plot(mpdata.short$well.depth.ft, log(mpdata.short$cr6.ppb), xlab="Well depth (ft)", 
     ylab= "logHex Cr (ppb)", col="darkgreen")
plot(mpdata.short$orp.mv, log(mpdata.short$cr6.ppb), xlab="Orp (mV)", 
     ylab="logHex Cr (ppb)", col="darkgreen")
plot(mpdata$well.depth.ft, log(mpdata$cr6.ppb), xlab="Well depth (ft)", 
     ylab= "logHex Cr (ppb)", col="darkblue")
plot(mpdata$orp.mv, log(mpdata$cr6.ppb), xlab="Orp (mV)", 
     ylab="logHex Cr (ppb)", col="darkblue")

# histogram of raw and log-transformed Cr6 from short (top) and full (bottom)
par(mfrow=c(2,2))
hist(mpdata.short$cr6.ppb, col="darkgreen", 
     main = "Hex Cr Histogram", xlab = "Hex Cr (ppb)")
hist(log(mpdata.short$cr6.ppb),col="darkgreen", 
     main = "logHex Cr Histogram", xlab = "logHex Cr (ppb)")
hist(mpdata$cr6.ppb, col="darkblue", 
     main = "Hex Cr Histogram", xlab = "Hex Cr (ppb)")
hist(log(mpdata$cr6.ppb),col="darkblue", 
     main = "logHex Cr Histogram", xlab = "logHex Cr (ppb)")

# histogram of ORP and Well Depth from short (top) and full (bottom)
par(mfrow=c(2,2))
hist(mpdata.short$well.depth.ft, col="darkgreen", 
     main = "Well Depth Histogram", xlab = "Well Depth (ft)")
hist(mpdata.short$orp.mv,col="darkgreen", 
     main = "ORP Histogram", xlab = "ORP (mV)")
hist(mpdata$well.depth.ft, col="darkblue", 
     main = "Well Depth Histogram", xlab = "Well Depth (ft)")
hist(mpdata$orp.mv,col="darkblue", 
     main = "ORP Histogram", xlab = "ORP (mV)")

## so Cr6 data looks better when bdl's are removed and then its log-transformed :)
## BUT ORP gets worse (Well depth also?) when bdl's are removed - try a transformation?

