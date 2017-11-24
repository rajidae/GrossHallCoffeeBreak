##### WORKSPACE SETUP #####

## This script requires the use of the "pwr" cran

# set working directory
## Update with respective path - comment out the other's path
#setwd("C:/Users/allth/Box Sync/StatsProject") # Kristen's Path
setwd("C:/Users/delan/Box Sync/StatsProject") # Delaney's path

# import manually created csv file
mpdata <- na.omit(read.csv("DEQ_Stats_Data.csv", header = TRUE, stringsAsFactors = FALSE))
  ## does not seem to be omitting na rows?

# rename relevant headers to better format
colnames(mpdata)[colnames(mpdata)=="Total.Well.depth..ft."] <- "well.depth.ft"
colnames(mpdata)[colnames(mpdata)=="ORP..mv."] <- "orp.mv"
colnames(mpdata)[colnames(mpdata)=="Cr6..ppb."] <- "cr6.ppb"
attach(mpdata)
  
  # alternative way to change name based on column number
  # names(mpdata)[8] <- "well.depth.ft"

# list of all column names
colnames(mpdata)

# need to deal with bdl's in cr6.ppb (factor -> numeric variable)
  # use criteria to create replacement value for bdl
  LOD <- 0.004
  LOQ <- 0.012
  bdl.crit <- runif(length(mpdata$cr6.ppb[mpdata$cr6.ppb=="n.a."]),0,LOD)
  
  # replace "bdl" in 'cr6.ppb' with "bdl.crit"
  # attach to dataframe as new column
  
  mpdata$cr6.ppb[mpdata$cr6.ppb=="n.a."] <- bdl.crit
  
  mpdata <- mpdata[!(mpdata$cr6.ppb==""), ]
  
  na.omit(mpdata$cr6.ppb)
  
  # replace "bdl" in 'cr6.ppb' with "bdl.crit"
  mpdata$cr6.ppb[mpdata$cr6.ppb=="bdl"] <- bdl.crit
  
# typeof(cr6.ppb) = "character" -> need to correct for this
storage.mode(mpdata$cr6.ppb) <- "double"

##### Exploration of Full Dataset #####  
  
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

covfefe.full <- lm(cr6.ppb ~ orp.mv*well.depth.ft, data=mpdata)
covfefe.1 <- update(covfefe.full,~.-orp.mv:well.depth.ft)
covfefe.2 <- update(covfefe.1,~.-orp.mv)
covfefe.3 <- update(covfefe.1,~.-well.depth.ft)

summary(covfefe.full)
summary(covfefe.1)
summary(covfefe.2)
summary(covfefe.3)

AIC(covfefe.full, covfefe.1, covfefe.2, covfefe.3)

## Lowest AIC is covfefe.1

par(mfrow=c(2,2))
plot(covfefe.1)

#####Analysis with out points 21, 44 or 55#####
## For some reason I couldn't combine these three steps into the same c() but it worked 
## separately so I'm not questioning it.

data.include <- mpdata[c(1:20), ]
data.include.2 <- mpdata[c(22:43), ]
data.include.3 <- mpdata[c(45:54), ]
fakenews <- na.omit(rbind.data.frame(data.include, data.include.2, data.include.3))

ogtrumpqueen.full <- lm(cr6.ppb ~ orp.mv*well.depth.ft, data=fakenews)
ogtrumpqueen.1 <- update(ogtrumpqueen.full,~.-orp.mv:well.depth.ft)
ogtrumpqueen.2 <- update(ogtrumpqueen.1,~.-orp.mv)
ogtrumpqueen.3 <- update(ogtrumpqueen.1,~.-well.depth.ft)

summary(ogtrumpqueen.full)
summary(ogtrumpqueen.1)
summary(ogtrumpqueen.2)
summary(ogtrumpqueen.3)

AIC(ogtrumpqueen.full, ogtrumpqueen.1, ogtrumpqueen.2, ogtrumpqueen.3)
##Lowest AIC is the full model, second lowest is model 1, which lacks the interaction factor

par(mfrow=c(2,2))
plot(ogtrumpqueen.full)

par(mfrow=c(2,2))
plot(ogtrumpqueen.1)

#####Power analysis#####

##effect size of 0.15 is "medium" according to Cohen

require(pwr)
#Power with all points
pwr.f2.test(u=53, v=2, f2= 0.15, sig.level= 0.05, power= NULL)

#####Without BDL Exploration#####

trumpyourcat.full <- lm(mpdata.short$cr6.ppb ~ mpdata.short$orp.mv*
                       mpdata.short$well.depth.ft, data = mpdata.short)
trumpyourcat.1 <- update(trumpyourcat.full,~.-mpdata.short$orp.mv:mpdata.short$well.depth.ft)
trumpyourcat.2 <- update(trumpyourcat.1,~.-mpdata.short$orp.mv)

summary(trumpyourcat.full)
summary(trumpyourcat.1)
summary(trumpyourcat.2)

AIC(trumpyourcat.full,trumpyourcat.1,trumpyourcat.2)
anova.mod.cat <- anova(trumpyourcat.full, trumpyourcat.1, trumpyourcat.2)
print(anova.mod.cat)

##Model trumpyourcat.2 was chosen
