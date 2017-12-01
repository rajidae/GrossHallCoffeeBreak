#### WORKSPACE SETUP ####

# Requirements

# Reset workspace
rm(list = ls())

# Working directory
setwd("C:/Users/allth/Box Sync/StatsProject") # Kristen's Path
# setwd("C:/Users/delan/Box Sync/StatsProject") # Delaney's path

#### DATA PREP ####

# Import data
fdat <- read.csv("DEQ_Data_Full.csv", header = TRUE, stringsAsFactors = FALSE)

# Rename Columns
colnames(fdat)

colnames(fdat)[colnames(fdat)=="Well.Depth..ft."] <- "well.depth.ft" 
colnames(fdat)[colnames(fdat)=="Cr6..ug.L."] <- "cr6.ppb"
colnames(fdat)[colnames(fdat)=="CrT..ug.L."] <- "crt.ppb"
colnames(fdat)[colnames(fdat)=="Mn..ug.L."] <- "mn.ppb"
colnames(fdat)[colnames(fdat)=="Fe..mg.L."] <- "fe.ppm"
colnames(fdat)[colnames(fdat)=="As..ug.L."] <- "as.ppb"
colnames(fdat)[colnames(fdat)=="V..ug.L."] <- "v.ppb"
colnames(fdat)[colnames(fdat)=="Temp..C.."] <- "temp.c"
colnames(fdat)[colnames(fdat)=="DO..mg.L."] <- "do.ppm"
colnames(fdat)[colnames(fdat)=="ORP..mv."] <- "orp.mv"
colnames(fdat)[colnames(fdat)=="EC..us.cm."] <- "ec.uscm"

colnames(fdat)

# change Cr6 data to binary variable - NC Public Health Goal
## Success = Cr6 > NC PHGL (0.07 ppb)
## Failure = Cr6 < NC PHGL (0.07 ppb)
phgl.nc <- 0.07
fdat$cr6.nc <- ifelse(fdat$cr6.ppb == "bdl", 0, ifelse(fdat$cr6.ppb >= phgl.nc, 1, 0))

length(fdat$cr6.nc[fdat$cr6.nc == 0]) # 52 "failures"
length(fdat$cr6.nc[fdat$cr6.nc == 1]) # 6 "successes"

# change Cr6 data to binary variable - California Public Health Goal
## Success = Cr6 > CA PHGL (0.02 ppb)
## Failure = Cr6 < CA PHGL (0.02 ppb)
phgl.ca <- 0.02
fdat$cr6.ca <- ifelse(fdat$cr6.ppb == "bdl", 0, ifelse(fdat$cr6.ppb >= phgl.ca, 1, 0))

length(fdat$cr6.ca[fdat$cr6.ca == 0]) # 41 "failures"
length(fdat$cr6.ca[fdat$cr6.ca == 1]) # 17 "successes"

# change Cr6 data to binary variable - Quantifiable
## Success = Cr6 > LOQ (0.012 ppb)
## Failure = Cr6 < LOQ (0.012 ppb)
LOQ <- 0.012
fdat$cr6.LOQ <- ifelse(fdat$cr6.ppb == "bdl", 0, ifelse(fdat$cr6.ppb >= LOQ, 1, 0))

length(fdat$cr6.LOQ[fdat$cr6.LOQ == 0]) # 36 "failures"
length(fdat$cr6.LOQ[fdat$cr6.LOQ == 1]) # 22 "successes"

# now deal with Cr6.ppb raw data
LOD <- 0.004
set.seed(1001)
bdl.crit <- runif(length(fdat$cr6.ppb[fdat$cr6.ppb=="bdl"]),0,LOD)
fdat$cr6.ppb[fdat$cr6.ppb=="bdl"] <- bdl.crit

# typeof(cr6.ppb) = "character" -> need to correct for this
storage.mode(fdat$cr6.ppb) <- "double"

attach(fdat)

#### ANALYSIS ####

var <- pH

plot(var, cr6.ppb)
plot(var, log(cr6.ppb))
  # log transformation gives linear relationship for cr6 and pH

qqnorm(as.numeric(cr6.ppb))
qqnorm(log(as.numeric(cr6.ppb)))

hist(cr6.ppb)


plot(var, cr6.nc, col="purple")
plot(var, cr6.ca, col="red")
plot(var, cr6.LOQ, col="green")


