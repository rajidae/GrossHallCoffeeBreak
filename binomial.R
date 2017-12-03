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
fdat$cr6.nc <- ifelse(fdat$cr6.ppb == "n.a.", 0, ifelse(fdat$cr6.ppb >= phgl.nc, 1, 0))

length(fdat$cr6.nc[fdat$cr6.nc == 0]) # 52 "failures"
length(fdat$cr6.nc[fdat$cr6.nc == 1]) # 6 "successes"

# change Cr6 data to binary variable - California Public Health Goal
## Success = Cr6 > CA PHGL (0.02 ppb)
## Failure = Cr6 < CA PHGL (0.02 ppb)
phgl.ca <- 0.02
fdat$cr6.ca <- ifelse(fdat$cr6.ppb == "n.a.", 0, ifelse(fdat$cr6.ppb >= phgl.ca, 1, 0))

length(fdat$cr6.ca[fdat$cr6.ca == 0]) # 41 "failures"
length(fdat$cr6.ca[fdat$cr6.ca == 1]) # 17 "successes"

# change Cr6 data to binary variable - Quantifiable
## Success = Cr6 > LOQ (0.012 ppb)
## Failure = Cr6 < LOQ (0.012 ppb)
LOQ <- 0.012
fdat$cr6.LOQ <- ifelse(fdat$cr6.ppb == "n.a.", 0, ifelse(fdat$cr6.ppb >= LOQ, 1, 0))

length(fdat$cr6.LOQ[fdat$cr6.LOQ == 0]) # 36 "failures"
length(fdat$cr6.LOQ[fdat$cr6.LOQ == 1]) # 22 "successes"

# now deal with Cr6.ppb raw data
# LOD <- 0.004
# set.seed(1001)
# bdl.crit <- runif(length(fdat$cr6.ppb[fdat$cr6.ppb=="bdl"]),0,LOD)
# fdat$cr6.ppb[fdat$cr6.ppb=="bdl"] <- bdl.crit

attach(fdat)

#### EXPLORATION ####

hist(as.numeric(cr6.ppb))
hist(log(as.numeric(cr6.ppb)))

# pH Relationship, excluding bdls
  var <- pH
  
  # linear
  plot(var, cr6.ppb)
  plot(var, log(as.numeric(cr6.ppb)))
    # log transformation gives linear relationship for cr6 and pH
  
  # binomial
  plot(var, cr6.nc, col="purple")
  plot(var, cr6.ca, col="red")
  plot(var, cr6.LOQ, col="green")

# ORP Relationship
  var <- orp.mv
  
  # linear, excluding bdls
  plot(var, cr6.ppb)
  plot(var, log(as.numeric(cr6.ppb)))
    # neither relationship seems well defined

  # binomial
  plot(var, cr6.nc, col="purple")
  plot(var, cr6.ca, col="red")
  plot(var, cr6.LOQ, col="green")
  
# well depth Relationship
  var <- well.depth.ft
  
  # linear, excluding bdls
  plot(var, cr6.ppb)
  plot(var, log(as.numeric(cr6.ppb)))
    # log may have better fit
  
  # binomial
  plot(var, cr6.nc, col="purple")
  plot(var, cr6.ca, col="red")
  plot(var, cr6.LOQ, col="green")
  
# Aquifer Relationship
  var <- factor(Aquifer)
  
  # linear, excluding bdls
  plot(var, cr6.ppb)
  plot(var, log(as.numeric(cr6.ppb)))
    # probably no relationship
  
  # binomial
  plot(var, cr6.nc, col="purple")
  plot(var, cr6.ca, col="red")
  plot(var, cr6.LOQ, col="green")
    # these seem to be useless for factors
  
#### ANALYSIS ####

# NC Goal model  
flynnflipped.full <- glm(cr6.nc ~ pH*orp.mv*well.depth.ft, 
                         family = binomial, data = fdat)
  summary(flynnflipped.full)  

flynnflipped.1 <- update(flynnflipped.full,~.-pH:orp.mv:well.depth.ft)
  summary(flynnflipped.1)
flynnflipped.2 <- update(flynnflipped.1,~.-pH:orp.mv)
  summary(flynnflipped.2)
flynnflipped.3 <- update(flynnflipped.2,~.-orp.mv:well.depth.ft)
  summary(flynnflipped.3)  
flynnflipped.4 <- update(flynnflipped.3,~.-pH:well.depth.ft)
  summary(flynnflipped.4)   
flynnflipped.5 <- update(flynnflipped.4,~.-well.depth.ft)
  summary(flynnflipped.5)  
flynnflipped.6 <- update(flynnflipped.5,~.-orp.mv)
  summary(flynnflipped.6)    
flynnflipped.0 <- update(flynnflipped.6,~.-pH)
  summary(flynnflipped.0)    
  
# test for minimum adequate model
  
  AIC(flynnflipped.full, flynnflipped.1, flynnflipped.2, flynnflipped.3, flynnflipped.4,
      flynnflipped.5, flynnflipped.6, flynnflipped.0)
    # flynnflipped.3     5 35.48320
  
  anova(flynnflipped.full, flynnflipped.1, flynnflipped.2, flynnflipped.3, flynnflipped.4,
        flynnflipped.5, flynnflipped.6, flynnflipped.0, test = "Chisq")
    # same result
    # Model 4: cr6.nc ~ pH + orp.mv + well.depth.ft + pH:well.depth.ft
  
#### DIAGNOSTICS ####

  flynnflipped.3$deviance/flynnflipped.3$df.residual
  pchisq(flynnflipped.3$deviance, flynnflipped.3$df.residual, lower = FALSE)
    # not significant = no evidence for lack of fit
  
  require(pscl)
  pR2(flynnflipped.3) # 0.339
  
  require(car)
  vif(flynnflipped.3)
    # well depth appears to be highly multicollinear with the other variables
    cor.test(well.depth.ft, pH)
      # significantly correlated
      # correlation coefficient = 0.604
    
    cor.test(pH, orp.mv)
      # significantly correlated
      # correlation coefficient = -0.657
    
    cor.test(well.depth.ft, orp.mv)
      # signifcantly correlated
      # correlation coefficient = -0.31
  
# CHOOSING flynnflipped.5 as more parsimonious model,
# and removing the multicollinear terms
  
  flynnflipped.5$deviance/flynnflipped.5$df.residual
  pchisq(flynnflipped.5$deviance, flynnflipped.5$df.residual, lower = FALSE)
    # not significant = no evidence for lack of fit
  
  require(pscl)
  pR2(flynnflipped.5) # 0.231  
  
  vif(flynnflipped.5) # no multicollinearity
  
  
#### INFERENCES ####
  
# Equation
  
  summary(flynnflipped.5)
  coefs.5 <- coefficients(flynnflipped.5)
  
  # Equation:
  # x1 = pH value
  # x2 = ORP value
  # probability eqn = inv.logit(coefs.5[1]+coefs.5[2]*x1+coefs.5[3]*x2)
  
  # convert to probabilities using inverse logit
  require(boot)
  coef.prob <- inv.logit(coef(flynnflipped.5))
    coef.prob
  
# Inferences from coefficients    
    
  # intercept only
     # interpretted as using 0 for the parameters
    coef.prob[1]*100 
      # 0.0007% chance Cr6>0.07ppb when pH=0, ORP=0 (super acidic, neutral ORP)
      # makes no sense for pH=0
    
  # if pH = 7, ORP = 0 (neutral pH and ORP)
  x1 = 7
  x2 = 0
  inv.logit(coefs.5[1]+coefs.5[2]*x1+coefs.5[3]*x2)
    # 10.7% chance for Cr6>0.07ppb when pH=7, ORP=0
  
  # if pH = 14, ORP = 0 (super basic, neutral ORP)
  x1 = 14
  x2 = 0
  inv.logit(coefs.5[1]+coefs.5[2]*x1+coefs.5[3]*x2)
    # 99.9% chance for Cr6>0.07ppb when pH=14, ORP=0
        
  # if pH = 7, ORP = -200
  x1 = 7
  x2 = -200
  inv.logit(coefs.5[1]+coefs.5[2]*x1+coefs.5[3]*x2)
    # 2.3% chance for Cr6>0.07ppb when pH=7, ORP=-200
  
  # if pH = 7, ORP = +200
  x1 = 7
  x2 = 200
  inv.logit(coefs.5[1]+coefs.5[2]*x1+coefs.5[3]*x2)
    # 37.5% chance for Cr6>0.07ppb when pH=7, ORP=+200  

#### PLOTS ####

# expected probabilities of the observed data given the model
fitted(flynnflipped.5)  

# get results (response) on original scale
predict(flynnflipped.5, type = "response")

# plot raw data
plot(jitter(pH), cr6.nc, las = 1, pch = 21, cex = 1.2, bg = "grey",
                      xlab = "pH", ylab = "Cr6 (ppb)", main = "Flynnflipped.5 Model",
     xlim = c(0,14))  
# add fitted data from model
points(pH, fitted(flynnflipped.5), col = "blue")
# add fitted curve
x <- min(pH)+(max(pH)-min(pH))*(0:100)/100
# with max ORP (500)
x2 <- max(orp.mv)
curve(expr = inv.logit(coefs.5[1]+coefs.5[2]*x+coefs.5[3]*x2), 
                       add = TRUE, col = "red", lwd = 2)
# with min ORP (-593.8)
x2 <- min(orp.mv)
curve(expr = inv.logit(coefs.5[1]+coefs.5[2]*x+coefs.5[3]*x2), 
                       add = TRUE, col = "green", lwd = 2)
# with mean ORP (-88.53)
x2 <- mean(orp.mv)
curve(expr = inv.logit(coefs.5[1]+coefs.5[2]*x+coefs.5[3]*x2), 
                       add = TRUE, col = "blue", lwd = 2)
# with neutral ORP (0)
x2 <- 0
curve(expr = inv.logit(coefs.5[1]+coefs.5[2]*x+coefs.5[3]*x2), 
                       add = TRUE, col = "purple", lwd = 2)
# add legend (500)
legend(x = "left", legend = c("min ORP (Reducing)","mean ORP","neutral ORP","max ORP (Oxidizing)"), 
       col = c("green","blue","purple","red"), lty = 1, lwd = 2)

#### ANALYSIS WO DP21 ####

# Model Selection without DP #21

sad.full <- glm(cr6.nc ~ pH*orp.mv*well.depth.ft, 
                family = binomial, data = fdat[-21,])
  summary(sad.full)  

sad.1 <- update(sad.full,~.-pH:orp.mv:well.depth.ft)
  summary(sad.1)
sad.2 <- update(sad.1,~.-orp.mv:well.depth.ft)
  summary(sad.2)
sad.3 <- update(sad.2,~.-pH:well.depth.ft)
  summary(sad.3)  
sad.4 <- update(sad.3,~.-well.depth.ft)
  summary(sad.4)   
sad.5 <- update(sad.4,~.-pH:orp.mv)
  summary(sad.5)  
sad.6 <- update(sad.5,~.-orp.mv)
  summary(sad.6)    
sad.0 <- update(sad.6,~.-pH)
  summary(sad.0)   

AIC(sad.full, sad.1, sad.2, sad.3, sad.4, sad.5, sad.6, sad.0)
# sad.5     3 22.58291 = cr6.nc ~ pH + orp.mv

anova(sad.full, sad.1, sad.2, sad.3, sad.4, sad.5, sad.6, sad.0, test = "Chisq")  
# Model 7: cr6.nc ~ pH

#### DIAGNOSTICS WO DP21 ####

with(fdat[-21,], cor.test(well.depth.ft, pH))
# significantly correlated
# correlation coefficient = 0.613 (increased)

with(fdat[-21], cor.test(pH, orp.mv))
# significantly correlated
# correlation coefficient = -0.657 (same)

with(fdat[-21,], cor.test(well.depth.ft, orp.mv))
# signifcantly correlated
# correlation coefficient = -0.333 (increased)

sad.5$deviance/sad.5$df.residual
pchisq(sad.5$deviance, sad.5$df.residual, lower = FALSE)
# not significant = no evidence for lack of fit

d2 <- sum(residuals(sad.5,"pearson")^2)
  disp <- d2/df.residual(sad.5)
    phi <- sqrt(disp)
      phi # 0.57 = not overdispersed

require(pscl)
pR2(sad.5) # 0.511 (really increased) 

vif(sad.5) # no multicollinearity for real


#### INFERENCES WO DP21 ####

# Equation
  
  summary(sad.5)
  coefs.s5 <- coefficients(sad.5)
  
  # Equation:
  # x1 = pH value
  # x2 = ORP value
  # probability eqn = inv.logit(coefs.5[1]+coefs.5[2]*x1+coefs.5[3]*x2)
  
  # convert to probabilities using inverse logit
  require(boot)
  coef.prob5 <- inv.logit(coef(sad.5))
  coef.prob5

# Inferences from coefficients    
  
  # intercept only
  # interpretted as using 0 for the parameters
  coef.prob5[1]*100 
  # 1.09e-10% chance Cr6>0.07ppb when pH=0, ORP=0 (super acidic, neutral ORP)
  # makes no sense for pH=0
  
  # if pH = 7, ORP = 0 (neutral pH and ORP)
  x1 = 7
  x2 = 0
  inv.logit(coefs.s5[1]+coefs.s5[2]*x1+coefs.s5[3]*x2)
  # 1.16% chance for Cr6>0.07ppb when pH=7, ORP=0
  
  # if pH = 14, ORP = 0 (super basic, neutral ORP)
  x1 = 14
  x2 = 0
  inv.logit(coefs.s5[1]+coefs.s5[2]*x1+coefs.s5[3]*x2)
  # 100% chance for Cr6>0.07ppb when pH=14, ORP=0
  
  # if pH = 7, ORP = -200
  x1 = 7
  x2 = -200
  inv.logit(coefs.s5[1]+coefs.s5[2]*x1+coefs.s5[3]*x2)
  # 0.24% chance for Cr6>0.07ppb when pH=7, ORP=-200
  
  # if pH = 7, ORP = +200
  x1 = 7
  x2 = 200
  inv.logit(coefs.s5[1]+coefs.s5[2]*x1+coefs.s5[3]*x2)
  # 5.4% chance for Cr6>0.07ppb when pH=7, ORP=+200

  # if pH = max, ORP = max
  x1 = with(fdat[-21,], max(pH))
  x2 = with(fdat[-21,], max(orp.mv))
  inv.logit(coefs.s5[1]+coefs.s5[2]*x1+coefs.s5[3]*x2)
  # 99.5% chance for Cr6>0.07ppb when pH=8.78, ORP=500 
  
  # if pH = mean, ORP = mean
  x1 = with(fdat[-21,], mean(pH))
  x2 = with(fdat[-21,], mean(orp.mv))
  inv.logit(coefs.s5[1]+coefs.s5[2]*x1+coefs.s5[3]*x2)
  # 0.51% chance for Cr6>0.07ppb when pH=6.92, ORP=-88
  
  # if pH = min, ORP = min
  x1 = with(fdat[-21,], min(pH))
  x2 = with(fdat[-21,], min(orp.mv))
  inv.logit(coefs.s5[1]+coefs.s5[2]*x1+coefs.s5[3]*x2)
  # 3.31e-7% chance for Cr6>0.07ppb when pH=3.85, ORP=-593.8

#### PLOTS WO DP21 ####  

# expected probabilities of the observed data given the model
fitted(sad.5)  

# get results (response) on original scale
predict(sad.5, type = "response")

# Cr6 vs pH
  
  # plot raw data
  with(fdat[-21,], plot(jitter(pH), cr6.nc, las = 1, pch = 21, cex = 1.2, bg = "grey",
                        xlab = "pH", ylab = "Cr6 (ppb)", main = "Sad.5 Model (-DP21)",
                        xlim = c(0,14)))  
  # add fitted data from model
  with(fdat[-21,], points(pH, fitted(sad.5), col = "blue"))
  # add fitted curve
  x <- with(fdat[-21,], min(pH)+(max(pH)-min(pH))*(0:100)/100)
  # with max ORP
  x2 <- with(fdat[-21,], max(orp.mv))
  with(fdat[-21,], curve(expr = inv.logit(coefs.s5[1]+coefs.s5[2]*x+coefs.s5[3]*x2), 
                         add = TRUE, col = "red", lwd = 2))
  # with min ORP (-593.8)
  x2 <- with(fdat[-21,], min(orp.mv))
  with(fdat[-21,], curve(expr = inv.logit(coefs.s5[1]+coefs.s5[2]*x+coefs.s5[3]*x2), 
                         add = TRUE, col = "green", lwd = 2))
  # with mean ORP (-95.2)
  x2 <- with(fdat[-21,], mean(orp.mv))
  with(fdat[-21,], curve(expr = inv.logit(coefs.s5[1]+coefs.s5[2]*x+coefs.s5[3]*x2), 
                         add = TRUE, col = "blue", lwd = 2))
  # with neutral ORP (0)
  x2 <- 0
  with(fdat[-21,], curve(expr = inv.logit(coefs.s5[1]+coefs.s5[2]*x+coefs.s5[3]*x2), 
                         add = TRUE, col = "purple", lwd = 2))
  # add legend (500)
  legend(x = "topleft", legend = c("max ORP (Oxidizing)","neutral ORP","mean ORP","min ORP (Reducing)"), 
         col = c("red","purple","blue","green"), lty = 1, lwd = 2)

# Cr6 vs ORP -> not as good representation 
  
  # plot raw data
  with(fdat[-21,], plot(jitter(orp.mv), cr6.nc, las = 1, pch = 21, cex = 1.2, bg = "grey",
                        xlab = "ORP (mV)", ylab = "Cr6 (ppb)", main = "Sad.5 Model (-DP21)"))
  # add fitted data from model
  with(fdat[-21,], points(orp.mv, fitted(sad.5), col = "blue"))
  # add fitted curve
  x <- with(fdat[-21,], min(orp.mv)+(max(orp.mv)-min(orp.mv))*(0:100)/100)
  # with max pH (8.78)
  x1 <- with(fdat[-21,], max(pH))
  with(fdat[-21,], curve(expr = inv.logit(coefs.s5[1]+coefs.s5[2]*x1+coefs.s5[3]*x), 
                         add = TRUE, col = "red", lwd = 2))
  # with min pH (3.85)
  x1 <- with(fdat[-21,], min(pH))
  with(fdat[-21,], curve(expr = inv.logit(coefs.s5[1]+coefs.s5[2]*x1+coefs.s5[3]*x), 
                         add = TRUE, col = "green", lwd = 2))
  # with mean/neutral pH (6.98)
    # why doesn't this one go through datapoints?
  x1 <- with(fdat[-21,], mean(pH))
  with(fdat[-21,], curve(expr = inv.logit(coefs.s5[1]+coefs.s5[2]*x1+coefs.s5[3]*x), 
                         add = TRUE, col = "blue", lwd = 2))
  # add legend (500)
  legend(x = "topleft", legend = c("max pH (basic)", "neutral pH", "min pH (acidic)"), 
         col = c("red","blue","green"), lty = 1, lwd = 2)
  
  
    
#### SCRATCH ####

# showing magnitude of coefficient from interaction term on pH and well depth
require(interplot)
interplot(m = flynnflipped.3, var1="pH", var2 = "well.depth.ft")
interplot(m = flynnflipped.3, var1="well.depth.ft", var2 = "pH")
    
# Alternative Combination of Main Effects 
  summary(flynnflipped.4)   

  flynnflipped.5a <- update(flynnflipped.4,~.-orp.mv)
    summary(flynnflipped.5a)
  flynnflipped.6a <- update(flynnflipped.5a,~.-well.depth.ft)
    summary(flynnflipped.6a)
  anova(flynnflipped.full, flynnflipped.1, flynnflipped.2, flynnflipped.3, flynnflipped.4,
        flynnflipped.5a, flynnflipped.6a, flynnflipped.0, test = "Chisq")
  AIC(flynnflipped.full, flynnflipped.1, flynnflipped.2, flynnflipped.3, flynnflipped.4,
      flynnflipped.5a, flynnflipped.6a, flynnflipped.0)
    # flynnflipped.3, then flynnflipped.4, 5a and 6a worse than 5 and 6
    
  flynnflipped.5b <- update(flynnflipped.4,~.-pH)
    summary(flynnflipped.5b) 
  flynnflipped.6b <- update(flynnflipped.5b,~.-orp.mv)
    summary(flynnflipped.6b)
  anova(flynnflipped.full, flynnflipped.1, flynnflipped.2, flynnflipped.3, flynnflipped.4,
        flynnflipped.5b, flynnflipped.6b, flynnflipped.0, test = "Chisq")
  AIC(flynnflipped.full, flynnflipped.1, flynnflipped.2, flynnflipped.3, flynnflipped.4,
      flynnflipped.5b, flynnflipped.6b, flynnflipped.0)    
  # flynnflipped.3, then flynnflipped.4, 5a and 6a worse than 5 and 6    

# Excluding pH Completely
  
  alternativefacts.full <- glm(cr6.nc ~ orp.mv*well.depth.ft, 
                           family = binomial, data = fdat)
  summary(alternativefacts.full)  
  
  alternativefacts.1 <- update(alternativefacts.full,~.-orp.mv:well.depth.ft)
    summary(alternativefacts.1)
  alternativefacts.2 <- update(alternativefacts.1,~.-orp.mv)
    summary(alternativefacts.2)
  alternativefacts.0 <- update(alternativefacts.2,~.-well.depth.ft)
    summary(alternativefacts.0)
  AIC(alternativefacts.full, alternativefacts.1, alternativefacts.2, alternativefacts.0)

  pR2(alternativefacts.1)
  
# Other variables

plot(mn.ppb, cr6.nc) 
plot(pH, mn.ppb)
plot(orp.mv, mn.ppb)
plot(well.depth.ft, mn.ppb)
  
drumpf.full <- glm(cr6.nc ~ pH+orp.mv+factor(County), 
                         family = binomial, data = fdat)
  summary(drumpf.full)  
    # looked at DO, Temp, County, Aquifer, Mn, EC -> none of these matter
