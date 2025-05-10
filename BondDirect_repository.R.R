#Directional distance function and OLS regression
#EURO bonds including perpetual bonds at 30 years
#data set includes DCALL (callable bonds) and DMAT (at maturity)


# Clear everything in memory
rm(list=ls(all=TRUE));

# Set working directory
setwd("C:\\bonds");


#==========================================================================================#
# Import the data                                                                          #
#==========================================================================================#


# Install the "Benchmarking" package (we need to do this only once)
install.packages("Benchmarking");

# Call the Benchmarking library
library(Benchmarking)

# bonds in EURO
dat_euro <- read.table("bondseuro_with_perp.prn", header = TRUE);

dat1 <- na.omit(dat_euro)
names(dat1)

dat1$Country <- as.factor(dat1$Country)
dat1$Country2<- as.factor(dat1$Country2)
dat1$Sector  <- as.factor(dat1$Sector)
dat1$Quarter <- as.factor(dat1$Quarter)
dat1$SLB <- as.factor(dat1$SLB)
dat1$SDI <- as.factor(dat1$SDI)
dat1$Year2 <- dat1$Year
dat1$Year    <- as.factor(dat1$Year)

dat1$Sector  <- relevel(dat1$Sector, ref ="Utilities")
dat1$Country <- relevel(dat1$Country, ref="DE")
dat1$Country2 <- relevel(dat1$Country2, ref="DE")
dat1$SLB     <- dat1$SLB
dat1$Quarter <- dat1$Quarter

#create input (x), output (y) and direction (z)
x <- with(dat1, cbind(Time,Debt))
y <- with(dat1, cbind(Q))
z <- c(1)

install.packages("RcmdrMisc")
library(RcmdrMisc)
Summary(dat1[,c("Q", "Time","Debt", "Amount")], statistics = c("mean", "sd"))
summary(x)
summary(y)
summary(dat1$Sector)
summary(dat1$Country2)
summary(dat1$Year)
summary(dat1$DCALL)
summary(dat1$SLB)
summary(dat1$SDI)


#run the directional distance function
direct.te <- dea.direct(x,y,ORIENTATION="out",DIRECT=z)
names(direct.te)

dat1$TE      <- direct.te$objval
#compute mean and standard deviation by country
aggregate(dat1$TE, list(dat1$Country2), FUN=mean)
aggregate(dat1$TE, list(dat1$Country2), FUN=sd)
aggregate(dat1$TE, list(dat1$Country2), FUN=min)
aggregate(dat1$TE, list(dat1$Country2), FUN=max)

#compute mean and standard deviation by year
aggregate(dat1$TE, list(dat1$Year), FUN=mean)
aggregate(dat1$TE, list(dat1$Year), FUN=sd)
aggregate(dat1$TE, list(dat1$Year), FUN=min)
aggregate(dat1$TE, list(dat1$Year), FUN=max)

#compute mean and standard deviation by sector
aggregate(dat1$TE, list(dat1$Sector), FUN=mean)
aggregate(dat1$TE, list(dat1$Sector), FUN=sd)
aggregate(dat1$TE, list(dat1$Sector), FUN=min)
aggregate(dat1$TE, list(dat1$Sector), FUN=max)

#second stage regression


#OLS models with DCALL

model_ols1 <- lm(TE ~ SLB + DCALL + Amount + Sector, data=dat1)

model_ols2 <- lm(TE ~ SLB + DCALL + Amount + Country2, data=dat1)

model_ols3 <- lm(TE ~ SLB + DCALL + Amount + Sector + Country2, data=dat1)


install.packages("sandwich")
install.packages("lmtest")
# Load libraries
library("lmtest")
library("sandwich")

# Robust t test
coeftest(model_ols1, df=Inf, vcov = vcovHC(model_ols1, type = "HC1"))
coeftest(model_ols2, df=Inf, vcov = vcovHC(model_ols2, type = "HC1"))
coeftest(model_ols3, df=Inf, vcov = vcovHC(model_ols3, type = "HC1"))

install.packages("sjPlot");
library(sjPlot)
tab_model(model_ols1, show.se = TRUE, digits = 3)
tab_model(model_ols2, show.se = TRUE, digits = 3)
tab_model(model_ols3, show.se = TRUE, digits = 3)



datREG <-subset(dat1, dat1$SLB == "N")
datSLB <-subset(dat1, dat1$SLB == "Y")

summary(datSLB)
summary(datREG)


