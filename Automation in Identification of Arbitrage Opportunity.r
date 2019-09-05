#install.packages("openxlsx")
#install.packages("goftest")
#install.packages("fitdistrplus")
#install.packages("GoFKernel")
#install.packages("NormalLaplace")
#install.packages("metRology")
library(openxlsx)
library(fitdistrplus)
library(goftest)
library(GoFKernel)
library(NormalLaplace)
library(MASS)

#1
data_SandP<- read.csv("C:/Users/rohit/Documents/NCSU/FIM528/Project/S&PIndex.csv", header = TRUE)
spot <- data_SandP[,2]
summary(spot)
sd(spot)
len_g <- length(spot)
returns_g <- {}
#S&P Index Returns
for(i in 1:len_g-1)
{
  returns_g[i] <- log(spot[i+1]/spot[i])
}
#Distribution of S&P Index Returns
plotdist(returns_g, histo = TRUE, demp = TRUE)
r = mean(data_SandP[,3])/100
q = 0.0176
ft =0
#Function to check for Arbitrage Opportunity on Equity Index
equityd = function(s,r,q,t,fm)
{
  ft=s*(exp((r-q)*t/360))
  ft
  if (fm>ft)
    {
    r <- c('Possible Arbitrage Opportunity exits. 1. Borrow', s,'amount at the rate', r*100,'% and use it to purchase underlying stocks equivalent to spot level of the index for immediate delivery and  
           2. Short a futures contract on index')
    }
  else if (fm<ft)
    {
    r <- c('Possible Arbitrage Opportunity exits. 1. Short underlying stocks equivalent to spot level of the index, invest the proceeds at rate', r*100,' % and 2. Long futures contract on index')
    }
  else
  {
    r <- c('No Arbitrage Opportunity')
  }
  return(r)
}
#Check 
#Equity(Spot, Risk Free Rate, Dividend Yield, Time To Maturity, Futures Market Price) )
equityd(2919.56,0.022,0.0176,80,2922.75)
