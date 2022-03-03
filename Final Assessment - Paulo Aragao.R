##################################
# Final Assessment - Paulo Aragao
##################################
### Hult Inter. Business School
##################################
# Wealth and Risk Management in R
##################################

#installing packages
install.packages("quantmod")
library(quantmod) #loading the quantmod package

#gettign data
getSymbols("WFC")

#definign separate assets/stocks
stock1 <- getSymbols("IXN", auto.assign = FALSE)
stock2 <- getSymbols("SPY", auto.assign = FALSE)
fixed_income1 <- getSymbols("LQD", auto.assign = FALSE)
fixed_income2 <- getSymbols("IEF", auto.assign = FALSE)
real_asset <- getSymbols("VNQ", auto.assign = FALSE)
commodities <- getSymbols("GLD", auto.assign = FALSE)

#putting them together in one table
joined_pricesExam <- merge.xts(stock1, 
                               stock2, 
                               fixed_income1, 
                               fixed_income2, 
                               real_asset, 
                               commodities)

#selecting adjusted prices
joined_prices_onlyExam <- joined_pricesExam[,c(6,12,18,24,30,36)]

install.packages("dplyr")
library(dplyr)

n <- 25 #trading days

#
joined_returnsExam <- as.data.frame(joined_prices_onlyExam) %>%
  mutate(IXN_ROR = log(IXN.Adjusted/lag(IXN.Adjusted, n )))%>% 
  mutate(SPY_ROR = log(SPY.Adjusted/lag(SPY.Adjusted, n )))%>%
  mutate(LQD_ROR = log(LQD.Adjusted/lag(LQD.Adjusted, n )))%>%
  mutate(IEF_ROR = log(IEF.Adjusted/lag(IEF.Adjusted, n )))%>%
  mutate(VNQ_ROR = log(VNQ.Adjusted/lag(VNQ.Adjusted, n )))%>%
  mutate(GLD_ROR = log(GLD.Adjusted/lag(GLD.Adjusted, n )))

#monthly
stock1_returns <- monthlyReturn(getSymbols("IXN", auto.assign = FALSE))
stock2_returns <- monthlyReturn(getSymbols("SPY", auto.assign = FALSE))
fixed_income1_returns <- monthlyReturn(getSymbols("LQD", auto.assign = FALSE))
fixed_income2_returns <- monthlyReturn(getSymbols("IEF", auto.assign = FALSE))
real_assets_returns <- monthlyReturn(getSymbols("VNQ", auto.assign = FALSE))
commodities_returns <- monthlyReturn(getSymbols("GLD", auto.assign = FALSE))

joined_monthlyreturnsExam <- merge.xts(stock1_returns, 
                                       stock2_returns, 
                                       fixed_income1_returns,
                                       fixed_income2_returns,
                                       real_assets_returns,
                                       commodities_returns)

IXN_alloc <- 0.175
SPY_alloc <- 0.221
LQD_alloc <- 0.182
IEF_alloc <- 0.113
VNQ_alloc <- 0.079
GLD_alloc <- 0.23

joined_portfolio_retExam <- as.data.frame(joined_monthlyreturnsExam) %>%
  mutate(portfolio = IXN_alloc * IXN_return + 
           SPY_alloc * SPY_return + 
           LQD_alloc * LQD_return + 
           IEF_alloc * IEF_return + 
           VNQ_alloc * VNQ_return + 
           GLD_alloc * GLD_return)


benchmark_returnsExam <- monthlyReturn(getSymbols("VONE", auto.assign = FALSE))

joined_monthlyreturnsExam <- merge.xts(stock1_returns, 
                                       stock2_returns, 
                                       fixed_income1_returns,
                                       fixed_income2_returns,
                                       real_assets_returns,
                                       commodities_returns,
                                       benchmark_returnsExam)

colnames(joined_monthlyreturnsExam) <- c("IXN_return", 
                                         "SPY_return", 
                                         "LQD_return", 
                                         "IEF_return", 
                                         "VNQ_return", 
                                         "GLD_return",
                                         "VONE_Bench")

############## SIGMA for the last 12 months
##############

time_index <- nrow(joined_monthlyreturnsExam)
joined_monthlyreturnsExam <- as.data.frame(joined_monthlyreturnsExam) #need to convert to data frame to be able to subset based on indexing
IXN_sigma <- sd(joined_monthlyreturnsExam$IXN_return[(time_index-11) : time_index]) * sqrt(12) #we need to annualize by sqrt(12)
SPY_sigma <- sd(joined_monthlyreturnsExam$SPY_return[(time_index-11) : time_index]) * sqrt(12)
LQD_sigma <- sd(joined_monthlyreturnsExam$LQD_return[(time_index-11) : time_index]) * sqrt(12)
IEF_sigma <- sd(joined_monthlyreturnsExam$IEF_return[(time_index-11) : time_index]) * sqrt(12)
VNQ_sigma <- sd(joined_monthlyreturnsExam$VNQ_return[(time_index-11) : time_index]) * sqrt(12)
GLD_sigma <- sd(joined_monthlyreturnsExam$GLD_return[(time_index-11) : time_index]) * sqrt(12)

RUS1000_sigma <- sd(joined_monthlyreturnsExam$VONE_Bench[(time_index-11) : time_index]) * sqrt(12)

Port_sigma <- sd(joined_portfolio_retExam$portfolio[(time_index-11) : time_index]) * sqrt(12)

print(IXN_sigma) #18.3% --- very volatile
print(SPY_sigma) #15.1% --- very volatile
print(LQD_sigma) #6.83%
print(IEF_sigma) #4.28%
print(VNQ_sigma) #13.47% 
print(GLD_sigma) #20.67% --- very volatile

#ANALYSIS -> the highest sigmas indicate higher risks, since they are more volatile. The 
#client needs a "tigther" distribution for optimal diversification of portfolio

print(RUS1000_sigma) #15.45%

print(Port_sigma) #10.10% --- low volatility of entire portfolio, seems it is 
                              #diversified, despite the high sigmas of some assets

#### BETA
###################

time_index <- nrow(joined_monthlyreturnsExam)
last_12_monthsExam <- joined_monthlyreturnsExam[(time_index-11) : time_index,]

IXN_reg <- lm(IXN_return ~ VONE_Bench, data=last_12_monthsExam)  
summary(IXN_reg) #1.12 --- stock has high risk, as it moves more dramatically than the market

SPY_reg <- lm(SPY_return ~ VONE_Bench, data=last_12_monthsExam)  
summary(SPY_reg) #0.97 --- stock is very close to 1, indicating risk, will tend to mimic the market movement

LQD_reg <- lm(LQD_return ~ VONE_Bench, data=last_12_monthsExam)  
summary(LQD_reg) #0.17 --- very close to zero which is lower risk. Good asset to keep, regarding the Beta alone

IEF_reg <- lm(IEF_return ~ VONE_Bench, data=last_12_monthsExam)  
summary(IEF_reg) #0.02 --- Very close to zero which is very lower risk. Ideal number due to its little correlation with the market

VNQ_reg <- lm(VNQ_return ~ VONE_Bench, data=last_12_monthsExam)  
summary(VNQ_reg) #0.70 --- High risk, close to 1. It will mimic the market movements

GLD_reg <- lm(GLD_return ~ VONE_Bench, data=last_12_monthsExam)  
summary(GLD_reg) #0.21 --- Closer to zero, but P-value insignificant


## TrackingError for last 12 months (need to annualize by sqrt(12))
##############

time_index <- nrow(joined_monthlyreturnsExam) #this is how many monthly observations we have in our data frame
joined_monthlyreturnsExam <- as.data.frame(joined_monthlyreturnsExam) #need to convert to data frame to be able to subset based on indexing
IXN_te <- sd(joined_monthlyreturnsExam$IXN_return[(time_index-11) : time_index] -
               joined_monthlyreturnsExam$VONE_Bench[(time_index-11) : time_index])*sqrt(12)
SPY_te <- sd(joined_monthlyreturnsExam$SPY_return[(time_index-11) : time_index] -
               joined_monthlyreturnsExam$VONE_Bench[(time_index-11) : time_index])*sqrt(12)
LQD_te <- sd(joined_monthlyreturnsExam$LQD_return[(time_index-11) : time_index] -
               joined_monthlyreturnsExam$VONE_Bench[(time_index-11) : time_index])*sqrt(12)
IEF_te <- sd(joined_monthlyreturnsExam$IEF_return[(time_index-11) : time_index] -
               joined_monthlyreturnsExam$VONE_Bench[(time_index-11) : time_index])*sqrt(12)
VNQ_te <- sd(joined_monthlyreturnsExam$VNQ_return[(time_index-11) : time_index] -
               joined_monthlyreturnsExam$VONE_Bench[(time_index-11) : time_index])*sqrt(12)
GLD_te <- sd(joined_monthlyreturnsExam$GLD_return[(time_index-11) : time_index] -
               joined_monthlyreturnsExam$VONE_Bench[(time_index-11) : time_index])*sqrt(12)

print(IXN_te) #6.36%
print(SPY_te) #1.32% --- very low tracking error which indicates high connection with the benchmark (will not deviate a lot when market moves)
print(LQD_te) #14.18%
print(IEF_te) #15.69%
print(VNQ_te) #9.05%
print(GLD_te) #23.75% --- very high tracking error which indicates highest risk

#Calculating Sharpe for single securities and for the entire portfolio:
# Share ratio will have a 12M expected return, and a 12M sigma
# the risk free rate is 0.001
riskfree <- 0.001

#we've calculated the expectation using the mean() function
IXN_sharpe <- (mean(joined_monthlyreturnsExam$IXN_return[(time_index-11) : time_index])-riskfree) / 
  IXN_sigma
SPY_sharpe <- (mean(joined_monthlyreturnsExam$SPY_return[(time_index-11) : time_index])-riskfree) / 
  SPY_sigma
LQD_sharpe <- (mean(joined_monthlyreturnsExam$LQD_return[(time_index-11) : time_index])-riskfree) / 
  LQD_sigma
IEF_sharpe <- (mean(joined_monthlyreturnsExam$IEF_return[(time_index-11) : time_index])-riskfree) / 
  IEF_sigma
VNQ_sharpe <- (mean(joined_monthlyreturnsExam$VNQ_return[(time_index-11) : time_index])-riskfree) / 
  VNQ_sigma
GLD_sharpe <- (mean(joined_monthlyreturnsExam$GLD_return[(time_index-11) : time_index])-riskfree) / 
  GLD_sigma

print(c(IXN_sharpe, SPY_sharpe, LQD_sharpe, IEF_sharpe,VNQ_sharpe, GLD_sharpe ))
# 0.150599783  0.163934841 -0.013477269 -0.125201953  0.159737367 -0.002986186
# According to the Sharpe ratios above, the highest ones are SPY, VNQ and IXN.

#FINAL ANALYSIS
##
##   Based on the results above, we can recommend the following:
##  - Client should keep LQD, IEF and SPY as they show the best indexes: not
## very volatile, close to zero Betas, low tracking error.
##  - Client should consider selling IXN (very high Beta, showing dramatic movements
## compared to the market, and high volatility) and GLD (very volatile, Beta is close to zero but
## P value is not significant, very high trackign error and negative Sharpe ratio)
## - CLient should watch closely VNQ because despite its high Sharpe ratio, the beta is
## close to 1 and it is quite volatile as well.

##
## thank you!







