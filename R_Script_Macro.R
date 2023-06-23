
################################################################################
#
# Macro Project 
#
################################################################################

rm(list=ls())
setwd("~/Desktop/ECO 4006F/Macro Project ")
library(stargazer) # For tables
library(lmtest) # DW test
library(tseries) # adf tests

################################################################################
#
# (1) Reading and cleaning data
#
################################################################################

# Variables: 
# GDP per capita (constant 2010 US$ prices)
# Gross fixed capital formation (% of GDP)
# Military expenditure (% of GDP)
# Population (proxy for labour force)

### (1.1) GDPC : GDP per capita in constant prices (2010 US$)

gdpc <- read.csv("Data/GDP_per_capita.csv")

# Removing columns 1,2 & 4
gdpc <- gdpc[,-c(1,2,4)]
colnames(gdpc) <- c("date", "gdpc")

# Transform date appropriately
#gdpc$date <- as.Date(as.character(gdpc$date), "%Y")
gdpc$gdpc <- as.numeric(gdpc$gdpc)

# Creating a year variable by taking the subset of the date variable
#gdpc$year <- substr(gdpc$date, 1, 4)
#gdpc$year <- as.integer(gdpc$year)

### (1.2) INV : gross fixed capital formation as a share of GDP

inv <- read.csv("Data/INV.csv")

# Removing columns 1,2 & 4
inv <- inv[,-c(1,2,4)]
colnames(inv) <- c("date", "INV")
inv$INV <- as.numeric(inv$INV)
inv.ts <- ts(inv$INV, start = c(1960), frequency =1) # Transforming date 
gdpc$inv <- as.vector(inv.ts) # Adding to same dataset

### (1.3) ME : Military expenditure as a share of GDP


me <- read.csv("Data/ME.csv")

# Removing columns 1,2 & 4
me <- me[,-c(1,2,4)]
colnames(me) <- c("date", "ME")
me$ME <- as.numeric(me$ME)
me.ts <- ts(me$ME, start = c(1960), frequency =1) # Transforming date 
gdpc$me <- as.vector(me.ts) # Adding to same dataset



### (1.4) POP : Population (in 000s) - this is used as a proxy for labour force

pop <- read.csv("Data/POP.csv")

# Removing columns 1,2 & 4
pop <- pop[,-c(1,2,4)]
colnames(pop) <- c("date", "POP")
pop$POP <- as.numeric(pop$POP)

#P <- pop$POP[-1]
#p <- pop$POP - P 

pop.ts <- ts(pop$POP, start = c(1960), frequency =1) # Transforming date 
gdpc$pop <- as.vector(pop.ts) # Adding to same dataset

# removing the variables we dont need 
rm(inv, me, pop, inv.ts, me.ts, pop.ts)

### (1.5) Time Trend 

gdpc$t <- 1:nrow(gdpc)

### Removing the extra collumns 

gdpc <- gdpc[-66,]
gdpc <- gdpc[-65,]
gdpc <- gdpc[-64,]
gdpc <- gdpc[-63,]
gdpc <- gdpc[-62,]
gdpc <- gdpc[-61,]

################################################################################
#
# (2) Variable Transformations
#
################################################################################

gdpc_sample <- gdpc

# (2.1) Log of GDPC

gdpc_sample$lgdpc=log(gdpc$gdpc)

# (2.2) Log of INV

gdpc_sample$linv=log(gdpc_sample$inv)

#(2.3) Log of Military Spending

gdpc_sample$lme=log(gdpc_sample$me)

#(2.4) Log of population growth 

# 1. gen popn. grwth 

#gdpc_sample$p <- gdpc_sample$pop - dplyr::lag(gdpc_sample$pop, 1)
#gdpc_sample$lp=log(gdpc_sample$p+0.05)

gdpc_sample$pop_1 <- dplyr::lag(gdpc_sample$pop, 1)

gdpc_sample$popg <- (gdpc_sample$pop - gdpc_sample$pop_1)/gdpc_sample$pop_1

gdpc_sample$lp <- log(gdpc_sample$popg + 0.05)

################################################################################
#
# (3) Graph 
#
################################################################################

plot(x = gdpc_sample$date,
     y = gdpc_sample$me,
     main = "Figure 1: Defence Burden Across Time",
     ylab = "Military Spending (%GDP)",
     xlab = "",
     type = "l")

################################################################################
#
# (3) Diagnostics for Variables with time trend
#
################################################################################

# (3.1) logGDPC

time_trend_reg_lgdpc <- lm(lgdpc ~ t, data = gdpc_sample)
stargazer(time_trend_reg_lgdpc, type = "text")

# R^2 is very high - 98% of variation in lgdpc can be explained by time
# Time trend will be a problem

# detrended lgdpc

detrended_lgdpc <- time_trend_reg_lgdpc$residuals
plot(detrended_lgdpc, type = "l")

# This seems to have got rid of the time trend 
# However non-stationarity looks to be a problem
# Let's confirm formally


adf.test(gdpc_sample$lgdpc, k = 1)
adf.test(gdpc_sample$lgdpc, k = 2)
adf.test(gdpc_sample$lgdpc, k = 3)
adf.test(gdpc_sample$lgdpc, k = 4)
adf.test(gdpc_sample$lgdpc, k = 5)

# High p-values in all instences 
# k=1, p=0.4186, cannot reject H0: non-stationarity 

i
adf.test(detrended_lgdpc, k = 2)
adf.test(detrended_lgdpc, k = 3)
adf.test(detrended_lgdpc, k = 4)
adf.test(detrended_lgdpc, k = 5)

#p = 0.4186, cannot reject H0


# Null of unit root cannot be rejected for both lgdpc and detrended lgdpc
# Also note that detrending does not affect the result of the ADF
# We will deal with this in Step 4 

#===========================================================================

# (3.2) logINV

time_trend_reg_linv <- lm(linv ~ t, data = gdpc_sample)
stargazer(time_trend_reg_linv, type = "text")

# R^2 is very high - 82% of variation in linv can be explained by time
# Time trend will be a problem

# detrended 

detrended_linv <- time_trend_reg_linv$residuals
plot(detrended_linv, type = "l")

# This seems to have got rid of the time trend 
# However non-stationarity looks to be a problem
# Let's confirm formally

adf.test(gdpc_sample$linv, k = 1)
adf.test(gdpc_sample$linv, k = 2)
adf.test(gdpc_sample$linv, k = 3)
adf.test(gdpc_sample$linv, k = 4)
adf.test(gdpc_sample$linv, k = 5)

# High p-values in all instences 
# k=1, p=0.1753, cannot reject H0: non-stationarity 

adf.test(detrended_linv, k = 1)
adf.test(detrended_linv, k = 2)
adf.test(detrended_linv, k = 3)
adf.test(detrended_linv, k = 4)
adf.test(detrended_linv, k = 5)

# Null of unit root cannot be rejected for both linv and detrended linv
# Also note that detrending does not affect the result of the ADF
# We will deal with this in Step 4 

#==============================================================================

# (3.3) log ME

time_trend_reg_lme <- lm(lme ~ t, data = gdpc_sample)
stargazer(time_trend_reg_lme, type = "text")

# R^2 is quite high - 41% of variation in lme can be explained by time
# Time trend will be a problem

# detrended 

detrended_lme <- time_trend_reg_lme$residuals
plot(detrended_lme, type = "l")

# This seems to have got rid of the time trend 
# However non-stationarity looks to be a problem
# Let's confirm formally

adf.test(gdpc_sample$lme, k = 1)
adf.test(gdpc_sample$lme, k = 2)
adf.test(gdpc_sample$lme, k = 3)
adf.test(gdpc_sample$lme, k = 4)
adf.test(gdpc_sample$lme, k = 5)

# High p-values in all instences 
# k=1, p=0.5061, cannot reject H0: non-stationarity 

adf.test(detrended_lme, k = 1)
adf.test(detrended_lme, k = 2)
adf.test(detrended_lme, k = 3)
adf.test(detrended_lme, k = 4)
adf.test(detrended_lme, k = 5)

# Null of unit root cannot be rejected for both lme and detrended lme
# Also note that detrending does not affect the result of the ADF
# We will deal with this in Step 4 

#=============================================================================

# (3.3) log pop

time_trend_reg_lp <- lm(lp ~ t, data = gdpc_sample)
stargazer(time_trend_reg_lp, type = "text")

# R^2 is quite high - 73% of variation in lp can be explained by time
# Time trend will be a problem

# detrended 

detrended_lp <- time_trend_reg_lp$residuals
plot(detrended_lp, type = "l")

# This seems to have got rid of the time trend 
# However non-stationarity looks to be a problem
# Let's confirm formally

# Because pop grwth transformations results in an NA in the first period
# Create new sample then filter to remove

gdpc_samplep <- gdpc_sample
gdpc_samplep <- gdpc_samplep[-1,]

adf.test(gdpc_samplep$lp, k = 1)
adf.test(gdpc_samplep$lp, k = 2)
adf.test(gdpc_samplep$lp, k = 3)
adf.test(gdpc_samplep$lp, k = 4)
adf.test(gdpc_samplep$lp, k = 5)

# High p-values in all instences 
# k=1, p=0.1435, cannot reject H0: non-stationarity 

adf.test(detrended_lp, k = 1)
adf.test(detrended_lp, k = 2)
adf.test(detrended_lp, k = 3)
adf.test(detrended_lp, k = 4)
adf.test(detrended_lp, k = 5)

# Null of unit root cannot be rejected for both lp and detrended lp
# Also note that detrending does not affect the result of the ADF
# We will deal with this in Step 4 

################################################################################
#
# Step 4: Diagnostics for Non-Stationarity 
#
################################################################################

# IN order to deal with this will transform each of the variables

# (4.1) First difference

# first difference lgdpc
gdpc_sample$dlgdpc <- gdpc_sample$lgdpc - dplyr::lag(gdpc_sample$lgdpc, 1)

# first difference linv
gdpc_sample$dlinv <- gdpc_sample$linv - dplyr::lag(gdpc_sample$linv, 1)

# first difference lme
gdpc_sample$dlme <- gdpc_sample$lme - dplyr::lag(gdpc_sample$lme, 1)

# first difference lp
gdpc_sample$dlp <- gdpc_sample$lp - dplyr::lag(gdpc_sample$lp, 1)
gdpc_samplep$dlp <- gdpc_samplep$lp - dplyr::lag(gdpc_samplep$lp, 1)

gdpc_sample$dt <- gdpc_sample$t - dplyr::lag(gdpc_sample$t, 1)

# All of these transformations will result in an NA in the first period
# Filter to remove
gdpc_sample <- gdpc_sample[-1,]
gdpc_samplep <- gdpc_samplep[-1,]

################################################################################
#
# Step 5: ADF test for Non-Stationarity with first differenced variables
#
################################################################################

#(5.1) dlgdpc 

adf.test(gdpc_sample$dlgdpc, k = 1)
adf.test(gdpc_sample$dlgdpc, k = 2)
adf.test(gdpc_sample$dlgdpc, k = 3)
adf.test(gdpc_sample$dlgdpc, k = 4)
adf.test(gdpc_sample$dlgdpc, k = 5)

# p-value = 0.01 in all lags
# Can reject H0: non-stationarity at 1% level 

#(5.2) dlinv

adf.test(gdpc_sample$dlinv, k = 1)
adf.test(gdpc_sample$dlinv, k = 2)
adf.test(gdpc_sample$dlinv, k = 3)
adf.test(gdpc_sample$dlinv, k = 4)
adf.test(gdpc_sample$dlinv, k = 5)

# p-value = 0.01 in all lags
# Can reject H0: non-stationarity at 1% level 

#(5.3) dlme

adf.test(gdpc_sample$dlme, k = 1)
adf.test(gdpc_sample$dlme, k = 2)
adf.test(gdpc_sample$dlme, k = 3)
adf.test(gdpc_sample$dlme, k = 4)
adf.test(gdpc_sample$dlme, k = 5)

# p-value = 0.01 in lags 1-3
#p-value = 0.1882 for lag 5 
# Can reject H0: non-stationarity at 1% and 5% level respectively


#(5.4) dlp

adf.test(gdpc_samplep$dlp, k = 1)
adf.test(gdpc_samplep$dlp, k = 2)
adf.test(gdpc_samplep$dlp, k = 3)
adf.test(gdpc_samplep$dlp, k = 4)
adf.test(gdpc_samplep$dlp, k = 5)

# lag 1 & 2 p = 0.01 
# lag 2,3,4 can reject H0 at 10% level

################################################################################
#
# Step 5: ARDL Model 
#
################################################################################

library(ARDL)

# Now all variables have been transformed to I(0), we can implement our Solow Model
# (5.1) Log Model - ARDL(1,1,0,0)
# Autoregressive Distributed Lag Estimates

# Using AIC

model1 <- auto_ardl(lgdpc ~ linv + lme + lp + trend(lgdpc), data = gdpc_sample,
                    max_order = c(5,4,4,4))

model1



# Using BIC as selection criterion instead of AIC
model1_b <- auto_ardl(lgdpc ~ linv + lme + lp + trend(lgdpc), data = gdpc_sample,
                      max_order = c(5,4,4,4), selection = "BIC")
model1_b$top_orders



# ARDL(2,1,2,1) is selected based on lowest BIC


ardl_2121 <- ardl(lgdpc ~ linv + lme + lp + trend(lgdpc), data = gdpc_sample, order = c(2,1,2,1))
summary(ardl_2121)

# Bounds Test

fbounds <- bounds_f_test(ardl_2121, case = 4, alpha = 0.01)
tbounds <- bounds_t_test(ardl_2121, case = 5, alpha = 0.01)

fbounds$tab
tbounds$tab

fbounds <- bounds_f_test(ardl_2121, case = 4, alpha = 0.05)
tbounds <- bounds_t_test(ardl_2121, case = 5, alpha = 0.05)

fbounds$tab
tbounds$tab

# For model with constant and trend
# Including the constant term and the trend in the short-run relationship
# (unrestricted constant and unrestricted trend)
bounds_f_test(ardl_2121, case = 4)

# For the model with constant and trend
# Including the constant term in the short-run and the trend in the long-run relationship 
# (unrestricted constant and restricted trend)
bounds_f_test(ardl_2121, case = 5)

# DW Test for serial correlation

dwtest(ardl_2121)

#p-value = 0.0502 , reject H0 in favor of alternative hypothesis -> serial correlation 

# Testing for Heteroskedasticity 

library(lmtest)
bptest(ardl_2121)

# Cannot reject H0: Homoskedasticy. thus, heteroskedasticiy not a problem

# RESET Test

resettest(ardl_2121, power = 2:3, type = c("fitted", "regressor",
                                         "princomp"), data = list())

# p-value = 0.001369

# Granger Test for Causality 

data(gdpc_sample)
granger.test(gdpc_sample, p=6)
## Which came first: the chicken or the egg?
data(gdpc_sample)
grangertest(lgdpc ~ lme, order = 3, data = gdpc_sample)


# (5.2) LR Relationship 

lrcoeff <- multipliers(ardl_2121)
lrcoeff
summary(lrcoeff)

uecm <- uecm(ardl_2121)
summary(uecm)

# this is the ECM we use
recm <- recm(uecm, case = 4)
summary(recm)
dwtest(recm)

library(dLagM)

ardlBound()

install.packages("modelsummary")
ardlBound()
library(modelsummary)
modelsummary(recm, output = "default", estimate = "{estimate} ({std.error}){stars}", statistic = "p.value")









