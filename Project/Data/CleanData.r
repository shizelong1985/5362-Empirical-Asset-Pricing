### Clear ws and load libraries
rm(list=ls())
library(zoo)
library(haven)
library(tidyverse)
library(rstatix)
library(dplyr)
library(car)
library(SIBER)


# Load data from .dta file
dfFunda <- as.data.frame(read_dta('Fundamentals.dta'))
dfRet <- as.data.frame(read_dta('Returns.dta'))

# Convert to tibble
dfFunda <- as_tibble(dfFunda)
dfRet <- as_tibble(dfRet)

# Convert all caps to lower case
names(dfFunda) <- tolower(names(dfFunda))
names(dfRet) <- tolower(names(dfRet))

# Convert date to date format
dfFunda$date <- as.Date(dfFunda$datadate, format = "%Y%m%d")
dfRet$date <- as.Date(dfRet$datadate, format = "%Y%m%d")

# Number of unique cusips
length(unique(dfFunda$cusip))
length(unique(dfRet$cusip))

# Join dfRet on dfFunda by cusip and date
df <- left_join(dfFunda,dfRet, by = c("cusip", "date"))

# list with variables to keep
lVars <- c("cusip", "date", "atq", "ceqq", "cshoq.x", "icaptq", "saleq", "wcapq", "xrdq", "capxy", "prccm")
df <- df[,lVars]
# Rename variables
names(df) <- c("cusip", "date", "Assets", "Equity", "Shares_Outstanding", "Invested_Capital", "Sales", "Working_Capital", "RnD", "Capital_Expenditure", "Share_Price")

# Calculate Sales Growth for each cusip
df <- df %>% group_by(cusip) %>% mutate(Sales_Growth = (log(Sales) - log(lag(Sales))))

# Calculate Returns for each cusip
df <- df %>% group_by(cusip) %>% mutate(Returns = (log(Share_Price) - log(lag(Share_Price))))

# Calculate cumulative sum of Returns for each cusip
df <- df %>% group_by(cusip) %>% mutate(Returns_Cumulative = cumsum(Returns))

# Tobin's Q
df$Tobins_Q <- (df$Assets + df$Shares_Outstanding*df$Share_Price - df$Equity) / df$Assets

# Market Cap
df$Market_Cap <- df$Share_Price * df$Shares_Outstanding

# Scale CapEx, Working Capital and R&D by Assets
df$RnD <- df$RnD/df$Assets
df$Working_Capital <- df$Working_Capital/df$Assets
df$Capital_Expenditure <- df$Capital_Expenditure/df$Assets

# Remove NA
df <- df %>% filter_if(~is.numeric(.), all_vars(!is.na(.)))
# Remove NaN
df <- df %>% filter_if(~is.numeric(.), all_vars(!is.nan(.)))
# Remove inf
df <- df %>% filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

# Remove cusips with sum(RnD) = 0
df <- df %>% group_by(cusip) %>% filter(sum(RnD) != 0) %>% ungroup()

# Remove cusips with less than 20 observations (5 years)
df <- df %>% group_by(cusip) %>% filter(n() >20) %>% ungroup()
# Number of unique cusips after removing cusips with less than 20 observations
length(unique(df$cusip))

### DESCRIPTIVE STATISTICS ###
summary(df)

# Pick random cusips to use for testing
set.seed(69)
sCusip <- sample(unique(df$cusip), 1)

# Dataframe for testing 
dfTest <- df[df$cusip == sCusip,]

# Plot Assets, RnD, Working Capital, Capital Expenditure, Tobin's Q, Market Cap and Share Price over time in a subplot
par(mfrow=c(3,2))
plot(dfTest$date, dfTest$Assets, type = "l", xlab = "Date", ylab = "Assets", main = "Assets")
plot(dfTest$date, dfTest$RnD, type = "l", xlab = "Date", ylab = "R&D", main = "R&D")
plot(dfTest$date, dfTest$Working_Capital, type = "l", xlab = "Date", ylab = "Working Capital", main = "Working Capital")
plot(dfTest$date, dfTest$Capital_Expenditure, type = "l", xlab = "Date", ylab = "Capital Expenditure", main = "Capital Expenditure")
plot(dfTest$date, dfTest$Tobins_Q, type = "l", xlab = "Date", ylab = "Tobin's Q", main = "Tobin's Q")
plot(dfTest$date, dfTest$Returns, type = "l", xlab = "Date", ylab = "Return", main = "Return")
par(mfrow=c(1,1))

# Assign each cusip a number from 1 to 5 based on quintile of CapEx over time from lowest to highest
df <- df %>% group_by(cusip) %>% mutate(Capital_Expenditure_Quintile = cut(Capital_Expenditure, breaks = 5, labels = 1:5))

# Plot CapEx quintile median Capital_Expenditure over time
df %>%
    group_by(date, Capital_Expenditure_Quintile) %>%
    summarise(Capital_Expenditure = median(Capital_Expenditure)) %>%
    ggplot(aes(x = date, y = Capital_Expenditure, color = Capital_Expenditure_Quintile)) +
    geom_line() +
    labs(x = "Date", y = "Median CapEx", title = "CapEx by Quintile")

# For each cusip in df, run a time series regression of each cusip's investment-to-capital ratio on
# its Tobin’s Q, lagged investment-to-capital ratio, sales growth, and R&D intensity.
# The residual from this regression is defined as the investment distortion of the firm.

# library(rio)
# library(plm)

df <- df %>% group_by(c(cusip,date)) %>% mutate(Investment_Distortion = lm(RnD ~ Tobins_Q + lag(Capital_Expenditure) + lag(Sales_Growth) + lag(RnD))$residuals)


# Assign each cusip a number from 1 to 5 based on quintile of Investment_Distortion over time from lowest to highest
df <- df %>% group_by(cusip) %>% mutate(Investment_Distortion_Quintile = cut(Investment_Distortion, breaks = 5, labels = 1:5))


df %>%
    group_by(date, Investment_Distortion_Quintile) %>%
    summarise(Investment_Distortion = median(Investment_Distortion)) %>%
    ggplot(aes(x = date, y = Investment_Distortion, color = Investment_Distortion_Quintile)) +
    geom_line() +
    labs(x = "Date", y = "Investment Distortion", title = "Investment Distortion by Quintile")


# Create a heatmap of median investment distortion by quintile of Tobin's Q and quintile of investment distortion
# Color the heatmap by the cumulative sum of returns
df %>%
    group_by(Capital_Expenditure_Quintile, Investment_Distortion_Quintile) %>%
    summarise(Returns_Cumulative = median(Returns_Cumulative)) %>%
    ggplot(aes(x = Capital_Expenditure_Quintile, y = Investment_Distortion_Quintile, fill = Returns_Cumulative)) +
    geom_tile() +
    geom_text(aes(label = Returns_Cumulative), color = "white") +
    labs(x = "CapEx Quintile", y = "Investment Distortion Quintile", title = "Cumulative Returns by CapEx and Investment Distortion Quintile")




###### Portfolio Construction ######

# Create a dataframe with the sum of returns for each cusip in each quintile of investment distortion
# and quintile of CapEx
dfReturns <- df %>%
    group_by(Capital_Expenditure_Quintile, Investment_Distortion_Quintile, date) %>%
    summarise(Returns = cumsum(Returns))

# Calculate the cumulative sum of returns for each quintile of investment distortion and quintile of CapEx
dfReturns <- dfReturns %>%
    group_by(Capital_Expenditure_Quintile, Investment_Distortion_Quintile) %>%
    mutate(Returns_Cumulative = cumsum(Returns))




