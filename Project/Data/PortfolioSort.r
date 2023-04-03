library(tidyverse)
library(lubridate)
library(scales)
library(RSQLite)
library(dbplyr)
library(RPostgres)
library(frenchdata)
library(lmtest)
library(sandwich)
library(ggthemes)
library(rlang)
library(knitr)

rm(list = ls())

defaultW <- getOption("warn")
options(warn = -1)
# options(warn = defaultW)

# Specify number of portfolios at once
N_port <- 5

set.seed(123)

setwd("/Users/tobiasbrammer/Library/Mobile Documents/com~apple~CloudDocs/Documents/Aarhus Uni/8. semester/5362-Empirical-Asset-Pricing/Project/Data")

# Connect database
db_connection <- dbConnect(
  SQLite(),
  "data/db_connection.sqlite",
  extended_types = TRUE
)

# Load data from local database
data <- tbl(db_connection, "data") |>
  collect()

summary(data)


# Calculate Tobin's Q
data <- data |>
  mutate(
    Q = coalesce((at + mktcap - be)/at , 1),
    ExcessQ = Q - 1
  )

# Get lagged variables
data <- data |>
  group_by(permno) |>
  mutate(
    # Lagged variables
    at_lag = coalesce(lag(at),0),
    lt_lag = coalesce(lag(lt),0),
    capx_lag = coalesce(lag(capx),0),
    mktcap = coalesce(lag(mktcap),0),
    revt_lag = coalesce(lag(revt),0),
    fcf_lag = coalesce(lag(fcf),0),
    lev_lag = coalesce(lag(lev),0),
    profit_lag = coalesce(lag(profit),0),
  ) |>
  ungroup()
# Compute variables
data <- data |>
  mutate(
    ItC = coalesce(capx/(at - lt),0),
    ItC_lag = coalesce(capx_lag/(at_lag - lt_lag),0),
    RnD = coalesce(xrd/at_lag,0),
    revt_g = coalesce(log(na_if(revt,0) - na_if(revt_lag,0)),0)
  )

# Replace inf with NA
data[sapply(data, is.infinite)] <- 0

data <- data %>% mutate_if(is.numeric , replace_na, replace = 0)

summary(data)

# Only keep data from 1965 and onwards
data <- data |>
  filter(year >= 1965)

# Get number of observations per PERMNO
data <- data |>
  group_by(permno) |>
  mutate(
    n = n()
  ) |>
  ungroup()

# Plot distribution of number of observations per PERMNO
data |>
  ggplot(aes(x = n)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Number of observations per PERMNO",
    x = "Number of observations",
    y = "Frequency"
  ) +
  theme_economist() +
  scale_color_economist()

# Keep PERMNOs with at least 4 years of data
data <- data |>
  filter(n >= 4*4)


# Number of observations left
nrow(data)
# Unique PERMNOs
n_distinct(data$permno)

### Investment Distortion ###
# Multicollinearity is not a problem, so we can proceed with the regression
# The variables on the right hand side are:
# ItC_lag: Investment to capital ratio lagged one year
# RnD: R&D intensity (R&D / revenue)
# Q: Tobin's Q (market value of equity / book value of equity)
# fcf: Free cash flow (cash from operations - capital expenditures)
# lev: Leverage (total debt / total assets)
# profit: Profitability (net income / total assets)

data <- data |>
  group_by(permno) |>
  mutate(
    invdist = abs(resid(lm(ItC ~ RnD + Q + lev + profit + fcf + revt_g
                       ))))

##### Descriptive statistics #####
create_summary <- function(data, column_name) {
  data |>
    select(value = {{ column_name }}) |>
    summarize(
      mean = mean(value),
      sd = sd(value),
      min = min(value),
      median = quantile(value, 0.50),
      max = max(value),
      n = n()
    )
}

# Check distribution of returns
data |>
  ggplot(aes(x = ret_excess)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribution of excess returns",
    x = "Excess return",
    y = "Frequency"
  ) +
  theme_economist() +
  scale_color_economist() +
  scale_x_continuous(labels = percent)

# Check distribution of investment distortion
data |>
  ggplot(aes(x = invdist)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribution of Investment Distortion",
    x = "Investment Distortion",
    y = "Frequency"
  ) +
  scale_color_economist() +
  theme_economist() 

# Check distribution of R&D intensity
data |>
  ggplot(aes(x = RnD)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribution of R&D Intensity",
    x = "R&D Intensity",
    y = "Frequency"
  ) +
  scale_color_economist() +
  theme_economist() 


# Write to DB
dbWriteTable(db_connection,
             "data",
             value = data,
             overwrite = TRUE
)

# Optimize DB
dbExecute(db_connection, "VACUUM;")

# Drop where Industry is "Missing"
data <- data |>
  filter(industry != "Missing")


summaryRet <- data |>
  filter(month == max(month)) |>
  group_by(exchange) |>
  create_summary(ret_excess)
summaryRet

summaryInvDist <- data |>
  filter(month == max(month)) |>
  group_by(exchange) |>
  create_summary(invdist)
summaryInvDist

# Table in LaTeX
summaryRet <- kable(summaryRet, format = "latex", booktabs = TRUE)
# Save to file
writeLines(summaryRet, "tables/summaryRet.tex")
summaryInvDist <- kable(summaryInvDist, format = "latex", booktabs = TRUE)
# Save to file
writeLines(summaryInvDist, "tables/summaryInvDist.tex")

# Plot variables using mktcap weighted average
data |>
  group_by(year = year(datadate)) |>
  reframe(
    mktcap = sum(mktcap, na.rm = TRUE),
    invdist = sum(mktcap*invdist, na.rm = TRUE)/mktcap,
    ItC = sum(mktcap*ItC, na.rm = TRUE)/mktcap,
    RnD = sum(mktcap*RnD, na.rm = TRUE)/mktcap,
    lev = sum(mktcap*lev, na.rm = TRUE)/mktcap,
    profit = sum(mktcap*profit, na.rm = TRUE)/mktcap,
    revt_g = sum(mktcap*revt_g, na.rm = TRUE)/mktcap
  ) |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = invdist, color = "Investment Distortion")) +
  geom_line(aes(y = ItC, color = "Investment-to-Capital")) +
  geom_line(aes(y = lev, color = "Leverage")) +
  geom_line(aes(y = profit, color = "Profitability")) +
  geom_line(aes(y = RnD, color = "R&D-Intensity")) +
  #geom_line(aes(y = revt_g, color = "Revenue growth")) +
  labs(
    x = NULL, y = NULL,
    title = "Market Cap Weighted Average of Variables"
  ) +
  scale_y_continuous() +
  # Legend fit in a 10x10 box
  theme(legend.position = "bottom", legend.box = "horizontal", legend.box.just = "bottom", legend.box.margin = unit(0.5, "lines")) +
  theme_economist() +
  scale_color_economist() +
  theme(legend.title=element_blank())
  ggsave("gfx/mktcap_avg_var.pdf", width = 20, height = 10)

### Visualize some aspects of the data ###
# id <- 14593
id <- sample(data$permno, 1)

# Plot variables
data |>
  filter(permno == id) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = invdist, color = "Investment Distortion")) +
  geom_line(aes(y = ItC, color = "Investment-to-Capital")) +
  geom_line(aes(y = lev, color = "Leverage")) +
  geom_line(aes(y = profit, color = "Profitability")) +
  geom_line(aes(y = RnD, color = "R&D-Intensity")) +
  labs(
    x = NULL, y = NULL,
    title = paste("Variables of PERMNO: ", as.character(id))
  ) +
  ##scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_economist() +
  scale_color_economist() +
  theme(legend.title=element_blank()) +
  scale_y_continuous()
  ggsave(paste("gfx/permno_", as.character(id), ".pdf", sep = ""), width = 20, height = 10)

# Plot investment distortion by exchange
data |>
  group_by(permno, year = year(datadate)) |>
  filter(date == max(date)) |>
  ungroup() |>
  group_by(exchange, year) |>
  reframe(
    invdist = mean(invdist)
  ) |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = invdist, color = exchange)) +
  theme_economist() +
  scale_color_economist() +
  labs(
    x = NULL, y = NULL, color = NULL, linetype = NULL,
    title = "Investment Distortion by Exchange"
  ) +
  scale_y_continuous() +
  coord_cartesian() 
  ggsave("gfx/invdist_exchange.pdf", width = 20, height = 10)

# Investment distortions by industry
data |>
  group_by(permno, year = year(datadate)) |>
  filter(date == max(date)) |>
  ungroup() |>
  group_by(industry, year) |>
  reframe(
    share = sum(invdist),
    .groups = "drop"
  ) |>
  ggplot(aes(
    x = year, 
    y = share, 
    color = industry,
    linetype = industry
  )) +
  geom_line() +
  theme_economist() +
  labs(
    x = NULL, y = NULL, color = NULL, linetype = NULL,
    title = "Investment Distortion by Industry"
  ) +
  scale_y_continuous() +
  coord_cartesian()
  ggsave("gfx/invdist_industry.pdf", width = 20, height = 10)


########## Assign portfolio ##########

# Function to assign portfolios
assign_portfolio <- function(data, var, n_portfolios) {
  breakpoints <- data |>
    reframe(
      breakpoint = quantile(
        {{ var }},
        probs = seq(0, 1, length.out = n_portfolios + 1),
        na.rm = TRUE)
    ) |>
    pull(breakpoint) |>
    as.numeric()

  assigned_portfolios <- data |>
    mutate(portfolio = findInterval({{ var }},
                                    breakpoints,
                                    all.inside = TRUE
    )) |>
    pull(portfolio)
  
  return(assigned_portfolios)
}

# Load Fama-French factors from DB
factors_ff_monthly <- tbl(db_connection, "factors_ff_monthly") |>
  collect()

##### All exchanges on Investment Distortion #####
### N portfolios ###

invdist_portfolios <- data |>
  group_by(month) |>
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = invdist,
      n_portfolios = N_port
    ),
    portfolio = as.factor(portfolio)
  ) |>
  group_by(portfolio, month) |>
  reframe(
    ret = weighted.mean(ret_excess, mktcap),
    cumret = weighted.mean(ret_cum, mktcap)
  )

# Drop NA values
invdist_portfolios <- invdist_portfolios |>
  filter(!is.na(ret) & !is.na(cumret))


invdist_summary <- invdist_portfolios |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  reframe(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[2]),
    cma_coef = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[6]),
    ret = mean(ret),
    cumret = mean(cumret)
  )

# Sort portfolio_summary by portfolio
invdist_summary <- invdist_summary |>
  mutate(portfolio = as.numeric(portfolio)) |>
  arrange(portfolio) |>
  mutate(portfolio = as.factor(portfolio))

# Plot CAPM alphas
invdist_summary |>
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CAPM Alphas of Distortion-sorted Portfolios",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")
  ggsave("gfx/longshort_invdist_alpha.pdf", width = 10, height = 8)

# Plot CAPM betas
invdist_summary |>
  ggplot(aes(x = portfolio, y = beta, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CAPM Betas of Distortion-sorted Portfolios",
    x = "Portfolio",
    y = "CAPM beta",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
  ggsave("gfx/longshort_invdist_beta.pdf", width = 10, height = 8)

# Plot CMA coefficients
invdist_summary |>
  ggplot(aes(x = portfolio, y = cma_coef, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CMA Coefficients of Distortion-sorted Portfolios",
    x = "Portfolio",
    y = "CMA coefficient",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
  ggsave("gfx/longshort_invdist_cma.pdf", width = 12, height = 8)
  
### Long-Short ###
longshort_invdist <- invdist_portfolios |>
  ungroup() |>
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) |>
  filter(portfolio %in% c("low", "high")) |>
  left_join(factors_ff_monthly, by = "month")

# Construct series with low - high portfolio returns
longshort_invdist <- longshort_invdist |>
  group_by(month) |>
  reframe(
    long_short = sum(ret * (portfolio == "low")) - sum(ret * (portfolio == "high")),
    mkt_excess = mean(mkt_excess)
  )

# Test for significance
coeftest(lm(long_short ~ 1, data = longshort_invdist),
         vcov = NeweyWest
)
test_InvDist <- summary(lm(long_short ~ 1, data = longshort_invdist))
# test_InvDist in a LaTeX table with coefficients and p-values
test_InvDist <- kable(test_InvDist$coefficients, digits = 3, format = "latex")
# Save table as .tex file
writeLines(test_InvDist, "tables/test_InvDist.tex")


# Plot cumulative sum of long-short returns vs. cumulative sum of market returns
longshort_invdist %>%
  mutate(
    long_short_sum = cumsum(long_short),
    mkt_sum = cumsum(mkt_excess)
  ) %>%
  ggplot(aes(x = ymd(month))) +
  geom_line(aes(y = long_short_sum, color = "Long-Short")) +
  geom_line(aes(y = mkt_sum, color = "Market")) +
  theme_economist() +
  scale_color_economist() +
  labs(
    title = "Long-Short vs. Market (Investment Distortion)",
    x = "",
    y = "Cumulative excess returns",
    color = "Strategy"
  ) +
  theme(legend.title=element_blank()) +
    theme(plot.subtitle = element_text(
    hjust = 0,
    vjust = -1,
    face = "italic"
  )) +
  labs(
    subtitle = "Long-Short: Low - High"
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = percent)
  ggsave("gfx/longshort_invdist_cumret.pdf", width = 20, height = 10)


########## Investigating Sector Effects ##########

### Distribution of industries in Low - High portfolio

# Share of industries in Low Portfolio
data_invdist_portfolios <- data |>
  group_by(month) |>
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = invdist,
      n_portfolios = N_port
    ),
    portfolio = as.factor(portfolio)
  )

# Industry distribution in Low Portfolio
data_invdist_portfolios |>
  filter(portfolio == min(as.numeric(portfolio))) |>
  group_by(industry) |>
  summarize(
    n = n(),
    .groups = "drop"
  ) |>
  mutate(
    share = n / sum(n)
  ) |>
  ggplot(aes(x = reorder(industry, share), y = share)) +
  geom_col() +
  theme_economist() +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    x = NULL, y = NULL,
    title = "Industry distribution in Low Portfolio"
  )
  ggsave("gfx/industry_distribution_low.pdf", width = 20, height = 10)

# Industry distribution in High Portfolio
data_invdist_portfolios |>
  filter(portfolio == max(as.numeric(portfolio))) |>
  group_by(industry) |>
  summarize(
    n = n(),
    .groups = "drop"
  ) |>
  mutate(
    share = n / sum(n)
  ) |>
  ggplot(aes(x = reorder(industry, share), y = share)) +
  geom_col() +
  theme_economist() +
  scale_color_economist() +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    x = NULL, y = NULL,
    title = "Industry distribution in High Portfolio"
  )
  ggsave("gfx/industry_distribution_high.pdf", width = 20, height = 10)

# Plot industry distribution in Low and High Portfolio in one plot with separate bars for each portfolio
data_invdist_portfolios |>
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) |>
  filter(portfolio %in% c("low", "high")) |>
  group_by(portfolio, industry) |>
  summarize(
    n = n(),
    .groups = "drop"
  ) |>
  mutate(
    share = n / sum(n)
  ) |>
  ggplot(aes(x = reorder(industry, share), y = share, fill = portfolio)) +
  geom_col() +
  theme_economist() +
  scale_color_economist() +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    x = NULL, y = NULL,
    title = "Industry distribution in Low and High Portfolio"
  )
  ggsave("gfx/industry_distribution_low_high.pdf", width = 20, height = 10)



########## Excluding Manufacturing ##########

data_ex_mfg <- data |>
  filter(industry != "Manufacturing")

invdist_portfolios_ex_mfg <- data_ex_mfg |>
  group_by(month) |>
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = invdist,
      n_portfolios = N_port
    ),
    portfolio = as.factor(portfolio)
  ) |>
  group_by(portfolio, month) |>
  reframe(
    ret = weighted.mean(ret_excess, mktcap),
    cumret = weighted.mean(ret_cum, mktcap)
  )

# Drop NA values
invdist_portfolios_ex_mfg <- invdist_portfolios_ex_mfg |>
  filter(!is.na(ret) & !is.na(cumret))


invdist_summary_ex_mfg <- invdist_portfolios_ex_mfg |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  reframe(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[2]),
    cma_coef = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[6]),
    ret = mean(ret),
    cumret = mean(cumret)
  )

# Sort portfolio_summary by portfolio
invdist_summary_ex_mfg <- invdist_summary_ex_mfg |>
  mutate(portfolio = as.numeric(portfolio)) |>
  arrange(portfolio) |>
  mutate(portfolio = as.factor(portfolio))

# Plot CAPM alphas
invdist_summary_ex_mfg |>
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CAPM Alphas of Distortion-sorted Portfolios (Ex. Manufacturing)",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")
  ggsave("gfx/longshort_invdist_alpha_ex_mfg.pdf", width = 12, height = 5)

# Plot CAPM betas
invdist_summary_ex_mfg |>
  ggplot(aes(x = portfolio, y = beta, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CAPM Betas of Distortion-sorted Portfolios (Ex. Manufacturing)",
    x = "Portfolio",
    y = "CAPM beta",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
  ggsave("gfx/longshort_invdist_beta_ex_mfg.pdf", width = 12, height = 5)

# Plot CMA coefficients
invdist_summary_ex_mfg |>
  ggplot(aes(x = portfolio, y = cma_coef, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CMA Coefficients of Distortion-sorted Portfolios (Ex. Manufacturing)",
    x = "Portfolio",
    y = "CMA coefficient",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
  ggsave("gfx/longshort_invdist_cma_ex_mfg.pdf", width = 12, height = 5)

### Long-Short ###
longshort_invdist_ex_mfg <- invdist_portfolios_ex_mfg |>
  ungroup() |>
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) |>
  filter(portfolio %in% c("low", "high")) |>
  left_join(factors_ff_monthly, by = "month")

# Construct series with low - high portfolio returns
longshort_invdist_ex_mfg <- longshort_invdist_ex_mfg |>
  group_by(month) |>
  reframe(
    long_short = sum(ret * (portfolio == "low")) - sum(ret * (portfolio == "high")),
    mkt_excess = mean(mkt_excess)
  )

# Test for significance
coeftest(lm(long_short ~ 1, data = longshort_invdist_ex_mfg),
         vcov = NeweyWest
)
test_InvDist_ex_mfg <- summary(lm(long_short ~ 1, data = longshort_invdist_ex_mfg))
# test_InvDist in a LaTeX table with coefficients and p-values
test_InvDist_ex_mfg <- kable(test_InvDist_ex_mfg$coefficients, digits = 3, format = "latex")
# Save table as .tex file
writeLines(test_InvDist_ex_mfg, "tables/test_InvDist_ex_mfg.tex")



# Plot cumulative sum of long-short returns vs. cumulative sum of market returns
longshort_invdist_ex_mfg %>%
  mutate(
    long_short_sum = cumsum(long_short),
    mkt_sum = cumsum(mkt_excess)
  ) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = long_short_sum, color = "Long-Short")) +
  geom_line(aes(y = mkt_sum, color = "Market")) +
  theme_economist() +
  scale_color_economist() +
  labs(
    title = "Long-Short vs. Market (Investment Distortion Ex. Manufacturing)",
    x = "",
    y = "Cumulative excess returns",
    color = "Strategy"
  ) +
  theme(legend.title=element_blank()) +
    theme(plot.subtitle = element_text(
    hjust = 0,
    vjust = -1,
    face = "italic"
  )) +
  labs(
    subtitle = "Long-Short: Low - High"
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = percent)
  ggsave("gfx/longshort_invdist_cumret_ex_mfg.pdf", width = 20, height = 10)

########## Manufacturing ##########

data_mfg <- data |>
  filter(industry == "Manufacturing")

invdist_portfolios_mfg <- data_mfg |>
  group_by(month) |>
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = invdist,
      n_portfolios = N_port
    ),
    portfolio = as.factor(portfolio)
  ) |>
  group_by(portfolio, month) |>
  reframe(
    ret = weighted.mean(ret_excess, mktcap),
    cumret = weighted.mean(ret_cum, mktcap)
  )

invdist_portfolios_mfg <- invdist_portfolios_mfg |>
  filter(!is.na(ret) & !is.na(cumret))

invdist_summary_mfg <- invdist_portfolios_mfg |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  reframe(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[2]),
    cma_coef = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[6]),
    ret = mean(ret),
    cumret = mean(cumret)
  )

# Sort portfolio_summary by portfolio
invdist_summary_mfg <- invdist_summary_mfg |>
  mutate(portfolio = as.numeric(portfolio)) |>
  arrange(portfolio) |>
  mutate(portfolio = as.factor(portfolio))

# Plot CAPM alphas
invdist_summary_mfg |>
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CAPM Alphas of Distortion-sorted Portfolios (Manufacturing)",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")
  ggsave("gfx/longshort_invdist_alpha_mfg.pdf", width = 12, height = 5)

# Plot CAPM betas
invdist_summary_mfg |>
  ggplot(aes(x = portfolio, y = beta, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CAPM Betas of Distortion-sorted Portfolios (Manufacturing)",
    x = "Portfolio",
    y = "CAPM beta",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
  ggsave("gfx/longshort_invdist_beta_mfg.pdf", width = 12, height = 5)

# Plot CMA coefficients
invdist_summary_mfg |>
  ggplot(aes(x = portfolio, y = cma_coef, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CMA Coefficients of Distortion-sorted Portfolios (Manufacturing)",
    x = "Portfolio",
    y = "CMA coefficient",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")
  ggsave("gfx/longshort_invdist_cma_mfg.pdf", width = 12, height = 5)

### Long-Short ###
longshort_invdist_mfg <- invdist_portfolios_mfg |>
  ungroup() |>
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) |>
  filter(portfolio %in% c("low", "high")) |>
  left_join(factors_ff_monthly, by = "month")

# Construct series with low - high portfolio returns
longshort_invdist_mfg <- longshort_invdist_mfg |>
  group_by(month) |>
  reframe(
    long_short = sum(ret * (portfolio == "low")) - sum(ret * (portfolio == "high")),
    mkt_excess = mean(mkt_excess)
  )

# Test for significance
coeftest(lm(long_short ~ 1, data = longshort_invdist_mfg),
         vcov = NeweyWest
)
test_InvDist_mfg <- summary(lm(long_short ~ 1, data = longshort_invdist_mfg))
# test_InvDist in a LaTeX table with coefficients and p-values
test_InvDist_mfg <- kable(test_InvDist_mfg$coefficients, digits = 3, format = "latex")
# Save table as .tex file
writeLines(test_InvDist_mfg, "tables/test_InvDist_mfg.tex")


# Plot cumulative sum of long-short returns vs. cumulative sum of market returns
longshort_invdist_mfg %>%
  mutate(
    long_short_sum = cumsum(long_short),
    mkt_sum = cumsum(mkt_excess)
  ) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = long_short_sum, color = "Long-Short")) +
  geom_line(aes(y = mkt_sum, color = "Market")) +
  theme_economist() +
  scale_color_economist() +
  labs(
    title = "Long-Short vs. Market (Investment Distortion Manufacturing)",
    x = "",
    y = "Cumulative excess returns",
    color = "Strategy"
  ) +
  theme(legend.title=element_blank()) +
    theme(plot.subtitle = element_text(
    hjust = 0,
    vjust = -1,
    face = "italic"
  )) +
  labs(
    subtitle = "Long-Short: Low - High"
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = percent)
  ggsave("gfx/longshort_invdist_cumret_mfg.pdf", width = 20, height = 10)





##### NYSE #####
  data_nyse <- data |>
    filter(exchange == "NYSE")

invdist_portfolios_nyse <- data_nyse |>
  group_by(month) |>
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = invdist,
      n_portfolios = N_port
    ),
    portfolio = as.factor(portfolio)
  ) |>
  group_by(portfolio, month) |>
  reframe(
    ret = weighted.mean(ret_excess, mktcap),
    cumret = weighted.mean(ret_cum, mktcap)
  )

invdist_portfolios_nyse <- invdist_portfolios_nyse |>
  filter(!is.na(ret) & !is.na(cumret))

invdist_summary_nyse <- invdist_portfolios_nyse |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  reframe(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[2]),
    cma_coef = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[6]),
    ret = mean(ret),
    cumret = mean(cumret)
  )

# Sort portfolio_summary by portfolio
invdist_summary_nyse <- invdist_summary_nyse |>
  mutate(portfolio = as.numeric(portfolio)) |>
  arrange(portfolio) |>
  mutate(portfolio = as.factor(portfolio))

# Plot CAPM alphas
invdist_summary_nyse |>
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CAPM Alphas of Distortion-sorted Portfolios (NYSE)",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")
  ggsave("gfx/longshort_invdist_alpha_nyse.pdf", width = 12, height = 5)

# Plot CAPM betas
invdist_summary_nyse |>
  ggplot(aes(x = portfolio, y = beta, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CAPM Betas of Distortion-sorted Portfolios (NYSE)",
    x = "Portfolio",
    y = "CAPM beta",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
  ggsave("gfx/longshort_invdist_beta_nyse.pdf", width = 12, height = 5)

# Plot CMA coefficients
invdist_summary_nyse |>
  ggplot(aes(x = portfolio, y = cma_coef, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CMA Coefficients of Distortion-sorted Portfolios (NYSE)",
    x = "Portfolio",
    y = "CMA coefficient",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")
  ggsave("gfx/longshort_invdist_cma_nyse.pdf", width = 12, height = 5)

### Long-Short ###
longshort_invdist_nyse <- invdist_portfolios_nyse |>
  ungroup() |>
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) |>
  filter(portfolio %in% c("low", "high")) |>
  left_join(factors_ff_monthly, by = "month")

# Construct series with low - high portfolio returns
longshort_invdist_nyse <- longshort_invdist_nyse |>
  group_by(month) |>
  reframe(
    long_short = sum(ret * (portfolio == "low")) - sum(ret * (portfolio == "high")),
    mkt_excess = mean(mkt_excess)
  )

# Test for significance
coeftest(lm(long_short ~ 1, data = longshort_invdist_nyse),
         vcov = NeweyWest
)
test_InvDist_nyse <- summary(lm(long_short ~ 1, data = longshort_invdist_nyse))
# test_InvDist in a LaTeX table with coefficients and p-values
test_InvDist_nyse <- kable(test_InvDist_nyse$coefficients, digits = 3, format = "latex")
# Save table as .tex file
writeLines(test_InvDist_nyse, "tables/test_InvDist_nyse.tex")


# Plot cumulative sum of long-short returns vs. cumulative sum of market returns
longshort_invdist_nyse %>%
  mutate(
    long_short_sum = cumsum(long_short),
    mkt_sum = cumsum(mkt_excess)
  ) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = long_short_sum, color = "Long-Short")) +
  geom_line(aes(y = mkt_sum, color = "Market")) +
  theme_economist() +
  scale_color_economist() +
  labs(
    title = "Long-Short vs. Market (Investment Distortion NYSE)",
    x = "",
    y = "Cumulative excess returns",
    color = "Strategy"
  ) +
  theme(legend.title=element_blank()) +
    theme(plot.subtitle = element_text(
    hjust = 0,
    vjust = -1,
    face = "italic"
  )) +
  labs(
    subtitle = "Long-Short: Low - High"
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = percent)
  ggsave("gfx/longshort_invdist_cumret_nyse.pdf", width = 20, height = 10)


########## NASDAQ ##########


data_nasdaq <- data |>
  filter(exchange == "NASDAQ")

invdist_portfolios_nasdaq <- data_nasdaq |>
  group_by(month) |>
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = invdist,
      n_portfolios = N_port
    ),
    portfolio = as.factor(portfolio)
  ) |>
  group_by(portfolio, month) |>
  reframe(
    ret = weighted.mean(ret_excess, mktcap),
    cumret = weighted.mean(ret_cum, mktcap)
  )

invdist_portfolios_nasdaq <- invdist_portfolios_nasdaq |>
  filter(!is.na(ret) & !is.na(cumret))

invdist_summary_nasdaq <- invdist_portfolios_nasdaq |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  reframe(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[2]),
    cma_coef = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[6]),
    ret = mean(ret),
    cumret = mean(cumret)
  )

# Sort portfolio_summary by portfolio
invdist_summary_nasdaq <- invdist_summary_nasdaq |>
  mutate(portfolio = as.numeric(portfolio)) |>
  arrange(portfolio) |>
  mutate(portfolio = as.factor(portfolio))

# Plot CAPM alphas
invdist_summary_nasdaq |>
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CAPM Alphas of Distortion-sorted Portfolios (NASDAQ)",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")
  ggsave("gfx/longshort_invdist_alpha_nasdaq.pdf", width = 12, height = 5)

# Plot CAPM betas
invdist_summary_nasdaq |>
  ggplot(aes(x = portfolio, y = beta, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CAPM Betas of Distortion-sorted Portfolios (NASDAQ)",
    x = "Portfolio",
    y = "CAPM beta",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
  ggsave("gfx/longshort_invdist_beta_nasdaq.pdf", width = 12, height = 5)

# Plot CMA coefficients
invdist_summary_nasdaq |>
  ggplot(aes(x = portfolio, y = cma_coef, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    title = "CMA Coefficients of Distortion-sorted Portfolios (NASDAQ)",
    x = "Portfolio",
    y = "CMA coefficient",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")
  ggsave("gfx/longshort_invdist_cma_nasdaq.pdf", width = 12, height = 5)


### Long-Short ###
longshort_invdist_nasdaq <- invdist_portfolios_nasdaq |>
  ungroup() |>
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) |>
  filter(portfolio %in% c("low", "high")) |>
  left_join(factors_ff_monthly, by = "month")

# Construct series with low - high portfolio returns
longshort_invdist_nasdaq <- longshort_invdist_nasdaq |>
  group_by(month) |>
  reframe(
    long_short = sum(ret * (portfolio == "low")) - sum(ret * (portfolio == "high")),
    mkt_excess = mean(mkt_excess)
  )

# Test for significance
coeftest(lm(long_short ~ 1, data = longshort_invdist_nasdaq),
         vcov = NeweyWest
)
test_InvDist_nasdaq <- summary(lm(long_short ~ 1, data = longshort_invdist_nasdaq))
# test_InvDist in a LaTeX table with coefficients and p-values
test_InvDist_nasdaq <- kable(test_InvDist_nasdaq$coefficients, digits = 3, format = "latex")
# Save table as .tex file
writeLines(test_InvDist_nasdaq, "tables/test_InvDist_nasdaq.tex")


# Plot cumulative sum of long-short returns vs. cumulative sum of market returns
longshort_invdist_nasdaq %>%
  mutate(
    long_short_sum = cumsum(long_short),
    mkt_sum = cumsum(mkt_excess)
  ) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = long_short_sum, color = "Long-Short")) +
  geom_line(aes(y = mkt_sum, color = "Market")) +
  theme_economist() +
  scale_color_economist() +
  labs(
    title = "Long-Short vs. Market (Investment Distortion NASDAQ)",
    x = "",
    y = "Cumulative excess returns",
    color = "Strategy"
  ) +
  theme(legend.title=element_blank()) +
    theme(plot.subtitle = element_text(
    hjust = 0,
    vjust = -1,
    face = "italic"
  )) +
  labs(
    subtitle = "Long-Short: Low - High"
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = percent)
  ggsave("gfx/longshort_invdist_cumret_nasdaq.pdf", width = 20, height = 10)

