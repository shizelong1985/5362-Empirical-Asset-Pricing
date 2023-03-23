# install.packages(c("tidyverse", "lubridate", "scales", "RSQLite", "dbplyr", "RPostgres", "frenchdata","sandwich"))

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

rm(list = ls())

setwd("/Users/tobiasbrammer/Library/Mobile Documents/com~apple~CloudDocs/Documents/Aarhus Uni/8. semester/5362-Empirical-Asset-Pricing/Project/Data")

# Connect database
db_connection <- dbConnect(
  SQLite(),
  "data/db_connection.sqlite",
  extended_types = TRUE
)

# Load data from DB
data <- tbl(db_connection, "data") |>
  collect()

### Drop where exchange is Other
data <- data |>
  filter(exchange != "Other")

# Calculate Tobin's Q
data <- data |>
  mutate(
    Q = (coalesce(at) + coalesce(mktcap) - coalesce(be))/coalesce(at)
  )

# Scale variables accordingly
data <- data |>
  mutate(
    ItC = capx/(at - lt),
    ItC_lag = capx_lag/(at_lag - lt_lag),
    RnD = xrd/revt
  )

# Drop Inf and NA
data[sapply(data, is.infinite)] <- NA

data <- data |>
  drop_na(ret_excess, ItC, ItC_lag, RnD, Q, sale_g, sale_g_lag)


# Investment Distortion
data <- data |>
  group_by(permno) |>
  mutate(
    invdist = abs(resid(lm(ItC ~ ItC_lag + RnD + sale_g + sale_g_lag + Q, 
                       na.action=na.exclude))))

# Plot investment distortion by exchange
data |>
  group_by(permno, year = year(datadate)) |>
  filter(date == max(date)) |>
  ungroup() |>
  group_by(exchange, year) |>
  summarize(
    invdist = sum(invdist),
    .groups = "drop"
  ) |>
  ggplot(aes(
    x = year, 
    y = invdist, 
    color = exchange
  )) +
  geom_line() +
  theme_economist() +
  labs(
    x = NULL, y = NULL, color = NULL, linetype = NULL,
    title = "Investment Distortion by Exchange"
  ) +
  scale_y_continuous() +
  coord_cartesian()

# Investment distortions by industry
data |>
  group_by(permno, year = year(datadate)) |>
  filter(date == max(date)) |>
  ungroup() |>
  group_by(industry, year) |>
  summarize(
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

# Drop where industry is Manufacturing or Missing
data <- data |>
  filter(industry != "Manufacturing")

data <- data |>
  filter(industry != "Missing")

# Check distribution of returns
data |>
  ggplot(aes(x = ret_excess)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribution of excess returns",
    x = "Excess return",
    y = "Frequency"
  ) +
  theme_economist() +
  scale_x_continuous(labels = percent)

# Drop observations with excess returns above 300%
data <- data |>
  filter(ret_excess < 3)

########## Assign portfolio ##########

# Function to assign portfolios
assign_portfolio <- function(data, var, n_portfolios) {
  breakpoints <- data |>
    summarize(breakpoint = quantile({{ var }},
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE
    )) |>
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

# Specify number of portfolios at once
N_port <- 3


##### All exchanges #####
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
  summarize(
    ret = weighted.mean(ret_excess, mktcap_lag),
    cumret = weighted.mean(ret_cum, mktcap_lag),
    .groups = "drop"
  )

portfolios_summary <- invdist_portfolios |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  summarize(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[2]),
    ret = mean(ret),
    cumret = mean(cumret)
  )

# Sort portfolio_summary by portfolio
portfolios_summary <- portfolios_summary |>
  mutate(portfolio = as.numeric(portfolio)) |>
  arrange(portfolio) |>
  mutate(portfolio = as.factor(portfolio))


# Plot CAPM alphas
portfolios_summary |>
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  labs(
    title = "CAPM Alphas of Distortion-sorted Portfolios",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")

### Long-Short ###
longshort <- invdist_portfolios |>
  ungroup() |>
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) |>
  filter(portfolio %in% c("low", "high")) |>
  pivot_wider(month, names_from = portfolio, values_from = ret) |>
  mutate(long_short = coalesce(low,NA) - coalesce(high,NA)) |>
  left_join(factors_ff_monthly, by = "month")

# Test for significance
coeftest(lm(long_short ~ 1, data = longshort),
         vcov = NeweyWest
)

# Plot cumulative sum of long-short returns vs. cumulative sum of market returns
longshort %>%
  mutate(
    long_short_sum = cumsum(long_short),
    mkt_sum = cumsum(mkt_excess)
  ) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = long_short_sum, color = "Long-Short")) +
  geom_line(aes(y = mkt_sum, color = "Market")) +
  theme_economist() +
  labs(
    title = "Long-Short vs. Market",
    x = "Month",
    y = "Cumulative excess returns",
    color = "Strategy"
  ) +
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = percent)



##### NYSE #####
data_NYSE <- data |>
  filter(exchange == "NYSE")

### Ten portfolios ###
invdist_portfolios_NYSE <- data_NYSE |>
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
  summarize(
    ret = weighted.mean(ret_excess, mktcap_lag),
    .groups = "drop"
  )

portfolios_summary_NYSE <- invdist_portfolios_NYSE |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  summarize(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[2]),
    ret = mean(ret)
  )

# Sort portfolio_summary by portfolio
portfolios_summary_NYSE <- portfolios_summary_NYSE |>
  mutate(portfolio = as.numeric(portfolio)) |>
  arrange(portfolio) |>
  mutate(portfolio = as.factor(portfolio))

portfolios_summary_NYSE |>
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  labs(
    title = "CAPM Alphas of Distortion-sorted Portfolios (NYSE)",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")


### Long-Short ###
longshort_NYSE <- invdist_portfolios_NYSE |>
  ungroup() |>
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) |>
  filter(portfolio %in% c("low", "high")) |>
  pivot_wider(month, names_from = portfolio, values_from = ret) |>
  mutate(long_short = coalesce(low,NA) - coalesce(high,NA)) |>
  left_join(factors_ff_monthly, by = "month")


# Test for significance
coeftest(lm(long_short ~ 1, data = longshort_NYSE),
         vcov = NeweyWest
)


# Plot cumulative sum of long-short returns vs. cumulative sum of market returns
longshort_NYSE %>%
  mutate(
    long_short_sum = cumsum(long_short),
    mkt_sum = cumsum(mkt_excess)
  ) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = long_short_sum, color = "Long-Short")) +
  geom_line(aes(y = mkt_sum, color = "Market")) +
  theme_economist() +
  labs(
    title = "Long-Short vs. Market (NYSE)",
    x = "Month",
    y = "Cumulative excess returns",
    color = "Strategy"
  ) +
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = percent)



##### NASDAQ #####
data_NASDAQ <- data |>
  filter(exchange == "NASDAQ")
### Ten portfolios ###
invdist_portfolios_NASDAQ <- data_NASDAQ |>
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
  summarize(
    ret = weighted.mean(ret_excess, mktcap_lag),
    .groups = "drop"
  )

portfolios_summary_NASDAQ <- invdist_portfolios_NASDAQ |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  summarize(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[2]),
    ret = mean(ret)
  )

# Sort portfolio_summary by portfolio
portfolios_summary_NASDAQ <- portfolios_summary_NASDAQ |>
  mutate(portfolio = as.numeric(portfolio)) |>
  arrange(portfolio) |>
  mutate(portfolio = as.factor(portfolio))

portfolios_summary_NASDAQ |>
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  labs(
    title = "CAPM Alphas of Distortion-sorted Portfolios (NASDAQ)",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")


### Long-Short ###
longshort_NASDAQ <- invdist_portfolios_NASDAQ |>
  ungroup() |>
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) |>
  filter(portfolio %in% c("low", "high")) |>
  pivot_wider(month, names_from = portfolio, values_from = ret) |>
  mutate(long_short = coalesce(low,NA) - coalesce(high,NA)) |>
  left_join(factors_ff_monthly, by = "month")


# Test for significance
coeftest(lm(long_short ~ 1, data = longshort_NASDAQ),
         vcov = NeweyWest
)


longshort_NASDAQ %>%
  mutate(
    long_short_sum = cumsum(long_short),
    mkt_sum = cumsum(mkt_excess)
  ) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = long_short_sum, color = "Long-Short")) +
  geom_line(aes(y = mkt_sum, color = "Market")) +
  theme_economist() +
  labs(
    title = "Long-Short vs. Market (NASDAQ)",
    x = "Month",
    y = "Cumulative excess returns",
    color = "Strategy"
  ) +
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = percent)


##### AMEX #####
data_AMEX <- data |>
  filter(exchange == "AMEX")
### Ten portfolios ###
invdist_portfolios_AMEX <- data_AMEX |>
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
  summarize(
    ret = weighted.mean(ret_excess, mktcap_lag),
    .groups = "drop"
  )

portfolios_summary_AMEX <- invdist_portfolios_AMEX |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  summarize(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[2]),
    ret = mean(ret)
  )

# Sort portfolio_summary by portfolio
portfolios_summary_AMEX <- portfolios_summary_AMEX |>
  mutate(portfolio = as.numeric(portfolio)) |>
  arrange(portfolio) |>
  mutate(portfolio = as.factor(portfolio))

portfolios_summary_AMEX |>
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  labs(
    title = "CAPM Alphas of Distortion-sorted Portfolios (AMEX)",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")


### Long-Short ###
longshort_AMEX <- invdist_portfolios_AMEX |>
  ungroup() |>
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) |>
  filter(portfolio %in% c("low", "high")) |>
  pivot_wider(month, names_from = portfolio, values_from = ret) |>
  mutate(long_short = coalesce(low,NA) - coalesce(high,NA)) |>
  left_join(factors_ff_monthly, by = "month")

# Test for significance
coeftest(lm(long_short ~ 1, data = longshort_AMEX),
         vcov = NeweyWest
)

longshort_AMEX %>%
  mutate(
    long_short_sum = cumsum(long_short),
    mkt_sum = cumsum(mkt_excess)
  ) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = long_short_sum, color = "Long-Short")) +
  geom_line(aes(y = mkt_sum, color = "Market")) +
  theme_economist() +
  labs(
    title = "Long-Short vs. Market (AMEX)",
    x = "Month",
    y = "Cumulative excess returns",
    color = "Strategy"
  ) +
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = percent)

