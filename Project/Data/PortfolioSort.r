library(mice)
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
library(monotonicity)
library(latex2exp)

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

data <- data |>
  mutate(
    year = year(month)
  )

# # Transform data to quarterly frequency and select all variables
# data <- data |>
#   group_by(permno, year) |>
#   summarise(
#     # Variables
#     at = sum(at),
#     lt = sum(lt),
#     be = sum(be),
#     capx = sum(capx),
#     mktcap = sum(mktcap),
#     revt = sum(revt),
#     fcf = sum(fcf),
#     lev = sum(lev),
#     ni = sum(ni),
#     profit = sum(profit),
#     xrd = sum(xrd),
#     ret_excess = sum(ret_excess),
#     ret_cum = sum(ret_cum),
#     datadate = first(datadate),
#     date = first(date),
#     industry = first(industry),
#     exchange = first(exchange),
#     n = n()
#   )



# Calculate Tobin's Q
data <- data |>
  mutate(
    Q = coalesce((at + mktcap - be)/at , 1),
    ExcessQ = Q - 1
  )

# data$month as the first day of the month in datadate (e.g. 2010-01-01)
data <- data |>
  mutate(
    month = ymd(str_c(year(datadate), "-", month(datadate), "-01"))
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
  
data <- data |>
  filter(year < 2020)

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
  geom_histogram(bins = 25) +
  labs(
    title = "Number of observations per PERMNO",
    x = "Number of observations",
    y = "Frequency"
  ) +
  # Breaks every 10
  scale_x_continuous(breaks = seq(0, 1000, by = 5)) +
  theme_economist() +
  scale_color_economist()

# Keep PERMNOs with at least 4 years of data
data <- data |>
  filter(n >= 4)

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
    invdist = abs(resid(lm(ItC ~ ItC_lag + RnD + Q + lev + profit + fcf + revt_g
                       ))))

##### Descriptive statistics #####
create_summary <- function(data, column_name) {
  data |>
    select(value = {{ column_name }}) |>
    summarize(
      mean = round(mean(value),4),
      sd = round(sd(value),4),
      min = round(min(value),4),
      median = round(quantile(value, 0.50),4),
      max = round(max(value),4),
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
  geom_histogram(bins = 50) +
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

# Drop where Industry is "Missing"
data <- data |>
  filter(industry != "Missing")

summaryRet <- data |>
  #filter(quarter == max(quarter)) |>
  group_by(exchange) |>
  create_summary(ret_excess)
summaryRet

# Only keep NYSE data
data <- data |>
  filter(exchange == "NYSE")

summaryInvDist <- data |>
  #filter(quarter == max(quarter)) |>
  group_by(exchange) |>
  create_summary(invdist)
summaryInvDist

# Table in LaTeX
summaryRet <- kable(summaryRet, format = "latex", booktabs = TRUE, digits = 3)
# Save to file
writeLines(summaryRet, "tables/summaryRet.tex")
summaryInvDist <- kable(summaryInvDist, format = "latex", booktabs = TRUE, digits = 3)
# Save to file
writeLines(summaryInvDist, "tables/summaryInvDist.tex")

weighted.mean(data$invdist,data$mktcap, na.rm = TRUE)

# Plot variables using mktcap weighted average
data |>
  #filter(quarter == max(quarter)) |>
  group_by(date = year) |>
  mutate(
    date = date,
    invdist = weighted.mean(invdist,mktcap, na.rm = TRUE),
    ItC = weighted.mean(ItC,mktcap, na.rm = TRUE),
    RnD = weighted.mean(RnD,mktcap, na.rm = TRUE),
    lev = weighted.mean(lev,mktcap, na.rm = TRUE),
    profit = weighted.mean(profit,mktcap, na.rm = TRUE),
    revt_g = weighted.mean(revt_g,mktcap, na.rm = TRUE)
  ) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = invdist, color = "Investment Distortion")) +
  geom_line(aes(y = ItC, color = "Investment-to-Capital")) +
  geom_line(aes(y = lev, color = "Leverage")) +
  geom_line(aes(y = profit, color = "Profitability")) +
  geom_line(aes(y = RnD, color = "R&D-Intensity")) +
  #geom_line(aes(y = revt_g, color = "Revenue growth")) +
  labs(
    x = NULL, y = NULL
  ) +
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(1965, 2022, by = 5)) +
  #scale_x_date(date_breaks = "5 years", date_labels = "%Y" ) +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.box.just = "bottom", legend.box.margin = unit(0.5, "lines")) +
  theme_economist() +
  scale_color_economist() +
  theme(legend.title=element_blank())
ggsave("gfx/mktcap_avg_var.pdf", width = 10, height = 5)

### Visualize some aspects of the data ###
id <- 12490
#id <- sample(data$permno, 1)

# Plot variables
data |>
  filter(permno == id) |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = invdist, color = "Investment Distortion")) +
  geom_line(aes(y = ItC, color = "Investment-to-Capital")) +
  geom_line(aes(y = lev, color = "Leverage")) +
  geom_line(aes(y = profit, color = "Profitability")) +
  geom_line(aes(y = RnD, color = "R&D-Intensity")) +
  labs(
    x = NULL, y = NULL
  ) +
  scale_x_continuous(breaks = seq(1965, 2022, by = 5)) +
  theme_economist() +
  scale_color_economist() +
  theme(legend.title=element_blank()) +
  # break date-axis every 5 years
  #scale_x_date(date_breaks = "5 years", date_labels = "%Y" ) +
  scale_y_continuous()
ggsave(paste("gfx/permno_", as.character(id), ".pdf", sep = ""), width = 10, height = 5)

# Investment distortions by industry
data |>
  group_by(permno, year = year) |>
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
    x = NULL, y = NULL, color = NULL, linetype = NULL
  ) +
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(1965, 2022, by = 5)) +
  coord_cartesian()
ggsave("gfx/invdist_industry.pdf", width = 10, height = 5)


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
    ret = weighted.mean(ret_excess, mktcap, na.rm = TRUE),
    cumret = weighted.mean(ret_cum, mktcap, na.rm = TRUE)
  )

# Drop NA values
invdist_portfolios <- invdist_portfolios |>
  filter(!is.na(ret) & !is.na(cumret))

invdist_summary <- invdist_portfolios |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  reframe(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[1]),
    alpha_pval = as.numeric(summary(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma))$coefficients[1, 4]),
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
    x = "Portfolio",
    y = TeX("\\alpha"),
    fill = "Portfolio"
  ) +
  geom_text(
    aes(
      label = ifelse(alpha_pval < 0.01, "***", ifelse(alpha_pval < 0.05, "**", ifelse(alpha_pval < 0.1, "*", "")))
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 10
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")
ggsave("gfx/longshort_invdist_alpha.pdf", width = 10, height = 5)

# Plot CAPM betas
invdist_summary |>
  ggplot(aes(x = portfolio, y = beta, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    x = "Portfolio",
    y = TeX("\\beta$_{mkt}$"),
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
ggsave("gfx/longshort_invdist_beta.pdf", width = 10, height = 5)

# Plot CMA coefficients
invdist_summary |>
  ggplot(aes(x = portfolio, y = cma_coef, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    x = "Portfolio",
    y = TeX("\\beta$_{cma}$"),
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
ggsave("gfx/longshort_invdist_cma.pdf", width = 10, height = 5)
  
### Long-Short ###
longshort_invdist <- invdist_portfolios |>
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
ggsave("gfx/longshort_invdist_cumret.pdf", width = 10, height = 5)


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
  ggplot(aes(x = reorder(industry, share), y = share, fill = as.factor(1))) +
  geom_col() +
  theme_economist() +
  scale_fill_economist() +
  theme(legend.position = "None") +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    x = NULL, y = NULL
  )
ggsave("gfx/industry_distribution_low.pdf", width = 10, height = 5)

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
  ggplot(aes(x = reorder(industry, share), y = share, fill = as.factor(1))) +
  geom_col() +
  theme_economist() +
  scale_fill_economist() +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    x = NULL, y = NULL
  )
ggsave("gfx/industry_distribution_high.pdf", width = 10, height = 5)



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
  scale_fill_economist() +
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    x = NULL, y = NULL
  )
ggsave("gfx/industry_distribution_low_high.pdf", width = 10, height = 5)

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
    ret = weighted.mean(ret_excess, mktcap, na.rm = TRUE),
    cumret = weighted.mean(ret_cum, mktcap, na.rm = TRUE)
  )

# Drop NA values
invdist_portfolios_ex_mfg <- invdist_portfolios_ex_mfg |>
  filter(!is.na(ret) & !is.na(cumret))


invdist_summary_ex_mfg <- invdist_portfolios_ex_mfg |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  reframe(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[1]),
    alpha_pval = as.numeric(summary(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma))$coefficients[1, 4]),
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
    x = "Portfolio",
    y = TeX("\\alpha"),
    fill = "Portfolio"
  ) +
  geom_text(
    aes(
      label = ifelse(alpha_pval < 0.01, "***", ifelse(alpha_pval < 0.05, "**", ifelse(alpha_pval < 0.1, "*", "")))
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 10
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")
ggsave("gfx/longshort_invdist_alpha_ex_mfg.pdf", width = 10, height = 5)

# Plot CAPM betas
invdist_summary_ex_mfg |>
  ggplot(aes(x = portfolio, y = beta, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    x = "Portfolio",
    y = TeX("\\beta$_{mkt}$"),
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
ggsave("gfx/longshort_invdist_beta_ex_mfg.pdf", width = 10, height = 5)

# Plot CMA coefficients
invdist_summary_ex_mfg |>
  ggplot(aes(x = portfolio, y = cma_coef, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    x = "Portfolio",
    y = TeX("\\beta$_{cma}$"),
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
ggsave("gfx/longshort_invdist_cma_ex_mfg.pdf", width = 10, height = 5)

### Long-Short ###
longshort_invdist_ex_mfg <- invdist_portfolios_ex_mfg |>
  ungroup() |>
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == median(as.numeric(portfolio)) ~ "medium",
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
ggsave("gfx/longshort_invdist_cumret_ex_mfg.pdf", width = 10, height = 5)




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
    ret = weighted.mean(ret_excess, mktcap, na.rm = TRUE),
    cumret = weighted.mean(ret_cum, mktcap, na.rm = TRUE)
  )

invdist_portfolios_mfg <- invdist_portfolios_mfg |>
  filter(!is.na(ret) & !is.na(cumret))

invdist_summary_mfg <- invdist_portfolios_mfg |>
  left_join(factors_ff_monthly, by = "month") |>
  group_by(portfolio) |>
  reframe(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[1]),
    alpha_pval = summary(lm(ret ~ 1 + mkt_excess + smb + hml + rmw + cma))$coefficients[1, 4],
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
    x = "Portfolio",
    y = TeX("\\alpha"),
    fill = "Portfolio"
  ) +
  geom_text(
    aes(
      label = ifelse(alpha_pval < 0.01, "***", ifelse(alpha_pval < 0.05, "**", ifelse(alpha_pval < 0.1, "*", "")))
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 10
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")
ggsave("gfx/longshort_invdist_alpha_mfg.pdf", width = 10, height = 5)

# Plot CAPM betas
invdist_summary_mfg |>
  ggplot(aes(x = portfolio, y = beta, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    x = "Portfolio",
    y = TeX("\\beta$_{mkt}$"),
    fill = "Portfolio"
  ) +
  theme(legend.position = "None") 
ggsave("gfx/longshort_invdist_beta_mfg.pdf", width = 10, height = 5)

# Plot CMA coefficients
invdist_summary_mfg |>
  ggplot(aes(x = portfolio, y = cma_coef, fill = portfolio)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_fill_economist() +
  labs(
    x = "Portfolio",
    y = TeX("\\beta$_{cma}$"),
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")
ggsave("gfx/longshort_invdist_cma_mfg.pdf", width = 10, height = 5)

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
ggsave("gfx/longshort_invdist_cumret_mfg.pdf", width = 10, height = 5)


#################### Bivariate Sorting ####################
# Sort stocks by investment distortion (low, medium, high) and size (small, medium, large).

# This method uses the entire sample to construct the portfolios.
# The portfolios are then rebalanced every month.
data <- data_ex_mfg 

data$year <- as.numeric(format(data$month, "%Y"))
# Create a data frame with the investment distortion and size of each stock's market cap
  data <- data |>
  group_by(year) |>
  mutate(size = case_when(
    mktcap < quantile(mktcap, 0.33) ~ "small",
    mktcap >= quantile(mktcap, 0.33) & mktcap < quantile(mktcap, 0.66) ~ "medium",
    mktcap >= quantile(mktcap, 0.66) ~ "large"
  ))

# For each size category, sort stocks by investment distortion and construct portfolios
# with the lowest, middle, and highest investment distortion
data <- data |>
  group_by(year, size) |>
  mutate(
    invdist_rank = rank(invdist),
    invdist_port = case_when(
        invdist < quantile(invdist, 0.33) ~ "low",
        invdist >= quantile(invdist, 0.33) & invdist < quantile(invdist, 0.66) ~ "medium",
        invdist >= quantile(invdist, 0.66) ~ "high"
    )
  )

# Average investment distortion by size and portfolio and count number of stocks
conditional <- data |>
  group_by(size, invdist_port, month) |>
  mutate(
    n = n(),
    mean_invdist = weighted.mean(invdist, mktcap),
    mean_mktcap = mean(mktcap),
    mean_ret = weighted.mean(ret_excess, mktcap, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(size, invdist_port) |>
  summarise(
    mean_invdist = weighted.mean(mean_invdist, mktcap),
    mean_mktcap = mean(mean_mktcap),
    mean_ret = weighted.mean(mean_ret, mktcap),
    n = mean(n)
  )

# Make geom_tile plot with investment distortion and size and label mean returns and number of stocks
conditional |>
  group_by(size, invdist_port) |>
  ggplot(aes(x = size, y = invdist_port, fill = as.factor((1:9)))) +
  geom_tile() +
  geom_text(aes(label = scales::percent(round((mean_ret), 3))), size = 8, color = "black") +
  geom_text(aes(label = paste0('(',round((n),0),')')), color = "black", vjust = 3, size = 5) +
  theme_economist() +
  #scale_colour_steps() +
  scale_fill_brewer(palette = "BrBG") +
  labs(
    x = "Size",
    y = "Investment Distortion",
    fill = "Average excess return"
  ) +
  theme(legend.position = "None")
ggsave("gfx/conditional_invdist_size.pdf", width = 10, height = 5)


ret <- data |>
  group_by(size, invdist_port, month) |>
  mutate(
    n = n(),
    mean_invdist = weighted.mean(invdist, mktcap, na.rm = TRUE),
    mean_mktcap = mean(mktcap, na.rm = TRUE),
    mean_ret = weighted.mean(ret_excess, mktcap, na.rm = TRUE)
  ) |>
  select(size, invdist_port, month, mean_ret)

# Join the average returns with the factors
ret <- ret |>
  left_join(factors_ff_monthly, by = "month")

ret$month <- ymd(ret$month)



ret_summary <- ret |>
  group_by(size,invdist_port) |>
  reframe(
    ex_ret = mean(mean_ret - mkt_excess, na.rm = TRUE),
    alpha = as.numeric(lm(mean_ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[1]),
    alpha_pval = as.numeric(summary(lm(mean_ret ~ 1 + mkt_excess + smb + hml + rmw + cma))$coefficients[1, 4]),
    beta = as.numeric(lm(mean_ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[2]),
    cma_coef = as.numeric(lm(mean_ret ~ 1 + mkt_excess + smb + hml + rmw + cma)$coefficients[6]),
    ret = mean(mean_ret, na.rm = TRUE)
  )

# Make geom_tile plot with investment distortion and size and label alpha and alpha p-values
ret_summary |>
  group_by(size, invdist_port) |>
  ggplot(aes(x = size, y = invdist_port, fill = as.factor((1:9)))) +
  geom_tile() +
  geom_text(aes(label = scales::percent(round((alpha), 3))), size = 8, color = "black") +
  geom_text(aes(label = ifelse(alpha_pval < 0.01, "***", ifelse(alpha_pval < 0.05, "**", ifelse(alpha_pval < 0.1, "*", "")))), vjust = 3, size = 5, color = "black") +
  theme_economist() +
  scale_fill_brewer(palette = "BrBG") +
  labs(
    x = "Size",
    y = "Investment Distortion",
    fill = TeX("\\alpha")
  ) +
  theme(legend.position = "None")
ggsave("gfx/conditional_invdist_size_significance.pdf", width = 10, height = 5)



# Get the returns of each portfolio
ret_test <- ret |>
  group_by(size,invdist_port) |>
  summarise(
    est = round(t.test(mean_ret - mkt_excess, mu = 0)$estimate,4),
    p = round(t.test(mean_ret - mkt_excess, mu = 0)$p.value,4)
  )

# For each portfolio and month get the average return from ret
ret_mat <- ret |>
  group_by(size, invdist_port, month) |>
  summarise(
    ret = mean(mean_ret, na.rm = FALSE)
  )
# Pivot wide to get a matrix of returns
ret_mat <- ret_mat |>
  pivot_wider(names_from = c(size,invdist_port), values_from = ret)
# Order by date
ret_mat <- ret_mat[order(ret_mat$month),]


# Convert to numeric
ret_mat <- as.data.frame(lapply(ret_mat, as.numeric))

# Remove month column
ret_mat <- ret_mat[,-1]

# Use mice to impute missing values
ret_mat <- complete(mice(ret_mat, m = 5, maxit = 10, method = "pmm", seed = 1234))

# Get the average return of the market
ret_mat <- as.matrix(ret_mat)

# Test for differences in mean returns between each portfolio and the market
# monoSummary implements the test for monotonicity in asset returns, based on portfolio sorts in Patton & Timmermann (2010).
mono_test <- monoSummary(ret_mat,bootstrapRep = 1000, wolakRep = 1000,
increasing = FALSE, difference = FALSE, plot = FALSE, block_length = 6, zero_treshold = 1e-6)
# Save the results to a table with:
# TopMinusBottom: Mean difference return between top and bottom portfolio.
# UP_pval: studentized p-value from Patton and Timmermanns (JoE, 2010) "Up and Down" 
#          test for assumed increasing monotonicity pattern and using absolute difference returns.
#DOWN_pval: studentized p-value from Patton and Timmermanns (JoE, 2010) "Up and Down" 
#          test for assumed decreasing monotonicity pattern and using absolute difference returns.
#MRall_pval: the numeric rank of the fitted linear model.
#Wolak_pval: p-value "TestOnePvalueWolak" for H0:d1>=0,d2>=0,...,dK>=0 vs. H1:(d1, d2, ..., dK) in R^K.
#Bonferroni_pval: H0:d1>=0,d2>=0,...,dK>=0 vs. H1: dj < 0 for some j=1,2,..,K.

mono_table <- data.frame(
  mean_difference = round(mono_test$TopMinusBottom,3),
  Up_pval = round(mono_test$UP_pval,3),
  Down_pval = round(mono_test$DOWN_pval,3),
  Rank = round(mono_test$MRall_pval,3),
  Wolak_pval = round(mono_test$Wolak_pval,3),
  Bonferroni_pval = round(mono_test$Bonferroni_pval,3)
)

# Format as LaTeX table
mono_table <- kable(mono_table, format = "latex", booktabs = TRUE, digits = 3)
mono_table
# Save to file for inclusion in LaTeX document
writeLines(mono_table, "tables/mono_table.tex")

# monoRelation implements the 'monotonic relationship' tests from Patton & Timmermann (2010).
# ∆_i = E[r(i,t)-r(i-1,t)] and test H0: ∆ <= 0 vs. H1: min(i=1..N)∆_i > 0
mono_rel <- monoRelation(ret_mat, block_length = 6)
mono_rel <- kable(mono_rel, format = "latex", booktabs = TRUE, digits = 3)
mono_rel
# Save to file for inclusion in LaTeX document
writeLines(mono_rel, "tables/mono_rel.tex")


ret_cond <- data |>
  group_by(size, invdist_port, month) |>
  mutate(
    n = n(),
    mean_invdist = weighted.mean(invdist, mktcap),
    mean_mktcap = mean(mktcap),
    mean_ret = (weighted.mean(ret_excess, mktcap, na.omit = TRUE))
  ) |>
  ungroup()

# Calculate the cumulative returns of each portfolio
ret_cond <- ret_cond |>
  group_by(size, invdist_port) |>
  mutate(
    cum_ret = cumsum(mean_ret)
  )

# Plot the cumulative returns of each portfolio
ret_cond |>
  ggplot(aes(x = year, y = cum_ret, color = as.factor(invdist_port))) +
  geom_line() +
  theme_economist() +
  labs(
    x = "year",
    y = "Cumulative excess return",
    color = "Investment distortion"
  ) +
  theme(legend.position = "bottom")
