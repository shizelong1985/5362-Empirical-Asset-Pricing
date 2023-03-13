# install.packages(c("tidyverse", "lubridate", "scales", "RSQLite", "dbplyr", "RPostgres", "frenchdata","sandwich"))

library(tidyverse)
library(lubridate)
library(scales)
library(RSQLite)
library(dbplyr)
library(RPostgres)
library(frenchdata)
library(sandwich)

rm(list = ls())

setwd("/Users/tobiasbrammer/Library/Mobile Documents/com~apple~CloudDocs/Documents/Aarhus Uni/8. semester/5362-Empirical-Asset-Pricing/Project/Data")

start_date <- ymd("1970-01-01")
end_date <- ymd("2019-12-31")

########## Returns Data and Factors ##########
### FF3 ###
factors_ff_monthly_raw <- download_french_data("Fama/French 3 Factors")
factors_ff_monthly <- factors_ff_monthly_raw$subsets$data[[1]] |>
  transmute(
    month = floor_date(ymd(str_c(date, "01")), "month"),
    rf = as.numeric(RF) / 100,
    mkt_excess = as.numeric(`Mkt-RF`) / 100,
    smb = as.numeric(SMB) / 100,
    hml = as.numeric(HML) / 100
  ) |>
  filter(month >= start_date & month <= end_date)

# Create database
db_connection <- dbConnect(
  SQLite(),
  "data/db_connection.sqlite",
  extended_types = TRUE
)

# Write table
dbWriteTable(db_connection,
             "factors_ff_monthly",
             value = factors_ff_monthly,
             overwrite = TRUE
)

factors_ff_monthly_db <- tbl(db_connection, "factors_ff_monthly")

### CRSP Database ###
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = "tbrammer",
  password = "naqgUf-bantas-1ruwby"
)

# the CRSP monthly security file,
msf_db <- tbl(wrds, in_schema("crsp", "msf"))

# the identifying information,
msenames_db <- tbl(wrds, in_schema("crsp", "msenames"))

# and the delisting information.
msedelist_db <- tbl(wrds, in_schema("crsp", "msedelist"))

# (i) we keep only data in the time windows of interest, 
# (ii) we keep only US-listed stocks as identified via share codes shrcd 10 and 11, and
# (iii) we keep only months within permno-specific start dates namedt and end dates nameendt.
# In addition, we add delisting codes and returns.
crsp_monthly <- msf_db |>
  filter(date >= start_date & date <= end_date) |>
  inner_join(
    msenames_db |>
      filter(shrcd %in% c(10, 11)) |>
      select(permno, exchcd, siccd, namedt, nameendt),
    by = c("permno")
  ) |>
  filter(date >= namedt & date <= nameendt) |>
  mutate(month = floor_date(date, "month")) |>
  left_join(
    msedelist_db |>
      select(permno, dlstdt, dlret, dlstcd) |>
      mutate(month = floor_date(dlstdt, "month")),
    by = c("permno", "month")
  ) |>
  select(
    permno, # Security identifier
    date, # Date of the observation
    month, # Month of the observation
    ret, # Return
    shrout, # Shares outstanding (in thousands)
    altprc, # Last traded price in a month
    exchcd, # Exchange code
    siccd, # Industry code
    dlret, # Delisting return
    dlstcd # Delisting code
  ) |>
  collect() |>
  mutate(
    month = ymd(month),
    shrout = shrout * 1000
  )

# Transforming listing exchange codes to explicit exchange names.
crsp_monthly <- crsp_monthly |>
  mutate(exchange = case_when(
    exchcd %in% c(1, 31) ~ "NYSE",
    exchcd %in% c(2, 32) ~ "AMEX",
    exchcd %in% c(3, 33) ~ "NASDAQ",
    TRUE ~ "Other"
  ))

# Industries
crsp_monthly <- crsp_monthly |>
  mutate(industry = case_when(
    siccd >= 1 & siccd <= 999 ~ "Agriculture",
    siccd >= 1000 & siccd <= 1499 ~ "Mining",
    siccd >= 1500 & siccd <= 1799 ~ "Construction",
    siccd >= 2000 & siccd <= 3999 ~ "Manufacturing",
    siccd >= 4000 & siccd <= 4899 ~ "Transportation",
    siccd >= 4900 & siccd <= 4999 ~ "Utilities",
    siccd >= 5000 & siccd <= 5199 ~ "Wholesale",
    siccd >= 5200 & siccd <= 5999 ~ "Retail",
    siccd >= 6000 & siccd <= 6799 ~ "Finance",
    siccd >= 7000 & siccd <= 8999 ~ "Services",
    siccd >= 9000 & siccd <= 9999 ~ "Public",
    TRUE ~ "Missing"
  ))

# Market cap
crsp_monthly <- crsp_monthly |>
  mutate(
    mktcap = abs(shrout * altprc) / 1000000,
    mktcap = na_if(mktcap, 0)
  )


# One-month lagged market cap.
mktcap_lag <- crsp_monthly |>
  mutate(month = month %m+% months(1)) |>
  select(permno, month, mktcap_lag = mktcap)

crsp_monthly <- crsp_monthly |>
  left_join(mktcap_lag, by = c("permno", "month"))

# Reflect the returns of investors who bought a stock in the month before
# delisting and held it until the delisting date. 
crsp_monthly <- crsp_monthly |>
  mutate(ret_adj = case_when(
    is.na(dlstcd) ~ ret,
    !is.na(dlstcd) & !is.na(dlret) ~ dlret,
    dlstcd %in% c(500, 520, 580, 584) |
      (dlstcd >= 551 & dlstcd <= 574) ~ -0.30,
    dlstcd == 100 ~ ret,
    TRUE ~ -1
  )) |>
  select(-c(dlret, dlstcd))

# Join with FF

factors_ff_monthly <- tbl(db_connection, "factors_ff_monthly") |>
  collect()

crsp_monthly <- crsp_monthly |>
  left_join(factors_ff_monthly |> select(month, rf),
            by = "month"
  ) |>
  mutate(
    ret_excess = ret_adj - rf,
    ret_excess = pmax(ret_excess, -1)
  ) |>
  select(-ret_adj, -rf)

# Order by date and calculate cumulative returns for each stock
crsp_monthly <- crsp_monthly |>
  group_by(permno) |>
  arrange(date) |>
  mutate(ret_cum = cumprod(1 + ret_excess) - 1)

# Plot cumulative returns
crsp_monthly |>
  filter(permno == 14593) |>
  ggplot(aes(x = date, y = ret_cum)) +
  geom_line() +
  labs(
    x = NULL, y = NULL,
    title = "Cumulative Returns of a Microsoft"
  ) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(labels = percent)

# Drop observations with no market cap and no returns
crsp_monthly <- crsp_monthly |>
  drop_na(ret_excess, mktcap, mktcap_lag)

# Store in DB
dbWriteTable(db_connection,
             "crsp_monthly",
             value = crsp_monthly,
             overwrite = TRUE
)

# Get crsp_monthly from DB
crsp_monthly <- tbl(db_connection, "crsp_monthly") |>
  collect()

########## Compustat Fundamentals ##########
funda_db <- tbl(wrds, in_schema("comp", "funda"))

# (i) we get only records in industrial data format, 
# (ii) in the standard format (i.e., consolidated information in standard presentation), and 
# (iii) only data in the desired time window.
compustat <- funda_db |>
  filter(
    indfmt == "INDL" &
      datafmt == "STD" &
      consol == "C" &
      datadate >= start_date & datadate <= end_date
  ) |>
  select(
    gvkey, # Firm identifier
    datadate, # Date of the accounting data
    seq, # Stockholders' equity
    ceq, # Total common/ordinary equity
    at, # Total assets
    lt, # Total liabilities
    txditc, # Deferred taxes and investment tax credit
    txdb, # Deferred taxes
    itcb, # Investment tax credit
    sale, # Sales
    revt, # Total revenue
    xrd, # R&D
    pstkrv, # Preferred stock redemption value
    pstkl, # Preferred stock liquidating value
    pstk, # Preferred stock par value
    capx, # Capital investment
    oancf # Operating cash flow
  ) |>
  collect()

# Calculate the book value of preferred stock and equity
compustat <- compustat |>
  mutate(
    be = coalesce(seq, ceq + pstk, at - lt) +
      coalesce(txditc, txdb + itcb, 0) -
      coalesce(pstkrv, pstkl, pstk, 0),
    be = if_else(be <= 0, as.numeric(NA), be)
  )

compustat <- compustat |>
  mutate(month = month(datadate),
         year = year(datadate)
         )

# Year lagged sales.
sale_lag <- compustat |>
  mutate(datadate = datadate %m+% months(12),
         month = month(datadate),
         year = year(datadate),
         sale = sale) |>
  select(gvkey, c(month, year), sale_lag = sale)

compustat <- compustat |>
  left_join(sale_lag, by = c("gvkey", c("month", "year")))

# Calculate sales growth
compustat <- compustat |>
  mutate(
    sale_g = coalesce(if_else(sale <= 0, as.numeric(NA), log(sale)) - 
                        if_else(sale_lag <= 0, as.numeric(NA), log(sale_lag)),
                      0)
  )

# Year lagged sales growth.
sale_g_lag <- compustat |>
  mutate(datadate = datadate %m+% months(12),
         month = month(datadate),
         year = year(datadate),
         sale_g = sale_g) |>
  select(gvkey, c(month, year), sale_g_lag = sale_g)

compustat <- compustat |>
  left_join(sale_g_lag, by = c("gvkey", c("month", "year")))

# Year lagged Investments
invest_lag <- compustat |>
  mutate(datadate = datadate %m+% months(12),
         month = month(datadate),
         year = year(datadate),
         capx = capx,
         at = at,
         lt = lt) |>
  select(gvkey, c(month, year), capx_lag = capx, at_lag = at, lt_lag = lt)

compustat <- compustat |>
  left_join(invest_lag, by = c("gvkey", c("month", "year")))

# We keep only the last available information for each firm-year group.
compustat <- compustat |>
  mutate(year = year(datadate)) |>
  group_by(gvkey, year) |>
  filter(datadate == max(datadate)) |>
  ungroup()

# Store in DB
dbWriteTable(db_connection,
             "compustat",
             value = compustat,
             overwrite = TRUE
)

############ Merge with CRSP ########## 
# Get the matching table from CRSP
ccmxpf_linktable_db <- tbl(
  wrds,
  in_schema("crsp", "ccmxpf_linktable")
)

# Only keep relevant info
ccmxpf_linktable <- ccmxpf_linktable_db |>
  filter(linktype %in% c("LU", "LC") &
           linkprim %in% c("P", "C") &
           usedflag == 1) |>
  select(permno = lpermno, gvkey, linkdt, linkenddt) |>
  collect() |>
  mutate(linkenddt = replace_na(linkenddt, today()))

# Id's
ccm_links <- crsp_monthly |>
  inner_join(ccmxpf_linktable, by = "permno") |>
  filter(!is.na(gvkey) & (date >= linkdt & date <= linkenddt)) |>
  select(permno, gvkey, date)

crsp_monthly <- crsp_monthly |>
  left_join(ccm_links, by = c("permno", "date"))

# Update CRSP
dbWriteTable(db_connection,
             "crsp_monthly",
             value = crsp_monthly,
             overwrite = TRUE
)

########## Create full dataset ########## 

# Left join compustat with CRSP using gvkey and year
data <- crsp_monthly |>
  group_by(permno, year = year(month)) |>
  filter(date == max(date)) |>
  ungroup() |>
  left_join(compustat, by = c("gvkey", "year"))

# Rename data$month.y to data$month_num and data$month.x to data$month
data <- data |>
  rename(month_num = month.y, month = month.x)

### Drop where exchange is Other ###
data <- data |>
  filter(exchange != "Other")

# Write to DB
dbWriteTable(db_connection,
             "data",
             value = data,
             overwrite = TRUE
)

# Optimize DB
dbExecute(db_connection, "VACUUM;")
