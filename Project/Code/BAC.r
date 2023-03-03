# Load the necessary libraries
library(quantmod)
library(tidyquant)

# Set the start and end dates
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2022-12-31")

# symbols <- quantr::nyse_tickers()$symbol

symbols <- c("BAC", "C", "GS", "JPM", "MS", "WFC")

# Get the data for all stocks on the NYSE
prices <- getSymbols(symbols, from = start_date, to = end_date, auto.assign = TRUE)

# Calculate the daily returns for each stock
returns <- na.omit(dailyReturn(prices))

# Calculate the pairwise correlations between stocks
corr_matrix <- cor(returns)

# Set the correlation threshold for selecting pairs
corr_threshold <- 0.7

# Select the pairs with correlation below the threshold
pair_indices <- which(abs(corr_matrix) < corr_threshold, arr.ind = TRUE)
pairs <- data.frame(stock1 = rownames(corr_matrix)[pair_indices[, 1]],
                    stock2 = rownames(corr_matrix)[pair_indices[, 2]])

# Calculate the spread between the stock pairs
pair_spreads <- apply(pairs, 1, function(pair) {
  spread <- returns[, pair[1]] - returns[, pair[2]]
  zscore <- (spread - mean(spread)) / sd(spread)
  return(zscore)
})

# Create a matrix to hold the portfolio weights
portfolio_weights <- matrix(0, ncol = ncol(pair_spreads), nrow = nrow(pair_spreads))

# Loop over the pairs and assign weights to the portfolio
for (i in 1:nrow(pair_spreads)) {
  # Calculate the mean and standard deviation of the pair spread
  pair_mean <- mean(pair_spreads[i, ])
  pair_sd <- sd(pair_spreads[i, ])
  
  # Set the weights for the long and short positions
  long_weight <- 0.5 + (pair_mean / (2 * pair_sd))
  short_weight <- 1 - long_weight
  
  # Assign the weights to the portfolio
  portfolio_weights[i, pair_indices[i, 1]] <- long_weight
  portfolio_weights[i, pair_indices[i, 2]] <- -short_weight
}

# Calculate the daily portfolio returns
portfolio_returns <- returns %*% portfolio_weights


# Calculate the cumulative returns of the portfolio
cumulative_returns <- cumprod(1 + portfolio_returns) - 1

# Plot the cumulative returns of the portfolio
plot(cumulative_returns, type = "l", col = "blue", xlab = "Date", ylab = "Cumulative Return
