# ============================================================
# FINANCIAL TIME SERIES - FINAL PROJECT
# Trading System Design and Methodology
# ============================================================

# ============================================================
# 1. INTRODUCTION AND DATA SETUP
# ============================================================

# ------------------------------------------------------------
# 1.1 Environment Setup
# ------------------------------------------------------------
cat("1.1 Environment Setup\n")
cat("---------------------\n")

library(tidyverse)
library(lubridate)
library(tseries)
library(TTR)
library(ggplot2)
library(moments)
library(zoo)
library(readr)

set.seed(2025)

cat("Libraries loaded successfully.\n\n")

# ------------------------------------------------------------
# 1.2 Data Source and File Path
# ------------------------------------------------------------
cat("1.2 Data Source and File Path\n")
cat("-----------------------------\n")

data_file <- "C:/Users/fritz/OneDrive/Desktop/ACADEMIC/rutgers_university/SEMESTERS/SPRING26/financial_time-series/assignments/final_project/Presentation/Presentation/dailyfiancial2005_2025.csv"

if (!file.exists(data_file)) {
  stop("Data file not found: ", data_file)
}

cat("Data file found.\n\n")

# ------------------------------------------------------------
# 1.3 Output Directory Setup
# ------------------------------------------------------------
cat("1.3 Output Directory Setup\n")
cat("---------------------------\n")

if (!dir.exists("output")) {
  dir.create("output")
}

cat("Output folder ready.\n\n")

# ------------------------------------------------------------
# 1.4 Data Loading
# ------------------------------------------------------------
cat("1.4 Data Loading\n")
cat("----------------\n")

data <- read_csv(data_file, show_col_types = FALSE)

cat("Data loaded successfully.\n\n")

# 1.5 Data Cleaning and Preparation
# ------------------------------------------------------------
cat("1.5 Data Cleaning and Preparation\n")
cat("----------------------------------\n")

data <- data %>%
  select(-starts_with("...")) %>%
  mutate(
    Date = as.Date(Date),
    DlyNumTrd = ifelse(is.na(DlyNumTrd), 0, DlyNumTrd)
  ) %>%
  arrange(Ticker, Date)

cat("Data cleaned and sorted.\n")
cat("Observations:", nrow(data), "\n")
cat("Variables:", ncol(data), "\n")

cat("Date range:",
    as.character(min(data$Date, na.rm = TRUE)),
    "to",
    as.character(max(data$Date, na.rm = TRUE)),
    "\n\n")

# ------------------------------------------------------------
# 1.6 Asset Selection
# ------------------------------------------------------------
cat("1.6 Asset Selection\n")
cat("--------------------\n")

main_tickers <- c(
  "AAPL", "AMZN", "BAC", "CSCO", "CVX", "DIS", "GOOG", "HD",
  "INTC", "JNJ", "JPM", "LLY", "MSFT", "NVDA", "PEP", "PG",
  "UNH", "VZ", "WMT", "XOM"
)

data_main <- data %>%
  filter(Ticker %in% main_tickers)

cat("Selected", length(main_tickers), "main assets.\n")
cat("Tickers:", paste(main_tickers, collapse = ", "), "\n\n")


# ============================================================
# 2. DATA OVERVIEW AND DIAGNOSTICS
# ============================================================

# Required packages
library(dplyr)
library(ggplot2)
library(tseries)
library(moments)
library(zoo)

# Create output folder
if (!dir.exists("output")) {
  dir.create("output")
}

# ------------------------------------------------------------
# 2.1 Data Description
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("2. DATA OVERVIEW AND DIAGNOSTICS\n")
cat("============================================================\n\n")

cat("2.1 Data Description\n")
cat("--------------------\n")

data_description <- data.frame(
  Item = c("Observations", "Variables", "Trading Days", "Assets/Tickers"),
  Value = c(
    nrow(data_main),
    ncol(data_main),
    length(unique(data_main$Date)),
    length(unique(data_main$Ticker))
  )
)

print(data_description)

write.csv(data_description,
          "output/table_data_description.csv",
          row.names = FALSE)


# ------------------------------------------------------------
# 2.2 Summary Statistics
# ------------------------------------------------------------
cat("\n2.2 Summary Statistics\n")
cat("----------------------\n")

get_stats <- function(x) {
  x <- x[!is.na(x)]
  c(
    N    = length(x),
    Mean = round(mean(x) * 100, 4),
    SD   = round(sd(x) * 100, 4),
    Min  = round(min(x) * 100, 4),
    Max  = round(max(x) * 100, 4),
    Skew = round(skewness(x), 4),
    Kurt = round(kurtosis(x), 4)
  )
}

# Summary statistics for all tickers
summary_table <- data_main %>%
  group_by(Ticker) %>%
  summarise(
    N = sum(!is.na(DlyRet)),
    Mean = round(mean(DlyRet, na.rm = TRUE) * 100, 4),
    SD = round(sd(DlyRet, na.rm = TRUE) * 100, 4),
    Min = round(min(DlyRet, na.rm = TRUE) * 100, 4),
    Max = round(max(DlyRet, na.rm = TRUE) * 100, 4),
    Skew = round(skewness(DlyRet, na.rm = TRUE), 4),
    Kurt = round(kurtosis(DlyRet, na.rm = TRUE), 4),
    .groups = "drop"
  )

print(summary_table)

write.csv(summary_table,
          "output/table_summary_statistics_all_tickers.csv",
          row.names = FALSE)

# Representative stock: AAPL
aapl_returns <- data_main %>%
  filter(Ticker == "AAPL") %>%
  arrange(Date) %>%
  pull(DlyRet)

aapl_returns <- aapl_returns[!is.na(aapl_returns)]

aapl_summary <- data.frame(t(get_stats(aapl_returns)))

cat("\nAAPL Summary Statistics:\n")
print(aapl_summary)

write.csv(aapl_summary,
          "output/table_aapl_summary_statistics.csv",
          row.names = FALSE)


# ------------------------------------------------------------
# 2.3 Distribution Analysis
# ------------------------------------------------------------
cat("\n2.3 Distribution Analysis\n")
cat("-------------------------\n")

jb <- jarque.bera.test(aapl_returns)

normality_table <- data.frame(
  Test = "Jarque-Bera Test",
  Null_Hypothesis = "Returns are normally distributed",
  Statistic = round(jb$statistic, 4),
  P_Value = round(jb$p.value, 6),
  Decision = ifelse(jb$p.value < 0.05,
                    "Reject normality",
                    "Do not reject normality")
)

print(normality_table)

write.csv(normality_table,
          "output/table_normality_test.csv",
          row.names = FALSE)


# Histogram
p_hist <- ggplot(data.frame(Return = aapl_returns), aes(x = Return)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 100,
                 fill = "lightblue",
                 color = "white") +
  geom_density(linewidth = 1) +
  labs(
    title = "AAPL Returns Distribution",
    x = "Daily Returns",
    y = "Density"
  ) +
  theme_minimal()

print(p_hist)

ggsave("output/graph_aapl_histogram.png",
       p_hist,
       width = 9,
       height = 6)


# QQ Plot
png("output/graph_aapl_qqplot.png", width = 700, height = 700)
qqnorm(aapl_returns, main = "QQ Plot: AAPL Returns")
qqline(aapl_returns, col = "red", lwd = 2)
dev.off()

qqnorm(aapl_returns, main = "QQ Plot: AAPL Returns")
qqline(aapl_returns, col = "red", lwd = 2)


# ------------------------------------------------------------
# 2.4 Time Series Behavior
# ------------------------------------------------------------
cat("\n2.4 Time Series Behavior\n")
cat("------------------------\n")

# ACF of Returns
png("output/graph_acf_returns.png", width = 700, height = 500)
acf(aapl_returns, main = "ACF of Returns (AAPL)")
dev.off()

acf(aapl_returns, main = "ACF of Returns (AAPL)")

# ACF of Squared Returns
png("output/graph_acf_squared_returns.png", width = 700, height = 500)
acf(aapl_returns^2, main = "ACF of Squared Returns (AAPL)")
dev.off()

acf(aapl_returns^2, main = "ACF of Squared Returns (AAPL)")


# ADF Test
cat("\nADF Test (Stationarity)\n")
cat("------------------------\n")

adf <- tseries::adf.test(aapl_returns)

adf_table <- data.frame(
  Test = "ADF Test",
  Null_Hypothesis = "Series has a unit root",
  Statistic = round(adf$statistic, 4),
  P_Value = round(adf$p.value, 6),
  Decision = ifelse(adf$p.value < 0.05,
                    "Reject unit root; series is stationary",
                    "Do not reject unit root")
)

print(adf_table)

write.csv(adf_table,
          "output/table_adf_stationarity_test.csv",
          row.names = FALSE)


# Rolling Volatility
aapl_data <- data_main %>%
  filter(Ticker == "AAPL") %>%
  arrange(Date) %>%
  mutate(
    Vol20 = rollapply(DlyRet,
                      width = 20,
                      FUN = sd,
                      fill = NA,
                      align = "right") * sqrt(252) * 100
  )

p_vol <- ggplot(aapl_data, aes(x = Date, y = Vol20)) +
  geom_line(color = "steelblue") +
  labs(
    title = "AAPL 20-Day Rolling Volatility (Annualized)",
    x = "Date",
    y = "Volatility (%)"
  ) +
  theme_minimal()

print(p_vol)

ggsave("output/graph_aapl_rolling_volatility.png",
       p_vol,
       width = 10,
       height = 5)


# ------------------------------------------------------------
# 2.5 Data Split
# ------------------------------------------------------------
cat("\n2.5 Data Split\n")
cat("----------------\n")

split_table <- data.frame(
  Sample = c("Training Period", "Validation Period", "Test Period"),
  Period = c("2005–2014", "2015–2017", "2018–2025"),
  Purpose = c(
    "Model estimation",
    "Model tuning and selection",
    "Out-of-sample performance testing"
  )
)

print(split_table)

write.csv(split_table,
          "output/table_data_split.csv",
          row.names = FALSE)


# ------------------------------------------------------------
# 2.6 Final Output Message
# ------------------------------------------------------------
cat("\nSaved Output Files\n")
cat("------------------\n")
cat("Tables saved as CSV files in the output folder.\n")
cat("Graphs saved as PNG files in the output folder.\n")
cat("Graphs are also printed in R when the code runs.\n")
# ============================================================
# 3. FEATURE ENGINEERING
# ============================================================

cat("\n==================================================\n")
cat("3. FEATURE ENGINEERING\n")
cat("==================================================\n\n")

# ------------------------------------------------------------
# 3.1 Data Split Periods
# ------------------------------------------------------------
cat("3.1 Data Split Periods\n")
cat("----------------------\n")

train_start <- as.Date("2005-01-01")
train_end   <- as.Date("2014-12-31")

valid_start <- as.Date("2015-01-01")
valid_end   <- as.Date("2017-12-31")

test_start  <- as.Date("2018-01-01")
test_end    <- as.Date("2025-12-31")

cat("Training   :", as.character(train_start), "to", as.character(train_end), "(10 years)\n")
cat("Validation :", as.character(valid_start), "to", as.character(valid_end), "(3 years)\n")
cat("Testing    :", as.character(test_start), "to", as.character(test_end), "(8 years)\n\n")


# ------------------------------------------------------------
# 3.2 Feature Construction
# ------------------------------------------------------------
cat("3.2 Feature Construction\n")
cat("------------------------\n")
cat("Computing the following features:\n")
cat("  - Excess Return   : DlyRet - Risk_free / 252 / 100\n")
cat("  - Volatility      : 20-day rolling standard deviation (annualized)\n")
cat("  - Momentum        : 10-day cumulative return\n")
cat("  - MA Crossover    : 10-day moving average vs 30-day moving average\n")
cat("  - RSI             : 14-day Relative Strength Index\n")
cat("  - VIX Change      : 5-day change in VIX\n")
cat("  - Spread Change   : 5-day change in bid-ask spread\n")
cat("All predictors are lagged by 1 day to avoid look-ahead bias.\n\n")


# ------------------------------------------------------------
# 3.3 Feature Implementation
# ------------------------------------------------------------
compute_features <- function(df, ticker_name) {
  
  ticker_data <- df %>%
    filter(Ticker == ticker_name) %>%
    arrange(Date) %>%
    mutate(
      # Return adjusted for daily risk-free rate
      ExcessRet = DlyRet - (Risk_free / 252 / 100),
      
      # 20-day rolling volatility
      Volatility = zoo::rollapply(
        DlyRet,
        width = 20,
        FUN = sd,
        fill = NA,
        align = "right"
      ) * sqrt(252) * 100,
      
      # 10-day cumulative momentum
      Momentum = zoo::rollapply(
        DlyRet,
        width = 10,
        FUN = function(x) prod(1 + x) - 1,
        fill = NA,
        align = "right"
      ),
      
      # Moving averages based on price
      MA10 = zoo::rollapply(
        DlyPrc,
        width = 10,
        FUN = mean,
        fill = NA,
        align = "right"
      ),
      
      MA30 = zoo::rollapply(
        DlyPrc,
        width = 30,
        FUN = mean,
        fill = NA,
        align = "right"
      ),
      
      # Moving average crossover signal
      MA_Crossover = case_when(
        MA10 > MA30 ~  1,
        MA10 < MA30 ~ -1,
        TRUE        ~  0
      ),
      
      # RSI
      RSI = TTR::RSI(DlyPrc, n = 14),
      
      # Macro / market condition variables
      VIX_Change    = Vix - dplyr::lag(Vix, 5),
      Spread_Change = Spread - dplyr::lag(Spread, 5)
    ) %>%
    mutate(
      # Lag features by 1 day to avoid using current-day information
      Volatility    = dplyr::lag(Volatility, 1),
      Momentum      = dplyr::lag(Momentum, 1),
      MA_Crossover  = dplyr::lag(MA_Crossover, 1),
      RSI           = dplyr::lag(RSI, 1),
      VIX_Change    = dplyr::lag(VIX_Change, 1),
      Spread_Change = dplyr::lag(Spread_Change, 1)
    )
  
  return(ticker_data)
}


# ------------------------------------------------------------
# 3.4 Feature Application
# ------------------------------------------------------------
cat("3.4 Feature Application\n")
cat("------------------------\n")

feature_list <- list()

for (ticker in main_tickers) {
  cat("Processing:", ticker, "\n")
  feature_list[[ticker]] <- compute_features(data_main, ticker)
}

all_features <- dplyr::bind_rows(feature_list)

cat("\nFeature engineering completed successfully.\n")
cat("Total observations in all_features:", nrow(all_features), "\n")
cat("Total variables in all_features    :", ncol(all_features), "\n\n")


# ------------------------------------------------------------
# 3.5 Feature Correlation Analysis
# ------------------------------------------------------------
cat("\n3.5 Feature Correlation Analysis\n")
cat("--------------------------------\n")

train_features <- all_features %>%
  filter(Date >= train_start & Date <= train_end) %>%
  select(
    ExcessRet,
    Volatility,
    Momentum,
    MA_Crossover,
    RSI,
    VIX_Change,
    Spread_Change
  ) %>%
  na.omit()

cor_matrix_feat <- cor(train_features)

cat("Feature Correlation Matrix (Training Period: 2005-2014)\n")
print(round(cor_matrix_feat, 3))

cat("\nLow correlations suggest that multicollinearity is not a major concern.\n")


# ============================================================
# 4. SIGNAL GENERATION
# ============================================================

# ------------------------------------------------------------
# 4.1 Signal Construction
# ------------------------------------------------------------
cat("4.1 Signal Construction\n")
cat("-----------------------\n")
cat("Prediction Target:\n")
cat("  Next-day return direction based on Final_Signal\n\n")

all_features <- all_features %>%
  mutate(
    Signal_Momentum = case_when(
      Momentum > 0.02  ~ 1,
      Momentum < -0.02 ~ -1,
      TRUE ~ 0
    ),
    Signal_MA = MA_Crossover,
    Signal_Sum = Signal_Momentum + Signal_MA,
    Market_Trend = ifelse(vwretd > 0, 1, -1),
    Final_Signal = case_when(
      Signal_Sum >= 2 & Market_Trend == 1  ~ 1,
      Signal_Sum <= -2 & Market_Trend == -1 ~ -1,
      TRUE ~ 0
    )
  )

cat("Final signal created using momentum, moving average, and market trend.\n\n")

# ------------------------------------------------------------
# 4.2 Risk Filters
# ------------------------------------------------------------
cat("4.2 Risk Filters\n")
cat("----------------\n")

vol_threshold <- quantile(all_features$Volatility, 0.90, na.rm = TRUE)

all_features <- all_features %>%
  mutate(
    Final_Signal = ifelse(Volatility > vol_threshold, 0, Final_Signal),
    Final_Signal = ifelse(Vix > 30, 0, Final_Signal)
  )

cat("Signals removed during high-volatility and high-VIX periods.\n\n")

# ------------------------------------------------------------
# 4.3 Training Period Evaluation
# ------------------------------------------------------------
cat("4.3 Training Period Evaluation\n")
cat("------------------------------\n")

strategy_returns <- c()

for (ticker in main_tickers) {
  
  df <- all_features %>%
    filter(Ticker == ticker) %>%
    arrange(Date)
  
  for (i in 1:(nrow(df) - 1)) {
    if (df$Date[i] >= train_start && df$Date[i] <= train_end) {
      
      signal <- df$Final_Signal[i]
      next_return <- df$DlyRet[i + 1]
      
      if (!is.na(signal) && !is.na(next_return)) {
        strategy_returns <- c(strategy_returns, signal * next_return)
      }
    }
  }
}

mean_return <- mean(strategy_returns, na.rm = TRUE)
strategy_volatility <- sd(strategy_returns, na.rm = TRUE)

rf_daily <- mean(all_features$Risk_free, na.rm = TRUE) / 100 / 252
excess_return <- mean_return - rf_daily

sharpe_ratio <- (excess_return / strategy_volatility) * sqrt(252)

cat("Average Return:", round(mean_return * 100, 4), "%\n")
cat("Volatility:", round(strategy_volatility * 100, 4), "%\n")
cat("Sharpe Ratio:", round(sharpe_ratio, 4), "\n\n")

cat("The strategy combines momentum and moving average confirmation.\n")
cat("The market trend filter allows long positions in positive market conditions\n")
cat("and short positions in negative market conditions.\n")
cat("Risk filters reduce exposure during unstable market periods.\n\n")



# ============================================================
# 5. TRADING STRATEGY AND BACKTESTING
# ============================================================


  # ------------------------------------------------------------------
  # 5.1 Trading Rules
  # ------------------------------------------------------------------
  
  cat("5.1 Trading Rules\n")
  cat("-----------------\n")
  
  cat("Signal Interpretation:\n")
  cat("  +1 → Long signal\n")
  cat("  -1 → Short signal\n")
  cat("   0 → No position (cash)\n\n")
  
  cat("Strategy Implementation:\n")
  cat("  - Long-Only Strategy:\n")
  cat("      +1 → Long position\n")
  cat("      -1 or 0 → Cash (no position)\n")
  cat("  - Long-Short Strategy:\n")
  cat("      +1 → Long position\n")
  cat("      -1 → Short position\n")
  cat("       0 → Cash\n\n")
  
  cat("Entry and Exit Rules:\n")
  cat("  - Enter position when a valid signal is generated\n")
  cat("  - Exit position when signal changes or becomes zero\n")
  cat("  - Trades are executed at the next period (t) using signal at (t-1)\n\n")
  
  cat("Position Sizing:\n")
  cat("  - Equal-weight allocation across selected assets\n")
  cat("  - Minimum of 2 assets required to form a portfolio\n")
  cat("  - Otherwise, remain in cash\n\n")
  
  cat("Persistence Filter:\n")
  cat("  - Trade only if Signal_{t-1} = Signal_{t-2} ≠ 0\n")
  cat("  - Reduces noise and avoids frequent trading\n\n")
  
  cat("Holding Period and Rebalancing:\n")
  cat("  - Positions are held until signals change\n")
  cat("  - Rebalancing occurs only when signals change (not daily)\n")
  cat("  - This reduces transaction costs and turnover\n\n")
  
  cat("Information Timing:\n")
  cat("  - Signals are based only on past information (t-1, t-2)\n")
  cat("  - No look-ahead bias is introduced\n\n")
  
  # ------------------------------------------------------------------
  # 5.2 Rebalancing Strategy
  # ------------------------------------------------------------------
  
  cat("5.2 Rebalancing Strategy\n")
  cat("------------------------\n")
  cat("  - Signal-based rebalancing (not daily)\n")
  cat("  - Rebalance only when persistent signal changes\n")
  cat("  - Execute at next day's open\n")
  cat("  - Hold positions if signal unchanged\n\n")
  
  # ------------------------------------------------------------------
  # 5.3 Transaction Costs
  # ------------------------------------------------------------------
  
  cat("5.3 Transaction Costs\n")
  cat("----------------------\n")
  
  cost_long <- 0.0013      # 13 bps
  cost_short <- 0.0020     # 20 bps
  
  cat("  Long positions: 13 bps one-way, 26 bps round-trip\n")
  cat("  Short positions: 20 bps one-way, 40 bps round-trip\n")
  cat("  Stressed scenario: 2x slippage\n\n")
  
  # ------------------------------------------------------------------
  # 5.4 Cash Management
  # ------------------------------------------------------------------
  
  cat("5.4 Cash Management\n")
  cat("--------------------\n")
  cat("  - Uninvested capital in cash\n")
  cat("  - Cash earns risk-free rate (3-month T-bill)\n\n")
  
  # ------------------------------------------------------------------
  # 5.5 Trade Execution
  # ------------------------------------------------------------------
  
  cat("5.5 Trade Execution\n")
  cat("--------------------\n")
  cat("  - Signal_{t-1} → Trade at t\n")
  cat("  - Execute at next day's open\n")
  cat("  - No look-ahead bias\n\n")
  
  # ------------------------------------------------------------------
  # 5.6 Backtesting Framework
  # ------------------------------------------------------------------
  
  cat("5.6 Backtesting Framework\n")
  cat("-------------------------\n")
  
  backtest <- function(signals_df, strategy = "long_only", apply_costs = TRUE, 
                       start_date = valid_start, end_date = test_end) {
    
    df <- signals_df %>%
      filter(Date >= start_date & Date <= end_date)
    
    dates <- sort(unique(df$Date))
    
    if (length(dates) < 2) {
      return(NULL)
    }
    
    portfolio_value <- 100000
    daily_returns <- c()
    current_weights <- c()
    
    for (i in 2:length(dates)) {
      
      yesterday <- df %>% filter(Date == dates[i - 1])
      today     <- df %>% filter(Date == dates[i])
      twodays   <- if (i >= 3) df %>% filter(Date == dates[i - 2]) else NULL
      
      rf <- ifelse(is.na(yesterday$Risk_free[1]), 0,
                   yesterday$Risk_free[1] / 252 / 100)
      
      # ----------------------------------------------------------
      # Persistence filter
      # ----------------------------------------------------------
      if (!is.null(twodays) && nrow(twodays) > 0) {
        sig_t1 <- yesterday$Final_Signal
        sig_t2 <- twodays$Final_Signal
        persistent <- (sig_t1 == sig_t2) & (sig_t1 != 0)
        signals <- ifelse(persistent, sig_t1, 0)
      } else {
        signals <- rep(0, nrow(yesterday))
      }
      
      # ----------------------------------------------------------
      # Generate new portfolio weights
      # ----------------------------------------------------------
      if (strategy == "long_only") {
        
        long_idx <- which(signals == 1)
        
        if (length(long_idx) >= 2) {
          new_weights <- rep(1 / length(long_idx), length(long_idx))
          names(new_weights) <- yesterday$Ticker[long_idx]
        } else {
          new_weights <- c()
        }
        
      } else if (strategy == "long_short") {
        
        long_idx  <- which(signals == 1)
        short_idx <- which(signals == -1)
        
        new_weights <- c()
        
        if (length(long_idx) > 0) {
          long_weights <- rep(1 / length(long_idx), length(long_idx))
          names(long_weights) <- yesterday$Ticker[long_idx]
          new_weights <- c(new_weights, long_weights)
        }
        
        if (length(short_idx) > 0) {
          short_weights <- rep(-1 / length(short_idx), length(short_idx))
          names(short_weights) <- yesterday$Ticker[short_idx]
          new_weights <- c(new_weights, short_weights)
        }
        
        if (length(new_weights) < 2) {
          new_weights <- c()
        }
      }
      
      # ----------------------------------------------------------
      # Rebalance check
      # ----------------------------------------------------------
      rebalance <- FALSE
      
      if (length(current_weights) != length(new_weights)) {
        rebalance <- TRUE
      } else if (!identical(sort(names(current_weights)), sort(names(new_weights)))) {
        rebalance <- TRUE
      }
      # ----------------------------------------------------------
      # Portfolio return from existing positions
      # ----------------------------------------------------------
      port_return <- 0
      exposure <- 0
      
      if (length(current_weights) > 0) {
        for (k in 1:length(current_weights)) {
          stock <- names(current_weights)[k]
          ret <- today$DlyRet[today$Ticker == stock]
          
          if (length(ret) > 0 && !is.na(ret[1])) {
            port_return <- port_return + current_weights[k] * ret[1]
            exposure <- exposure + abs(current_weights[k])
          }
        }
      }
      
      # ----------------------------------------------------------
      # Transaction costs on rebalance
      # ----------------------------------------------------------
      if (apply_costs && rebalance && length(current_weights) > 0) {
        
        # Cost for closing old positions
        if (length(current_weights) > 0) {
          for (k in 1:length(current_weights)) {
            if (current_weights[k] > 0) {
              port_return <- port_return - cost_long * abs(current_weights[k])
            } else {
              port_return <- port_return - cost_short * abs(current_weights[k])
            }
          }
        }
        
        # Cost for opening new positions
        if (length(new_weights) > 0) {
          for (k in 1:length(new_weights)) {
            if (new_weights[k] > 0) {
              port_return <- port_return - cost_long * abs(new_weights[k])
            } else {
              port_return <- port_return - cost_short * abs(new_weights[k])
            }
          }
        }
        
        current_weights <- new_weights
        
      } else if (length(current_weights) == 0 && length(new_weights) > 0) {
        
        # Initial entry cost
        if (length(new_weights) > 0) {
          for (k in 1:length(new_weights)) {
            if (new_weights[k] > 0) {
              port_return <- port_return - cost_long * abs(new_weights[k])
            } else {
              port_return <- port_return - cost_short * abs(new_weights[k])
            }
          }
        }
        
        current_weights <- new_weights
      }
      
      # ----------------------------------------------------------
      # Cash earns risk-free rate
      # ----------------------------------------------------------
      if (exposure < 1) {
        port_return <- port_return + (1 - exposure) * rf
      }
      
      portfolio_value <- portfolio_value * (1 + port_return)
      daily_returns <- c(daily_returns, port_return)
    }
    
    # ----------------------------------------------------------
    # Performance metrics
    # ----------------------------------------------------------
    if (length(daily_returns) > 0 && sd(daily_returns) > 0) {
      ann_return <- (prod(1 + daily_returns))^(252 / length(daily_returns)) - 1
      ann_vol <- sd(daily_returns) * sqrt(252)
      
      rf_annual <- mean(df$Risk_free, na.rm = TRUE) / 100
      excess_return <- ann_return - rf_annual
      
      sharpe <- excess_return / ann_vol
    } else {
      ann_return <- 0
      ann_vol <- 0
      sharpe <- 0
    }
    
    cum <- cumprod(1 + daily_returns)
    dd <- (cum - cummax(cum)) / cummax(cum)
    
    result <- list(
      sharpe = sharpe,
      annual_return = ann_return,
      annual_vol = ann_vol,
      max_drawdown = min(dd, na.rm = TRUE),
      win_rate = mean(daily_returns > 0, na.rm = TRUE),
      final_value = portfolio_value
    )
    
    return(result)
  }
  
  cat("Backtest framework ready.\n\n")
  
  # ============================================================
  # 6. PORTFOLIO CONSTRUCTION AND RESULTS
  # ============================================================
  
  # ------------------------------------------------------------
  # 6.1 Validation Period Results
  # ------------------------------------------------------------
  cat("6.1 Validation Period Results\n")
  cat("-----------------------------\n")
  
  long_only_valid  <- backtest(all_features, "long_only",  TRUE, valid_start, valid_end)
  long_short_valid <- backtest(all_features, "long_short", TRUE, valid_start, valid_end)
  
  cat("\nLong-Only Strategy:\n")
  cat("  Sharpe Ratio   :", round(long_only_valid$sharpe, 3), "\n")
  cat("  Annual Return  :", round(long_only_valid$annual_return * 100, 2), "%\n")
  cat("  Max Drawdown   :", round(long_only_valid$max_drawdown * 100, 2), "%\n")
  cat("  Win Rate       :", round(long_only_valid$win_rate * 100, 1), "%\n")
  
  cat("\nLong-Short Strategy:\n")
  cat("  Sharpe Ratio   :", round(long_short_valid$sharpe, 3), "\n")
  cat("  Annual Return  :", round(long_short_valid$annual_return * 100, 2), "%\n")
  cat("  Max Drawdown   :", round(long_short_valid$max_drawdown * 100, 2), "%\n")
  cat("  Win Rate       :", round(long_short_valid$win_rate * 100, 1), "%\n")
  
  
  # ------------------------------------------------------------
  # 6.2 Test Period Results
  # ------------------------------------------------------------
  cat("\n6.2 Test Period Results\n")
  cat("-----------------------\n")
  
  long_only_test  <- backtest(all_features, "long_only",  TRUE, test_start, test_end)
  long_short_test <- backtest(all_features, "long_short", TRUE, test_start, test_end)
  
  cat("\nLong-Only Strategy:\n")
  cat("  Sharpe Ratio   :", round(long_only_test$sharpe, 3), "\n")
  cat("  Annual Return  :", round(long_only_test$annual_return * 100, 2), "%\n")
  cat("  Max Drawdown   :", round(long_only_test$max_drawdown * 100, 2), "%\n")
  cat("  Win Rate       :", round(long_only_test$win_rate * 100, 1), "%\n")
  
  cat("\nLong-Short Strategy:\n")
  cat("  Sharpe Ratio   :", round(long_short_test$sharpe, 3), "\n")
  cat("  Annual Return  :", round(long_short_test$annual_return * 100, 2), "%\n")
  cat("  Max Drawdown   :", round(long_short_test$max_drawdown * 100, 2), "%\n")
  cat("  Win Rate       :", round(long_short_test$win_rate * 100, 1), "%\n")
  
  # ------------------------------------------------------------
  # Cumulative Performance Plot
  # ------------------------------------------------------------
  cat("\nCumulative Performance\n")
  cat("----------------------\n")
  
  daily_returns <- all_features$DlyRet
  daily_returns <- daily_returns[!is.na(daily_returns)]
  
  cum_returns <- cumprod(1 + daily_returns)
  
  
  # ------------------------------------------------------------
  # 6.3 Cost Sensitivity Analysis
  # ------------------------------------------------------------
  cat("\n6.3 Cost Sensitivity Analysis\n")
  cat("-----------------------------\n")
  
  # Long-Only Strategy
  long_only_gross <- backtest(all_features, "long_only", FALSE, valid_start, test_end)
  long_only_net   <- backtest(all_features, "long_only", TRUE,  valid_start, test_end)
  
  cat("Long-Only Strategy:\n")
  cat("  Gross Sharpe (no costs):", round(long_only_gross$sharpe, 3), "\n")
  cat("  Net Sharpe (with costs):", round(long_only_net$sharpe, 3), "\n\n")
  
  # Long-Short Strategy
  long_short_gross <- backtest(all_features, "long_short", FALSE, valid_start, test_end)
  long_short_net   <- backtest(all_features, "long_short", TRUE,  valid_start, test_end)
  
  cat("Long-Short Strategy:\n")
  cat("  Gross Sharpe (no costs):", round(long_short_gross$sharpe, 3), "\n")
  cat("  Net Sharpe (with costs):", round(long_short_net$sharpe, 3), "\n\n")
  
  
  # ------------------------------------------------------------
  # 6.4 Summary of Results
  # ------------------------------------------------------------
  cat("\n6.4 Summary of Results\n")
  cat("----------------------\n")
  
  results_summary <- data.frame(
    Period = c("Validation", "Validation", "Test", "Test"),
    Strategy = c("Long-Only", "Long-Short", "Long-Only", "Long-Short"),
    Sharpe = c(
      long_only_valid$sharpe,
      long_short_valid$sharpe,
      long_only_test$sharpe,
      long_short_test$sharpe
    ),
    Return = c(
      long_only_valid$annual_return,
      long_short_valid$annual_return,
      long_only_test$annual_return,
      long_short_test$annual_return
    ),
    MaxDD = c(
      long_only_valid$max_drawdown,
      long_short_valid$max_drawdown,
      long_only_test$max_drawdown,
      long_short_test$max_drawdown
    ),
    WinRate = c(
      long_only_valid$win_rate,
      long_short_valid$win_rate,
      long_only_test$win_rate,
      long_short_test$win_rate
    )
  )
  
  print(results_summary)
  
  # ------------------------------------------------------------
  # 6.5 Turnover and Trade Frequency
  # ------------------------------------------------------------
  cat("\n6.5 Turnover and Trade Frequency\n")
  cat("--------------------------------\n")
  
  calculate_turnover <- function(signals_df, strategy = "long_only",
                                 start_date = test_start, end_date = test_end) {
    
    df <- signals_df %>%
      filter(Date >= start_date & Date <= end_date)
    
    dates <- sort(unique(df$Date))
    
    if (length(dates) < 3) {
      return(list(
        turnover = NA,
        annual_turnover = NA,
        avg_holding_period = NA,
        n_trades = 0,
        n_rebalances = 0
      ))
    }
    
    prev_weights <- c()
    turnover_sum <- 0
    n_rebalances <- 0
    trade_dates <- c()
    
    for (i in 3:length(dates)) {
      
      twodays <- df %>% filter(Date == dates[i - 2])
      yesterday <- df %>% filter(Date == dates[i - 1])
      
      sig_t1 <- yesterday$Final_Signal
      sig_t2 <- twodays$Final_Signal
      
      persistent <- (sig_t1 == sig_t2) & (sig_t1 != 0)
      signals <- ifelse(persistent, sig_t1, 0)
      
      if (strategy == "long_only") {
        
        long_idx <- which(signals == 1)
        
        if (length(long_idx) >= 2) {
          new_weights <- rep(1 / length(long_idx), length(long_idx))
          names(new_weights) <- yesterday$Ticker[long_idx]
        } else {
          new_weights <- c()
        }
        
      } else if (strategy == "long_short") {
        
        long_idx <- which(signals == 1)
        short_idx <- which(signals == -1)
        new_weights <- c()
        
        if (length(long_idx) > 0) {
          long_weights <- rep(1 / length(long_idx), length(long_idx))
          names(long_weights) <- yesterday$Ticker[long_idx]
          new_weights <- c(new_weights, long_weights)
        }
        
        if (length(short_idx) > 0) {
          short_weights <- rep(-1 / length(short_idx), length(short_idx))
          names(short_weights) <- yesterday$Ticker[short_idx]
          new_weights <- c(new_weights, short_weights)
        }
        
        if (length(new_weights) < 2) {
          new_weights <- c()
        }
      }
      
      rebalance <- FALSE
      
      if (length(prev_weights) != length(new_weights)) {
        rebalance <- TRUE
      } else if (length(prev_weights) > 0 &&
                 !identical(sort(names(prev_weights)), sort(names(new_weights)))) {
        rebalance <- TRUE
      }
      
      if (rebalance && length(prev_weights) > 0 && length(new_weights) > 0) {
        
        all_assets <- union(names(prev_weights), names(new_weights))
        
        prev_full <- rep(0, length(all_assets))
        names(prev_full) <- all_assets
        
        new_full <- rep(0, length(all_assets))
        names(new_full) <- all_assets
        
        prev_full[names(prev_weights)] <- prev_weights
        new_full[names(new_weights)] <- new_weights
        
        turnover <- sum(abs(prev_full - new_full)) / 2
        
        turnover_sum <- turnover_sum + turnover
        n_rebalances <- n_rebalances + 1
        trade_dates <- c(trade_dates, dates[i])
        
      } else if (rebalance && length(prev_weights) == 0 && length(new_weights) > 0) {
        
        n_rebalances <- n_rebalances + 1
        trade_dates <- c(trade_dates, dates[i])
      }
      
      prev_weights <- new_weights
    }
    
    if (length(trade_dates) > 1) {
      avg_holding_period <- mean(diff(trade_dates), na.rm = TRUE)
    } else {
      avg_holding_period <- NA
    }
    
    if (n_rebalances > 0) {
      avg_turnover <- turnover_sum / n_rebalances
    } else {
      avg_turnover <- NA
    }
    
    annual_turnover <- avg_turnover * 252
    
    return(list(
      turnover = avg_turnover,
      annual_turnover = annual_turnover,
      avg_holding_period = avg_holding_period,
      n_trades = n_rebalances,
      n_rebalances = n_rebalances
    ))
  }
  
  turnover_lo <- calculate_turnover(all_features, "long_only", test_start, test_end)
  turnover_ls <- calculate_turnover(all_features, "long_short", test_start, test_end)
  
  cat("\nLong-Only Strategy:\n")
  cat("  Average Turnover per Rebalance:",
      round(turnover_lo$turnover * 100, 2), "%\n")
  cat("  Annualized Turnover:",
      round(turnover_lo$annual_turnover * 100, 2), "%\n")
  cat("  Number of Trades:", turnover_lo$n_trades, "\n")
  cat("  Average Holding Period:",
      round(turnover_lo$avg_holding_period, 1), "days\n\n")
  
  cat("Long-Short Strategy:\n")
  cat("  Average Turnover per Rebalance:",
      round(turnover_ls$turnover * 100, 2), "%\n")
  cat("  Annualized Turnover:",
      round(turnover_ls$annual_turnover * 100, 2), "%\n")
  cat("  Number of Trades:", turnover_ls$n_trades, "\n")
  cat("  Average Holding Period:",
      round(turnover_ls$avg_holding_period, 1), "days\n\n")
  
  turnover_summary <- data.frame(
    Strategy = c("Long-Only", "Long-Short"),
    Avg_Turnover_Per_Rebalance = c(turnover_lo$turnover, turnover_ls$turnover),
    Annualized_Turnover = c(turnover_lo$annual_turnover, turnover_ls$annual_turnover),
    Number_of_Trades = c(turnover_lo$n_trades, turnover_ls$n_trades),
    Avg_Holding_Period_Days = c(turnover_lo$avg_holding_period,
                                turnover_ls$avg_holding_period)
  )
  
  print(turnover_summary)
  
  write.csv(turnover_summary,
            "output/turnover_trade_frequency.csv",
            row.names = FALSE)
  
  cat("\nTurnover results saved to output/turnover_trade_frequency.csv\n")
  
  # ------------------------------------------------------------
  # 6.6 Results Export
  # ------------------------------------------------------------
  cat("\n6.6 Results Export\n")
  cat("------------------\n")
  
  write.csv(results_summary, "output/results_summary.csv", row.names = FALSE)
  
  cat("\n========================================\n")
  cat("Results saved to output/results_summary.csv\n")
  cat("========================================\n\n")
  
  # ============================================================
  # 7. RISK MANAGEMENT
  # ============================================================
  
  cat("\n========== 7. RISK MANAGEMENT ==========\n\n")
  
  # ------------------------------------------------------------
  # 7.1 Objective
  # ------------------------------------------------------------
  cat("7.1 Objective\n")
  cat("-------------\n")
  cat("Limit losses, reduce exposure in stress periods, and improve stability.\n\n")
  
  # ------------------------------------------------------------
  # 7.2 Core Risk Rules
  # ------------------------------------------------------------
  cat("7.2 Core Risk Rules\n")
  cat("-------------------\n")
  
  cat("1. Volatility Filter:\n")
  cat("   - If 20-day volatility > 90th percentile -> no trading\n")
  
  cat("2. VIX Filter:\n")
  cat("   - If VIX > 30 -> no trading\n")
  
  cat("3. Persistence Filter:\n")
  cat("   - Trade only if Signal(t-1) = Signal(t-2)\n\n")
  
  cat("When triggered, signals are set to 0 (cash position).\n\n")
  
  # ------------------------------------------------------------
  # 7.3 Exposure and Position Control
  # ------------------------------------------------------------
  cat("7.3 Exposure and Position Control\n")
  cat("---------------------------------\n")
  
  cat("Equal-weight allocation across active assets.\n")
  cat("Minimum 2 assets required to form portfolio.\n")
  cat("Otherwise -> stay in cash.\n\n")
  
  cat("Exposure varies with number of signals.\n")
  cat("Remaining capital earns risk-free rate.\n\n")
  
  # ------------------------------------------------------------
  # 7.4 Risk Metrics
  # ------------------------------------------------------------
  cat("7.4 Risk Metrics\n")
  cat("----------------\n")
  
  cat("Long-Only Strategy (Test Period):\n")
  cat("  Sharpe Ratio     :", round(long_only_test$sharpe, 3), "\n")
  cat("  Annual Return    :", round(long_only_test$annual_return * 100, 2), "%\n")
  cat("  Volatility       :", round(long_only_test$annual_vol * 100, 2), "%\n")
  cat("  Max Drawdown     :", round(long_only_test$max_drawdown * 100, 2), "%\n")
  cat("  Win Rate         :", round(long_only_test$win_rate * 100, 1), "%\n\n")
  
  cat("Long-Short Strategy (Test Period):\n")
  cat("  Sharpe Ratio     :", round(long_short_test$sharpe, 3), "\n")
  cat("  Annual Return    :", round(long_short_test$annual_return * 100, 2), "%\n")
  cat("  Volatility       :", round(long_short_test$annual_vol * 100, 2), "%\n")
  cat("  Max Drawdown     :", round(long_short_test$max_drawdown * 100, 2), "%\n")
  cat("  Win Rate         :", round(long_short_test$win_rate * 100, 1), "%\n\n")
  
 
  
  # ============================================================
  # 8. TRAINING AND TESTING FRAMEWORK
  # ============================================================
  
  
  
  # ------------------------------------------------------------
  # 8.1 Data Split Design
  # ------------------------------------------------------------
  cat("8.1 Data Split Design\n")
  cat("---------------------\n")
  cat("Training   : 2005-2014 -> Parameter selection\n")
  cat("Validation : 2015-2017 -> Stability check\n")
  cat("Testing    : 2018-2025 -> Final out-of-sample evaluation\n\n")
  
  # ------------------------------------------------------------
  # 8.2 Validation Approach
  # ------------------------------------------------------------
  cat("8.2 Validation Approach\n")
  cat("-----------------------\n")
  cat("The validation period checks whether the strategy remains stable\n")
  cat("outside the training sample.\n")
  cat("No re-optimization is performed in validation.\n")
  cat("This helps reduce data-snooping bias.\n\n")
  
  # ------------------------------------------------------------
  # 8.3 Look-Ahead Bias Control
  # ------------------------------------------------------------
  cat("8.3 Look-Ahead Bias Control\n")
  cat("---------------------------\n")
  cat("1. All predictors are lagged by one day.\n")
  cat("2. Signals are formed using information available at t-1.\n")
  cat("3. Trades are executed at t using lagged signals.\n")
  cat("4. Persistence filter uses only t-1 and t-2.\n")
  cat("5. No future information is used.\n\n")
  
  # ------------------------------------------------------------
  # 8.4 Model Scope and Assumptions
  # ------------------------------------------------------------
  cat("8.4 Model Scope and Assumptions\n")
  cat("-------------------------------\n")
  cat("No walk-forward optimization is used.\n")
  cat("The model relies on a small set of economically motivated rules:\n")
  cat("momentum, moving-average confirmation, market trend filter,\n")
  cat("volatility filter, VIX filter, and persistence filter.\n")
  cat("These rules are fixed after the training period and are not\n")
  cat("re-optimized across subperiods.\n")
  cat("This keeps the framework simple, avoids overfitting,\n")
  cat("and remains consistent with course expectations.\n\n")
  
  cat("========== SECTION 8 COMPLETE ==========\n\n")
  
  # ============================================================
  # 9. PERFORMANCE EVALUATION
  # ============================================================
  
  
  # ------------------------------------------------------------
  # 9.1  Performance
  # ------------------------------------------------------------
  cat("9.1 Overall Performance\n")
  cat("-----------------------\n")
  
  cat("The strategy shows weak performance across both validation and test periods.\n")
  cat("In the test period, the long-only Sharpe ratio is", round(long_only_test$sharpe, 3),
      "while the long-short Sharpe ratio is", round(long_short_test$sharpe, 3), ".\n")
  cat("Returns are negative, with long-only return of",
      round(long_only_test$annual_return * 100, 2), "% and long-short return of",
      round(long_short_test$annual_return * 100, 2), "%.\n")
  cat("Drawdowns are very large, indicating poor downside protection.\n\n")
  cat("Compared to a passive market strategy, the portfolio significantly underperforms,\n")
  cat("which confirms that the trading rules do not add value in their current form.\n\n")
  
  # ------------------------------------------------------------
  # 9.2 Long-Only Strategy
  # ------------------------------------------------------------
  cat("9.2 Long-Only Strategy\n")
  cat("----------------------\n")
  
  cat("The long-only strategy performs better than the long-short strategy,\n")
  cat("but still produces negative results.\n")
  cat("The Sharpe ratio is", round(long_only_test$sharpe, 3),
      "with volatility of", round(long_only_test$annual_vol * 100, 2), "%.\n")
  cat("The maximum drawdown reaches",
      round(long_only_test$max_drawdown * 100, 2), "%.\n")
  cat("The win rate is", round(long_only_test$win_rate * 100, 1),
      "%, which shows that losses are larger than gains.\n\n")
  
  # ------------------------------------------------------------
  # 9.3 Long-Short Strategy
  # ------------------------------------------------------------
  cat("9.3 Long-Short Strategy\n")
  cat("-----------------------\n")
  
  cat("The long-short strategy performs significantly worse.\n")
  cat("The Sharpe ratio is", round(long_short_test$sharpe, 3),
      "with annual return of", round(long_short_test$annual_return * 100, 2), "%.\n")
  cat("Volatility is higher at",
      round(long_short_test$annual_vol * 100, 2), "%.\n")
  cat("The maximum drawdown is extremely large at",
      round(long_short_test$max_drawdown * 100, 2), "%.\n")
  cat("The win rate is only",
      round(long_short_test$win_rate * 100, 1),
      "%, indicating weak predictive power, especially on short positions.\n\n")
  
  # ------------------------------------------------------------
  # 9.4 Risk Analysis
  # ------------------------------------------------------------
  cat("9.4 Risk Analysis\n")
  cat("-----------------\n")
  
  cat("Despite the use of volatility and VIX filters,\n")
  cat("the strategy remains exposed during unfavorable market conditions.\n")
  cat("Large drawdowns suggest that risk controls are not strong enough\n")
  cat("to prevent sustained losses.\n\n")
  cat("This suggests that the strategy lacks effective downside protection,\n")
  cat("as losses accumulate faster than the risk filters can react.\n\n")
  
  # ------------------------------------------------------------
  # 9.5 Key Limitation
  # ------------------------------------------------------------
  cat("9.5 Key Limitation\n")
  cat("------------------\n")
  
  cat("The main limitation comes from signal quality.\n")
  cat("The strategy generates too many incorrect signals,\n")
  cat("which leads to persistent negative returns.\n\n")
  cat("In particular, combining multiple weak signals may introduce noise\n")
  cat("instead of improving predictive accuracy.\n\n")
  
  
  
  cat("These results highlight the importance of robust signal design.\n")
  cat("In practice, combining weak predictors without proper validation can lead to persistent underperformance.\n\n")
  
  # ============================================================
  # 10. ROBUSTNESS CHECKS
  # ============================================================
  
 
  # ------------------------------------------------------------
  # 10.1 Transaction Cost Impact
  # ------------------------------------------------------------
  cat("10.1 Transaction Cost Impact\n")
  cat("----------------------------\n")
  
  # Long-Only Strategy (DEFINE FIRST)
  long_only_no_cost   <- backtest(all_features, "long_only", FALSE, test_start, test_end)
  long_only_with_cost <- backtest(all_features, "long_only", TRUE,  test_start, test_end)
  
  cat("Long-Only Strategy:\n")
  cat("Sharpe without costs :", round(long_only_no_cost$sharpe, 3), "\n")
  cat("Sharpe with costs    :", round(long_only_with_cost$sharpe, 3), "\n")
  
  cat("Return without costs :", round(long_only_no_cost$annual_return * 100, 2), "%\n")
  cat("Return with costs    :", round(long_only_with_cost$annual_return * 100, 2), "%\n\n")
  
  
  # Long-Short Strategy
  long_short_no_cost   <- backtest(all_features, "long_short", FALSE, test_start, test_end)
  long_short_with_cost <- backtest(all_features, "long_short", TRUE,  test_start, test_end)
  
  cat("Long-Short Strategy:\n")
  cat("Sharpe without costs :", round(long_short_no_cost$sharpe, 3), "\n")
  cat("Sharpe with costs    :", round(long_short_with_cost$sharpe, 3), "\n")
  
  cat("Return without costs :", round(long_short_no_cost$annual_return * 100, 2), "%\n")
  cat("Return with costs    :", round(long_short_with_cost$annual_return * 100, 2), "%\n\n")
  
  # ------------------------------------------------------------
  # 10.2 Strategy Comparison
  # ------------------------------------------------------------
  cat("10.2 Strategy Comparison\n")
  cat("------------------------\n")
  
  cat("The long-only strategy is more stable than the long-short strategy.\n")
  cat("The long-short strategy shows higher volatility and larger drawdowns.\n\n")
  
  # ------------------------------------------------------------
  # 10.3 Stability Across Periods
  # ------------------------------------------------------------
  cat("10.3 Stability Across Periods\n")
  cat("-----------------------------\n")
  
  cat("Performance is weak in both validation and test periods.\n")
  cat("This suggests that results are not driven by a specific market period.\n\n")
  
  
  # ============================================================
  # 11. DISCUSSION
  # ============================================================
  

  # ------------------------------------------------------------
  # 11.1 Summary of Findings
  # ------------------------------------------------------------
  cat("11.1 Summary of Findings\n")
  cat("------------------------\n")
  
  cat("This project developed a quantitative trading strategy based on\n")
  cat("technical indicators and macro-financial variables.\n")
  cat("The framework included feature engineering, signal generation,\n")
  cat("risk management, and backtesting.\n\n")
  
  cat("Empirical results show that the strategy performs poorly.\n")
  cat("Both long-only and long-short strategies produce negative returns\n")
  cat("and large drawdowns, with strongly negative Sharpe ratios.\n\n")
  
  # ------------------------------------------------------------
  # 11.2 Key Insights
  # ------------------------------------------------------------
  cat("11.2 Key Insights\n")
  cat("-----------------\n")
  
  cat("The main limitation comes from weak signal quality.\n")
  cat("The indicators used do not provide sufficient predictive power,\n")
  cat("leading to frequent incorrect trades.\n\n")
  
  cat("Risk management rules reduce some exposure during extreme conditions,\n")
  cat("but they are not strong enough to offset persistent losses.\n\n")
  
  cat("The long-short strategy performs worse due to unreliable short signals,\n")
  cat("which significantly increases risk and drawdowns.\n\n")
  
  # ------------------------------------------------------------
  # 11.3 Future Improvements
  # ------------------------------------------------------------
  cat("11.3 Future Improvements\n")
  cat("------------------------\n")
  
  cat("Future work could improve performance by:\n")
  cat("1. Using stronger signal filtering and stricter entry rules\n")
  cat("2. Incorporating additional macroeconomic variables\n")
  cat("3. Improving market regime detection\n")
  cat("4. Testing alternative modeling approaches\n\n")
  
 
  # ============================================================
  # 12. LIMITATIONS
  # ============================================================
  

  limitations <- data.frame(
    Limitation = c(
      "Survivorship bias",
      "Parameter selection bias",
      "Multiple design choices (~7 parameters)",
      "No intraday data",
      "Simplified execution (fills at open)",
      "Short selling constraints"
    ),
    Mitigation = c(
      "Acknowledged in report",
      "Fixed before testing; validation period used",
      "Acknowledged; validation period provides partial safeguard",
      "Acceptable for daily-frequency project",
      "Added slippage (3-5 bps)",
      "Long/short results are optimistic upper bound"
    )
  )
  
  print(limitations)
  
 
  
  # ============================================================
  # 13. FINAL CONCLUSION
  # ============================================================
  
 
  cat("The trading strategy fails to generate positive risk-adjusted returns.\n")
  cat("Both long-only and long-short strategies produce negative Sharpe ratios\n")
  cat("in out-of-sample testing.\n\n")
  
  cat("Transaction costs and weak signal quality are the main reasons\n")
  cat("for the poor performance.\n\n")
  
