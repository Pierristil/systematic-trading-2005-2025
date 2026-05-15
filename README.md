# Systematic Trading Strategy: 20 Large-Cap US Stocks (2005–2025)

## Overview

This project develops a rule-based trading strategy for 20 US large-cap stocks using financial time series analysis, technical indicators, macroeconomic variables, and backtesting techniques in R.

The objective is to evaluate how systematic trading signals and risk management filters perform across different market conditions from 2005 to 2025.

## Key Results

Metric	              Long-Only	    Long-Short
Sharpe Ratio (Net)	  -1.35	         -1.48
Annual Return	        -9.90%	       -14.89%
Max Drawdown	        -57.20%	      -72.80%
Win Rate            	74.60%	      63.60%


**Key Insight:** Gross Sharpe was +0.38, but transaction costs (13 bps long / 20 bps short) turned it negative. Weak signals cannot survive strong costs.

## Strategy Logic

Component	Rule
Momentum	+1 if 10-day return > 2%; −1 if 10-day return < −2%
MA Crossover	+1 if MA(10) > MA(30); −1 if MA(10) < MA(30)
Market Trend	+1 if VWRETD > 0; −1 otherwise
Final Signal	Long: Signal sum ≥ 2 and Market Trend = +1 Short: Signal sum ≤ −2 and Market Trend = −1


## Risk Filters (Applied Sequentially)

Three filters applied before any trade:

1. Volatility Cap – No trade if 20-day volatility > 90th percentile
2. VIX Threshold – No trade if VIX > 30
3. Persistence Filter – Requires 2 consecutive identical signals before trading

## Dataset

**Sources**
- CRSP equity data
- FRED macroeconomic data
- CBOE VIX index

**Coverage**
- 20 US large-cap stocks
- Daily observations
- Period: 2005–2025

**Assets:** AAPL, AMZN, BAC, CSCO, CVX, DIS, GOOG, HD, INTC, JNJ, JPM, LLY, MSFT, NVDA, PEP, PG, UNH, VZ, WMT, XOM

## Data Split (Chronological)
Period	        Years	        Purpose
Training	      2005–2014	    Parameter calibration and model development
Validation	    2015–2017	    Stability assessment without re-optimization
Testing	        2018–2025	     Final out-of-sample performance evaluation

## Features & Indicators

- Momentum (10-day cumulative return)
- Moving averages (10-day and 30-day crossovers)
- Relative Strength Index (RSI – 14-day)
- Rolling volatility (20-day, annualized)
- VIX filters and market stress detection
- Bid-ask spread analysis


## Methodology

1. Data cleaning and preprocessing
2. Feature engineering (all predictors lagged 1 day to avoid look-ahead bias)
3. Signal generation (momentum + MA crossover + market trend)
4. Risk filter application (volatility cap, VIX threshold, persistence)
5. Portfolio construction (equal-weight, minimum 2 assets)
6. Backtesting with transaction costs (13 bps long / 20 bps short)
7. Performance evaluation (Sharpe, drawdown, turnover, win rate)


## Repository Structure

```text
/data       → Raw & processed datasets
/code       → Trading strategy scripts
/output     → Performance metrics and results
/figures    → Plots and diagnostics
