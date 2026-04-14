# 🥦 Forecasting Vegetable Prices in Kalimati Market (Nepal)

Time series forecasting project on wholesale vegetable prices from Kalimati Market (Kathmandu), comparing multiple seasonal models for short-term price prediction.

> **Course:** Business Economics and Financial Data — University of Padova  
> **Author:** Simone Zanetti  
> **Date:** September 2025

---

## 📌 Objective

Forecast monthly average prices of **Cauli Local (cauliflower)** in the Kalimati wholesale market using a range of time series approaches, and compare their predictive accuracy via standard metrics (RMSE, MAE, MAPE).

---

## 📁 Repository Structure

```
Kalimati-Price-Forecasting/
├── Kalimati_Forecasting.Rmd    # Full analysis and modeling code
├── Report/
│   └── Simone_Zanetti_ProgettoBEFD.pdf
└── README.md
```

---

## 📊 Dataset

- **Source:** [Kalimati Tarkari Dataset](https://www.kaggle.com/datasets/sagyamthapa/kalimati-tarkari-dataset) (Kaggle)
- **Coverage:** June 16, 2013 – May 13, 2021 (daily records, ~2,750 obs per commodity)
- **Focus commodity:** Cauli Local (cauliflower)
- **Preprocessing:** Daily → monthly aggregation to reduce noise and highlight seasonal structure

---

## 🤖 Models Applied

| Model | MAE | MAPE |
|---|---|---|
| **GAM (log + cyclic season + cabbage)** | **8.6** | **20.2%** |
| STL + ETS | 13.7 | 31.2% |
| ETS | 14.2 | 32.5% |
| TSLM (+ Cabbage + Potato + Onion) | 14.0 | 33.3% |
| TSLM (+ Cabbage) | 15.0 | 34.0% |
| Seasonal ARIMA | 15.6 | 34.4% |
| SARIMAX (+ Cabbage) | 20.1 | 40.4% |
| TSLM (trend + season) — baseline | 18.2 | 42.1% |
| GAM (linear spline + month + cabbage) | 26.3 | 50.7% |

Evaluation on a 12-month hold-out window (2018).

---

## 📈 Key Findings

- Cauliflower prices exhibit **strong annual seasonality** (peaks Jul–Oct), a mild upward trend, and increasing volatility post-2018
- The **log-GAM with cyclic seasonality** achieves the best accuracy (MAPE ~20%), cutting error by more than half vs. the baseline
- Among univariate models, **STL+ETS** is the most robust (MAPE ~31%)
- Adding exogenous regressors (cabbage prices) improves in-sample fit but can be unstable out-of-sample
- **Gradient Boosting** (MAPE ~29%) does not outperform classical seasonal models on this short, highly seasonal series

---

## 🛠️ Tools & Libraries

- **Language:** R  
- **Main libraries:** `forecast`, `fpp3`, `tsibble`, `mgcv`, `ggplot2`, `tidyverse`, `xgboost`
