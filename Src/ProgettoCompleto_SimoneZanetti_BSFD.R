suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(knitr)
  library(ggplot2)
  library(tidyr)
  library(forecast)
  library(mgcv)
  library(xgboost)
})

data_path <- "kalimati_tarkari_dataset.csv"

if (!file.exists(data_path)) {
  stop(paste0("File not found: ", data_path,
              "\nPlace kalimati_tarkari_dataset.csv in the working directory and knit again."))
}

raw <- readr::read_csv(data_path, show_col_types = FALSE)

dat <- raw %>%
  dplyr::rename(
    sn        = SN,
    commodity = Commodity,
    date      = Date,
    unit      = Unit,
    min_price = Minimum,
    max_price = Maximum,
    avg_price = Average
  ) %>%
  dplyr::mutate(
    date      = lubridate::ymd(date),
    commodity = as.character(commodity),
    unit      = as.character(unit),
    min_price = as.numeric(min_price),
    max_price = as.numeric(max_price),
    avg_price = as.numeric(avg_price)
  )

#Global range and missingness
date_range <- range(dat$date, na.rm = TRUE)
na_avg     <- sum(is.na(dat$avg_price))

cat(sprintf("**Date range:** %s to %s  \n", date_range[1], date_range[2]))
cat(sprintf("**Missing avg_price:** %d\n\n", na_avg))

# Coverage by commodity
coverage <- dat %>%
  dplyr::group_by(commodity) %>%
  dplyr::summarise(
    n_obs      = dplyr::n(),
    start_date = min(date, na.rm = TRUE),
    end_date   = max(date, na.rm = TRUE),
    na_avg     = sum(is.na(avg_price)),
    .groups    = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(n_obs), na_avg, start_date)

kable(head(coverage, 20), caption = "Top-20 commodities by observation count (daily coverage)")

focus <- dat %>%
  dplyr::filter(commodity == "Cauli Local") %>%
  dplyr::arrange(date)

#Check summary
summary(focus$avg_price)
range(focus$date)

#Aggragate
monthly <- focus %>%
  dplyr::mutate(year_month = floor_date(date, "month")) %>%
  dplyr::group_by(year_month) %>%
  dplyr::summarise(
    avg_price_m = mean(avg_price, na.rm = TRUE),
    .groups = "drop"
  )
#check
head(monthly)
tail(monthly)
ggplot(monthly, aes(year_month, avg_price_m)) +
  geom_line(color = "black", linewidth = 0.3) +  
  labs(
    title = NULL,                            
    x = NULL, 
    y = "Average price (NRs per Kg)"
  ) +
  theme_gray(base_size = 12) 

monthly %>%
  mutate(month = factor(month(year_month), labels = month.abb),
         year = year(year_month)) %>%
  ggplot(aes(x = month, y = avg_price_m, group = year, color = as.factor(year))) +
  geom_line() +
  labs(x = "Month", y = "Average price (NRs per Kg)", color = "Year",
       title = "Seasonality of Cauli Local prices") +
  theme_gray(base_size = 12)

monthly_full <- monthly %>%
  tidyr::complete(year_month = seq.Date(min(year_month), max(year_month), by = "month")) %>%
  dplyr::arrange(year_month)

monthly_full$avg_price_m_imp <- forecast::na.interp(ts(monthly_full$avg_price_m, frequency = 12))

start_year  <- lubridate::year(min(monthly_full$year_month))
start_month <- lubridate::month(min(monthly_full$year_month))
x_ts <- ts(monthly_full$avg_price_m_imp,
           start = c(start_year, start_month),
           frequency = 12)

par(mfrow = c(1,2), mar = c(4,4,2,1)) 

acf(x_ts, lag.max = 36,
    xaxt = "n",                   
    xlab = "Lag (Months)",
    main = "ACF — Cauli Local (Months)")
axis(1,
     at = seq(0, 36/12, by = 6/12),  
     labels = seq(0, 36, by = 6))    


pacf(x_ts, lag.max = 36,
     xaxt = "n",
     xlab = "Lag (Months)",
     main = "PACF — Cauli Local (Months)")
axis(1,
     at = seq(0, 36/12, by = 6/12),
     labels = seq(0, 36, by = 6))

par(mfrow = c(1,1))

stl_fit <- stl(x_ts, s.window = "periodic")

autoplot(stl_fit) +
  labs(title = "STL Decomposition — Monthly Price Cauli Local")

library(forecast)
fit_full <- tslm(x_ts ~ trend + season)
summary(fit_full)
checkresiduals(fit_full) 

n <- length(x_ts)
start_year  <- start(x_ts)[1]
start_month <- start(x_ts)[2]

date_seq <- seq(as.Date(sprintf("%d-%02d-01", start_year, start_month)),
                by = "month", length.out = n)

df_fit <- data.frame(
  date   = date_seq,
  actual = as.numeric(x_ts),
  fitted = as.numeric(fitted(fit_full))
)


ggplot(df_fit, aes(date)) +
  geom_line(aes(y = actual), color = "black", linewidth = 0.3) +          # linea nera continua
  geom_line(aes(y = fitted), color = "black", linewidth = 0.3, linetype = 2) + # linea nera tratteggiata
  labs(
    title = NULL,                                 
    x = NULL,
    y = "Monthly average price (NRs per Kg)"      
  ) +
  theme_gray(base_size = 12) 
n <- length(x_ts)
h <- if (n >= 24) 12 else max(1, round(0.2 * n))  # robusto
train <- ts(head(x_ts, n - h), start = start(x_ts), frequency = frequency(x_ts))
test  <- ts(tail(x_ts, h),
            start = c(floor(time(x_ts)[n - h + 1]),
                      cycle(x_ts)[n - h + 1]),
            frequency = frequency(x_ts))

fit_tr  <- tslm(train ~ trend + season)
fc_tslm <- forecast(fit_tr, h = h)

# Acc out-of-sample
acc_tslm <- accuracy(fc_tslm, test)
print(acc_tslm)

autoplot(fc_tslm) +
  autolayer(test, series = "Test", color = "black") +
  labs(title = "TSLM (trend + season) — Forecast vs Test",
       x = NULL, y = "Monthly avarage price") +
  theme_minimal(base_size = 13)
make_monthly <- function(df, name){
  df %>%
    filter(commodity == name) %>%
    mutate(year_month = floor_date(date, "month")) %>%
    group_by(year_month) %>%
    summarise(value = mean(avg_price, na.rm = TRUE), .groups = "drop") %>%
    complete(year_month = seq.Date(min(year_month), max(year_month), by = "month")) %>%
    arrange(year_month) %>%
    mutate(value = na.interp(ts(value, frequency = 12))) %>%
    rename(!!name := value)
}

cauli_m <- monthly_full %>% select(year_month, avg_price_m_imp) %>% rename(cauli = avg_price_m_imp)

potato_m   <- make_monthly(dat, "Potato Red")
cabbage_m  <- make_monthly(dat, "Cabbage(Local)")
onion_m    <- make_monthly(dat, "Onion Dry (Indian)")

# Merge
X <- cauli_m %>%
  left_join(potato_m,  by = "year_month") %>%
  left_join(cabbage_m, by = "year_month") %>%
  left_join(onion_m,   by = "year_month") %>%
  drop_na() 

round(cor(select(X, cauli, `Potato Red`, `Cabbage(Local)`, `Onion Dry (Indian)`)), 2) %>%
  kable(caption = "Correlation Matrix between Average Monthly Prices")
start_year  <- year(min(X$year_month))
start_month <- month(min(X$year_month))
to_ts <- function(v) ts(v, start = c(start_year, start_month), frequency = 12)

y      <- to_ts(X$cauli)
x_pot  <- to_ts(X$`Potato Red`)
x_cab  <- to_ts(X$`Cabbage(Local)`)
x_oni  <- to_ts(X$`Onion Dry (Indian)`)

n <- length(y); h <- if (n >= 24) 12 else max(1, round(0.2 * n))
y_tr   <- ts(head(y, n - h), start = start(y), frequency = 12)
pot_tr <- ts(head(x_pot, n - h), start = start(y), frequency = 12)
cab_tr <- ts(head(x_cab, n - h), start = start(y), frequency = 12)
oni_tr <- ts(head(x_oni, n - h), start = start(y), frequency = 12)
y_te   <- ts(tail(y, h),
             start = c(floor(time(y)[n - h + 1]), cycle(y)[n - h + 1]),
             frequency = 12)

#fit tslm and reg.
fit_tslm_cab <- tslm(y_tr ~ trend + season + cab_tr)
summary(fit_tslm_cab)

# Future regressors (12 months)
cab_te <- ts(tail(x_cab, h), start = start(y_te), frequency = 12)

# Forecast
newx <- data.frame(cab_tr = as.numeric(cab_te))
fc_tslm_cab <- forecast(fit_tslm_cab, newdata = newx)

acc_tslm_cab <- accuracy(fc_tslm_cab, y_te)
acc_tslm_cab

autoplot(fc_tslm_cab) +
  autolayer(y_te, series = "Test", color = "black") +
  labs(title = "TSLM (trend + season + Cabbage) — Forecast vs Test",
       x = NULL, y = "Monthly avarage price") +
  theme_minimal(base_size = 13)

#nuovo modello
fit_tslm_x <- tslm(y_tr ~ trend + season + pot_tr + cab_tr + oni_tr)
summary(fit_tslm_x)


pot_te <- ts(tail(x_pot, h), start = start(y_te), frequency = 12)
cab_te <- ts(tail(x_cab, h), start = start(y_te), frequency = 12)
oni_te <- ts(tail(x_oni, h), start = start(y_te), frequency = 12)
newx <- data.frame(
  pot_tr = as.numeric(pot_te),
  cab_tr = as.numeric(cab_te),
  oni_tr = as.numeric(oni_te)
)

fc_tslm_x <- forecast(fit_tslm_x, newdata = newx)
acc_tslm_x <- accuracy(fc_tslm_x, y_te)


autoplot(fc_tslm_x) +
  autolayer(y_te, series = "Test", color = "black") +
  labs(title = "TSLM (trend + season + Potato + Cabbage + Onion) — Forecast vs Test",
       x = NULL, y = "Monthly avarage price") +
  theme_minimal(base_size = 13)
if (!exists("ts_cauli")) {
  ts_cauli <- ts(
    monthly$avg_price_m,
    start     = c(year(min(monthly$year_month)), month(min(monthly$year_month))),
    frequency = 12
  )
}

y <- ts_cauli; h <- 12
test_start_date <- as.Date("2018-01-01")

dates <- seq(as.Date(sprintf("%d-%02d-01", start(y)[1], start(y)[2])),
             by="month", length.out=length(y))
idx_start <- which(dates >= test_start_date)[1]
stopifnot(!is.na(idx_start), length(y) - idx_start + 1 >= h)

y_tr <- window(y, end   = c(year(dates[idx_start-1]), month(dates[idx_start-1])))
y_te <- window(y, start = c(year(dates[idx_start]),   month(dates[idx_start])),
               end   = c(year(dates[idx_start+h-1]), month(dates[idx_start+h-1])))

fit_snaive <- snaive(y_tr, h=h)
fit_ets    <- ets(y_tr, model="MMM")
fit_arima  <- auto.arima(y_tr, seasonal=TRUE, stepwise=TRUE, approximation=FALSE)

fc_snaive <- forecast(fit_snaive, h=h)
fc_ets    <- forecast(fit_ets,    h=h)
fc_arima  <- forecast(fit_arima,  h=h)

acc <- tibble(
  Model = c("Seasonal Naive","ETS (M,*,M)","SARIMA (auto)"),
  RMSE  = c(accuracy(fc_snaive, y_te)["Test set","RMSE"],
            accuracy(fc_ets,    y_te)["Test set","RMSE"],
            accuracy(fc_arima,  y_te)["Test set","RMSE"]),
  MAE   = c(accuracy(fc_snaive, y_te)["Test set","MAE"],
            accuracy(fc_ets,    y_te)["Test set","MAE"],
            accuracy(fc_arima,  y_te)["Test set","MAE"]),
  MAPE  = c(accuracy(fc_snaive, y_te)["Test set","MAPE"],
            accuracy(fc_ets,    y_te)["Test set","MAPE"],
            accuracy(fc_arima,  y_te)["Test set","MAPE"]),
  MASE  = c(accuracy(fc_snaive, y_te)["Test set","MASE"],
            accuracy(fc_ets,    y_te)["Test set","MASE"],
            accuracy(fc_arima,  y_te)["Test set","MASE"])
) %>% arrange(RMSE)

kable(acc, digits=2, caption=sprintf("Test-set accuracy (12 months) starting %s", format(test_start_date,"%Y-%m")))

# quick visual: best two vs test
df_plot <- tibble(
  date  = seq(dates[idx_start], by="month", length.out=h),
  test  = as.numeric(y_te),
  ets   = as.numeric(fc_ets$mean),
  arima = as.numeric(fc_arima$mean)
)
ggplot(df_plot, aes(date)) +
  geom_line(aes(y=test), color="black", linewidth=0.4) +
  geom_line(aes(y=ets),  linetype=2, linewidth=0.4) +
  geom_line(aes(y=arima),linetype=1, linewidth=0.4) +
  labs(x=NULL, y="NRs/kg",
       caption="Solid: SARIMA | Dashed: ETS | Black: Test (actual)")

fit_arima <- auto.arima(y_tr, seasonal = TRUE,
                        stepwise = TRUE, approximation = FALSE)
summary(fit_arima)

fc_arima <- forecast(fit_arima, h = length(y_te))

acc_arima <- accuracy(fc_arima, y_te)
acc_arima

#forecast vs test
autoplot(fc_arima) +
  autolayer(y_te, series = "Test", color = "black") +
  labs(title = "Seasonal ARIMA — Forecast vs Test",
       x = NULL, y = "Prezzo medio mensile") +
  theme_minimal(base_size = 13)

fit_ets <- ets(y_tr)                   
summary(fit_ets)
fc_ets  <- forecast(fit_ets, h = length(y_te))
acc_ets <- accuracy(fc_ets, y_te)
acc_ets

# STL decomposition + ETS residui/trend
fit_stlm <- stlm(y_tr, s.window = "periodic", method = "ets")
summary(fit_stlm)

fc_stlm  <- forecast(fit_stlm, h = length(y_te))
acc_stlm <- accuracy(fc_stlm, y_te)
acc_stlm

xreg_cab_tr <- ts(head(x_cab, length(y_tr)), start = start(y_tr), frequency = 12)
xreg_cab_te <- ts(tail(x_cab, length(y_te)),  start = start(y_te), frequency = 12)

fit_sarimax <- auto.arima(y_tr, xreg = as.numeric(xreg_cab_tr),
                          seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
summary(fit_sarimax)

fc_sarimax  <- forecast(fit_sarimax, xreg = as.numeric(xreg_cab_te), h = length(y_te))
acc_sarimax <- accuracy(fc_sarimax, y_te)
acc_sarimax

# DF date/month
n_tr <- length(y_tr); n_te <- length(y_te)
start_year  <- start(y)[1]; start_month <- start(y)[2]

date_all <- seq(as.Date(sprintf("%d-%02d-01", start_year, start_month)),
                by = "month", length.out = length(y))
df_all <- data.frame(
  date  = date_all,
  t     = 1:length(y),                
  month = factor(cycle(y)),           
  y     = as.numeric(y),
  cab   = as.numeric(x_cab)
)

df_tr <- head(df_all, n_tr)
df_te <- tail(df_all, n_te)

# GAM: spline on time (trend flexibility) + seasonality + regressor cabbage.

dates <- seq(as.Date(sprintf("%d-%02d-01", start(y)[1], start(y)[2])),
             by = "month", length.out = length(y))

df_all <- data.frame(
  date  = dates,
  t     = 1:length(y),
  month = factor(cycle(y), levels = 1:12),
  y     = as.numeric(y),
  cab   = as.numeric(x_cab)
)

idx_start <- which(dates >= test_start_date)[1]
idx_end   <- idx_start + length(y_te) - 1

df_tr <- df_all[1:(idx_start-1), ]
df_te <- df_all[idx_start:idx_end, ]
df_te$month <- factor(df_te$month, levels = levels(df_tr$month))
gam_fit <- gam(y ~ s(t, k = 12) + month + cab, data = df_tr)
gam_fc  <- predict(gam_fit, newdata = df_te, se.fit = TRUE)

gam_measures <- data.frame(
  ME   = mean(gam_fc$fit - df_te$y),
  RMSE = sqrt(mean((gam_fc$fit - df_te$y)^2)),
  MAE  = mean(abs(gam_fc$fit - df_te$y)),
  MAPE = mean(abs((gam_fc$fit - df_te$y) / df_te$y)) * 100
)
gam_measures


# month numerico 1..12 - cycle

df_tr$m  <- as.numeric(df_tr$month)
df_te$m  <- as.numeric(df_te$month)

gam_log <- mgcv::gam(log(y) ~ s(t, k=10) + s(m, bs="cc", k=12) + cab,
                     data=df_tr, method="REML", select=TRUE)

pred_log <- predict(gam_log, newdata=df_te, se.fit=TRUE)
yhat     <- exp(pred_log$fit) 

gam2 <- data.frame(
  ME   = mean(yhat - df_te$y),
  RMSE = sqrt(mean((yhat - df_te$y)^2)),
  MAE  = mean(abs(yhat - df_te$y)),
  MAPE = mean(abs((yhat - df_te$y) / df_te$y))*100
)
gam2


autoplot(fc_ets) +
  autolayer(y_te, series = "Test", color = "black") +
  labs(title = "ETS — Forecast vs Test",
       x = NULL, y = "Prezzo medio mensile") +
  theme_minimal(base_size = 13)

# STL + ETS
autoplot(fc_stlm) +
  autolayer(y_te, series = "Test", color = "black") +
  labs(title = "STL + ETS — Forecast vs Test",
       x = NULL, y = "Prezzo medio mensile") +
  theme_minimal(base_size = 13)

# SARIMAX (+ Cabbage)
autoplot(fc_sarimax) +
  autolayer(y_te, series = "Test", color = "black") +
  labs(title = "SARIMAX (with Cabbage) — Forecast vs Test",
       x = NULL, y = "Prezzo medio mensile") +
  theme_minimal(base_size = 13)

# Seasonal ARIMA

autoplot(fc_arima) +
  autolayer(y_te, series = "Test", color = "black") +
  labs(title = "Seasonal ARIMA — Forecast vs Test",
       x = NULL, y = "Prezzo medio mensile") +
  theme_minimal(base_size = 13)


# GAM (spline + month + cabbage)
df_te$gam_fit <- gam_fc$fit
ggplot(df_te, aes(date)) +
  geom_line(aes(y = y), color = "black", linewidth = 0.8) +   # actual
  geom_line(aes(y = gam_fit), color = "tomato", linewidth = 0.8, linetype = 2) +
  labs(title = "GAM (spline + month + cabbage) — Forecast vs Test",
       x = NULL, y = "Prezzo medio mensile") +
  theme_minimal(base_size = 13)


# GAM (log + cyclic season + cabbage)
df_te$gam_fit_log <- yhat  # già back-transform con exp()
ggplot(df_te, aes(date)) +
  geom_line(aes(y = y), color = "black", linewidth = 0.8) +   # actual
  geom_line(aes(y = gam_fit_log), color = "blue", linewidth = 0.8, linetype = 2) +
  labs(title = "GAM (log + cyclic season + cabbage) — Forecast vs Test",
       x = NULL, y = "Prezzo medio mensile") +
  theme_minimal(base_size = 13)




# Predizioni
pred_log <- predict(gam_log, newdata = df_te, se.fit = TRUE)
mu  <- pred_log$fit
se  <- pred_log$se.fit

mean_bt <- exp(mu + 0.5 * se^2)
lo_bt   <- exp(mu - 1.96 * se)
hi_bt   <- exp(mu + 1.96 * se)

fc_gam_log <- tibble::tibble(
  date = df_te$date,
  mean = mean_bt,
  lo   = lo_bt,
  hi   = hi_bt
)

hist <- tibble::tibble(date = df_tr$date, y = df_tr$y)
test <- tibble::tibble(date = df_te$date, y = df_te$y)

ggplot() +
  geom_line(data = hist, aes(date, y), color = "black", linewidth = 0.4) +
  geom_ribbon(data = fc_gam_log, aes(date, ymin = lo, ymax = hi),
              fill = "blue", alpha = 0.25) +
  geom_line(data = fc_gam_log, aes(date, mean), color = "steelblue4", linewidth = 0.7) +
  geom_line(data = test, aes(date, y), color = "black", linewidth = 0.7) +
  labs(title = "GAM (log + cyclic season + cabbage) — Forecast vs Test",
       x = NULL, y = "Prezzo medio mensile") +
  theme_minimal(base_size = 13)

compare <- rbind(
  data.frame(Model = "TSLM: trend+season",         MAE = acc_tslm["Test set","MAE"],  MAPE = acc_tslm["Test set","MAPE"]),
  data.frame(Model = "TSLM: + Cabbage",            MAE = acc_tslm_cab["Test set","MAE"], MAPE = acc_tslm_cab["Test set","MAPE"]),
  data.frame(Model = "TSLM: + Cabb+Pot+Onion",     MAE = acc_tslm_x["Test set","MAE"],   MAPE = acc_tslm_x["Test set","MAPE"]),
  data.frame(Model = "ARIMA (seasonal)",           MAE = acc_arima["Test set","MAE"],    MAPE = acc_arima["Test set","MAPE"]),
  data.frame(Model = "ETS",                        MAE = acc_ets["Test set","MAE"],      MAPE = acc_ets["Test set","MAPE"]),
  data.frame(Model = "STL + ETS (stlm)",           MAE = acc_stlm["Test set","MAE"],     MAPE = acc_stlm["Test set","MAPE"]),
  data.frame(Model = "SARIMAX (+ Cabbage)",        MAE = acc_sarimax["Test set","MAE"],  MAPE = acc_sarimax["Test set","MAPE"]),
  data.frame(Model = "GAM (spline t + month + cab)", 
             MAE = gam_measures$MAE, MAPE = gam_measures$MAPE),
  data.frame(Model = "GAM (log + cyclic season + cabbage)", 
             MAE = gam2$MAE, MAPE = gam2$MAPE)  
)

compare[order(compare$MAPE), ] %>%
  kable(digits = 3, caption = "Forecasting model comparison (sorted by MAPE)")

y_log <- log(y)   
h <- length(y_te) 

y_tr_log <- window(y_log, end = end(y_tr))
y_te_log <- window(y_log, start = start(y_te))

inv_exp <- function(z) exp(z)

fit_ets_log <- ets(y_tr_log)
fc_ets_log  <- forecast(fit_ets_log, h = h)
fc_ets_bt   <- inv_exp(fc_ets_log$mean)

fit_stlm_log <- stlm(y_tr_log, s.window = "periodic", method = "ets")
fc_stlm_log  <- forecast(fit_stlm_log, h = h)
fc_stlm_bt   <- inv_exp(fc_stlm_log$mean)

fit_arima_log <- auto.arima(y_tr_log, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
fc_arima_log  <- forecast(fit_arima_log, h = h)
fc_arima_bt   <- inv_exp(fc_arima_log$mean)

y_te_orig <- as.numeric(y_te)

acc_backtransform <- function(fc_bt, y_te){
  data.frame(
    ME   = mean(fc_bt - y_te),
    RMSE = sqrt(mean((fc_bt - y_te)^2)),
    MAE  = mean(abs(fc_bt - y_te)),
    MAPE = mean(abs((fc_bt - y_te) / y_te)) * 100
  )
}

#metriche
acc_ets_log   <- acc_backtransform(fc_ets_bt,   y_te_orig)
acc_stlm_log  <- acc_backtransform(fc_stlm_bt,  y_te_orig)
acc_arima_log <- acc_backtransform(fc_arima_bt, y_te_orig)

compare_log <- bind_rows(
  data.frame(Model = "ETS (log)",      acc_ets_log),
  data.frame(Model = "STL+ETS (log)",  acc_stlm_log),
  data.frame(Model = "SARIMA (log)",   acc_arima_log)
)

kable(compare_log, digits = 2, caption = "Accuracy of models fitted on log-transformed series (back-transformed to original scale)")

df <- monthly %>%
  rename(y = avg_price_m) %>%
  mutate(month = month(year_month),
         year  = year(year_month)) %>%
  left_join(cabbage_m, by = "year_month") %>%
  left_join(potato_m,  by = "year_month") %>%
  rename(cabbage = `Cabbage(Local)`,
         potato  = `Potato Red`)

df <- df %>%
  arrange(year_month) %>%
  mutate(
    # Lags price
    lag1  = dplyr::lag(y, 1),
    lag2  = dplyr::lag(y, 2),
    lag3  = dplyr::lag(y, 3),
    lag6  = dplyr::lag(y, 6),
    lag12 = dplyr::lag(y, 12),
    
    ma3 = zoo::rollmean(y, 3, fill = NA, align = "right"),
    ma6 = zoo::rollmean(y, 6, fill = NA, align = "right"),
    
    # Regressori + lag
    cab_lag1 = dplyr::lag(cabbage, 1),
    pot_lag1 = dplyr::lag(potato,  1),
    
    # Fourier
    sin1 = sin(2*pi*month/12),  cos1 = cos(2*pi*month/12),
    sin2 = sin(4*pi*month/12),  cos2 = cos(4*pi*month/12),
    
    # Target 
    y_log = log(pmax(y, 1e-6))
  )

df <- na.omit(df) 
train <- df %>% filter(year_month < as.Date("2020-01-01"))
test  <- df %>% filter(year_month >= as.Date("2020-01-01"))

X_cols <- c("lag1","lag2","lag3","lag6","lag12","ma3","ma6",
            "cabbage","potato","cab_lag1","pot_lag1",
            "sin1","cos1","sin2","cos2","month","year")

X_train <- as.matrix(train[, X_cols])
X_test  <- as.matrix(test[,  X_cols])

y_train_log <- train$y_log
y_test      <- test$y 

dtrain <- xgb.DMatrix(data = X_train, label = y_train_log)
dtest  <- xgb.DMatrix(data = X_test,  label = log(pmax(y_test,1e-6)))

params <- list(
  objective = "reg:squarederror",
  booster   = "gbtree",
  eta       = 0.05,
  max_depth = 6,
  subsample = 0.9,
  colsample_bytree = 0.9,
  min_child_weight = 3
)

set.seed(42)
cv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 2000,
  nfold = 5,
  metrics = "rmse",
  early_stopping_rounds = 50,
  verbose = 0
)
best_nrounds <- cv$best_iteration

fit_gb_log <- xgb.train(
  params = params,
  data   = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain),
  verbose = 0
)

pred_log <- predict(fit_gb_log, dtest)   
pred_bt  <- exp(pred_log)               

acc_gb2 <- data.frame(
  ME   = mean(pred_bt - y_test),
  RMSE = sqrt(mean((pred_bt - y_test)^2)),
  MAE  = mean(abs(pred_bt - y_test)),
  MAPE = mean(abs((pred_bt - y_test)/y_test))*100
)
kable(acc_gb2, digits = 2, caption = "Gradient Boosting (rich features, log-target) — Test accuracy")

plot_df <- data.frame(
  date   = test$year_month,
  actual = y_test,
  gb     = pred_bt
)

ggplot(plot_df, aes(date)) +
  geom_line(aes(y = actual), color = "black", linewidth = 0.7) +
  geom_line(aes(y = gb), color = "darkred", linetype = 2, linewidth = 0.7) +
  labs(title = "Gradient Boosting (log-target) — Forecast vs Test",
       x = NULL, y = "NRs/kg") +
  theme_minimal(base_size = 13)
imp <- xgb.importance(model = fit_gb_log)
knitr::kable(head(imp, 10), digits = 3, caption = "Top-10 feature importance (XGBoost)")
