library(tidyverse)
library(fpp3)
library(BVAR)

liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

# level of sales
liquor |>  
  autoplot(sales) +
  labs(title = "U.S. Retail Sales: Beer, Wine & Liquor Stores (NSA)",
       x = "Month", y = "Millions of dollars")

# Season plot of log sales
liquor |> 
  gg_season(log(sales)) + 
  labs(title = "Seasonal Plot: Log of Liquor Store Sales",
       x = "Month", y = "log(sales)"
  )
# The series trend rises steadily over the whole sample, as nominal sales grow with prices and population. On top of that,
# there is a clear seasonal pattern where the most liquor store sales from November to December.
# In contrast, January to February illustrates the lowest liquor sales most likely from post holiday drops. We log sales since the swings growth with the level, and this transformation turns multiplicative effects into additive ones.

# 2.2 Fit both models

liquor_fit <- liquor |> 
  model(arima = ARIMA(log(sales)),
        ets = ETS(log(sales)))

liquor_fit |>  select(arima) |> report()
liquor_fit |>  select(ets) |>  report()

# The ARIMA model specifications ARIMA(1,0,2)(2,1,1)[12], meaning that there is one seasonal difference and no regular difference.
# The model works with log(sales) - log(sales)t-12, which is roughly the year-over-year growth rate.
# Furthermore, there is a drift where the constant is 0.0036 indicating that the year-over-year growht has a nonzero average.
# ar1 = 0.8948 meaning that the year-over-year growth is very persistent: a good year tends to be followed by another good year.

# ETS(A,A,A) on log sales means that there are additive error, additive trend(not damped), and additive seasonality.
# The smoothing parameters alpha = 0.33 (the level updates moderately with each new observation), beta = 0.0076, and gamma = 0.0001 (the seasonal pattern is essentially fixed for all 28 years)

liquor_fit |>  select(arima) |>  gg_tsresiduals()

liquor_fit |>  
  select(arima) |> 
  augment() |> 
  features(.innov, ljung_box, lag = 24, dof = 6)
# Q = 120, p-value = 0, so we reject white noise.

# 2.3 The Horse Race

liquor_cv <- liquor |> 
  stretch_tsibble(.init = 120, .step = 1) |> 
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |> 
  model(snaive = SNAIVE(sales), 
        ets = ETS(log(sales)),
        arima = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")
# There are 216 forecast origins. The sample has 336 months which we deduct 120 for the first training set, plus 1, hence giving 217 training sets.
# You drop the last one because there's no 2020 Jan actual in the sample to compare against.

liquor_cv_fc <- liquor_cv_fit |> 
  forecast(h = 1)

liquor_cv_fc |> 
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |> 
  knitr::kable(digits = 2)

# The first forecast: 2002 Jan, from the training set ending 2001 Dec. The last forecast: 2019 Dec, from the training set ending 2019 Nov.
# The ARIMA model forecasts most accurately on both metrics. Its RMSE is about 14% lower than ETS and about half of SNAIVE's. Both fable models easily beat the benchmark, which shows that using recent momentum and the trend is worth a lot compared wit just copying last year's same month.
# The ARIMA failed the Ljung-Box test but still won the horse race. This explains how leftover residual structure doesn't mean the model forecasts badly. 
# Out-of-sample accuracy is the test that matters.

# 2.4 Are the Differences Significant?

liquor_errs <- liquor_cv_fc |>
  as_tibble() |> 
  left_join(as_tibble(liquor) |>  select(Month, actual = sales),
            by = "Month") |> 
  mutate(e = actual - .mean) |> 
  select(Month, .model, e) |> 
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)
forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)

#  The DM statistic for ARIMA vs. ETS is -2.9062 with a p-value of 0.004, while the ARIMA vs. SNAIVE
# DM statistic was -10.75 and the p-value is close to 0. These results mean that we reject equal accuracy at 1% level. The negative sign means that ARIMA is significantly more accurate, so the 14% RMSE gap isn't just noise.
# for ARIMA vs. SNAIVE, there is an overwhelming rejection, where the ARIMA is far more accurate than the naive benchmark.
# The liquor store should use the ARIMA model since it is ranked first on both RMSE and MAE, and its lead over both alternatives is statistically significant.

# 3 Forecast Combination

liquor_combo_fc <- liquor_cv_fit |> 
  mutate(combo = (arima + ets) / 2) |> 
  forecast(h =1)

liquor_combo_fc |> 
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |> 
  knitr::kable(digits =2)

# The combination beats ETS by a lot and is close to ARIMA, but it doesn't beat ARIMA. On MAE it's nearly extremely close. 
# On RMSE, it lands between the two models, closer to ARIMA.

cor(liquor_errs$arima, liquor_errs$ets)
# The error correlation is 0.878, meaning that when ARIMA misses high, ETS generally misses high as well.
# The combination didn't win since averaging only help if the two models' error partly cancel. ETS is significantly less accurate in comparison, so equal weights over-weight the weaker model.
# Furthermore, combing still acts as insurance since in real time, you wouldn't know ahead of time which model would win, and the combo got almost all of ARIMA's accuracy without your having to pick a winner.

qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |> 
  slice(-(1:2))

growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]

# There are 120 forecasts per variable. The first is 1990 Q1 (1990-03-01), and the last is 2019 Q4 (2019-12-01)

var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)

# VAR(1) wins for 7 out of the 8 variables. The only exception is inflation, where VAR(4) is ahead by 0.001, which is effectively a tie.
# The biggest gaps are the fed fund rate (0.390 vs. 0.497), which is about 22% worse for VAR(4)) and employment (0.196 vs. 0.228). 
# The smaller model can perform better since VAR(1) has 72 coefficients and VAR(4) has 264. To expand on this, in the first training
# window, each equation in the VAR(4) estimates 33 coefficeints from the 116 observations which is noisy.
# The extra lags can only reduce in-sample error, and out of sample estimation noise usually outweighs whatever real dynamics they capture (bias-variance tradeoff)

bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")

set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")

var_results |> knitr::kable(digits = 3)

# 1) BVAR(4), lambda = 0.3 vs. OLS VAR(4): The BVAR wins for all 8 variables, cutting RMSE by aobut 5-8% for most and 17% for the fed funds rate. the variables 
# and lags are identical, so the only difference is the shrinkage. Pulling the 264 coefficients toward 0 trades a little bias for a big reduction in estimation noise.

# 2) How does it compare? The BVAR wins for 3 varaibles and VAR(1) wins for the other 5. So the results are relatively split. Dropping lags entirely
# and shrinking them are two ways of fixing the same overfitting problem, and here they perform about equally well.

# 3) lambda = 0.05 is too much shrinkage for most variables. Variables such as employment and inflation get much worse. Those series are persistent, and forcing their own-lag coefficients toward 0 throws away real dynamics.
# some shrinkage helps, but too much hurts.
# The winner's RMSE is not a fair estimate of future accuracy. If you pick the lowest RMSE out of 4 models on the same 120 forecasts you're using to evaluate them, you're partly picking whichever model got lucky over the 1990-2019 (Selection Bias).

e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]
dm_gdp <- forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

# Second variable:
e_var4_infl <- actual[, "infl"] - fc_var4[, 5]
e_bvar_infl <- actual[, "infl"] - fc_bvar_03[, 5]
dm_infl <- forecast::dm.test(e_var4_infl, e_bvar_infl, h = 1, power = 2)
saveRDS(list(gdp = dm_gdp, infl = dm_infl), "results/dm_var.rds")
forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)





