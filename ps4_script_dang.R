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







