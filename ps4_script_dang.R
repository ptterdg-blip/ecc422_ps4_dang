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

