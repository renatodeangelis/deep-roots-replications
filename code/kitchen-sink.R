library(tidyverse)
library(ivreg)

pwt = read_csv("datasets/pwt_analytic.csv")
wb_regions = read_csv("datasets/world-regions-according-to-the-world-bank.csv")

pwt_merge = pwt |>
  left_join(wb_regions, by = c("country_code" = "Code")) |>
  select(-Entity, -Year) |>
  rename(wb_region = `World Region according to the World Bank`)
  
lm(growth ~ pop_growth + schooling + domestic_investment +
     fertility + life_expectancy + trade_to_gdp + govt_consumption +
     cum_inflation + `World Region according to the World Bank` + eastern +
     hindu + jewish + muslim + orthodox + protestant + other + near_coast +
     tropical + language + ethnic + avexpr,
          data = pwt_merge) |>
  summary()

