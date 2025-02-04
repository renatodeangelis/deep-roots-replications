library(readxl)
library(dplyr)
library(readr)
library(ggplot2)

pwt_init = read_excel("Datasets/pwt1001.xlsx")
rugged = read_csv("Datasets/rugged_data.csv")
fertility = read_csv("Datasets/world-bank-fertility-rate.csv", skip = 3)
mortality = read_csv("Datasets/world-bank-life-expectancy.csv", skip = 3)
frac = read_excel("Datasets/2003_fractionalization.xls", skip = 1)

pwt_inter = pwt_init |>
  filter(!is.na(rgdpe))

avg = function(x, country, column, start_year, end_year) {
  x |>
    filter(year >= start_year, year <= end_year, country == country) |>
    pull(column) |>
    mean(na.rm = TRUE)
}

names_to_remove = c(
  "Africa Eastern and Southern", "Africa Western and Central", "Arab World",
  "Central Europe and the Baltics", "Caribbean small states",
  "East Asia & Pacific (excluding high income)", "Early-demographic dividend",
  "East Asia & Pacific", "Europe & Central Asia (excluding high income)",
  "Europe & Central Asia", "Euro area", "European Union",
  "Fragile and conflict affected situations", "High income",
  "Heavily indebted poor countries (HIPC)", "IBRD only", "IDA & IBRD total",
  "IDA total", "IDA blend", "IDA only",
  "Latin America & Caribbean (excluding high income)",
  "Latin America & Caribbean", "Least developed countries: UN classification",
  "Low income", "Lower middle income", "Low & middle income",
  "Late-demographic dividend", "Middle East & North Africa", "Middle income",
  "Middle East & North Africa (excluding high income)", "North America",
  "OECD members", "Other small states", "Pre-demographic dividend",
  "Pacific island small states", "Post-demographic dividend", "South Asia",
  "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa",
  "Small states", "East Asia & Pacific (IDA & IBRD countries)",
  "Europe & Central Asia (IDA & IBRD countries)",
  "Latin America & the Caribbean (IDA & IBRD countries)",
  "Middle East & North Africa (IDA & IBRD countries)",
  "South Asia (IDA & IBRD)", "Sub-Saharan Africa (IDA & IBRD countries)",
  "Upper middle income", "World")

tryout = fertility |>
  left_join(mortality, by = c("Country Name", "Country Code")) |>
  filter(!(`Country Name` %in% names_to_remove)) |>
  left_join(rugged, by = c("Country Code" = "isocode")) #|>
  #left_join(frac, by = c("Country Name" = "Country"))
