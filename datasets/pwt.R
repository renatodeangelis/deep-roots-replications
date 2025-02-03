library(readxl)
library(dplyr)
library(readr)

pwt_init = read_excel("Datasets/pwt1001.xlsx")

pwt_inter = pwt_init |>
  filter(!is.na(rgdpe))

avg = function(x, country, start_year, end_year) {
  x |>
    filter(year >= start_year, year <= end_year, country == country) |>
    pull(x) |>
    mean(na.rm = TRUE)
}