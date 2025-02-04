library(readxl)
library(dplyr)
library(readr)
library(ggplot2)

pwt_init = read_excel("Datasets/pwt1001.xlsx")

pwt_inter = pwt_init |>
  filter(!is.na(rgdpe))

avg = function(x, country, column, start_year, end_year) {
  x |>
    filter(year >= start_year, year <= end_year, country == country) |>
    pull(column) |>
    mean(na.rm = TRUE)
}