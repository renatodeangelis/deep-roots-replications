library(readxl)
library(dplyr)
library(readr)

pwt_init = read_excel("Datasets/pwt1001.xlsx")

pwt_inter = pwt_init |>
  filter(!is.na(rgdpe))


