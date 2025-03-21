library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(haven)

pwt_71 = read_csv("datasets/pwt71_wo_country_names_wo_g_vars.csv") |>
  janitor::clean_names() |>
  select(isocode, year, pop, rgdpch, rgdpwok, kc, kg, ki, openk) |>
  group_by(isocode) |>
  mutate(gr_rgdpwok = (rgdpwok - lag(rgdpwok)) / lag(rgdpwok),
         period = case_when(
           year %in% 1970:1984 ~ "1975-1984",
           year %in% 1985:1994 ~ "1985-1994",
           year %in% 1995:2004 ~ "1995-2004")) |>
  relocate(period, .after = year) |>
  relocate(gr_rgdpwok, .after = rgdpwok) |>
  filter(!is.na(gr_rgdpwok),
         year >= 1970 & year <= 2004) |>
  ungroup()

pwt_61 = read_excel("datasets/dkt-ej-to-be-distributed.xls") |>
  mutate(across(dum6575:ssafr, as.numeric))