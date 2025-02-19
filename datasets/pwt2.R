library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(haven)

pwt_init = read_excel("Datasets/pwt1001.xlsx") |>
  mutate(rgdppw = rgdpe / emp) |>
  group_by(country) |>
  mutate(gr_rgdppw = (rgdppw - lag(rgdppw)) / lag(rgdppw),
         period = case_when(
           year %in% 1970:1984 ~ "1975-1984",
           year %in% 1985:1994 ~ "1985-1994",
           year %in% 1995:2004 ~ "1995-2004",
           year %in% 2005:2014 ~ "2005-2014"),
         trade = (abs(csh_x) + abs(csh_m)) * rgdpe,
         govt = csh_g * rgdpe,
         gcf = csh_i * rgdpe) |>
  relocate(period, .after = year) |>
  select(-starts_with("i_"), -starts_with("pl_"), -statcap, -cor_exp, -avh,
         -currency_unit, -xr) |>
  filter(year >= 1970 & year <= 2014,
         !is.na(gr_rgdppw),
         countrycode != "BMU",
         govt <= 0)

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
  "Upper middle income", "World", "Channel Islands", "Curacao", "Isle of Man",
  "Not classified", "St. Martin (French part)", "Sint Maarten (Dutch part)")

fertility = read_csv("Datasets/world-bank-fertility-rate.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "fertility") |>
  janitor::clean_names() |>
  filter(year >= 1970 & year <= 2014,
         !(country_name %in% names_to_remove)) |>
  mutate(year = as.numeric(year)) |>
  select(-indicator_name, -indicator_code, -country_name)

mortality = read_csv("Datasets/world-bank-life-expectancy.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "life_expectancy") |>
  janitor::clean_names() |>
  filter(year >= 1970 & year <= 2014,
         !(country_name %in% names_to_remove)) |>
  mutate(year = as.numeric(year)) |>
  select(-indicator_name, -indicator_code, -country_name)

inflation = read_csv("Datasets/inflation.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "inflation") |>
  janitor::clean_names() |>
  filter(year >= 1970 & year <= 2014,
         !(country_name %in% names_to_remove)) |>
  mutate(year = as.numeric(year)) |>
  select(-indicator_name, -indicator_code, -country_name)

rugged = read_csv("Datasets/rugged_data.csv") |>
  select(isocode, near_coast, tropical, colony_esp, colony_prt) |>
  mutate(colony_esp_prt = colony_esp + colony_prt) |>
  select(-colony_esp, -colony_prt)

pwt_wb = pwt_init |>
  left_join(fertility, by = c("countrycode" = "country_code", "year")) |>
  left_join(mortality, by = c("countrycode" = "country_code", "year")) |>
  left_join(inflation, by = c("countrycode" = "country_code", "year")) |>
  left_join(rugged, by = c("countrycode" = "isocode"))










