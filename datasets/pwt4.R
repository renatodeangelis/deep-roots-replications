library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(haven)

pwt_init = read_excel("datasets/pwt61_data.xlsx", sheet = 2) |>
  janitor::clean_names() |>
  select(country_isocode, year, pop, csave, rgdpl, rgdpch, grgdpch, 
         rgdpwok, openk, kc, kg, ki) |>
  rename(isocode = country_isocode) |>
  group_by(isocode) |>
  mutate(across(pop:ki, ~na_if(.x, "na")),
         across(year:ki, as.numeric),
         gr_rgdpwok = (rgdpwok - lag(rgdpwok)) / lag(rgdpwok),
         period = case_when(
           year %in% 1960:1974 ~ "1965-1974",
           year %in% 1975:1984 ~ "1975-1984",
           year %in% 1985:1994 ~ "1985-1994")) |>
  relocate(period, .after = year) |>
  relocate(gr_rgdpwok, .after = rgdpwok) |>
  filter(!is.na(gr_rgdpwok),
         year >= 1960 & year <= 1994) |>
  ungroup()

wb_regions = read_csv("datasets/world-regions-according-to-the-world-bank.csv")

schooling = read_csv("datasets/BL2013_MF2599_v2.2.csv") |>
  select(WBcode, year, yr_sch)

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

inflation = read_csv("Datasets/inflation.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "inflation") |>
  janitor::clean_names() |>
  filter(year >= 1970 & year <= 2004,
         !(country_name %in% names_to_remove)) |>
  mutate(year = as.numeric(year)) |>
  select(-indicator_name, -indicator_code, -country_name)

enrollment = read_csv("datasets/API_SE.SEC.ENRR_DS2_en_csv_v2_14171.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "enrollment") |>
  janitor::clean_names() |>
  filter(year >= 1970 & year <= 2004,
         !(country_name %in% names_to_remove)) |>
  mutate(year = as.numeric(year)) |>
  select(-indicator_name, -indicator_code, -country_name)

pwt_inter = pwt_init |>
  mutate(isocode = case_when(
    isocode == "ZAR" ~ "COD",
    TRUE ~ isocode)) |>
  left_join(inflation, by = c("isocode" = "country_code", "year")) |>
  left_join(enrollment, by = c("isocode" = "country_code", "year")) |>
  left_join(rugged, by = c("isocode")) |>
  left_join(schooling, by = c("isocode" = "WBcode", "year")) |>
  left_join(wb_regions, by = c("isocode" = "Code")) |>
  select(-Entity, -Year) |>
  rename(wb_region = `World Region according to the World Bank`)

pwt_mid = pwt_inter |>
  mutate(period_1 = ifelse(period == "1965-1974", 1, 0),
         period_2 = ifelse(period == "1975-1984", 1, 0),
         period_3 = ifelse(period == "1985-1994", 1, 0)) |>
  group_by(isocode) |>
  complete(year = full_seq(year, 1)) |>
  fill(yr_sch, .direction = "down") |>
  ungroup()

pwt_instr = pwt_mid |>
  group_by(isocode) |>
  mutate(
    initial_gdp = case_when(
      period == "1975-1984" & any(year == 1975) ~ first(rgdpwok[year == 1975]),
      period == "1985-1994" & any(year == 1985) ~ first(rgdpwok[year == 1985]),
      period == "1995-2004" & any(year == 1995) ~ first(rgdpwok[year == 1995]),
      TRUE ~ NA_real_),
    initial_gdp_instr = case_when(
      period == "1975-1984" & any(year == 1970) ~ first(rgdpwok[year == 1970]),
      period == "1985-1994" & any(year == 1980) ~ first(rgdpwok[year == 1980]),
      period == "1995-2004" & any(year == 1990) ~ first(rgdpwok[year == 1990]),
      TRUE ~ NA_real_),
    initial_hc = case_when(
      period == "1975-1984" & any(year == 1975) ~ first(yr_sch[year == 1975]),
      period == "1985-1994" & any(year == 1985) ~ first(yr_sch[year == 1985]),
      period == "1995-2004" & any(year == 1995) ~ first(yr_sch[year == 1995]),
      TRUE ~ NA_real_),
    initial_hc_instr = case_when(
      period == "1975-1984" & any(year == 1970) ~ first(yr_sch[year == 1970]),
      period == "1985-1994" & any(year == 1980) ~ first(yr_sch[year == 1980]),
      period == "1995-2004" & any(year == 1990) ~ first(yr_sch[year == 1990]),
      TRUE ~ NA_real_),
    fertility = case_when(
      period == "1975-1984" & any(year == 1970) ~ first(fertility[year == 1970]),
      period == "1985-1994" & any(year == 1980) ~ first(fertility[year == 1980]),
      period == "1995-2004" & any(year == 1990) ~ first(fertility[year == 1990]),
      TRUE ~ NA_real_),
    life_expectancy = case_when(
      period == "1975-1984" & any(year == 1970) ~ first(life_expectancy[year == 1970]),
      period == "1985-1994" & any(year == 1980) ~ first(life_expectancy[year == 1980]),
      period == "1995-2004" & any(year == 1990) ~ first(life_expectancy[year == 1990]),
      TRUE ~ NA_real_))

pwt_instr2 = pwt_instr |>
  group_by(isocode) |>
  summarise(
    year = year,
    pop_growth = case_when(
      period == "1975-1984" ~ (first(pop[year == 1984]) - first(pop[year == 1975])) / first(pop[year == 1975]),
      period == "1985-1994" ~ (first(pop[year == 1994]) - first(pop[year == 1985])) / first(pop[year == 1985]),
      period == "1995-2004" ~ (first(pop[year == 2004]) - first(pop[year == 1995])) / first(pop[year == 1995]),
      TRUE ~ NA_real_),
    pop_growth_instr = case_when(
      period == "1975-1984" ~ (first(pop[year == 1974]) - first(pop[year == 1970])) / first(pop[year == 1970]),
      period == "1985-1994" ~ (first(pop[year == 1984]) - first(pop[year == 1980])) / first(pop[year == 1980]),
      period == "1995-2004" ~ (first(pop[year == 1994]) - first(pop[year == 1990])) / first(pop[year == 1990]),
      TRUE ~ NA_real_),
    growth_rate = case_when(
      period == "1975-1984" ~ mean(gr_rgdpwok[year %in% 1975:1984], na.rm = FALSE),
      period == "1985-1994" ~ mean(gr_rgdpwok[year %in% 1985:1994], na.rm = FALSE),
      period == "1995-2004" ~ mean(gr_rgdpwok[year %in% 1995:2004], na.rm = FALSE),
      TRUE ~ NA_real_),
    ki_instr = case_when(
      period == "1975-1984" ~ mean(ki[year %in% 1970:1975], na.rm = FALSE),
      period == "1985-1994" ~ mean(ki[year %in% 1980:1985], na.rm = FALSE),
      period == "1995-2004" ~ mean(ki[year %in% 1990:1995], na.rm = FALSE),
      TRUE ~ NA_real_),
    openk_instr = case_when(
      period == "1975-1984" ~ mean(openk[year %in% 1970:1975], na.rm = FALSE),
      period == "1985-1994" ~ mean(openk[year %in% 1980:1985], na.rm = FALSE),
      period == "1995-2004" ~ mean(openk[year %in% 1990:1995], na.rm = FALSE),
      TRUE ~ NA_real_),
    kg_instr = case_when(
      period == "1975-1984" ~ mean(kg[year %in% 1970:1975], na.rm = FALSE),
      period == "1985-1994" ~ mean(kg[year %in% 1980:1985], na.rm = FALSE),
      period == "1995-2004" ~ mean(kg[year %in% 1990:1995], na.rm = FALSE),
      TRUE ~ NA_real_),
    enrollment_instr = case_when(
      period == "1975-1984" ~ mean(enrollment[year %in% 1970:1975], na.rm = FALSE),
      period == "1985-1994" ~ mean(enrollment[year %in% 1980:1985], na.rm = FALSE),
      period == "1995-2004" ~ mean(enrollment[year %in% 1990:1995], na.rm = FALSE),
      TRUE ~ NA_real_)) |>
  mutate_all(~ifelse(is.nan(.), NA, .)) |>
  left_join(pwt_instr, by = c("isocode", "year"))

pwt_analytic = pwt_instr2 |>
  group_by(country, isocode, period) |>
  filter(!(year %in% 1970:1974),
         year != 2005) |>
  mutate(year_count = n()) |>
  filter(year_count == 10) |>
  mutate(
    cum_inflation = case_when(
      period == "1975-1984" ~ prod(1 + inflation[year %in% 1975:1984] / 100, na.rm = FALSE) - 1,
      period == "1985-1994" ~ prod(1 + inflation[year %in% 1985:1994] / 100, na.rm = FALSE) - 1,
      period == "1995-2004" ~ prod(1 + inflation[year %in% 1995:2004] / 100, na.rm = FALSE) - 1,
      TRUE ~ NA_real_),
    ethnic = na_if(ethnic, "."),
    ethnic = as.numeric(ethnic),
    language = na_if(language, "."),
    language = as.numeric(language),
    religion = na_if(religion, "."),
    religion = as.numeric(religion)) |>
  select(-year, -year_count) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = FALSE)), .groups = "drop") |>
  filter(pop_growth > -0.1,
         pop_growth_instr > -0.1)

pwt_final = pwt_analytic |>
  mutate(
    initial_gdp = log(initial_gdp),
    initial_gdp_instr = log(initial_gdp_instr),
    enrollment = log(enrollment),
    pop_growth = log(pop_growth * 100 + 10),
    pop_growth_instr = log(pop_growth_instr * 100 + 10),
    ki = log(ki),
    ki_instr = log(ki_instr),
    mortality = 1/life_expectancy,
    fertility = log(fertility),
    french_origin = ifelse(legal_origin == "French", 1, 0),
    british_origin = ifelse(legal_origin == "British", 1, 0))

write_csv(pwt_final, "datasets/pwt_final.csv")


































