library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(haven)

pwt_init = read_csv("datasets/pwt71_wo_country_names_wo_g_vars.csv") |>
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

rugged = read_csv("Datasets/rugged_data.csv") |>
  select(isocode, near_coast, tropical, colony_esp, colony_prt) |>
  mutate(colony_esp_prt = colony_esp + colony_prt) |>
  select(-colony_esp, -colony_prt)

frac = read_excel("Datasets/2003_fractionalization.xls", skip = 1) |>
  janitor::clean_names() |>
  select(country, ethnic, language, religion) |>
  filter(!is.na(country))

formalism = read_dta("Datasets/divergence_data.dta") |>
  filter(check == 1) |>
  select(country, legal_origin, allstpi1980, allstpi1990, allstpi2000) |>
  mutate(country = case_when(
    country == "Senegal (1965)" ~ "Senegal",
    country == "Tunisia (1960)" ~ "Tunisia",
    country == "USA" ~ "United States",
    TRUE ~ country))

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

fertility = read_csv("Datasets/world-bank-fertility-rate.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "fertility") |>
  janitor::clean_names() |>
  filter(year >= 1970 & year <= 2004,
         !(country_name %in% names_to_remove)) |>
  mutate(year = as.numeric(year)) |>
  select(-indicator_name, -indicator_code)

mortality = read_csv("Datasets/world-bank-life-expectancy.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "life_expectancy") |>
  janitor::clean_names() |>
  filter(year >= 1970 & year <= 2004,
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
  filter(year >= 1970 & year <= 2004,
         !(country_name %in% names_to_remove)) |>
  mutate(year = as.numeric(year)) |>
  select(-indicator_name, -indicator_code, -country_name)


colony_data = read_dta("datasets/7-ajr-2001/maketable3.dta")
colony_names = read_dta("datasets/7-ajr-2001/maketable2.dta")
instit = colony_data |>
  left_join(colony_names) |>
  filter(!is.na(avexpr), !is.na(shortnam)) |>
  select(shortnam, avexpr) |>
  rename("country_code" = "shortnam")

religions_to_remove = c(". . Mahayanists", ". . unaffiliated Christians",
                        ". . Vaishnavites", ". . Shaivites", ". . Saktists",
                        ". . Theravadins", ". . Lamaists", ". . Islamic schismatics",
                        ". . Independents", ". . Sunnis", ". . Shias", ". . doubly-affiliated",
                        "Christians")
other_religions = c("Baha'is", "Ethnic religionists", "Spiritists",
                    "Zoroastrians", "New religionists")
eastern_religions = c("Buddhists", "Confucianists", "Chinese folk-religionists",
                      "Daoists", "Shintoists", "Sikhs", "Jains")

religion = read_excel("Datasets/wrd-religion-by-country.xlsx") |>
  slice(-1) |>
  select(-"Year", -`2020`) |>
  rename("country" = "...2", "religion" = "...3") |>
  filter(!(religion %in% religions_to_remove)) |>
  mutate(across(c(`1900`, `1970`, `2000`), as.numeric)) |>
  mutate(across(c(`1900`, `1970`, `2000`), ~replace_na(., 0))) |>
  mutate(religion = case_when(
    religion %in% other_religions ~ "other",
    religion %in% eastern_religions ~ "eastern",
    religion %in% c("Agnostics", "Atheists") ~ "non-religious",
    TRUE ~ religion)) |>
  group_by(country, religion) |>
  summarise(across(everything(), sum), .groups = "drop") |>
  ungroup() |>
  mutate(religion = case_when(
    religion == ". . Catholics" ~ "catholic",
    religion == ". . Protestants" ~ "protestant",
    religion == ". . Orthodox" ~ "orthodox",
    religion == "Muslims" ~ "muslim",
    religion == "Jews" ~ "jewish",
    religion == "Hindus" ~ "hindu",
    TRUE ~ religion)) |>
  pivot_longer(cols = c(`1900`, `1970`, `2000`),
               names_to = "year",
               values_to = "value") |>
  group_by(country, year) |>
  pivot_wider(names_from = religion,
              values_from = value) |>
  ungroup() |>
  mutate(across(catholic:other, ~replace_na(., 0))) |>
  select(-`NA`)

religion_1900 = religion |>
  filter(year == "1900") |>
  pivot_wider(
    names_from = year,
    values_from = c(catholic, protestant, orthodox, muslim,jewish, hindu, eastern, `non-religious`, other),
    names_glue = "{.value}_{year}")

religion_final = religion |>
  left_join(religion_1900, by = "country") |>
  filter(year %in% c("1970", "2000")) |>
  mutate(year = ifelse(year == "1970", 1970, 2000))

pwt_inter = pwt_init |>
  left_join(mortality, by = c("isocode" = "country_code", "year")) |>
  left_join(fertility, by = c("isocode" = "country_code", "year")) |>
  left_join(inflation, by = c("isocode" = "country_code", "year")) |>
  left_join(rugged, by = c("isocode")) |>
  left_join(schooling, by = c("isocode" = "WBcode", "year")) |>
  rename(country = country_name) |>
  relocate(country)




