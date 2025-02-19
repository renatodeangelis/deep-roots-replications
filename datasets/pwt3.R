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

wb_regions = read_csv("datasets/world-regions-according-to-the-world-bank.csv")

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
  filter(year >= 1970 & year <= 2005,
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

east_asia = c("CHN", "HKG", "JPN", "KOR", "MAC", "MNG")

pwt_inter = pwt_init |>
  mutate(isocode = case_when(
    isocode == "ZAR" ~ "COD",
    TRUE ~ isocode)) |>
  left_join(mortality, by = c("isocode" = "country_code", "year")) |>
  left_join(fertility, by = c("isocode" = "country_code", "year")) |>
  left_join(inflation, by = c("isocode" = "country_code", "year")) |>
  left_join(enrollment, by = c("isocode" = "country_code", "year")) |>
  left_join(rugged, by = c("isocode")) |>
  left_join(schooling, by = c("isocode" = "WBcode", "year")) |>
  left_join(wb_regions, by = c("isocode" = "Code")) |>
  select(-Entity, -Year) |>
  rename(wb_region = `World Region according to the World Bank`) |>
  mutate(isocode = case_when(
    isocode == "ROU" ~ "ROM",
    isocode == "COD" ~ "ZAR",
    TRUE ~ isocode)) |>
  left_join(instit, by = c("isocode" = "country_code")) |>
  mutate(country_name = case_when(
    country_name == "Bahamas, The" ~ "Bahamas",
    country_name == "Brunei Darussalam" ~ "Brunei",
    country_name == "Congo, Dem. Rep." ~ "Congo, Dem. Rep. (Zaire)",
    country_name == "Congo, Rep." ~ "Congo",
    country_name == "Cabo Verde" ~ "Cape Verde",
    country_name == "Czechia" ~ "Czech Republic",
    country_name == "Egypt, Arab Rep." ~ "Egypt",
    country_name == "Micronesia, Fed. Sts." ~ "Micronesia",
    country_name == "Hong Kong SAR, China" ~ "Hong Kong",
    country_name == "Iran, Islamic Rep." ~ "Iran",
    country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
    country_name == "St. Kitts and Nevis" ~ "St Kitts & Nevis",
    country_name == "Korea, Rep." ~ "Korea, South",
    country_name == "Lao PDR" ~ "Lao People's Dem Rep",
    country_name == "St. Lucia" ~ "Saint Lucia",
    country_name == "Macao SAR, China" ~ "Macau",
    country_name == "North Macedonia" ~ "Macedonia (Former Yug. Rep)",
    country_name == "Myanmar" ~ "Myanmar (Burma)",
    country_name == "Korea, Dem. People's Rep." ~ "Korea, North",
    country_name == "Eswatini" ~ "Swaziland",
    country_name == "Syrian Arab Republic" ~ "Syria",
    country_name == "Timor-Leste" ~ "East Timor",
    country_name == "Turkiye" ~ "Turkey",
    country_name == "St. Vincent and the Grenadines" ~
      "Saint Vincent and Grenadines",
    country_name == "Venezuela, RB" ~ "Venezuela",
    country_name == "Viet Nam" ~ "Vietnam",
    country_name == "Samoa" ~ "Western Samoa",
    country_name == "Yemen, Rep." ~ "Yemen",
    TRUE ~ country_name)) |>
  left_join(frac, by = c("country_name" = "country")) |>
  left_join(formalism, by = c("country_name" = "country")) |>
  left_join(religion_final |> mutate(year = as.numeric(year)),
            by = c("country_name" = "country", "year" = "year")) |>
  group_by(country_name) |>
  mutate(across(c(catholic:other),
                ~case_when(
                  year <= 1985 ~ .x[match(1970, year)],
                  year > 1985 ~ .x[match(2000, year)])),
         across(c(catholic_1900:other_1900),
                ~first(.))) |>
  rename(country = country_name) |>
  relocate(country)

pwt_mid = pwt_inter |>
  mutate(formalism = ifelse(period == "1975-1984", allstpi1980,
                            ifelse(period == "1985-1994", allstpi1990,
                                   allstpi2000)),
         ssa = ifelse(wb_region == "Sub-Saharan Africa" |
           isocode == "ZAR", 1, 0),
         east_asia = ifelse(isocode %in% east_asia, 1, 0),
         latam = ifelse(wb_region == "Latin America and Caribbean", 1, 0),
         period_1 = ifelse(period == "1975-1984", 1, 0),
         period_2 = ifelse(period == "1985-1994", 1, 0),
         period_3 = ifelse(period == "1995-2004", 1, 0),
         legal_origin = case_when(
           legal_origin == 1 ~ "French",
           legal_origin == 2 ~ "English",
           legal_origin == 3 ~ "Japanese",
           legal_origin == 4 ~ "Nordic",
           TRUE ~ as.character(legal_origin))) |>
  select(-starts_with("allstpi")) |>
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
  
  

















  
  
  
  
  
  
  
  
  
  
  
  
  


