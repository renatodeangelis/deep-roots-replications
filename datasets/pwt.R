library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(haven)

pwt_init = read_excel("Datasets/pwt1001.xlsx")
rugged = read_csv("Datasets/rugged_data.csv") |>
  select(isocode, country, near_coast, tropical, colony_esp, colony_prt) |>
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
  filter(year >= 1971 & year <= 2005,
         !(country_name %in% names_to_remove)) |>
  select(-indicator_name, -indicator_code)

mortality = read_csv("Datasets/world-bank-life-expectancy.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "life_expectancy") |>
  janitor::clean_names() |>
  filter(year >= 1971 & year <= 2005,
         !(country_name %in% names_to_remove)) |>
  select(-indicator_name, -indicator_code)

govt = read_csv("Datasets/govt-consumption-to-gdp.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "govt_consumption") |>
  janitor::clean_names() |>
  filter(year >= 1971 & year <= 2005,
         !(country_name %in% names_to_remove)) |>
  select(-indicator_name, -indicator_code)

investment = read_csv("Datasets/gross-capital-formation.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "domestic_investment") |>
  janitor::clean_names() |>
  filter(year >= 1971 & year <= 2005,
         !(country_name %in% names_to_remove)) |>
  select(-indicator_name, -indicator_code)

schooling = read_csv("Datasets/gross-school-enrollment.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "schooling") |>
  janitor::clean_names() |>
  filter(year >= 1971 & year <= 2005,
         !(country_name %in% names_to_remove)) |>
  select(-indicator_name, -indicator_code)

trade = read_csv("Datasets/trade-to-gdp.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "trade_to_gdp") |>
  janitor::clean_names() |>
  filter(year >= 1971 & year <= 2005,
         !(country_name %in% names_to_remove)) |>
  select(-indicator_name, -indicator_code)

inflation = read_csv("Datasets/inflation.csv", skip = 3) |>
  select(-`...69`, -`2023`) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to = "inflation") |>
  janitor::clean_names() |>
  filter(year >= 1971 & year <= 2005,
         !(country_name %in% names_to_remove)) |>
  select(-indicator_name, -indicator_code)

## Merging together World Bank, geography, fractionalization datasets

tryout = fertility |>
  left_join(mortality, by = c("country_name", "country_code", "year")) |>
  left_join(trade, by = c("country_name", "country_code", "year")) |>
  left_join(inflation, by = c("country_name", "country_code", "year")) |>
  left_join(investment, by = c("country_name", "country_code", "year")) |>
  left_join(schooling, by = c("country_name", "country_code", "year")) |>
  left_join(govt, by = c("country_name", "country_code", "year")) |>
  left_join(rugged,
            by = c("country_code" = "isocode", "country_name" = "country")) |>
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
  janitor::clean_names()

## Merging in PWT data

pwt_final = tryout |>
  mutate(year = as.numeric(year)) |>
  left_join(pwt_init, by = c("country_code" = "countrycode",
                              "year" = "year")) |>
  filter(!is.na(rgdpna)) |>
  select(-country, -starts_with("i_"), -starts_with("pl_"),
         -statcap, -cor_exp, -starts_with("csh_"), -currency_unit)

## Loading in religion data

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
  mutate(year = ifelse(year == "1970", 1971, 2000))

pwt_relig = pwt_final |>
  left_join(religion_final |> mutate(year = as.numeric(year)),
            by = c("country_name" = "country", "year" = "year")) |>
  group_by(country_name) |>
  mutate(across(c(catholic:other),
           ~case_when(
             year <= 1985 ~ .x[match(1971, year)],
             year > 1985 ~ .x[match(2000, year)])),
         across(c(catholic_1900:other_1900),
           ~first(.)))

## Loading in AJR (2001) dataset

colony_data = read_dta("datasets/7-ajr-2001/maketable3.dta")
colony_names = read_dta("datasets/7-ajr-2001/maketable2.dta")
instit = colony_data |>
  left_join(colony_names) |>
  filter(!is.na(avexpr), !is.na(shortnam)) |>
  select(shortnam, avexpr) |>
  rename("country_code" = "shortnam")

pwt_instit = pwt_relig |>
  mutate(country_code = case_when(
    country_code == "ROU" ~ "ROM",
    country_code == "COD" ~ "ZAR",
    TRUE ~ country_code)) |>
  left_join(instit, by = c("country_code" = "country_code"))

pwt_pre = pwt_instit |>
  filter(!is.na(rgdpna), !is.na(emp), !is.na(pop)) |>
  group_by(country_name) |>
  mutate(ln_rgdppw = log(rgdpna / emp),
         log_income = log(rgdpna),
         period = case_when(
           year %in% 1971:1985 ~ "1976-1985",
           year %in% 1986:1995 ~ "1986-1995",
           year %in% 1996:2005 ~ "1996-2005")) |>
  ungroup()

pwt_instr = pwt_pre |>
  group_by(country_name) |>
  mutate(
    ln_rgdppw_instr = case_when(
      period == "1976-1985" & any(year == 1971) ~ first(ln_rgdppw[year == 1971]),
      period == "1986-1995" & any(year == 1981) ~ first(ln_rgdppw[year == 1981]),
      period == "1996-2005" & any(year == 1991) ~ first(ln_rgdppw[year == 1991]),
      TRUE ~ NA_real_),
    initial_gdp = case_when(
      period == "1976-1985" & any(year == 1976) ~ first(ln_rgdppw[year == 1976]),
      period == "1986-1995" & any(year == 1986) ~ first(ln_rgdppw[year == 1986]),
      period == "1996-2005" & any(year == 1996) ~ first(ln_rgdppw[year == 1996]),
      TRUE ~ NA_real_),
    initial_gdp_instr = case_when(
      period == "1976-1985" & any(year == 1971) ~ first(ln_rgdppw[year == 1971]),
      period == "1986-1995" & any(year == 1981) ~ first(ln_rgdppw[year == 1981]),
      period == "1996-2005" & any(year == 1991) ~ first(ln_rgdppw[year == 1991]),
      TRUE ~ NA_real_),
    initial_hc = case_when(
      period == "1976-1985" & any(year == 1976) ~ first(hc[year == 1976]),
      period == "1986-1995" & any(year == 1986) ~ first(hc[year == 1986]),
      period == "1996-2005" & any(year == 1996) ~ first(hc[year == 1996]),
      TRUE ~ NA_real_),
    initial_hc_instr = case_when(
      period == "1976-1985" & any(year == 1971) ~ first(hc[year == 1971]),
      period == "1986-1995" & any(year == 1981) ~ first(hc[year == 1981]),
      period == "1996-2005" & any(year == 1991) ~ first(hc[year == 1991]),
      TRUE ~ NA_real_)
  )

pwt_instr2 = pwt_instr |>
  group_by(country_name) |>
  summarise(
    year = year,
    pop_growth = case_when(
      period == "1976-1985" ~ (first(pop[year == 1985]) - first(pop[year == 1976])) / first(pop[year == 1976]),
      period == "1986-1995" ~ (first(pop[year == 1995]) - first(pop[year == 1986])) / first(pop[year == 1986]),
      period == "1996-2005" ~ (first(pop[year == 2005]) - first(pop[year == 1996])) / first(pop[year == 1996]),
      TRUE ~ NA_real_),
    pop_growth_instr = case_when(
      period == "1976-1985" ~ (first(pop[year == 1975]) - first(pop[year == 1971])) / first(pop[year == 1971]),
      period == "1986-1995" ~ (first(pop[year == 1985]) - first(pop[year == 1981])) / first(pop[year == 1981]),
      period == "1996-2005" ~ (first(pop[year == 1995]) - first(pop[year == 1991])) / first(pop[year == 1991]),
      TRUE ~ NA_real_),
    school_instr = case_when(
      period == "1976-1985" ~ mean(schooling[year %in% 1971:1975], na.rm = TRUE),
      period == "1986-1995" ~ mean(schooling[year %in% 1981:1985], na.rm = TRUE),
      period == "1996-2005" ~ mean(schooling[year %in% 1991:1995], na.rm = TRUE),
      TRUE ~ NA_real_),
    investment_instr = case_when(
      period == "1976-1985" ~ mean(domestic_investment[year %in% 1971:1975], na.rm = TRUE),
      period == "1986-1995" ~ mean(domestic_investment[year %in% 1981:1985], na.rm = TRUE),
      period == "1996-2005" ~ mean(domestic_investment[year %in% 1991:1995], na.rm = TRUE),
      TRUE ~ NA_real_),
    trade_instr = case_when(
      period == "1976-1985" ~ mean(trade_to_gdp[year %in% 1971:1975], na.rm = TRUE),
      period == "1986-1995" ~ mean(trade_to_gdp[year %in% 1981:1985], na.rm = TRUE),
      period == "1996-2005" ~ mean(trade_to_gdp[year %in% 1991:1995], na.rm = TRUE),
      TRUE ~ NA_real_),
    govt_instr = case_when(
      period == "1976-1985" ~ mean(govt_consumption[year %in% 1971:1975], na.rm = TRUE),
      period == "1986-1995" ~ mean(govt_consumption[year %in% 1981:1985], na.rm = TRUE),
      period == "1996-2005" ~ mean(govt_consumption[year %in% 1991:1995], na.rm = TRUE),
      TRUE ~ NA_real_)
  ) |>
  left_join(pwt_instr, by = c("country_name", "year"))

pwt_analytic = pwt_instr2 |>
  group_by(country_name, country_code, period) |>
  filter(!(year %in% 1971:1975)) |>
  mutate(year_count = n()) |>
  filter(year_count == 10) |>
  mutate(
    cum_inflation = case_when(
      period == "1976-1985" ~ prod(1 + inflation[year %in% 1976:1985] / 100, na.rm = TRUE) - 1,
      period == "1986-1995" ~ prod(1 + inflation[year %in% 1986:1995] / 100, na.rm = TRUE) - 1,
      period == "1996-2005" ~ prod(1 + inflation[year %in% 1996:2005] / 100, na.rm = TRUE) - 1,
      TRUE ~ NA_real_),
    growth = (ln_rgdppw[year == max(year)] - ln_rgdppw[year == min(year)]) /
      ln_rgdppw[year == min(year)],
    hc_growth = (hc[year == max(year)] - hc[year == min(year)]) /
      hc[year == min(year)],
    phys_capital = rnna / emp,
    pc_growth = (phys_capital[year == max(year)] - phys_capital[year == min(year)]) /
      phys_capital[year == min(year)],
    ethnic = na_if(ethnic, "."),
    ethnic = as.numeric(ethnic),
    language = na_if(language, "."),
    language = as.numeric(language),
    religion = na_if(religion, "."),
    religion = as.numeric(religion)) |>
  filter(!is.na(growth) & !is.na(pop_growth)) |>
  select(-year, -year_count) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") |>
  mutate(period_1 = period == "1976-1985",
         period_2 = period == "1986-1995",
         period_3 = period == "1996-2005")

write_csv(pwt_analytic, "datasets/pwt_analytic.csv")
