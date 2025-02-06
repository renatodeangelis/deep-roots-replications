library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(haven)

pwt_init = read_excel("Datasets/pwt1001.xlsx")
rugged = read_csv("Datasets/rugged_data.csv")
fertility = read_csv("Datasets/world-bank-fertility-rate.csv", skip = 3)
mortality = read_csv("Datasets/world-bank-life-expectancy.csv", skip = 3)
frac = read_excel("Datasets/2003_fractionalization.xls", skip = 1)

religions_to_remove = c(
  ". . Mahayanists", ". . unaffiliated Christians",
  ". . Vaishnavites", ". . Shaivites", ". . Saktists",
  ". . Theravadins", ". . Lamaists", ". . Islamic schismatics",
  ". . Independents", ". . Sunnis", ". . Shias", ". . doubly-affiliated",
  "Christians")
other_religions = c(
  "Baha'is", "Ethnic religionists", "Spiritists",
  "Zoroastrians", "New religionists")
eastern_religions = c(
  "Buddhists", "Confucianists", "Chinese folk-religionists",
  "Daoists", "Shintoists", "Sikhs", "Jains")

religion = read_excel("Datasets/wrd-religion-by-country.xlsx") |>
  slice(-1) |>
  select(-"Year") |>
  rename("country" = "...2", "religion" = "...3") |>
  filter(!(religion %in% religions_to_remove)) |>
  mutate(across(c(`1970`, `2000`, `2015`, `2020`), as.numeric)) |>
  mutate(across(c(`1970`, `2000`, `2015`, `2020`), ~replace_na(., 0))) |>
  mutate(religion = case_when(
    religion %in% other_religions ~ "other",
    religion %in% eastern_religions ~ "eastern",
    religion %in% c("Agnostics", "Atheists") ~ "non-religious",
    TRUE ~ religion
  )) |>
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
    TRUE ~ religion
  )) |>
  pivot_longer(
    cols = c(`1970`, `2000`, `2015`, `2020`),
    names_to = "year",
    values_to = "value"
  ) |>
  group_by(country, year) |>
  pivot_wider(
    names_from = religion,
    values_from = value
  ) |>
  ungroup() |>
  select(-`NA`)

colony_data = read_dta("datasets/7-ajr-2001/maketable3.dta")
colony_names = read_dta("datasets/7-ajr-2001/maketable2.dta")
instit = colony_data |>
  left_join(colony_names) |>
  filter(!is.na(avexpr), !is.na(shortnam)) |>
  select(shortnam, avexpr) |>
  rename("country_code" = "shortnam")

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
  "Upper middle income", "World", "Channel Islands", "Curacao", "Isle of Man",
  "Not classified", "St. Martin (French part)", "Sint Maarten (Dutch part)")

tryout = fertility |>
  left_join(mortality, by = c("Country Name", "Country Code")) |>
  filter(!(`Country Name` %in% names_to_remove)) |>
  left_join(rugged, by = c("Country Code" = "isocode")) |>
  mutate(`Country Name` = case_when(
    `Country Name` == "Bahamas, The" ~ "Bahamas",
    `Country Name` == "Brunei Darussalam" ~ "Brunei",
    `Country Name` == "Congo, Dem. Rep." ~ "Congo, Dem. Rep. (Zaire)",
    `Country Name` == "Congo, Rep." ~ "Congo",
    `Country Name` == "Cabo Verde" ~ "Cape Verde",
    `Country Name` == "Czechia" ~ "Czech Republic",
    `Country Name` == "Egypt, Arab Rep." ~ "Egypt",
    `Country Name` == "Micronesia, Fed. Sts." ~ "Micronesia",
    `Country Name` == "Hong Kong SAR, China" ~ "Hong Kong",
    `Country Name` == "Iran, Islamic Rep." ~ "Iran",
    `Country Name` == "Kyrgyz Republic" ~ "Kyrgyzstan",
    `Country Name` == "St. Kitts and Nevis" ~ "St Kitts & Nevis",
    `Country Name` == "Korea, Rep." ~ "Korea, South",
    `Country Name` == "Lao PDR" ~ "Lao People's Dem Rep",
    `Country Name` == "St. Lucia" ~ "Saint Lucia",
    `Country Name` == "Macao SAR, China" ~ "Macau",
    `Country Name` == "North Macedonia" ~ "Macedonia (Former Yug. Rep)",
    `Country Name` == "Myanmar" ~ "Myanmar (Burma)",
    `Country Name` == "Korea, Dem. People's Rep." ~ "Korea, North",
    `Country Name` == "Eswatini" ~ "Swaziland",
    `Country Name` == "Syrian Arab Republic" ~ "Syria",
    `Country Name` == "Timor-Leste" ~ "East Timor",
    `Country Name` == "Turkiye" ~ "Turkey",
    `Country Name` == "St. Vincent and the Grenadines" ~ "Saint Vincent and Grenadines",
    `Country Name` == "Venezuela, RB" ~ "Venezuela",
    `Country Name` == "Viet Nam" ~ "Vietnam",
    `Country Name` == "Samoa" ~ "Western Samoa",
    `Country Name` == "Yemen, Rep." ~ "Yemen",
    TRUE ~ `Country Name`
  )) |>
  left_join(frac, by = c("Country Name" = "Country")) |>
  janitor::clean_names()

columns_to_keep = c(
  colnames(tryout)[grepl("[0-9]", colnames(tryout))],
  "country_name", "country_code", "indicator_name_x",
  "indicator_name_y", "near_coast", "tropical", "language",
  "ethnic", "religion"
)

intermed = tryout |>
  select(all_of(columns_to_keep)) |>
  rename_with(
    ~str_replace(., "^x(\\d+)_x$", "fertility_\\1"),
    matches("^x\\d+_x$")) |>
  rename_with(
    ~str_replace(., "^x(\\d+)_y$", "life_expectancy_\\1"),
    matches("^x\\d+_y$")) |>
  select(-fertility_69, -life_expectancy_69,
         -indicator_name_x, -indicator_name_y) |>
  relocate(country_name, country_code)

intermed_panel = intermed |>
  pivot_longer(
    cols = starts_with("fertility_"),
    names_to = "year",
    names_prefix = "fertility_",
    values_to = "fertility"
  ) |>
  pivot_longer(
    cols = starts_with("life_expectancy_"),
    names_to = "year_le",
    names_prefix = "life_expectancy_",
    values_to = "life_expectancy"
  ) |>
  filter(year == year_le) |>
  select(-year_le, -rgdppc_2000, -rgdppc_1950_m,
         -rgdppc_1975_m, -rgdppc_2000_m, -rgdppc_1950_2000_m) |>
  relocate(country_name, country_code, year)

pwt_final = intermed_panel |>
  mutate(year = as.numeric(year)) |>
  left_join(pwt_inter, by = c("country_code" = "countrycode",
                              "year" = "year")) |>
  relocate(pop_1400, .after = pop)

pwt_relig = pwt_final |>
  left_join(religion |> mutate(year = as.numeric(year)),
            by = c("country_name" = "country", "year" = "year")) |>
  group_by(country_name) |>
  mutate(across(c(catholic:other),
           ~case_when(
             year <= 1985 ~ .x[match(1970, year)],
             year > 1985 & year <= 2008 ~ .x[match(2000, year)], 
             year > 2008 & year <= 2018 ~ .x[match(2015, year)],
             year > 2018 ~ .x[match(2020, year)]
           )))

pwt_instit = pwt_relig |>
  mutate(country_code = case_when(
    country_code == "ROU" ~ "ROM",
    country_code == "COD" ~ "ZAR",
    TRUE ~ country_code
  )) |>
  left_join(instit, by = c("country_code" = "country_code"))

pwt_complete = pwt_instit |>
  mutate(ln_rgdppw = log(rgdpe / emp),
         trade_to_gdp = pl_x + pl_m,
         log_income = log(rgdpe),
         pop = pop * 10^5)

pwt_analysis = pwt_complete |>
  filter(year > 1975 & year < 2006) |>
  mutate(
    growth_1 = (ln_rgdppw[year == 1985] - ln_rgdppw[year == 1976]) / ln_rgdppw[year == 1976],
    pop_growth_1 = log(pop[year == 1985] - pop[year == 1976]) + 0.05,
    growth_2 = (ln_rgdppw[year == 1995] - ln_rgdppw[year == 1986]) / ln_rgdppw[year == 1986],
    pop_growth_2 = log(pop[year == 1995] - pop[year == 1986]) + 0.05,
    growth_3 = (ln_rgdppw[year == 2005] - ln_rgdppw[year == 1996]) / ln_rgdppw[year == 1996],
    pop_growth_3 = log(pop[year == 2005] - pop[year == 1996]) + 0.05) |>
  ungroup() |>
  filter(!is.na(growth_1), !is.na(pop_growth_1),
         !is.na(growth_2), !is.na(pop_growth_2),
         !is.na(growth_3), !is.na(pop_growth_3)) |>
  mutate(period = case_when(
    year %in% 1976:1985 ~ "1976-1985",
    year %in% 1986:1995 ~ "1986-1995",
    year %in% 1996:2005 ~ "1996-2005",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(period)) |>
  group_by(country_name, period) |>
  mutate(growth = ifelse(period == "1976-1985", growth_1,
                      ifelse(period == "1986-1995", growth_2,
                               growth_3))) |>
  mutate(pop_growth = ifelse(period == "1976-1985", pop_growth_1,
                             ifelse(period == "1986-1995", pop_growth_2,
                                    pop_growth_3))) |>
  mutate(initial_gdp = ifelse(period == "1976-1985", ln_rgdppw[year == 1976],
                              ifelse(period == "1986-1995", ln_rgdppw[year == 1986],
                                     ln_rgdppw[year == 1996]))) |>
  mutate(initial_pop = ifelse(period == "1976-1985", pop[year == 1976],
                              ifelse(period == "1986-1995", pop[year == 1986],
                                     pop[year == 1996]))) |>
  mutate(initial_hc = ifelse(period == "1976-1985", hc[year == 1976],
                              ifelse(period == "1986-1995", hc[year == 1986],
                                     hc[year == 1996])))
  
  #summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE), .groups = "drop"))
