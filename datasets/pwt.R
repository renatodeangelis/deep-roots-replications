library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)

pwt_init = read_excel("Datasets/pwt1001.xlsx")
rugged = read_csv("Datasets/rugged_data.csv")
fertility = read_csv("Datasets/world-bank-fertility-rate.csv", skip = 3)
mortality = read_csv("Datasets/world-bank-life-expectancy.csv", skip = 3)
frac = read_excel("Datasets/2003_fractionalization.xls", skip = 1)

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
