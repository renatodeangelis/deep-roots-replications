library(readxl)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(WDI)

pwt_init = read_excel("Datasets/pwt1001.xlsx") |>
  select(countrycode, year, pop, hc, rgdpna, rnna, rtfpna) |>
  group_by(countrycode) |>
  mutate(rgdppw = rgdpna / pop,
         period = case_when(
           year %in% 1970:1979 ~ "1970-1979",
           year %in% 1980:1989 ~ "1980-1989",
           year %in% 1990:1999 ~ "1990-1999",
           year %in% 2000:2009 ~ "2000-2009",
           year %in% 2010:2019 ~ "2010-2019")) |>
  relocate(period, .after = year) |>
  filter(!is.na(rgdppw),
         year >= 1970 & year <= 2020) |>
  ungroup()

pwt_6 = read_excel("datasets/dkt-ej-to-be-distributed.xls") |>
  mutate(across(dum6575:ssafr, as.numeric)) |>
  filter(dum8595 == 1) |>
  select(isocode, kkz96, kgatrstr, lang) |>
  mutate(kgatrstr = case_when(isocode == "SGP" ~ 1,
                              isocode == "HKG" ~ 1,
                              isocode == "MLT" ~ 0,
                              isocode == "MUS" ~ 1,
                              isocode == "BHR" ~ 0,
                              TRUE ~ kgatrstr),
         isocode = case_when(isocode == "GER" ~ "DEU",
                             isocode == "ROM" ~ "ROU",
                             TRUE ~ isocode)) |>
  filter(!is.na(kkz96), !is.na(kgatrstr), !is.na(lang))

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

worldbank = WDI(country = "all", indicator = c(
  "NE.EXP.GNFS.ZS",  # Exports of goods and services (constant 2010 US$)
  "NE.IMP.GNFS.ZS",  # Imports of goods and services (constant 2010 US$)
  "NE.CON.GOVT.ZS",  # Government consumption (constant 2010 US$)
  "SE.XPD.TOTL.GD.ZS",  # Education spending (% of GDP)
  "MS.MIL.XPND.ZS",  # Military spending (% of GDP)
  "NY.GDP.MKTP.KD",  # GDP (constant 2010 US$)
  "SP.POP.TOTL",     # Total Population
  "AG.SRF.TOTL.K2",   # Land Area (sq. km)
  "FP.CPI.TOTL.ZG", # CPI % change
  "NE.GDI.FTOT.ZS" # Investment as % of GDP
), start = 1970, end = 2020, extra = TRUE) |>
  select(-iso2c, -status, -lastupdated, -capital, -longitude, -latitude, -income, -lending) |>
  filter(!(country %in% names_to_remove))

wb_data = worldbank |>
  group_by(country, year) |>
  filter(NE.GDI.FTOT.ZS > 0) |>
  mutate(gv = NE.CON.GOVT.ZS,
         gv_net = NE.CON.GOVT.ZS - SE.XPD.TOTL.GD.ZS - MS.MIL.XPND.ZS,
         trade_ratio = NE.EXP.GNFS.ZS + NE.IMP.GNFS.ZS,
         inv = log(NE.GDI.FTOT.ZS)) |>
  filter(!is.na(trade_ratio), !is.na(AG.SRF.TOTL.K2))

model = lm(trade_ratio ~ log(SP.POP.TOTL) + log(AG.SRF.TOTL.K2), data = wb_data)
wb_data$opres = residuals(model)

wb_data1 = wb_data |>
  rename(cpi = FP.CPI.TOTL.ZG) |>
  select(country, iso3c, year, region, gv, inv, opres, cpi)

pwt_inter = pwt_init |>
  left_join(wb_data1, by = c("countrycode" = "iso3c",
                            "year" = "year")) |>
  relocate(country, .before = countrycode) |>
  relocate(region, .after = countrycode) |>
  group_by(countrycode, period, country, region) |>
  mutate(year_count = n()) |>
  filter(year_count == 10) |>
  select(-year_count) |>
  mutate(yinit = rgdppw[year == min(year)],
        gy = (rgdppw[year == max(year)] - rgdppw[year == min(year)]) /
           rgdppw[year == min(year)],
         gh = (hc[year == max(year)] - hc[year == min(year)]) /
           hc[year == min(year)],
         pc = rnna / pop,
         gk = (pc[year == max(year)] - pc[year == min(year)]) /
           pc[year == min(year)],
         gtfp = (rtfpna[year == max(year)] - rtfpna[year == min(year)]) /
           rtfpna[year == min(year)],
         gpop = (pop[year == max(year)] - pop[year == min(year)]) /
           pop[year == min(year)]) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = FALSE)), .groups = "drop") |>
  mutate(pc = log(pc),
         yinit = log(yinit)) |>
  ungroup() |>
  left_join(pwt_6, by = c("countrycode" = "isocode"))

write_csv(pwt_inter, "datasets/data_final.csv")










