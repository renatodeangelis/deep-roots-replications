packages = c("dplyr", "readr", "haven", "estimatr", 
             "conleyreg", "lmtest", "sandwich", "tibble",
             "quantreg", "stargazer", "readxl")
sapply(packages, library, character.only = TRUE)

wb_data = read_csv("datasets/world-regions-according-to-the-world-bank.csv")
macro_data = read_dta("datasets/2-comin-easterly-gong/primitive_aejmacro.dta")
pwt_init = read_excel("Datasets/pwt1001.xlsx") |>
  filter(year == 2002) |>
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    country == "D.R. of the Congo" ~ "Democratic Republic of the Congo",
    country == "Congo" ~ "Republic of the Congo",
    country == "Côte d'Ivoire" ~ "Cote D'Ivoire",
    country == "Czech Republic" ~ "Czechoslovakia",
    country == "Eswatini" ~ "Swaziland",
    country == "Gambia" ~ "The Gambia",
    country == "China, Hong Kong SAR" ~ "Hong Kong",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Republic of Korea" ~ "Korea",
    country == "Lao People's DR" ~ "Laos",
    country == "North Macedonia" ~ "Macedonia",
    country == "Republic of Moldova" ~ "Moldova",
    country == "Russian Federation" ~ "Russia",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "U.R. of Tanzania: Mainland" ~ "Tanzania",
    country == "United Kingdom" ~ "England",
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ country
  ))

macro_strip = lapply(macro_data, function(col) {
  attr(col, "format.stata") <- NULL
  return(col)
})

macro_strip = as.data.frame(macro_strip)

wb_regions = wb_data |>
  janitor::clean_names() |>
  select(-year, -code) |>
  rename(wb_region = world_region_according_to_the_world_bank,
         country = entity)

macro_final = left_join(macro_strip, wb_regions,
                        by = "country") |>
  mutate(wb_region = case_when(
    country %in% c("French Guiana", "Lesser Antilles") ~ "Latin American and Caribbean",
    country %in% c("Democratic Republic of the Congo", "Republic of the Congo", 
                   "The Gambia", "Swaziland", "Cote D'Ivoire") ~ "Sub-Saharan Africa",
    country %in% c("Czechoslovakia", "Macedonia", "Serbia & Montenegro",
                   "England") ~ "Europe and Central Asia",
    country %in% c("Cook Islands", "Korea") ~ "East Asia and Pacific",
    TRUE ~ wb_region
  )) |> 
  mutate(mena = as.integer(wb_region == "Middle East and North Africa"),
         eur = as.integer(wb_region == "Europe and Central Asia"),
         sa = as.integer(wb_region == "South Asia"),
         eap = as.integer(wb_region == "East Asia and Pacific"),
         na = as.integer(wb_region == "North America"),
         latam = as.integer(wb_region == "Latin America and Caribbean"),
         afr = as.integer(wb_region == "Sub-Saharan Africa")) |>
  left_join(pwt_init, by = "country")

clus1000 = macro_final$clus1000

mod1 = lm(ly2002 ~ tr1, 
               data = macro_final)
mod1_se = vcovCL(mod1, cluster = clus1000)
rob1 = coeftest(mod1, vcov = mod1_se)[, "Std. Error"]

mod2 = lm(ly2002 ~ tr1 + mena + eur + sa + eap + na + latam,
            data = macro_final)
mod2_se = vcovCL(mod2, cluster = clus1000)
rob2 = coeftest(mod2, vcov = mod2_se)[, "Std. Error"]

clusc = na.omit(macro_final$clusc)

mod3 = lm(ly2002 ~ tr1500cc,
          data = macro_final |> filter(!is.na(clusc)))
mod3_se = vcovCL(mod3, cluster = clusc)
rob3 = coeftest(mod3, vcov = mod3_se)[, "Std. Error"]

mod4 = lm(ly2002 ~ tr1500cc + mena + eur + sa + eap + na + latam,
                 data = macro_final |> filter(!is.na(clusc)))
mod4_se = vcovCL(mod4, cluster = clusc)
rob4 = coeftest(mod4, vcov = mod4_se)[, "Std. Error"]

#output = stargazer(mod1, mod2, mod3, mod4,
#          se = list(rob1, rob2, rob3, rob4),
#          type = "latex",
#          dep.var.labels = "Log income per capita in 2002",
#          covariate.labels = c("Overall technology adoption level in 1000 BC",
#                               "MENA dummy", "Europe dummy", "South Asia dummy",
#                               "East Asia dummy", "North American dummy",
#                               "Latin America dummy",
#                               "Overall technology adoption level in 1500 AD"),
#          star.cutoffs = c(0.05, 0.01, 0.001),
#          star.char = c("*", "**", "***"),
#          omit.stat = c("f", "ser"))

mod5 = lm(ly2002 ~ tr1 + eu + af + as + am,
          data = macro_final)
mod5_se = vcovCL(mod5, cluster = clus1000)
coeftest(mod5, vcov = mod5_se)

mod6 = lm(ly2002 ~ tr2 + eu + af + as + am,
          data = macro_final)
mod6_se = vcovCL(mod6, cluster = clus1000)
coeftest(mod6, vcov = mod6_se)

mod7 = lm(ly2002 ~ tr1500cc + eu + af + as + am,
          data = macro_final |> filter(!is.na(clusc)))
mod7_se = vcovCL(mod7, cluster = clusc)
coeftest(mod7, vcov = mod7_se)

mod8 = lm(ly2002 ~ tr1mig + mena + eur + sa + eap + na + latam,
          data = macro_final)
mod8_se = vcovCL(mod8, cluster = clus1000)
rob8 = coeftest(mod8, vcov = mod8_se)[, "Std. Error"]

mod9 = lm(ly2002 ~ tr2mig + mena + eur + sa + eap + na + latam,
          data = macro_final)
mod9_se = vcovCL(mod9, cluster = clus1000)
rob9 = coeftest(mod9, vcov = mod9_se)[, "Std. Error"]

mod10 = lm(ly2002 ~ t3mig + mena + eur + sa + eap + na + latam,
           data = macro_final |> filter(!is.na(clusc)))
mod10_se = vcovCL(mod10, cluster = clusc)
rob10 = coeftest(mod10, vcov = mod10_se)[, "Std. Error"]

# Replicating Table 7A, columns 3, 4

mod11 = lm(tr1500cc ~ tr1 + eu + af + as + am,
           data = macro_final)
mod11_se = vcovCL(mod11, cluster = clus1000)
coeftest(mod11, vcov = mod11_se)

mod12 = lm(tr1500cc ~ tr2 + eu + af + as + am,
           data = macro_final)
mod12_se = vcovCL(mod12, cluster = clus1000)
coeftest(mod12, vcov = mod12_se)

mod13 = lm(tr1500cc ~ tr1 + mena + eur + sa + eap + na + latam,
           data = macro_final)
mod13_se = vcovCL(mod13, cluster = clus1000)
coeftest(mod13, vcov = mod13_se)

mod14 = lm(tr1500cc ~ tr2 + mena + eur + sa + eap + na + latam,
           data = macro_final)
mod14_se = vcovCL(mod14, cluster = clus1000)
coeftest(mod14, vcov = mod14_se)

# Replicating Table 7B, column 2

mod15 = lm(tr2 ~ tr1 + eu + af + as + am,
           data = macro_final)
mod15_se = vcovCL(mod15, cluster = clus1000)
coeftest(mod15, vcov = mod15_se)

mod16 = lm(tr2 ~ tr1 + mena + eur + sa + eap + na + latam,
           data = macro_final)
mod16_se = vcovCL(mod16, cluster = clus1000)
coeftest(mod16, vcov = mod16_se)
