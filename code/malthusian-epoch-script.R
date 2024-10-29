packages = c("dplyr", "readr", "haven", "estimatr", 
             "conleyreg", "lmtest", "sandwich", "tibble",
             "quantreg", "stargazer")
sapply(packages, library, character.only = TRUE)

data = read_dta("../datasets/1-ashraf-galor/20081371_Dataset.dta")
wb_regions = read_csv("../datasets/world-regions-according-to-the-world-bank.csv")

wb_regions = wb_regions |> 
  janitor::clean_names() |>
  select(-entity, -year) |>
  rename(wb_region = world_region_according_to_the_world_bank)

data_set = left_join(data, wb_regions, by = "code") |>
  mutate(wb_region = case_when(
    code == "WBG" ~ "Middle East and North Africa",
    code %in% c("ADO", "CHI", "IMY", "ROM", "YUG") ~ "Europe and Central Asia",
    code %in% c("FSM", "TMP") ~ "East Asia and Pacific",
    code == "ANT" ~ "Latin America and Caribbean",
    code %in% c("MYT", "ZAR") ~ "Sub-Saharan Africa",
    TRUE ~ wb_region))

parallel_10 = c("ARG", "CHL", "PRY", "URY", "NAM", "BWA", "ZWE", "ZAF",
                "SWZ", "LSO", "MOZ", "MDG", "AUS", "NZL", "ZMB", "BRA",
                "PER", "BOL", "MWI")

final_data = data_set |>
  mutate(mena = as.integer(wb_region == "Middle East and North Africa"),
         eur = as.integer(wb_region == "Europe and Central Asia"),
         sa = as.integer(wb_region == "South Asia"),
         eap = as.integer(wb_region == "East Asia and Pacific"),
         na = as.integer(wb_region == "North America"),
         latam = as.integer(wb_region == "Latin America and Caribbean"),
         afr = as.integer(wb_region == "Sub-Saharan Africa")) |>
  mutate(control = as.integer(eur == 1 | sa == 1)) |>
  mutate(south_10  = as.integer(code %in% parallel_10))

mod1 = lm(ln_pd1500 ~ ln_yst + pc_lnar_lnas + ln_abslat + 
            distcr1000 + land100cr + africa + europe + asia,
          data = final_data |> filter(cleanhibbs == 1))
mod1_se = vcovHC(mod1, type = "HC1")
coeftest(mod1, vcov = mod1_se)

mod2 = lm(ln_pd1500 ~ ln_yst + pc_lnar_lnas + ln_abslat + 
            distcr1000 + land100cr + eur + sa + south_10,
          data = final_data |> filter(cleanhibbs == 1))
mod2_se = vcovHC(mod2, type = "HC1")
coeftest(mod2, vcov = mod2_se)

#output = stargazer(mod1, mod2,
#          se = list(rob1, rob2),
#          type = "latex",
#          dep.var.labels = "Log Population Density in 1500 CE",
#          covariate.labels = c("Log years since Neolithic transition",
#                               "Log land productivity", "Log absolute latitude",
#                               "Mean distance to nearest coast or river",
#                               "Percentage of land within 100km of coast or river",
#                               "Africa", "Europe", "Asia", "Eur", "SA", "South_10"),
#          star.cutoffs = c(0.05, 0.01, 0.001),
#          star.char = c("*", "**", "***"),
#          omit.stat = c("f", "ser"))
```






