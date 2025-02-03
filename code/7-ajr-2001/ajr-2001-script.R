packages = c("dplyr", "readr", "haven", "estimatr", 
             "conleyreg", "lmtest", "sandwich", "tibble",
             "quantreg", "stringr", "spatInfer", "modelsummary",
             "ggplot2", "tinytable", "plm", "stargazer", "reaxl")
lapply(packages, library, character.only = TRUE)

colony_data = read_dta("datasets/7-ajr-2001/maketable3.dta")
colony_names = read_dta("datasets/7-ajr-2001/maketable2.dta")
wb_regions = read_csv("datasets/world-regions-according-to-the-world-bank.csv")  |> 
  janitor::clean_names() |>
  select(-entity, -year) |>
  rename(wb_region = world_region_according_to_the_world_bank)
pwt_init = read_excel("Datasets/pwt1001.xlsx") |>
  filter(year == 1995) |>
  mutate(countrycode = ifelse(countrycode == "ROU", "ROM", countrycode))

colony_final = colony_data |>
  left_join(colony_names) |>
  left_join(wb_regions, by = c("shortnam" = "code")) |>
  mutate(wb_region = case_when(
    shortnam %in% c("ROM", "YUG") ~ "Europe and Central Asia",
    shortnam == "TWN" ~ "East Asia and Pacific",
    shortnam == "ZAR" ~ "Sub-Saharan Africa",
    TRUE ~ wb_region)) |>
  left_join(pwt_init, by = c("shortnam" = "countrycode"))

mod1 = lm(avexpr ~ logem4,
          data = colony_data |> filter(excolony == 1,
                                       !is.na(extmort4),
                                       !is.na(logpgp95)))

mod2 = lm(avexpr ~ logem4 + lat_abst,
          data = colony_data |> filter(excolony == 1,
                                       !is.na(extmort4),
                                       !is.na(logpgp95)))

mod3 = lm(avexpr ~ logem4 + lat_abst + wb_region,
          data = colony_final |> filter(excolony == 1,
                                       !is.na(extmort4),
                                       !is.na(logpgp95)))

output = stargazer(
  mod1, mod2, mod3,
  type = "latex",
  title = "Regression Results",
  dep.var.labels = "Average Protection Against Expropriation Risk, 1985-1995",
  column.labels = c("Model 1", "Model 2", "Model 3"),
  covariate.labels = c("Log European settler mortality", "Latitude"),
  omit.stat = c("f"),  # Omit F-statistic and standard error
  add.lines = list(
    c("Observations", 
      nrow(colony_data |> filter(excolony == 1, !is.na(extmort4), !is.na(logpgp95))), 
      nrow(colony_data |> filter(excolony == 1, !is.na(extmort4), !is.na(logpgp95))), 
      nrow(colony_final |> filter(excolony == 1, !is.na(extmort4), !is.na(logpgp95)))),
    c("R-squared", 
      round(summary(mod1)$r.squared, 3),
      round(summary(mod2)$r.squared, 3),
      round(summary(mod3)$r.squared, 3)),
    c("wb_region Controls", "", "", "\\checkmark")  # Checkmark for wb_region in Model 3
  ),
  star.cutoffs = c(0.05, 0.01, 0.001),
  no.space = TRUE
)
