packages = c("dplyr", "readr", "haven", "estimatr", 
             "conleyreg", "lmtest", "sandwich", "tibble",
             "quantreg", "stringr", "spatInfer", "modelsummary",
             "ggplot2", "tinytable", "plm", "stargazer")
lapply(packages, library, character.only = TRUE)

colony_data = read_dta("datasets/8-ajr-2002/maketable3.dta")
wb_regions = read_csv("datasets/world-regions-according-to-the-world-bank.csv") |> 
  janitor::clean_names() |>
  select(-entity, -year) |>
  rename(wb_region = world_region_according_to_the_world_bank)
malaria = read_dta("datasets/6-nunn/country.dta") |>
  select(code, malfal)

colony_final = colony_data |>
  left_join(malaria, by = c("shortnam" = "code")) |>
  left_join(wb_regions, by = c("shortnam" = "code")) |>
  mutate(wb_region = case_when(
    shortnam %in% c("ROM", "YUG") ~ "Europe and Central Asia",
    shortnam == "TWN" ~ "East Asia and Pacific",
    shortnam == "ZAR" ~ "Sub-Saharan Africa",
    TRUE ~ wb_region),
    malaria_dummy = malfal <= 0.05)

mod1 = lm(logpgp95 ~ sjb1500, 
          data = colony_final |> filter(baserf == 1))

mod2 = lm(logpgp95 ~ sjb1500 + malaria_dummy + wb_region,
          data = colony_final |> filter(baserf == 1))

output = stargazer(
  mod1, mod2,
  type = "latex",
  title = "Regression Results",
  dep.var.labels = "Log GDP per capita (PPP), 1995",
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







