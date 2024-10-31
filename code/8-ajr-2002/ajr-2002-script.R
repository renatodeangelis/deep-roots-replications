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
  left_join(wb_regions, by = c("shortnam" = "code")) #|>
  mutate(wb_region = case_when(
    shortnam %in% c("ROM", "YUG") ~ "Europe and Central Asia",
    shortnam == "TWN" ~ "East Asia and Pacific",
    shortnam == "ZAR" ~ "Sub-Saharan Africa",
    TRUE ~ wb_region))
