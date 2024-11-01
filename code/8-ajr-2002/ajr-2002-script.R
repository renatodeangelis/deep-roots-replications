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

mod2 = lm(logpgp95 ~ sjb1500 + wb_region + malaria_dummy,
          data = colony_final |> filter(baserf == 1))

mod3 = mod1 = lm(logpgp95 ~ sjb1500 + lat_abst + coal + landlock + island + 
                   goldm + iron + silv +  zinc + oilres + steplow + deslow + 
                   stepmid + desmid + f_french + f_spain + f_pothco + f_dutch + 
                   catho80 + muslim80 + notmcp80,
                 data = colony_final |> filter(baserf == 1))







