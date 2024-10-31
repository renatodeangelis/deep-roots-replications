packages = c("dplyr", "readr", "haven", "estimatr", 
             "conleyreg", "lmtest", "sandwich", "tibble",
             "quantreg", "stringr", "spatInfer", "modelsummary",
             "ggplot2", "tinytable", "plm", "stargazer")
lapply(packages, library, character.only = TRUE)

colony_data = read_dta("datasets/7-ajr-2001/maketable3.dta")

mod1 = lm(avexpr ~ logem4,
          data = colony_data |> filter(excolony == 1,
                                       !is.na(extmort4),
                                       !is.na(logpgp95)))

mod2 = lm(avexpr ~ logem4 + lat_abst,
          data = colony_data |> filter(excolony == 1,
                                       !is.na(extmort4),
                                       !is.na(logpgp95)))

