packages = c("dplyr", "readr", "haven", "estimatr", 
             "conleyreg", "lmtest", "sandwich", "tibble",
             "quantreg", "stringr", "spatInfer", "modelsummary",
             "ggplot2", "tinytable", "plm")
lapply(packages, library, character.only = TRUE)

slave_data = read_dta("../../datasets/6-nunn/slave_trade_QJE.dta")

slave_final = slave_data |>
  mutate(congo_dummy = isocode == "ZAR")

mod1 = lm(ln_maddison_pcgdp2000 ~ ln_export_area + colony0 + colony1 + colony2 +
            colony3 + colony4 + colony5 + colony6,
          data = slave_data)

mod2 = lm(ln_maddison_pcgdp2000 ~ ln_export_area + colony0 + colony1 + colony2 +
            colony3 + colony4 + colony5 + colony6 + abs_latitude + longitude +
            rain_min + humid_max + low_temp + ln_coastline_area + island_dum +
            islam + legor_fr + region_n + ln_avg_gold_pop + ln_avg_oil_pop +
            ln_avg_all_diamonds_pop,
          data = slave_data)

mod3 = lm(ln_maddison_pcgdp2000 ~ ln_export_area + colony0 + colony1 + colony2 +
            colony3 + colony4 + colony5 + colony6 + abs_latitude + longitude +
            rain_min + humid_max + low_temp + ln_coastline_area + island_dum +
            islam + legor_fr + region_n + ln_avg_gold_pop + ln_avg_oil_pop +
            ln_avg_all_diamonds_pop + congo_dummy,
          data = slave_final)
