library(tidyverse)
library(AER)

pwt = read_csv("datasets/pwt_analytic.csv")
wb_regions = read_csv("datasets/world-regions-according-to-the-world-bank.csv")

pwt_merge = pwt |>
  left_join(wb_regions, by = c("country_code" = "Code")) |>
  select(-Entity, -Year) |>
  rename(wb_region = `World Region according to the World Bank`)
  
east_asia = c("CHN", "HKG", "JPN", "KOR", "MAC", "MNG")

pwt_final = pwt_merge |>
  mutate(initial_gdp = log(initial_gdp),
         pop_growth = log(pop_growth*100 + 0.1),
         pop_growth_instr = log((pop_growth_instr + 0.1) * 100),
         schooling = log(schooling),
         school_instr = log(school_instr),
         domestic_investment = log(domestic_investment),
         investment_instr = log(investment_instr),
         mortality = 1/life_expectancy,
         ssa = wb_region == "Sub-Saharan Africa" |
           country_code == "ZAR",
         east_asia = country_code %in% east_asia,
         latam = wb_region == "Latin America and Caribbean")

endog_vars <- c("domestic_investment", "schooling", "govt_consumption", "inflation")

iv_vars <- c("ln_rgdppw_instr", "pop_growth_instr", "school_instr", "investment_instr",
             "trade_instr", "govt_instr", "colony_esp_prt", 
             "catholic_1900", "protestant_1900", "orthodox_1900", "muslim_1900",
             "jewish_1900", "hindu_1900", "eastern_1900", "non-religious_1900", "other_1900",
             "legal_origin")

# Define the full model formula
formula_iv <- as.formula("growth ~ log_income + pop_growth + fertility + mortality +
                         trade_to_gdp + govt_consumption + inflation +
                         protestant + orthodox + muslim + hindu + jewish + eastern + other +
                         near_coast + tropical + language + ethnic + avexpr +
                         ssa + latam + east_asia | . - domestic_investment - schooling - govt_consumption - inflation +
                         ln_rgdppw_instr + pop_growth_instr + school_instr + investment_instr +
                         trade_instr + govt_instr + colony_esp_prt +
                         catholic_1900 + protestant_1900 + orthodox_1900 + muslim_1900 +
                         jewish_1900 + hindu_1900 + eastern_1900 + other_1900 +
                         legal_origin")

model_iv <- ivreg(formula_iv, data = pwt_final)

# View results
summary(model_iv)
