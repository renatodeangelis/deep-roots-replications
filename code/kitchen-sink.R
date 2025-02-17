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
         schooling = log(schooling),
         school_instr = log(school_instr),
         domestic_investment = log(domestic_investment),
         investment_instr = log(investment_instr),
         mortality = 1/life_expectancy,
         ssa = wb_region == "Sub-Saharan Africa" |
           country_code == "ZAR",
         east_asia = country_code %in% east_asia,
         latam = wb_region == "Latin America and Caribbean",
         formalism = ifelse(period_1 == TRUE, allstpi1980,
                            ifelse(period_2 == TRUE, allstpi1990, allstpi2000)))

## Model 1: Kitchen sink regression

# First stage
iv_initial_gdp = lm(initial_gdp ~ initial_gdp_instr, data = pwt_final)
iv_hc = lm(initial_hc ~ initial_hc_instr, data = pwt_final)
iv_schooling = lm(schooling ~ school_instr, data = pwt_final)
iv_investment = lm(domestic_investment ~ investment_instr, data = pwt_final)
iv_pop = lm(pop_growth ~ pop_growth_instr, data = pwt_final)
iv_trade = lm(trade_to_gdp ~ trade_instr, data = pwt_final)
iv_govt = lm(govt_consumption ~ govt_instr, data = pwt_final)
iv_inflation = lm(cum_inflation ~ colony_esp_prt, data = pwt_final)
iv_catholic = lm(catholic ~ catholic_1900, data = pwt_final)
iv_orthodox = lm(orthodox ~ orthodox_1900, data = pwt_final)
iv_protestant = lm(protestant ~ protestant_1900, data = pwt_final)
iv_hindu = lm(hindu ~ hindu_1900, data = pwt_final)
iv_muslim = lm(muslim ~ muslim_1900, data = pwt_final)
iv_jewish = lm(jewish ~ jewish_1900, data = pwt_final)
iv_eastern = lm(eastern ~ eastern_1900, data = pwt_final)
iv_other = lm(other ~ other_1900, data = pwt_final)
iv_nonreligious = lm(`non-religious` ~ `non-religious_1900`, data = pwt_final)
iv_formalism = lm(formalism ~ legal_origin, data = pwt_final)







