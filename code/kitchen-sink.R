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
         initial_gdp_instr = log(initial_gdp_instr),
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

pwt_final$pred_initial_gdp <- predict(iv_initial_gdp, pwt_final)
pwt_final$pred_hc <- predict(iv_hc, pwt_final)
pwt_final$pred_schooling <- predict(iv_schooling, pwt_final)
pwt_final$pred_investment <- predict(iv_investment, pwt_final)
pwt_final$pred_pop <- predict(iv_pop, pwt_final)
pwt_final$pred_trade <- predict(iv_trade, pwt_final)
pwt_final$pred_govt <- predict(iv_govt, pwt_final)
pwt_final$pred_inflation <- predict(iv_inflation, pwt_final)
pwt_final$pred_catholic <- predict(iv_catholic, pwt_final)
pwt_final$pred_orthodox <- predict(iv_orthodox, pwt_final)
pwt_final$pred_protestant <- predict(iv_protestant, pwt_final)
pwt_final$pred_hindu <- predict(iv_hindu, pwt_final)
pwt_final$pred_muslim <- predict(iv_muslim, pwt_final)
pwt_final$pred_jewish <- predict(iv_jewish, pwt_final)
pwt_final$pred_eastern <- predict(iv_eastern, pwt_final)
pwt_final$pred_other <- predict(iv_other, pwt_final)
pwt_final$pred_nonreligious <- predict(iv_nonreligious, pwt_final)
pwt_final$pred_formalism <- predict(iv_formalism, pwt_final)

kitchen_sink = lm(growth ~ pred_initial_gdp + pred_pop + pred_schooling +
                    pred_investment + pred_trade +
                    pred_govt + east_asia + ssa + latam +
                    pred_eastern + pred_jewish + pred_muslim + pred_orthodox +
                    pred_protestant + pred_nonreligious + pred_other +
                    near_coast + tropical + language + ethnic + avexpr,
                  data = pwt_final)






