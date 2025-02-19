library(tidyverse)
library(AER)

pwt_final = read_csv("datasets/pwt_final.csv") |>
  mutate(growth = growth_rate * 100)

## Model 1: Kitchen sink regression

# First stage
iv_initial_gdp = lm(initial_gdp ~ initial_gdp_instr, data = pwt_final)
iv_enrollment = lm(enrollment ~ enrollment_instr, data = pwt_final)
iv_investment = lm(ki ~ ki_instr, data = pwt_final)
iv_pop = lm(pop_growth ~ pop_growth_instr, data = pwt_final)
iv_trade = lm(openk ~ openk_instr, data = pwt_final)
iv_govt = lm(kg ~ kg_instr, data = pwt_final)
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
iv_formalism = lm(formalism ~ french_origin + british_origin, data = pwt_final)

pwt_final$pred_initial_gdp <- predict(iv_initial_gdp, pwt_final)
pwt_final$pred_enrollment <- predict(iv_enrollment, pwt_final)
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

kitchen_sink = lm(growth_rate ~ pred_initial_gdp + pred_pop + pred_investment +
                    fertility + mortality + pred_trade + pred_inflation +
                    pred_govt + east_asia + ssa + latam + pred_eastern +
                    pred_hindu + pred_jewish + pred_muslim + pred_orthodox +
                    pred_protestant + pred_nonreligious + pred_other +
                    near_coast + tropical + language + ethnic +
                    period_2 + period_3,
                  data = pwt_final |> filter(!(isocode %in% c("GNQ", "CH2"))))






