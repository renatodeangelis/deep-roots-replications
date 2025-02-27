packages = c("dplyr", "readr", "haven", "lmtest", "sandwich",
             "quantreg", "stargazer")
sapply(packages, library, character.only = TRUE)

nazi_data = read_dta("datasets/5-voigtlander-voth/Dataset_QJE_Replicate_with_Cities(1).dta")

nazi_final = nazi_data |>
  filter(judaica == 1 | comm1349 == 1) |>
  mutate(nazi_1928 = n285nsda / n285gs,
         dvfp_1924 = n245dvfp / n245gs,
         sturmer = stuer1 + stuer2 + stuer3,
         pop33 = ifelse(is.na(pop33), n333pop, pop33),
         ln_pop_24 = log(n245pop),
         ln_pop_28 = log(n285pop),
         ln_pop_33 = log(pop33),
         ln_jews_33 = log(1 + jews33),
         frac_jew_25 = 100 * c25juden / c25pop,
         frac_prot_25 = 100 * c25prot / c25pop,
         frac_jew_33 = 100 * jews33 / pop33,
         frac_jew_33 = case_when(
           frac_jew_33 == 1 ~ frac_jew_25,
           is.na(frac_jew_33) & !is.na(frac_jew_25) ~ frac_jew_25,
           TRUE ~ frac_jew_33),
         prop_deport = 100 * deptotal / jews33,
         syn_attack = syndam == 1 | syndest == 1,
#         syn_33 = case_when(
#           syn33 == 0 & betraum == 1 ~ 1,
#           TRUE ~ syn_33)
         )

## Guinnane & Hoffman Table 1

kreis_nr = nazi_final |> 
  pull(kreis_nr)

nazi_final = nazi_final |>
  mutate(std_pog20s = pog20s / pop33,
         std_sturmer = sturmer / pop33,
         std_deport = prop_deport / pop33,
         std_syn_attack = syn_attack / pop33)

pca1 = nazi_final |>
  filter(syn33 == 1) |>
  select(std_pog20s, nazi_1928, dvfp_1924, std_deport, std_sturmer, std_syn_attack) |>
  prcomp() |>
  summary()

mod1 = lm(nazi_1928 ~ pog1349 + ln_pop_28 + frac_jew_25 + frac_prot_25,
          data = nazi_final)
mod1_res = coeftest(mod1, vcov = vcovCL(mod1, cluster = kreis_nr))

mod2 = rq(nazi_1928 ~ pog1349 + ln_pop_28 + frac_jew_25 + frac_prot_25,
          data = nazi_final)
summary(mod2)
mod2_res = summary(mod2, se = "boot", cluster = kreis_nr, R = 1000)


mod5 = glm(deptotal ~ pog1349 + ln_pop_33 + ln_jews_33 + frac_jew_33 + frac_prot_25,
           data = nazi_final,
           family = poisson)
mod5_res = coeftest(mod5, vcov = vcovCL(mod5, type = "HC0"))

mod6 = glm(deptotal ~ pog1349 + ln_pop_33 + frac_jew_33 + frac_prot_25,
           data = nazi_final |> filter(!is.na(ln_jews_33)),
           family = poisson)
mod6_res = coeftest(mod6, vcov = vcovCL(mod6, type = "HC0"))

## Table 2

#reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25

mod7 = lm(dvfp_1924 ~ pog1349 + ln_pop_24 + frac_jew_25 + frac_prot_25,
          data = nazi_final)
mod7_res = coeftest(mod7, vcov = vcovCL(mod7, cluster = kreis_nr))

## Table 3





















