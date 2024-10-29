packages = c("dplyr", "readr", "haven", "estimatr", 
             "conleyreg", "lmtest", "sandwich", "tibble",
             "quantreg", "stargazer")
sapply(packages, library, character.only = TRUE)

nazi_data = read_dta("../datasets/5-voigtlander-voth/Dataset_QJE_Replicate_with_Cities(1).dta")

nazi_final = nazi_data |>
  filter(judaica == 1 | comm1349 == 1) |>
  mutate(nazi_1928 = n285nsda / n285gs,
         ln_pop = log(n285pop),
         frac_jew = c25juden / c25pop,
         frac_prot = c25prot / c25pop)

kreis_nr = nazi_final |> 
  pull(kreis_nr)
mod1 = lm(nazi_1928 ~ pog1349 + ln_pop + frac_jew + frac_prot,
          data = nazi_final)
mod1_se = vcovCL(mod1, cluster = kreis_nr)
coeftest(mod1, vcov = mod1_se)

kreis_nr1 = nazi_final |> 
  filter(nazi_1928 < 0.2) |>
  pull(kreis_nr)
mod2 = lm(nazi_1928 ~ pog1349 + ln_pop + frac_jew + frac_prot,
          data = nazi_final |> filter(nazi_1928 < 0.2))
mod2_se = vcovCL(mod2, cluster = kreis_nr1)
coeftest(mod2, vcov = mod2_se)

mod3 = rq(nazi_1928 ~ pog1349 + ln_pop + frac_jew + frac_prot,
          data = nazi_final)
summary(mod3, se = "boot", R = 1000)
