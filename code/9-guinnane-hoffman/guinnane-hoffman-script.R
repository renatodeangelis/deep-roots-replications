packages = c("dplyr", "readr", "haven", "lmtest", "sandwich",
             "quantreg", "stargazer")
sapply(packages, library, character.only = TRUE)

nazi_data = read_dta("datasets/5-voigtlander-voth/Dataset_QJE_Replicate_with_Cities(1).dta")

## Generating variables

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
         syn33 = case_when(
           syn33 == 0 & betraum == 1 ~ 1,
           TRUE ~ syn33))

## Principal components

nazi_final = nazi_final |>
  mutate(std_pog20s = pog20s / pop33,
         std_stuermer = sturmer / pop33,
         std_deport = prop_deport / pop33,
         std_syn_attack = syn_attack / pop33)

pca1 <- nazi_final |>
  filter(syn33 == 1) |>
  select(std_pog20s, dvfp_1924, nazi_1928, std_stuermer, std_deport, std_syn_attack) |>
  na.omit() |>
  prcomp(center = TRUE, scale. = TRUE)

pca1_scores <- predict(pca1)[, 1]

pca1_data <- nazi_final |>
  filter(syn33 == 1) |>
  select(std_pog20s, dvfp_1924, nazi_1928, std_stuermer, std_deport, std_syn_attack) |>
  na.omit()

nazi_final <- nazi_final |>
  mutate(row_id = row_number())

pca1_rows <- nazi_final |>
  filter(syn33 == 1) |>
  select(std_pog20s, dvfp_1924, nazi_1928, std_stuermer, std_deport, std_syn_attack, row_id) |>
  na.omit() |>
  pull(row_id)

nazi_final <- nazi_final |>
  mutate(PCA_20C_AS = NA_real_)

for(i in seq_along(pca1_rows)) {
  nazi_final$PCA_20C_AS[pca1_rows[i]] <- pca1_scores[i]
}

pca2 <- nazi_final |>
  filter(syn33 == 1) |>
  select(std_pog20s, dvfp_1924, nazi_1928, std_stuermer, std_syn_attack) |>
  na.omit() |>
  prcomp(center = TRUE, scale. = TRUE)

pca2_scores <- predict(pca2)[, 1]

pca2_rows <- nazi_final |>
  filter(syn33 == 1) |>
  select(std_pog20s, dvfp_1924, nazi_1928, std_stuermer, std_syn_attack, row_id) |>
  na.omit() |>
  pull(row_id)

for(i in seq_along(pca2_rows)) {
  if(is.na(nazi_final$PCA_20C_AS[pca2_rows[i]])) {
    nazi_final$PCA_20C_AS[pca2_rows[i]] <- pca2_scores[i]
  }
}

pca3 <- nazi_final |>
  select(std_pog20s, dvfp_1924, nazi_1928, std_stuermer, std_deport) |>
  na.omit() |>
  prcomp(center = TRUE, scale. = TRUE)

pca3_scores <- predict(pca3)[, 1]

pca3_rows <- nazi_final |>
  select(std_pog20s, dvfp_1924, nazi_1928, std_stuermer, std_deport, row_id) |>
  na.omit() |>
  pull(row_id)

for(i in seq_along(pca3_rows)) {
  if(is.na(nazi_final$PCA_20C_AS[pca3_rows[i]])) {
    nazi_final$PCA_20C_AS[pca3_rows[i]] <- pca3_scores[i]
  }
}

pca4 <- nazi_final |>
  filter(syn33 == 1) |>
  select(dvfp_1924, nazi_1928, std_stuermer, std_deport, std_syn_attack) |>
  na.omit() |>
  prcomp(center = TRUE, scale. = TRUE)

pca4_scores <- predict(pca4)[, 1]

pca4_rows <- nazi_final |>
  filter(syn33 == 1) |>
  select(dvfp_1924, nazi_1928, std_stuermer, std_deport, std_syn_attack, row_id) |>
  na.omit() |>
  pull(row_id)

for(i in seq_along(pca4_rows)) {
  if(is.na(nazi_final$PCA_20C_AS[pca4_rows[i]]) && nazi_final$syn33[pca4_rows[i]] == 1) {
    nazi_final$PCA_20C_AS[pca4_rows[i]] <- pca4_scores[i]
  }
}

nazi_final <- nazi_final |>
  select(-row_id)

vars_to_std <- c("PCA_20C_AS", "pog1349", "ln_pop_28", "ln_pop_33", 
                 "frac_jew_25", "frac_jew_33", "frac_prot_25")

# Apply standardization to each variable
for(var in vars_to_std) {
  var_data <- nazi_final[[var]][!is.na(nazi_final$pog1349)]
  var_mean <- mean(var_data, na.rm = TRUE)
  var_sd <- sd(var_data, na.rm = TRUE)
  
  nazi_final[[paste0("Std_", var)]] <- NA_real_
  nazi_final[[paste0("Std_", var)]][!is.na(nazi_final$pog1349)] <- 
    (var_data - var_mean) / var_sd
}

# Display summary of standardized variables
std_vars <- paste0("Std_", vars_to_std)
summary_data <- nazi_final |> 
  select(all_of(std_vars)) |> 
  summary()

print(summary_data)

## Table 1

kreis_nr = nazi_final |> 
  pull(kreis_nr)

mod1 = lm(nazi_1928 ~ pog1349 + ln_pop_28 + frac_jew_25 + frac_prot_25,
          data = nazi_final)
mod1_res = coeftest(mod1, vcov = vcovCL(mod1, cluster = kreis_nr))

mod2 = rq(nazi_1928 ~ pog1349 + ln_pop_28 + frac_jew_25 + frac_prot_25,
          data = nazi_final)
mod2_res = summary(mod2, se = "boot", cluster = kreis_nr, R = 1000)

mod3 = lm(Std_PCA_20C_AS ~ pog1349 + Std_ln_pop_33 + Std_frac_jew_33 + Std_frac_prot_25,
          data = nazi_final)
mod3_res = coeftest(mod3, vcov = vcovCL(mod3, cluster = kreis_nr))

mod4 = rq(Std_PCA_20C_AS ~ pog1349 + Std_ln_pop_33 + Std_frac_jew_33 + Std_frac_prot_25,
          data = nazi_final)
mod4_res = summary(mod4, se = "boot", cluster = kreis_nr, R = 1000)

mod5 = glm(deptotal ~ pog1349 + ln_pop_33 + ln_jews_33 + frac_jew_33 + frac_prot_25,
           data = nazi_final,
           family = poisson)
mod5_res = coeftest(mod5, vcov = vcovCL(mod5, type = "HC0"))

mod6 = glm(deptotal ~ pog1349 + ln_pop_33 + frac_jew_33 + frac_prot_25,
           data = nazi_final |> filter(!is.na(ln_jews_33)),
           family = poisson)
mod6_res = coeftest(mod6, vcov = vcovCL(mod6, type = "HC0"))

## Table 3





















