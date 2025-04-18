packages = c("dplyr", "readr", "haven", "estimatr", 
             "conleyreg", "lmtest", "sandwich", "tibble",
             "quantreg", "stringr", "spatInfer", "modelsummary",
             "ggplot2", "tinytable")
sapply(packages, library, character.only = TRUE)

city_data = read_dta("datasets/3-guiso-sapienza-zingales/ltp1F.dta")
coords = read_csv("datasets/3-guiso-sapienza-zingales/places.csv")

codici = read_delim("datasets/3-guiso-sapienza-zingales/codici-istat.csv", 
                    delim = ";",
                    locale = locale(encoding = "ISO-8859-1")) |>
  janitor::clean_names()

preced = read_delim("datasets/3-guiso-sapienza-zingales/comuni-precedenti.csv",
                    delim = ";",
                    locale = locale(encoding = "ISO-8859-1")) |>
  janitor::clean_names()

soppr = read_delim("datasets/3-guiso-sapienza-zingales/comuni-soppressi.csv",
                   delim = ";",
                   locale = locale(encoding = "ISO-8859-1")) |>
  janitor::clean_names()

codici1 = codici |>
  select(denominazione_in_italiano, 
         codice_comune_numerico_con_103_province_dal_1995_al_2005) |>
  rename(codice_comune = codice_comune_numerico_con_103_province_dal_1995_al_2005)

soppr1 = soppr |>
  select(codice_comune, denominazione_comune) |>
  mutate(codice_comune = str_replace(codice_comune, "^0+", ""),
         codice_comune = as.double(codice_comune)) 

preced1 = preced |>
  select(denominazione_precedente, codice_comune) |>
  mutate(codice_comune = str_replace(codice_comune, "^0+", ""),
         codice_comune = as.double(codice_comune)) 

istat = codici1 |>
  full_join(soppr1, by = c("codice_comune" = "codice_comune",
                           "denominazione_in_italiano" = "denominazione_comune")) |>
  full_join(preced1, by = c("codice_comune" = "codice_comune",
                            "denominazione_in_italiano" = "denominazione_precedente")) |>
  group_by(codice_comune) |>
  slice(1) |>
  ungroup()

#standardize_name = function(name) {
#  name |>
#    str_to_lower() |>
#    str_replace_all("[[:punct:]]", "") |>
#    str_squish()
#}

coords = coords |>
  group_by(name) |>
  filter(n() == 1 | population > 0) |>
  slice(1) |>
  ungroup() |>
  select(X, Y, name) #|>
#  mutate(name = standardize_name(name))

city_merged = city_data |>
  left_join(istat, by = c("istcom" = "codice_comune")) |>
  #  mutate(name = standardize_name(denominazione_in_italiano)) |>
  left_join(coords, by = c("denominazione_in_italiano" = "name"))

city_final = city_merged |>
  filter(dummyroma == 0,
         regione < 20 | is.na(regione),
         totassoc_p > 0) |>
  janitor::clean_names() |>
  rename(X = x, Y = y)

mod1 = lm(totassoc_p ~ libero_comune_allnord + altitudine + escursione +
            costal + nearsea + population + pop2 + gini_income + gini_land +
            income_p, 
          data = city_final |> filter(!is.na(X)), 
          weights = population)
coeftest(mod1, vcov = vcovHC(mod1, type = "HC1"))

summary(mod1)










