packages = c("dplyr", "readr", "haven", "estimatr", 
             "conleyreg", "lmtest", "sandwich", "tibble",
             "quantreg")
sapply(packages, library, character.only = TRUE)

nazi_data = read_dta("../datasets/4-satyanath-voigtlander-voth/Dataset_Bowling_Replication_JPE.dta")

nazi_int = nazi_data |>
  mutate(lnNSentry_total = log(1 + NSentry_total), # log entry rates
         lnNSentry_FU_total = log(1 + NSentry_FU_total),
         lnclubs_all = log(clubs_all),
         pop2 = (pop25 / 1000)^2, # population
         pop3 = (pop25 / 1000)^3,
         lnpop25 = log(pop25),
         pop25_quintiles = ntile(pop25, 5), # size quintile dummy
         lnpop_density = log(pop_density), # population density
         dummy_maps = as.integer(area_source == "Maps"),
         i_popden_maps = lnpop_density * dummy_maps)

landweimar = nazi_int |> pull(landweimar)
mod1 = lm(pcNSentry_std ~ clubs_all_pc + lnpop25 + share_cath25 + bcollar25,
          data = nazi_int)
rob1 = coeftest(mod1, vcov = vcovCL(mod1, cluster = landweimar))[, "Std. Error"]

mod2 = lm(pcNSentry_std ~ clubs_all_pc + lnpop25 + share_cath25 + bcollar25 +
            share_prot25,
          data = nazi_int)
rob2 = coeftest(mod2, vcov = vcovCL(mod2, cluster = landweimar))[, "Std. Error"]

mod3 = rq(pcNSentry_std ~ clubs_all_pc + lnpop25 + share_cath25 + bcollar25,
          data = nazi_int)
summary(mod3, se = "boot", R = 1000)

pca_data = nazi_int |>
  dplyr::select(landweimar, govt_longest_perc, 
                party_longest_perc, 
                weimar_coalition_perc) |>
  distinct() |>
  filter(!landweimar %in% c("Lübeck", "Saarland", "Bremen")) |>
  mutate(weimar_coalition_perc = case_when(
    landweimar == "Bayern" ~ 1,
    TRUE ~ weimar_coalition_perc
  )) |>
  column_to_rownames(var = "landweimar")

stability = princomp(pca_data, cor = TRUE) # extract first principal component
govt_stability = as.data.frame(stability$scores) |>
  rownames_to_column(var = "landweimar") |>
  dplyr::select(-Comp.2, -Comp.3) |>
  arrange(desc(Comp.1)) |>
  rename(pc1 = Comp.1)
#  mutate(stability1 = as.integer(PC1 > median(PC1)), # make non-Prussia median unstable
#         stability2 = as.integer(PC1 > -0.5)) # make it stable

nazi_pca = left_join(nazi_int, govt_stability, by = "landweimar") |>
  filter(!landweimar %in% c("Lübeck", "Saarland", "Bremen", "Preußen")) |>
  mutate(stability1 = as.integer(pc1 >= median(pc1)),
         stability2 = as.integer(pc1 > median(pc1)))

#Median state stable
mod3 = lm(pcNSentry_PRS_std ~ clubs_all_pc + share_cath25 + lnpop25 + bcollar25,
          data = nazi_pca |> filter(stability1 == 0))
rob3 = coeftest(mod3, vcov = vcovHC(mod3, type = "HC1"))[, "Std. Error"]

mod4 = lm(pcNSentry_PRS_std ~ clubs_all_pc + share_cath25 + lnpop25 + bcollar25,
          data = nazi_pca |> filter(stability1 == 1))
rob4 = coeftest(mod4, vcov = vcovHC(mod4, type = "HC1"))[, "Std. Error"]

#Median state unstable
mod5 = lm(pcNSentry_PRS_std ~ clubs_all_pc + share_cath25 + lnpop25 + bcollar25,
          data = nazi_pca |> filter(stability2 == 0))
rob5 = coeftest(mod5, vcov = vcovHC(mod5, type = "HC1"))[, "Std. Error"]

mod6 = lm(pcNSentry_PRS_std ~ clubs_all_pc + share_cath25 + lnpop25 + bcollar25,
          data = nazi_pca |> filter(stability2 == 1))
rob6 = coeftest(mod6, vcov = vcovHC(mod6, type = "HC1"))[, "Std. Error"]

nazi_pca1 = nazi_pca |>
  mutate(prussia = landweimar == "Preußen")

mod4 = lm(pcNSentry_PRS_std ~ pc1*(clubs_all_pc + share_cath25 + 
                                     lnpop25 + bcollar25 + prussia),
          data = nazi_pca1)
coeftest(mod4, vcov = vcovHC(mod2, type = "HC1"))

#pca_data1 = nazi_int |>
#  dplyr::select(landweimar, govt_longest_perc, 
#                party_longest_perc) |>
#  distinct() |>
#  filter(!landweimar %in% c("Lübeck", "Saarland", "Bremen")) |>
#  column_to_rownames(var = "landweimar")

#stability = princomp(pca_data1, cor = TRUE) # extract first principal component
#govt_stability1 = as.data.frame(stability$scores) |>
#  rownames_to_column(var = "landweimar") |>
#  select(-Comp.2) |>
#  arrange(desc(Comp.1)) |>
#  rename(pc1 = Comp.1)

#nazi_pca1 = left_join(nazi_int, govt_stability1, by = "landweimar") |>
#  filter(!landweimar %in% c("Lübeck", "Saarland", "Bremen", "Preußen")) |>
# mutate(stability1 = as.integer(pc1 >= median(pc1)))











