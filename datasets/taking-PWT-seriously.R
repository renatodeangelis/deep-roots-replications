library(readxl)
library(dplyr)

dkt = read_excel("datasets/dkt-ej-to-be-distributed.xls") |>
  mutate(across(dum6575:ssafr, as.numeric))

legend = read_excel("datasets/dkt-ej-to-be-distributed.xls", sheet = 2)

pwt = read_excel("datasets/pwt1001.xlsx")

## TO MERGE: religion, check, lang, ethtens, exprsk, kk96, excondec, legal origin,
## spanpor, region, geography

## TO FIND: demography, policy, initial conditions, education

