library(readxl)
library(dplyr)
library(AER)

data = read_excel("datasets/dkt-ej-to-be-distributed.xls") |>
  mutate(across(dum6575:ssafr, as.numeric))

## Model 1: Kitchen sink regression

mod = ivreg(gyw ~ lifee1r + fertl + easia + ssafr + latincar + lcr100km +
              kgatrstr + lang + ethtens + exprsk + kkz96 + dum7585 + dum8595 +
              yw0l + gpop + lfshl + inv + opres + gv + dp + easrel2a + hindua +
              jewsa + muslima + ortha + prota + othrel2a + excondec + check |
              lifee1r + fertl + easia + ssafr + latincar + lcr100km + kgatrstr +
              lang + ethtens + exprsk + kkz96 + dum7585 + dum8595 + yw0llag +
              gpoplag + lfshllag + invlag + opreslag + gvlag + spanpor +
              easrel21900a + hindu1900a + jews1900a + muslim1900a + orth1900a +
              prot1900a + othrel21900a + exconlag + frecivil + britcommon,
            data = data)
