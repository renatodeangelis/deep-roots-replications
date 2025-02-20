library(readxl)

data = read_excel("datasets/dkt-ej-to-be-distributed.xls") |>
  mutate(across(dum6575:ssafr, as.numeric))

## Model 1: Kitchen sink regression

kitchen_sink = ivreg(formula = gyw ~ yw0l + lfshl + inv + gpop + opres + gv + 
        dp + ortha + prota + hindua + muslima + jewsa + easrel2a + 
        othrel2a + check + excondec + fertl + lifee1r + easia + ssafr + 
        latincar + lcr100km + kgatrstr + lang + ethtens + exprsk + 
        kkz96 + dum8595 + dum7585 | yw0llag + lfshllag + invlag + 
        gpoplag + opreslag + gvlag + spanpor + orth1900a + prot1900a + 
        hindu1900a + muslim1900a + jews1900a + easrel21900a + othrel21900a + 
        frecivil + britcommon + exconlag + fertl + lifee1r + easia + 
        ssafr + latincar + lcr100km + kgatrstr + lang + ethtens + 
        exprsk + kkz96 + dum8595 + dum7585, data = data)

