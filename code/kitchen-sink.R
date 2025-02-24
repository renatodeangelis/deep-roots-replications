library(readxl)
library(dplyr)
library(ivreg)
library(stargazer)

data = read_excel("datasets/dkt-ej-to-be-distributed.xls") |>
  mutate(across(dum6575:ssafr, as.numeric))

legend = read_excel("datasets/dkt-ej-to-be-distributed.xls", sheet = 2)

## Model 1: Kitchen sink regressions

mod_iv = ivreg(gyw ~ lifee1r + fertl + easia + ssafr + latincar + lcr100km +
                 kgatrstr + lang + ethtens + exprsk + kkz96 + dum7585 + dum8595 |
                 yw0l + gpop + lfshl + inv + opres + gv + dp + easrel2a +
                 hindua + jewsa + muslima + ortha + prota + othrel2a + excondec +
                 check | yw0llag + gpoplag + lfshllag + invlag + opreslag + gvlag + 
                 spanpor + easrel21900a + hindu1900a + jews1900a + muslim1900a + 
                 orth1900a + prot1900a + othrel21900a + exconlag + frecivil + 
                 britcommon,
               data = data)

mod_iv_65 = ivreg(gyw ~ lifee1r + fertl + easia + ssafr + latincar + lcr100km +
                 kgatrstr + lang + ethtens + exprsk + kkz96 |
                 yw0l + gpop + lfshl + inv + opres + gv + dp + easrel2a +
                 hindua + jewsa + muslima + ortha + prota + othrel2a + excondec +
                 check | yw0llag + gpoplag + lfshllag + invlag + opreslag + gvlag + 
                 spanpor + easrel21900a + hindu1900a + jews1900a + muslim1900a + 
                 orth1900a + prot1900a + othrel21900a + exconlag + frecivil + 
                 britcommon,
                 data = data |> filter(dum6575 == 1))

mod_iv_75 = ivreg(gyw ~ lifee1r + fertl + easia + ssafr + latincar + lcr100km +
                 kgatrstr + lang + ethtens + exprsk + kkz96 |
                 yw0l + gpop + lfshl + inv + opres + gv + dp + easrel2a +
                 hindua + jewsa + muslima + ortha + prota + othrel2a + excondec +
                 check | yw0llag + gpoplag + lfshllag + invlag + opreslag + gvlag + 
                 spanpor + easrel21900a + hindu1900a + jews1900a + muslim1900a + 
                 orth1900a + prot1900a + othrel21900a + exconlag + frecivil + 
                 britcommon,
                 data = data |> filter(dum7585 == 1))

mod_iv_85 = ivreg(gyw ~ lifee1r + fertl + easia + ssafr + latincar + lcr100km +
                 kgatrstr + lang + ethtens + exprsk + kkz96 |
                 yw0l + gpop + lfshl + inv + opres + gv + dp + easrel2a +
                 hindua + jewsa + muslima + ortha + prota + othrel2a + excondec +
                 check | yw0llag + gpoplag + lfshllag + invlag + opreslag + gvlag + 
                 spanpor + easrel21900a + hindu1900a + jews1900a + muslim1900a + 
                 orth1900a + prot1900a + othrel21900a + exconlag + frecivil + 
                 britcommon,
                 data = data |> filter(dum8595 == 1))

## Model 2: Solow regressions

mod_solow = ivreg(gyw ~ yw0l + gpop + gk + gh + lifee1r + fertl + dum6575 + dum7585 |
                    yw0llag + gpoplag + gklag + ghlag + lifee1r + fertl +
                    dum6575 + dum7585,
                  data = data)

mod_solow_65 = ivreg(gyw ~ yw0l + gpop + gk + gh + lifee1r + fertl |
                    yw0llag + gpoplag + gklag + ghlag + lifee1r + fertl,
                  data = data |> filter(dum6575 == 1))

mod_solow_75 = ivreg(gyw ~ yw0l + gpop + gk + gh + lifee1r + fertl |
                    yw0llag + gpoplag + gklag + ghlag + lifee1r + fertl,
                  data = data |> filter(dum7585 == 1))

mod_solow_85 = ivreg(gyw ~ yw0l + gpop + gk + gh + lifee1r + fertl |
                    yw0llag + gpoplag + gklag + ghlag + lifee1r + fertl,
                  data = data |> filter(dum8595 == 1))

## Model 3: Fundamental Determinants

mod_yw0l = ivreg(yw0l ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                   ethtens + exprsk + kkz96 + dum6575 + dum7585 | easrel2a +
                   hindua + jewsa + muslima + ortha + prota + othrel2a +
                   excondec + check | easrel21900a + hindu1900a + jews1900a +
                   muslim1900a + orth1900a + prot1900a + othrel21900a +
                   exconlag + frecivil + britcommon,
                 data = data)

mod_gk = ivreg(gk ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                 ethtens + exprsk + kkz96 + dum6575 + dum7585 | easrel2a +
                 hindua + jewsa + muslima + ortha + prota + othrel2a +
                 excondec + check | easrel21900a + hindu1900a + jews1900a +
                 muslim1900a + orth1900a + prot1900a + othrel21900a +
                 exconlag + frecivil + britcommon,
               data = data)

mod_gh = ivreg(gh ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                 ethtens + exprsk + kkz96 + dum6575 + dum7585 | easrel2a +
                 hindua + jewsa + muslima + ortha + prota + othrel2a +
                 excondec + check | easrel21900a + hindu1900a + jews1900a +
                 muslim1900a + orth1900a + prot1900a + othrel21900a +
                 exconlag + frecivil + britcommon,
               data = data)

mod_opres = ivreg(opres ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                    ethtens + exprsk + kkz96 + dum6575 + dum7585 | easrel2a +
                    hindua + jewsa + muslima + ortha + prota + othrel2a +
                    excondec + check | easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a +
                    exconlag + frecivil + britcommon,
                  data = data)

mod_gv = ivreg(gv ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                 ethtens + exprsk + kkz96 + dum6575 + dum7585 | easrel2a +
                 hindua + jewsa + muslima + ortha + prota + othrel2a +
                 excondec + check | easrel21900a + hindu1900a + jews1900a +
                 muslim1900a + orth1900a + prot1900a + othrel21900a +
                 exconlag + frecivil + britcommon,
               data = data)

mod_dp = ivreg(dp ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                 ethtens + exprsk + kkz96 + dum6575 + dum7585 | easrel2a +
                 hindua + jewsa + muslima + ortha + prota + othrel2a +
                 excondec + check | easrel21900a + hindu1900a + jews1900a +
                 muslim1900a + orth1900a + prot1900a + othrel21900a +
                 exconlag + frecivil + britcommon,
               data = data)

# Splitting by 65-75

mod_yw0l_65 = ivreg(yw0l ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                   ethtens + exprsk + kkz96 | easrel2a +
                   hindua + jewsa + muslima + ortha + prota + othrel2a +
                   excondec + check | easrel21900a + hindu1900a + jews1900a +
                   muslim1900a + orth1900a + prot1900a + othrel21900a +
                   exconlag + frecivil + britcommon,
                 data = data |> filter(dum6575 == 1))

mod_gk_65 = ivreg(gk ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                      ethtens + exprsk + kkz96 | easrel2a +
                      hindua + jewsa + muslima + ortha + prota + othrel2a +
                      excondec + check | easrel21900a + hindu1900a + jews1900a +
                      muslim1900a + orth1900a + prot1900a + othrel21900a +
                      exconlag + frecivil + britcommon,
                    data = data |> filter(dum6575 == 1))

mod_gh_65 = ivreg(gh ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                      ethtens + exprsk + kkz96 | easrel2a +
                      hindua + jewsa + muslima + ortha + prota + othrel2a +
                      excondec + check | easrel21900a + hindu1900a + jews1900a +
                      muslim1900a + orth1900a + prot1900a + othrel21900a +
                      exconlag + frecivil + britcommon,
                    data = data |> filter(dum6575 == 1))

mod_opres_65 = ivreg(opres ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                      ethtens + exprsk + kkz96 | easrel2a +
                      hindua + jewsa + muslima + ortha + prota + othrel2a +
                      excondec + check | easrel21900a + hindu1900a + jews1900a +
                      muslim1900a + orth1900a + prot1900a + othrel21900a +
                      exconlag + frecivil + britcommon,
                    data = data |> filter(dum6575 == 1))

mod_gv_65 = ivreg(gv ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                      ethtens + exprsk + kkz96 | easrel2a +
                      hindua + jewsa + muslima + ortha + prota + othrel2a +
                      excondec + check | easrel21900a + hindu1900a + jews1900a +
                      muslim1900a + orth1900a + prot1900a + othrel21900a +
                      exconlag + frecivil + britcommon,
                    data = data |> filter(dum6575 == 1))

mod_dp_65 = ivreg(dp ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                      ethtens + exprsk + kkz96 | easrel2a +
                      hindua + jewsa + muslima + ortha + prota + othrel2a +
                      excondec + check | easrel21900a + hindu1900a + jews1900a +
                      muslim1900a + orth1900a + prot1900a + othrel21900a +
                      exconlag + frecivil + britcommon,
                    data = data |> filter(dum6575 == 1))

# Splitting by 75-85

mod_yw0l_75 = ivreg(yw0l ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                      ethtens + exprsk + kkz96 | easrel2a +
                      hindua + jewsa + muslima + ortha + prota + othrel2a +
                      excondec + check | easrel21900a + hindu1900a + jews1900a +
                      muslim1900a + orth1900a + prot1900a + othrel21900a +
                      exconlag + frecivil + britcommon,
                    data = data |> filter(dum7585 == 1))

mod_gk_75 = ivreg(gk ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                    ethtens + exprsk + kkz96 | easrel2a +
                    hindua + jewsa + muslima + ortha + prota + othrel2a +
                    excondec + check | easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a +
                    exconlag + frecivil + britcommon,
                  data = data |> filter(dum7585 == 1))

mod_gh_75 = ivreg(gh ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                    ethtens + exprsk + kkz96 | easrel2a +
                    hindua + jewsa + muslima + ortha + prota + othrel2a +
                    excondec + check | easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a +
                    exconlag + frecivil + britcommon,
                  data = data |> filter(dum7585 == 1))

mod_opres_75 = ivreg(opres ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                       ethtens + exprsk + kkz96 | easrel2a +
                       hindua + jewsa + muslima + ortha + prota + othrel2a +
                       excondec + check | easrel21900a + hindu1900a + jews1900a +
                       muslim1900a + orth1900a + prot1900a + othrel21900a +
                       exconlag + frecivil + britcommon,
                     data = data |> filter(dum7585 == 1))

mod_gv_75 = ivreg(gv ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                    ethtens + exprsk + kkz96 | easrel2a +
                    hindua + jewsa + muslima + ortha + prota + othrel2a +
                    excondec + check | easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a +
                    exconlag + frecivil + britcommon,
                  data = data |> filter(dum7585 == 1))

mod_dp_75 = ivreg(dp ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                    ethtens + exprsk + kkz96 | easrel2a +
                    hindua + jewsa + muslima + ortha + prota + othrel2a +
                    excondec + check | easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a +
                    exconlag + frecivil + britcommon,
                  data = data |> filter(dum7585 == 1))

# Splitting by 85-95

mod_yw0l_85 = ivreg(yw0l ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                      ethtens + exprsk + kkz96 | easrel2a +
                      hindua + jewsa + muslima + ortha + prota + othrel2a +
                      excondec + check | easrel21900a + hindu1900a + jews1900a +
                      muslim1900a + orth1900a + prot1900a + othrel21900a +
                      exconlag + frecivil + britcommon,
                    data = data |> filter(dum8595 == 1))

mod_gk_85 = ivreg(gk ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                    ethtens + exprsk + kkz96 | easrel2a +
                    hindua + jewsa + muslima + ortha + prota + othrel2a +
                    excondec + check | easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a +
                    exconlag + frecivil + britcommon,
                  data = data |> filter(dum8595 == 1))

mod_gh_85 = ivreg(gh ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                    ethtens + exprsk + kkz96 | easrel2a +
                    hindua + jewsa + muslima + ortha + prota + othrel2a +
                    excondec + check | easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a +
                    exconlag + frecivil + britcommon,
                  data = data |> filter(dum8595 == 1))

mod_opres_85 = ivreg(opres ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                       ethtens + exprsk + kkz96 | easrel2a +
                       hindua + jewsa + muslima + ortha + prota + othrel2a +
                       excondec + check | easrel21900a + hindu1900a + jews1900a +
                       muslim1900a + orth1900a + prot1900a + othrel21900a +
                       exconlag + frecivil + britcommon,
                     data = data |> filter(dum8595 == 1))

mod_gv_85 = ivreg(gv ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                    ethtens + exprsk + kkz96 | easrel2a +
                    hindua + jewsa + muslima + ortha + prota + othrel2a +
                    excondec + check | easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a +
                    exconlag + frecivil + britcommon,
                  data = data |> filter(dum8595 == 1))

mod_dp_85 = ivreg(dp ~ easia + ssafr + latincar + lcr100km + kgatrstr + lang +
                    ethtens + exprsk + kkz96 | easrel2a +
                    hindua + jewsa + muslima + ortha + prota + othrel2a +
                    excondec + check | easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a +
                    exconlag + frecivil + britcommon,
                  data = data |> filter(dum8595 == 1))

#output_solow = stargazer(mod_yw0l, mod_gpop, mod_lifee1r, mod_fertl, mod_gk, mod_gh,
#          type = "latex", 
#          omit = c("dum6575", "dum7585", "Constant"), 
#          keep.stat = c("n", "rsq"), 
#          digits = 3, 
#          column.sep.width = "0pt")




















