library(readxl)
library(dplyr)
library(AER)
library(sandwich)
library(lmtest)
library(stargazer)

data = read_excel("datasets/dkt-ej-to-be-distributed.xls") |>
  mutate(across(dum6575:ssafr, as.numeric))

legend = read_excel("datasets/dkt-ej-to-be-distributed.xls", sheet = 2)

## Model 1: Kitchen sink regression and Solow

kitchen_sink = ivreg(gyw ~ yw0l + gpop + lfshl + inv + lifee1r + fertl + opres + gv +
              dp + easia + ssafr + latincar + easrel2a + hindua + jewsa +
              muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
              lang + ethtens + exprsk + excondec + kkz96 + check + dum7585 +
              dum8595 | lifee1r + fertl + easia + ssafr + latincar + lcr100km +
              kgatrstr + lang + ethtens + exprsk + kkz96 + dum7585 + dum8595 +
              yw0llag + gpoplag + lfshllag + invlag + opreslag + gvlag + 
              spanpor + easrel21900a + hindu1900a + jews1900a + muslim1900a + 
              orth1900a + prot1900a + othrel21900a + exconlag + frecivil + 
              britcommon, data = data)

solow = ivreg(gyw ~ yw0l + gpop + gk + gh + lifee1r + fertl + dum6575 + dum7585 |
                yw0llag + gpoplag + gklag + ghlag + lifee1r + fertl + dum6575 + dum7585,
              data = data)

#output_kc = stargazer(kitchen_sink,
#                      type = "latex", 
#                      omit = c("dum6575", "dum7585", "Constant"), 
#                      keep.stat = c("n", "rsq"), 
#                      digits = 3,
#                      column.sep.width = "0pt")

output_solow = stargazer(solow,
                                     type = "latex", 
                                     omit = c("dum6575", "dum7585", "Constant"), 
                                     keep.stat = c("n", "rsq"), 
                                     digits = 3,
                                     column.sep.width = "0pt")

## Model 2: Fundamental determinants against Solow and policy

mod_yw0l = ivreg(yw0l ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                  muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                  lang + ethtens + exprsk + excondec + kkz96 + check + dum6575 +
                  dum7585 | easia + ssafr + latincar + easrel21900a + hindu1900a +
                  jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                  lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                  britcommon + exconlag + dum6575 + dum7585, data = data)

mod_gpop = ivreg(gpop ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                   muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                   lang + ethtens + exprsk + excondec + kkz96 + check + dum6575 +
                   dum7585 | easia + ssafr + latincar + easrel21900a + hindu1900a +
                   jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                   lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                   britcommon + exconlag + dum6575 + dum7585, data = data)

mod_lifee1r = ivreg(lifee1r ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                      muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                      lang + ethtens + exprsk + excondec + kkz96 + check + dum6575 +
                      dum7585 | easia + ssafr + latincar + easrel21900a + hindu1900a +
                      jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                      lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                      britcommon + exconlag + dum6575 + dum7585, data = data)

mod_fertl = ivreg(fertl ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                    muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                    lang + ethtens + exprsk + excondec + kkz96 + check + dum6575 +
                    dum7585 | easia + ssafr + latincar + easrel21900a + hindu1900a +
                    jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                    lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                    britcommon + exconlag + dum6575 + dum7585, data = data)

mod_gk = ivreg(gk ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check + dum6575 +
                 dum7585 | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag + dum6575 + dum7585, data = data)

mod_gh = ivreg(gh ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check + dum6575 +
                 dum7585 | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag + dum6575 + dum7585, data = data)

mod_opres = ivreg(opres ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                  muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                  lang + ethtens + exprsk + excondec + kkz96 + check + dum6575 + dum7585 | easia +
                  ssafr + latincar + easrel21900a + hindu1900a + jews1900a +
                  muslim1900a + orth1900a + prot1900a + othrel21900a + lcr100km +
                  kgatrstr + lang + ethtens + exprsk + frecivil + britcommon +
                  exconlag + dum6575 + dum7585, data = data)

mod_gv = ivreg(gv ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                  muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                  lang + ethtens + exprsk + excondec + kkz96 + check + dum6575 + dum7585 | easia +
                  ssafr + latincar + easrel21900a + hindu1900a + jews1900a +
                  muslim1900a + orth1900a + prot1900a + othrel21900a + lcr100km +
                  kgatrstr + lang + ethtens + exprsk + frecivil + britcommon +
                  exconlag + dum6575 + dum7585, data = data)

mod_dp = ivreg(dp ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check + dum6575 +
                 dum7585 | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag + dum6575 + dum7585, data = data)

#output_solow = stargazer(mod_yw0l, mod_gpop, mod_lifee1r, mod_fertl, mod_gk, mod_gh,
#          type = "latex", 
#          omit = c("dum6575", "dum7585", "Constant"), 
#          keep.stat = c("n", "rsq"), 
#          digits = 3, 
#          column.sep.width = "0pt")

output_policy = stargazer(mod_opres, mod_dp, mod_gv,
                         type = "latex", 
                         omit = c("dum6575", "dum7585", "Constant"), 
                         keep.stat = c("n", "rsq"), 
                         digits = 3, 
                         column.sep.width = "0pt")


## Model 3: Splitting by decade

kitchen_sink1 = ivreg(gyw ~ yw0l + gpop + lfshl + inv + lifee1r + fertl + opres + gv +
                       dp + easia + ssafr + latincar + easrel2a + hindua + jewsa +
                       muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                       lang + ethtens + exprsk + excondec + kkz96 + check | lifee1r + fertl + easia + ssafr + latincar + lcr100km +
                       kgatrstr + lang + ethtens + exprsk + kkz96 +
                       yw0llag + gpoplag + lfshllag + invlag + opreslag + gvlag + 
                       spanpor + easrel21900a + hindu1900a + jews1900a + muslim1900a + 
                       orth1900a + prot1900a + othrel21900a + exconlag + frecivil + 
                       britcommon, data = data |> filter(dum6575 == 1))

solow1 = ivreg(gyw ~ yw0l + gpop + gk + gh + lifee1r + fertl | yw0llag +
                gpoplag + gklag + ghlag + lifee1r + fertl,
              data = data |> filter(dum6575 == 1))

kitchen_sink2 = ivreg(gyw ~ yw0l + gpop + lfshl + inv + lifee1r + fertl + opres + gv +
                       dp + easia + ssafr + latincar + easrel2a + hindua + jewsa +
                       muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                       lang + ethtens + exprsk + excondec + kkz96 + check | lifee1r + fertl + easia + ssafr + latincar + lcr100km +
                       kgatrstr + lang + ethtens + exprsk + kkz96 +
                       yw0llag + gpoplag + lfshllag + invlag + opreslag + gvlag + 
                       spanpor + easrel21900a + hindu1900a + jews1900a + muslim1900a + 
                       orth1900a + prot1900a + othrel21900a + exconlag + frecivil + 
                       britcommon, data = data |> filter(dum7585 == 1))

solow2 = ivreg(gyw ~ yw0l + gpop + gk + gh + lifee1r + fertl| yw0llag +
                gpoplag + gklag + ghlag + lifee1r + fertl,
              data = data |> filter(dum7585 == 1))

kitchen_sink3 = ivreg(gyw ~ yw0l + gpop + lfshl + inv + lifee1r + fertl + opres + gv +
                       dp + easia + ssafr + latincar + easrel2a + hindua + jewsa +
                       muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                       lang + ethtens + exprsk + excondec + kkz96 + check | lifee1r + fertl + easia + ssafr + latincar + lcr100km +
                       kgatrstr + lang + ethtens + exprsk + kkz96 +
                       yw0llag + gpoplag + lfshllag + invlag + opreslag + gvlag + 
                       spanpor + easrel21900a + hindu1900a + jews1900a + muslim1900a + 
                       orth1900a + prot1900a + othrel21900a + exconlag + frecivil + 
                       britcommon, data = data |> filter(dum8595 == 1))

solow3 = ivreg(gyw ~ yw0l + gpop + gk + gh + lifee1r + fertl | yw0llag +
                gpoplag + gklag + ghlag + lifee1r + fertl,
              data = data |> filter(dum8595 == 1))

mod_yw0l1 = ivreg(yw0l ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                   muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                   lang + ethtens + exprsk + excondec + kkz96 + check| easia + ssafr + latincar + easrel21900a + hindu1900a +
                   jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                   lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                   britcommon + exconlag, data = data |> filter(dum6575 == 1))

mod_gpop1 = ivreg(gpop ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                   muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                   lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                   jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                   lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                   britcommon + exconlag, data = data |> filter(dum6575 == 1))

mod_lifee1r1 = ivreg(lifee1r ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                      muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                      lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                      jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                      lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                      britcommon + exconlag, data = data |> filter(dum6575 == 1))

mod_fertl1 = ivreg(fertl ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                    muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                    lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                    jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                    lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                    britcommon + exconlag, data = data |> filter(dum6575 == 1))

mod_gk1 = ivreg(gk ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag, data = data |> filter(dum6575 == 1))

mod_gh1 = ivreg(gh ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag, data = data |> filter(dum6575 == 1))

mod_opres1 = ivreg(opres ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                    muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                    lang + ethtens + exprsk + excondec + kkz96 + check | easia +
                    ssafr + latincar + easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a + lcr100km +
                    kgatrstr + lang + ethtens + exprsk + frecivil + britcommon +
                    exconlag, data = data |> filter(dum6575 == 1))

mod_gv1 = ivreg(gv ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia +
                 ssafr + latincar + easrel21900a + hindu1900a + jews1900a +
                 muslim1900a + orth1900a + prot1900a + othrel21900a + lcr100km +
                 kgatrstr + lang + ethtens + exprsk + frecivil + britcommon +
                 exconlag, data = data |> filter(dum6575 == 1))

mod_dp1 = ivreg(dp ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag, data = data |> filter(dum6575 == 1))

mod_yw0l2 = ivreg(yw0l ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                   muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                   lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                   jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                   lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                   britcommon + exconlag, data = data |> filter(dum7585 == 1))

mod_gpop2 = ivreg(gpop ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                   muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                   lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                   jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                   lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                   britcommon + exconlag, data = data |> filter(dum7585 == 1))

mod_lifee1r2 = ivreg(lifee1r ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                      muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                      lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                      jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                      lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                      britcommon + exconlag, data = data |> filter(dum7585 == 1))

mod_fertl2 = ivreg(fertl ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                    muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                    lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                    jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                    lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                    britcommon + exconlag, data = data |> filter(dum7585 == 1))

mod_gk2 = ivreg(gk ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag, data = data |> filter(dum7585 == 1))

mod_gh2 = ivreg(gh ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag, data = data |> filter(dum7585 == 1))

mod_opres2 = ivreg(opres ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                    muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                    lang + ethtens + exprsk + excondec + kkz96 + check | easia +
                    ssafr + latincar + easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a + lcr100km +
                    kgatrstr + lang + ethtens + exprsk + frecivil + britcommon +
                    exconlag, data = data |> filter(dum7585 == 1))

mod_gv2 = ivreg(gv ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia +
                 ssafr + latincar + easrel21900a + hindu1900a + jews1900a +
                 muslim1900a + orth1900a + prot1900a + othrel21900a + lcr100km +
                 kgatrstr + lang + ethtens + exprsk + frecivil + britcommon +
                 exconlag, data = data |> filter(dum7585 == 1))

mod_dp2 = ivreg(dp ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag, data = data |> filter(dum7585 == 1))

mod_yw0l3 = ivreg(yw0l ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                   muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                   lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                   jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                   lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                   britcommon + exconlag, data = data |> filter(dum8595 == 1))

mod_gpop3 = ivreg(gpop ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                   muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                   lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                   jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                   lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                   britcommon + exconlag, data = data |> filter(dum8595 == 1))

mod_lifee1r3 = ivreg(lifee1r ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                      muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                      lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                      jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                      lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                      britcommon + exconlag, data = data |> filter(dum8595 == 1))

mod_fertl3 = ivreg(fertl ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                    muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                    lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                    jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                    lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                    britcommon + exconlag, data = data |> filter(dum8595 == 1))

mod_gk3 = ivreg(gk ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag, data = data |> filter(dum8595 == 1))

mod_gh3 = ivreg(gh ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag, data = data |> filter(dum8595 == 1))

mod_opres3 = ivreg(opres ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                    muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                    lang + ethtens + exprsk + excondec + kkz96 + check | easia +
                    ssafr + latincar + easrel21900a + hindu1900a + jews1900a +
                    muslim1900a + orth1900a + prot1900a + othrel21900a + lcr100km +
                    kgatrstr + lang + ethtens + exprsk + frecivil + britcommon +
                    exconlag, data = data |> filter(dum8595 == 1))

mod_gv3 = ivreg(gv ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia +
                 ssafr + latincar + easrel21900a + hindu1900a + jews1900a +
                 muslim1900a + orth1900a + prot1900a + othrel21900a + lcr100km +
                 kgatrstr + lang + ethtens + exprsk + frecivil + britcommon +
                 exconlag, data = data |> filter(dum8595 == 1))

mod_dp3 = ivreg(dp ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | easia + ssafr + latincar + easrel21900a + hindu1900a +
                 jews1900a + muslim1900a + orth1900a + prot1900a + othrel21900a +
                 lcr100km + kgatrstr + lang + ethtens + exprsk + frecivil +
                 britcommon + exconlag, data = data |> filter(dum8595 == 1))

output_kc = stargazer(kitchen_sink3,
                      type = "latex", 
                      omit = c("Constant"), 
                      keep.stat = c("n", "rsq"), 
                      digits = 3,
                      column.sep.width = "0pt")

output_solow = stargazer(solow3,
                                     type = "latex", 
                                     omit = c("Constant"), 
                                     keep.stat = c("n", "rsq"), 
                                     digits = 3,
                                     column.sep.width = "0pt")

output_solow2 = stargazer(mod_yw0l3, mod_gpop3, mod_lifee1r3, mod_gh3, mod_gk3,
          type = "latex", 
          omit = c("Constant"), 
          keep.stat = c("n", "rsq"), 
          digits = 3, 
          column.sep.width = "0pt")

output_policy = stargazer(mod_opres3, mod_dp3, mod_gv3,
                         type = "latex", 
                         omit = c("Constant"), 
                         keep.stat = c("n", "rsq"), 
                         digits = 3, 
                         column.sep.width = "0pt")


#n <- nobs(mod_iv)  # Number of observations
#k <- length(coef(mod_iv))  # Number of estimated parameters

# Compute standard errors using the original study's finite sample adjustment
#adjustment_factor <- sqrt(n / (n - k))

# Robust standard errors with finite sample correction
#robust_se <- sqrt(diag(vcovHC(mod_iv, type = "HC0"))) * adjustment_factor

# Compute significance levels (p-values)
#t_values <- coef(mod_iv) / robust_se
#p_values <- 2 * (1 - pnorm(abs(t_values)))

# Display results
#results <- data.frame(
#  Coefficient = coef(mod_iv),
#  Robust_SE = robust_se,
#  t_value = t_values,
#  p_value = p_values
#)
#print(results)
