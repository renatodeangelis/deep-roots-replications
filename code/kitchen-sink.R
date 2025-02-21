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

solow = ivreg(gyw ~ yw0l + gpop + lfshl + inv | yw0llag + gpoplag + lfshllag +
                invlag,
              data = data)

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
                  lang + ethtens + exprsk + excondec + kkz96 + check | easia +
                  ssafr + latincar + easrel21900a + hindu1900a + jews1900a +
                  muslim1900a + orth1900a + prot1900a + othrel21900a + lcr100km +
                  kgatrstr + lang + ethtens + exprsk + frecivil + britcommon +
                  exconlag, data = data)

mod_gv = ivreg(gv ~ easia + ssafr + latincar + easrel2a + hindua + jewsa +
                  muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                  lang + ethtens + exprsk + excondec + kkz96 + check | easia +
                  ssafr + latincar + easrel21900a + hindu1900a + jews1900a +
                  muslim1900a + orth1900a + prot1900a + othrel21900a + lcr100km +
                  kgatrstr + lang + ethtens + exprsk + frecivil + britcommon +
                  exconlag, data = data)

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

#output_policy = stargazer(mod_opres, mod_dp, mod_gv,
#                         type = "latex", 
#                         omit = c("dum6575", "dum7585", "Constant"), 
#                         keep.stat = c("n", "rsq"), 
#                         digits = 3, 
#                         column.sep.width = "0pt")


## Model 3: Splitting by decade

mod2 = ivreg(gyw ~ yw0l + gpop + lfshl + inv + lifee1r + fertl + opres + gv +
                 dp + easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | lifee1r + 
                 fertl + easia + ssafr + latincar + lcr100km +
                 kgatrstr + lang + ethtens + exprsk + kkz96 +
                 yw0llag + gpoplag + lfshllag + invlag + opreslag + gvlag + 
                 spanpor + easrel21900a + hindu1900a + jews1900a + muslim1900a + 
                 orth1900a + prot1900a + othrel21900a + exconlag + frecivil + 
                 britcommon, data = data |> filter(dum6575 == 1))

mod3 = ivreg(gyw ~ yw0l + gpop + lfshl + inv + lifee1r + fertl + opres + gv +
                 dp + easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | lifee1r + 
                 fertl + easia + ssafr + latincar + lcr100km +
                 kgatrstr + lang + ethtens + exprsk + kkz96 +
                 yw0llag + gpoplag + lfshllag + invlag + opreslag + gvlag + 
                 spanpor + easrel21900a + hindu1900a + jews1900a + muslim1900a + 
                 orth1900a + prot1900a + othrel21900a + exconlag + frecivil + 
                 britcommon, data = data |> filter(dum7585 == 1))

mod4 = ivreg(gyw ~ yw0l + gpop + lfshl + inv + lifee1r + fertl + opres + gv +
                 dp + easia + ssafr + latincar + easrel2a + hindua + jewsa +
                 muslima + ortha + prota + othrel2a + lcr100km + kgatrstr +
                 lang + ethtens + exprsk + excondec + kkz96 + check | lifee1r + 
                 fertl + easia + ssafr + latincar + lcr100km +
                 kgatrstr + lang + ethtens + exprsk + kkz96 +
                 yw0llag + gpoplag + lfshllag + invlag + opreslag + gvlag + 
                 spanpor + easrel21900a + hindu1900a + jews1900a + muslim1900a + 
                 orth1900a + prot1900a + othrel21900a + exconlag + frecivil + 
                 britcommon, data = data |> filter(dum8595 == 1))


n <- nobs(mod_iv)  # Number of observations
k <- length(coef(mod_iv))  # Number of estimated parameters

# Compute standard errors using the original study's finite sample adjustment
adjustment_factor <- sqrt(n / (n - k))

# Robust standard errors with finite sample correction
robust_se <- sqrt(diag(vcovHC(mod_iv, type = "HC0"))) * adjustment_factor

# Compute significance levels (p-values)
t_values <- coef(mod_iv) / robust_se
p_values <- 2 * (1 - pnorm(abs(t_values)))

# Display results
results <- data.frame(
  Coefficient = coef(mod_iv),
  Robust_SE = robust_se,
  t_value = t_values,
  p_value = p_values
)
print(results)
