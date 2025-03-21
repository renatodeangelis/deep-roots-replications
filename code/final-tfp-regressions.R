library(dplyr)
library(readr)
library(modelsummary)
library(kableExtra)
library(stargazer)

confucian = c("CHN", "MAC", "HKG", "SGP", "JPN", "KOR", "MNG", "VNM")

data = read_csv("datasets/data_final.csv") |>
  mutate(conf = ifelse(countrycode %in% confucian, 1, 0))

# Table 1:

# Confucianism

mod1 = lm(gtfp ~ conf + yinit + hc + inv + gpop + period, data = data)

mod2 = lm(gtfp ~ conf + yinit + hc + inv + gpop,
          data = data |> filter(period == "1970-1979"))

mod3 = lm(gtfp ~ conf + yinit + hc + inv + gpop,
          data = data |> filter(period == "1980-1989"))

mod4 = lm(gtfp ~ conf + yinit + hc + inv + gpop,
          data = data |> filter(period == "1990-1999"))

mod5 = lm(gtfp ~ conf + yinit + hc + inv + gpop,
          data = data |> filter(period == "2000-2009"))

mod6 = lm(gtfp ~ conf + yinit + hc + inv + gpop,
          data = data |> filter(period == "2010-2019"))

mod7 = lm(gtfp ~ conf + yinit + hc + inv + gpop + period + region,
          data = data)

mod8 = lm(gtfp ~ conf + yinit + hc + inv + gpop + period + kkz96 +
            kgatrstr + lang, data = data)

mod21 = lm(gtfp ~ kkz96 + yinit + hc + inv + gpop + period, data = data)

mod22 = lm(gtfp ~ kkz96 + yinit + hc + inv + gpop,
           data = data |> filter(period == "1970-1979"))

mod23 = lm(gtfp ~ kkz96 + yinit + hc + inv + gpop,
           data = data |> filter(period == "1980-1989"))

mod24 = lm(gtfp ~ kkz96 + yinit + hc + inv + gpop,
           data = data |> filter(period == "1990-1999"))

mod25 = lm(gtfp ~ kkz96 + yinit + hc + inv + gpop,
           data = data |> filter(period == "2000-2009"))

mod26 = lm(gtfp ~ kkz96 + yinit + hc + inv + gpop,
           data = data |> filter(period == "2010-2019"))

mod27 = lm(gtfp ~ kkz96 + yinit + hc + inv + gpop + period + region,
           data = data)

mod28 = lm(gtfp ~ kkz96 + yinit + hc + inv + gpop + period + conf +
             kgatrstr + lang, data = data)

mod31 = lm(gtfp ~ kgatrstr + yinit + hc + inv + gpop + period, data = data)

mod32 = lm(gtfp ~ kgatrstr + yinit + hc + inv + gpop,
           data = data |> filter(period == "1970-1979"))

mod33 = lm(gtfp ~ kgatrstr + yinit + hc + inv + gpop,
           data = data |> filter(period == "1980-1989"))

mod34 = lm(gtfp ~ kgatrstr + yinit + hc + inv + gpop,
           data = data |> filter(period == "1990-1999"))

mod35 = lm(gtfp ~ kgatrstr + yinit + hc + inv + gpop,
           data = data |> filter(period == "2000-2009"))

mod36 = lm(gtfp ~ kgatrstr + yinit + hc + inv + gpop,
           data = data |> filter(period == "2010-2019"))

mod37 = lm(gtfp ~ kgatrstr + yinit + hc + inv + gpop + period + region,
           data = data)

mod38 = lm(gtfp ~ kgatrstr + yinit + hc + inv + gpop + period + kkz96 +
             conf + lang, data = data)

mod41 = lm(gtfp ~ lang + yinit + hc + inv + gpop + period, data = data)

mod42 = lm(gtfp ~ lang + yinit + hc + inv + gpop,
           data = data |> filter(period == "1970-1979"))

mod43 = lm(gtfp ~ lang + yinit + hc + inv + gpop,
           data = data |> filter(period == "1980-1989"))

mod44 = lm(gtfp ~ lang + yinit + hc + inv + gpop,
           data = data |> filter(period == "1990-1999"))

mod45 = lm(gtfp ~ lang + yinit + hc + inv + gpop,
           data = data |> filter(period == "2000-2009"))

mod46 = lm(gtfp ~ lang + yinit + hc + inv + gpop,
           data = data |> filter(period == "2010-2019"))

mod47 = lm(gtfp ~ lang + yinit + hc + inv + gpop + period + region,
           data = data)

mod48 = lm(gtfp ~ lang + yinit + hc + inv + gpop + period + kkz96 +
             kgatrstr + conf, data = data)

models_conf <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)
models_kkz96    <- list(mod21, mod22, mod23, mod24, mod25, mod26, mod27, mod28)
models_kgatrstr <- list(mod31, mod32, mod33, mod34, mod35, mod36, mod37, mod38)
models_lang     <- list(mod41, mod42, mod43, mod44, mod45, mod46, mod47, mod48)

all_models <- list(models_conf, models_kkz96, models_kgatrstr, models_lang)
labels <- c("East Asia Religion", "KKZ96", "KGATRSTR", "Language")

extract_main_coeff <- function(model_list, var) {
  sapply(model_list, function(model) {
    coefs <- coef(summary(model))
    if (var %in% rownames(coefs)) {
      est <- round(coefs[var, 1], 3)  # Coefficient
      se  <- round(coefs[var, 2], 3)  # Standard error
      pval <- coefs[var, 4]  # P-value for significance
      
      # Significance stars
      sig <- ifelse(pval < 0.01, "***", 
                    ifelse(pval < 0.05, "**", 
                           ifelse(pval < 0.1, "*", "")))
      
      return(c(paste0(est, sig), paste0("(", se, ")")))  # Return as a vector
    } else {
      return(c(NA, NA))  # Maintain structure if missing
    }
  })
}


coefficients_table <- rbind(
  extract_main_coeff(models_conf, "conf"),
  extract_main_coeff(models_kkz96, "kkz96"),
  extract_main_coeff(models_kgatrstr, "kgatrstr"),
  extract_main_coeff(models_lang, "lang")
)

# Assign proper row names (root variables
# Assign proper column names (Model 1 to Model 8)
colnames(coefficients_table) <- paste0("(", 1:8, ")")

kable(coefficients_table, format = "latex", booktabs = TRUE, align = "c",
      caption = "Main Coefficients with Standard Errors") |>
  kable_styling(latex_options = c("hold_position")) |>
  footnote(symbol = "Standard errors in parentheses. Significance: * p<0.1, ** p<0.05, *** p<0.01")
