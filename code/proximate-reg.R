library(dplyr)
library(readr)
library(modelsummary)
library(kableExtra)
library(stargazer)

data = read_csv("datasets/data_final.csv") |>
  mutate(dp = cpi / 100,
         dp1 = log(cpi + 0.26))

# Table 1:

mod21 = lm(gy ~ opres + yinit + hc + inv + gpop + period, data = data)

mod22 = lm(gy ~ opres + yinit + hc + inv + gpop,
           data = data |> filter(period == "1970-1979"))

mod23 = lm(gy ~ opres + yinit + hc + inv + gpop,
           data = data |> filter(period == "1980-1989"))

mod24 = lm(gy ~ opres + yinit + hc + inv + gpop,
           data = data |> filter(period == "1990-1999"))

mod25 = lm(gy ~ opres + yinit + hc + inv + gpop,
           data = data |> filter(period == "2000-2009"))

mod26 = lm(gy ~ opres + yinit + hc + inv + gpop,
           data = data |> filter(period == "2010-2019"))

mod27 = lm(gy ~ opres + yinit + hc + inv + gpop + period + region,
           data = data)

mod28 = lm(gy ~ opres + yinit + hc + inv + gpop + period +
             dp + gv, data = data)

mod31 = lm(gy ~ dp + yinit + hc + inv + gpop + period, data = data)

mod32 = lm(gy ~ dp + yinit + hc + inv + gpop,
           data = data |> filter(period == "1970-1979"))

mod33 = lm(gy ~ dp + yinit + hc + inv + gpop,
           data = data |> filter(period == "1980-1989"))

mod34 = lm(gy ~ dp + yinit + hc + inv + gpop,
           data = data |> filter(period == "1990-1999"))

mod35 = lm(gy ~ dp + yinit + hc + inv + gpop,
           data = data |> filter(period == "2000-2009"))

mod36 = lm(gy ~ dp + yinit + hc + inv + gpop,
           data = data |> filter(period == "2010-2019"))

mod37 = lm(gy ~ dp + yinit + hc + inv + gpop + period + region,
           data = data)

mod38 = lm(gy ~ dp + yinit + hc + inv + gpop + period + opres + gv, data = data)

mod41 = lm(gy ~ gv + yinit + hc + inv + gpop + period, data = data)

mod42 = lm(gy ~ gv + yinit + hc + inv + gpop,
           data = data |> filter(period == "1970-1979"))

mod43 = lm(gy ~ gv + yinit + hc + inv + gpop,
           data = data |> filter(period == "1980-1989"))

mod44 = lm(gy ~ gv + yinit + hc + inv + gpop,
           data = data |> filter(period == "1990-1999"))

mod45 = lm(gy ~ gv + yinit + hc + inv + gpop,
           data = data |> filter(period == "2000-2009"))

mod46 = lm(gy ~ gv + yinit + hc + inv + gpop,
           data = data |> filter(period == "2010-2019"))

mod47 = lm(gy ~ gv + yinit + hc + inv + gpop + period + region,
           data = data)

mod48 = lm(gy ~ gv + yinit + hc + inv + gpop + period + opres +
             dp, data = data)

models_opres    <- list(mod21, mod22, mod23, mod24, mod25, mod26, mod27, mod28)
models_dp <- list(mod31, mod32, mod33, mod34, mod35, mod36, mod37, mod38)
models_gv     <- list(mod41, mod42, mod43, mod44, mod45, mod46, mod47, mod48)

all_models <- list(models_opres, models_dp, models_gv)
labels <- c("East Asia Religion", "opres", "dp", "gvuage")

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
  extract_main_coeff(models_opres, "opres"),
  extract_main_coeff(models_dp, "dp"),
  extract_main_coeff(models_gv, "gv")
)

# Assign proper column names (Model 1 to Model 8)
colnames(coefficients_table) <- paste0("Model ", 1:8)

kable(coefficients_table, format = "latex", booktabs = TRUE, align = "c",
      caption = "Main Coefficients with Standard Errors") |>
  kable_styling(latex_options = c("hold_position")) |>
  footnote(symbol = "Standard errors in parentheses. Significance: * p<0.1, ** p<0.05, *** p<0.01")




