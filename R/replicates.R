#==================================#
# mecor tutorial                   #
# Replicates study                 #
#                                  #
# 20210218 lindanab4@gmail.com     #
#==================================#

# Paper: https://bmjopen.bmj.com/content/5/11/e009492
# Data: https://datadryad.org/stash/dataset/doi:10.5061/dryad.0bq15
# Original rq: Effectiveness and safety of 1 vs 4 h blood pressure profile with
# clinical and laboratory assessment for the exclusion of gestational ht and
# pre-clampsia: a retrospective study in a universty affiliated maternity
# hospital

# Here, we'll be aiming to estimate the association between creatinine and 
# blood pressure in pregnant women (adjusted for age)

# 0. load libraries
library(haven)
library(tidyverse)
library(mecor)

# 1. load data
data <-
  read.csv2(file = "./McCarthy_gestationalht.csv")

# 2. data analysis
# Number of individuals in original data
data %>% nrow() # 475
# SBP_30 x Creatinine
with(data, plot(SBP_30, Creatinine))

# Select complete cases (of whom outcome (creatinine) and exposure (sbp_30) is
# known)
data <-
  data %>%
  select(Creatinine, Age, SBP_30, SBP_60, SBP_90, SBP_120) %>%
  filter(!is.na(Creatinine) & !is.na(SBP_30))
data %>% nrow() # 451 (24 cases filtered)
# Correct typo
data[which(data$SBP_90 == 1354),]$SBP_90 <- 135
# Delete outlier in creatinine from data
data <-
  data %>%
  filter(Creatinine > 10)
data %>% nrow() # 450

# Create balanced replicates study (select only individuals of whom 3 replicate
# measures are available)
cc <-
  which(!is.na(data$SBP_60) & !is.na(data$SBP_90) & !is.na(data$SBP_120))
data$SBP_60[-cc] <- NA
data$SBP_90[-cc] <- NA
data$SBP_120[-cc] <- NA

# Linear fit using only the first BP measurement:
naivefit <-
  lm(Creatinine ~ SBP_30 + Age,
     data = data,
     x = TRUE)
naivefit
confint(naivefit)

# Correcting for measurement error using the 3 replicates using regcal:
mefit <-
  mecor(Creatinine ~ MeasError(SBP_30, replicate =
                                 cbind(SBP_60, SBP_90, SBP_120)) + Age,
        data = data)
mefit
mefit %>% summary()
mefit_mle <-
  mecor(Creatinine ~ MeasError(SBP_30, replicate =
                                 cbind(SBP_60, SBP_90, SBP_120)) + Age,
        data = data,
        method = "mle")
mefit_mle # different from reg cal

# Calculate variance in bp measurements:
sbp_var <- apply(data %>%
                   select(SBP_30, SBP_60, SBP_90, SBP_120), 1, var,
                 use = "pairwise.complete.obs") %>%
  mean(na.rm = TRUE)

# Correcting for measurement error using the estimated variance
mefit_random <-
  mecor(Creatinine ~ MeasErrorRandom(SBP_30, var = sbp_var) + Age,
        data = data)
mefit_random

mefit_simex <-
  simex(naivefit,
        SIMEXvariable = "SBP_30",
        measurement.error = sqrt(sbp_var),
        asymptotic = TRUE)
mefit_simex
mefit_simex$coefficients[2] + qnorm(0.025) *
  summary(mefit_simex)$coefficients$asymptotic[2,2]
mefit_simex$coefficients[2] - qnorm(0.025) *
  summary(mefit_simex)$coefficients$asymptotic[2,2]

png("./output/pregnantwomen.png")
with(data, plot(SBP_30, Creatinine,
                pch = 20,
                xlab = "Systolic blood pressure (mm Hg, first measurement)",
                ylab = "Serum Creatinine (umol/L)",
                main = "Pregnant women",
                col = "gray"))
abline(a = naivefit$coefficients[1],
       b = naivefit$coefficients[2], col = "black",
       lwd = 2)
abline(a = mefit$corfit$coef[1],
       b = mefit$corfit$coef[2], col = "red",
       lwd = 2,
       lty = 2)
legend("topright", c("Uncorrected", "Corrected"),
       lwd = 2,
       lty = c(1, 2),
       col = c("black", "red"))
dev.off()

