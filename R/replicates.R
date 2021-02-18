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

## 0. load libraries
library(mecor)
library(dplyr)

## 1. load data
repdata <-
  readRDS(file = "./data/sbp_creatinine.RDS")

## 2. data analysis
# Show data to view structure 
View(repdata)
# Creatinine: serum creatinine in umol/L
# Age: age in years
# SBP_30: systolic blood pressure in mm Hg after 30 minutes
# SBP_60: systolic blood pressure in mm Hg after 60 minutes
# SBP_90: systolic blood pressure in mm Hg after 90 minutes
# SBP_120: systolic blood pressure in mm Hg after 120 minutes
# Systolic blood pressure is measured in all individuals after 30 minutes,
# and measured after 60, 90 and 120 in most of the individuals

# Plot first systolic blood pressure vs creatinine 
with(repdata, 
     plot(SBP_30, Creatinine, 
          xlab = "Systolic blood pressure (mm Hg)",
          ylab = "Creatinine (umol/L)"))

# Fit a linear model using the first BP measurement:
naivefit <-
  lm(Creatinine ~ SBP_30 + Age,
     data = repdata)
naivefit %>% summary()

# Correcting for measurement error using the 3 replicates applying regcal:
mefit <-
  mecor(Creatinine ~ MeasError(SBP_30, replicate =
                                 cbind(SBP_60, SBP_90, SBP_120)) + Age,
        data = repdata,
        method = "standard")
mefit %>% summary()

# Correcting for measurement error using the 3 replicates applying mle:
mefit_mle <-
  mecor(Creatinine ~ MeasError(SBP_30, replicate =
                                 cbind(SBP_60, SBP_90, SBP_120)) + Age,
        data = repdata,
        method = "mle")
mefit_mle %>% summary() # different from reg cal

# Calculate within individuals variance in bp measurements:
sbp_var <-
  apply(repdata %>% select(SBP_30, SBP_60, SBP_90, SBP_120),
        1, 
        var,
        use = "pairwise.complete.obs") %>%
  mean(na.rm = TRUE)

# Correcting for measurement error using the estimated variance
mefit_random <-
  mecor(Creatinine ~ MeasErrorRandom(SBP_30, var = sbp_var) + Age,
        data = repdata)
mefit_random %>% summary()