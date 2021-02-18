#==================================#
# mecor tutorial                   #
# Replicates study                 #
# preprocess data                  #
#                                  #
# 20210218 lindanab4@gmail.com     #
#==================================#

# Data: https://datadryad.org/stash/dataset/doi:10.5061/dryad.0bq15
# Paper: https://bmjopen.bmj.com/content/5/11/e009492
# Original rq: Effectiveness and safety of 1 vs 4 h blood pressure profile with
# clinical and laboratory assessment for the exclusion of gestational ht and
# pre-clampsia: a retrospective study in a universty affiliated maternity
# hospital

# Here, we'll be aiming to estimate the association between creatinine and 
# blood pressure in pregnant women (adjusted for age)

# 0. load libraries
library(haven)
library(dplyr)

# 1. load data
data <-
  read.csv2(file = "./data/McCarthy_gestationalht.csv")

# Number of individuals in original data
data %>% nrow() # 475

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

# save RDS file
saveRDS(data, file = "./data/sbp_creatinine.RDS")