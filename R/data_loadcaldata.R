#==================================#
# mecor tutorial                   #
# Calibration study                #
# preprocess data                  #
#                                  #
# 20210218 lindanab4@gmail.com     #
#==================================#

# Data: https://datadryad.org/stash/dataset/doi:10.5061/dryad.ck501
# Paper: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0189254
# Original rq: Irisin, physical activity and fitness status in healthy humans: 
# No association under resting conditions in a cross-sectional study

# Here, we'll be aiming to estimate the association between ....

# 0. load libraries
library(haven)
library(dplyr)

# 1. load data
data <-
  read.csv2(file = "./data/raw/dataset_irisin_PA.csv")

data <-
  data %>%
  mutate(MET = Total.MET..MET.min.week.,
         weight = body.weight..kg.,
         TEE = TEE..kcal.d.)
data <-
  data %>%
  mutate(MET_kcal_day = ((((MET * 3.5 * 86.3) / 200) * 60) / 7 / 24))

with(data, plot(MET_kcal_day, TEE))



