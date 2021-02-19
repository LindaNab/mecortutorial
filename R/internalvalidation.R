#==================================#
# mecor tutorial                   #
# Internal validation study        #
#                                  #
# 20210219 lindanab4@gmail.com     #
#==================================#

# Data: https://wwwn.cdc.gov/Nchs/Nhanes/continuousnhanes/default.aspx?BeginYear=2015
# NHANES data: National Health and Nutritional Examination Survey 2015-2016

# Here, we'll be aiming to estimate the association between bmi and 
# waist circumference, given gender and age
# Exclusion criteria: pregn woman

## 0. load libraries
library(mecor)
library(dplyr)

## 1. load data
intvaldata <-
  readRDS(file = "./data/processed/bmi_wc_2021-02-19.RDS")

## 2. data analysis
# Show data to view structure 
View(intvaldata)
# SEQN: subject id
# wc: waist circumference in cm
# age: age in years
# gender: 1 = female, 0 = male
# bmi_selfrep: self reported bmi in kg/m2
# bmi: bmi measured by health professionals in kg/m2 (not measured in all subjects)
intvaldata %>% nrow() # 5415
intvaldata %>% filter(!is.na(bmi)) %>% nrow() # in 1354 individuals, bmi is known

# Plot bmi vs self reported bmi
with(intvaldata, plot(bmi, bmi_selfrep, 
                      xlab = "BMI (kg/m2)", 
                      ylab = "Self-reported BMI (kg/m2)"))

# Plot bmi vs bmi minus bmi_selfrep
with(intvaldata, plot(bmi, bmi - bmi_selfrep,
                      xlab = "BMI (kg/m2)", 
                      ylab = "BMI minus Self-reported BMI (kg/m2)")) 
abline(h = 0, 
       col = "red")
mtext("Self-reported BMI is more often lower than greater than true BMI!",
      side = 3,
      col = "red")

# Plot bmi vs wc
with(intvaldata, plot(bmi, wc,
                      xlab = "BMI (kg/m2)",
                      ylab = "Waist circumference (cm)"))

# Fit a linear model using the 1354 subjects, using bmi measured
# by health professionals:
fit_bmi <-
  lm(wc ~ bmi + gender + age,
     data = intvaldata) 
fit_bmi %>% summary()

# Fit a linear model using the 5415 subjects, using self-reported bmi:
naivefit <-
  lm(wc ~ bmi_selfrep + gender + age, 
     data = intvaldata)
naivefit %>% summary()

# Correcting for measurement error using regression calibration:
mecorfit_regcal <- 
  mecor(wc ~ MeasError(bmi_selfrep, reference = bmi) + gender + age, 
        data = intvaldata)
mecorfit_regcal %>% summary()

# Correcting for measurement error using regression calibration:
mecorfit_efregcal <- 
  mecor(wc ~ MeasError(bmi_selfrep, reference = bmi) + gender + age, 
        data = intvaldata, 
        method = "efficient")
mecorfit_efregcal %>% summary()

# Correcting for measurement error using validation regression calibration:
mecorfit_valregcal <- 
  mecor(wc ~ MeasError(bmi_selfrep, reference = bmi) + gender + age, 
        data = intvaldata, 
        method = "valregcal")
mecorfit_valregcal %>% summary()