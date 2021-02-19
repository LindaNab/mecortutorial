#==================================#
# mecor tutorial                   #
# Internal validation study        #
# preprocess data                  #
#                                  #
# 20210219 lindanab4@gmail.com     #
#==================================#

# Data: https://wwwn.cdc.gov/Nchs/Nhanes/continuousnhanes/default.aspx?BeginYear=2015
# NHANES data: National Health and Nutritional Examination Survey 2015-2016

# Here, we'll be aiming to estimate the association between bmi and 
# serum creatinine, given age
# Exclusion criteria: pregn woman

## 0. load libraries
library(haven)
library(dplyr)

## 1. load data 
# data is obtained from the subsequent websites
# https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.htm
demographics_raw <-
  read_xpt(file = "./data/raw/DEMO_I.XPT") # demographics
# https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.htm
bodymeasures_raw <- 
  read_xpt(file = "./data/raw/BMX_I.XPT") # body measures by health prof
# https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/WHQ_I.htm
weighth_raw <-
  read_xpt(file = "./data/raw/WHQ_I.XPT") # weight history - questionnaire
# https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/TCHOL_I.htm
cholesterol_raw <-
  read_xpt(file = "./data/raw/TCHOL_I.XPT")

# Select demographic variables needed for analysis
demographics <-
  demographics_raw %>%
  select(SEQN, RIDAGEYR, RIDEXPRG, RIAGENDR) # age, pregnant, gender
# age in years 0-79: 0-79, 80: 80 and 80+
nrow(demographics[demographics$RIDAGEYR == 80,])
# pregnancy 1: Yes, 2: No, 3: Cannot ascertain pregnancy
table(demographics$RIDEXPRG, useNA = "always")
# gender 1: Male, 2: Female
table(demographics$RIAGENDR, useNA = "always")

# Select body measures needed for analysis
bodymeasures <- 
  bodymeasures_raw %>%
  select(SEQN, BMXBMI, BMXWT, BMXHT, BMXWAIST) # body mass index (kg/m**2); weight (kg); 
# height (cm)
range(bodymeasures$BMXBMI, na.rm = TRUE) # > 60 super-super-morbidly obese
bodymeasures %>% 
 filter(BMXBMI > 60)

# Select weight history variables needed for analysis
weighth <- 
  weighth_raw %>%
  select(SEQN, WHD010, WHD020) # current self-reported height (inches); 
# current self-reported weight (pounds)
weighth <-
  weighth %>%
  filter(!WHD010 %in% c(9999, 7777)) %>%
  filter(!WHD020 %in% c(9999, 7777)) # remove missings
weighth <-
  weighth %>%
  mutate(WHD010_m = WHD010 * 0.0254,
         WHD020_kg = WHD020 * 0.45359237) # convert to m and kg respectively
weighth <-
  weighth %>%
  mutate(BMI_selfrep = WHD020_kg / WHD010_m ^ 2) # calculate bmi
range(weighth$BMI_selfrep, na.rm = TRUE) # > 60 super-super-morbidly obese
weighth %>% 
  filter(BMI_selfrep > 50) %>%
  left_join(bodymeasures,
            by = "SEQN") # %>% View()
# SEQN 91820 has a measured weight of approx 152.8 pounds, 
# and self-rep weight of 364 pounds --> change to 164 (makes more sense)
weighth <- 
  weighth %>%
  mutate(BMI_selfrep = case_when(SEQN == 91820 ~ (164 * 0.45359237) / WHD010_m ^ 2,
                                 TRUE ~ BMI_selfrep))
weighth %>% 
  inner_join(bodymeasures, by = "SEQN") %>%
  with(., plot(BMXBMI, BMI_selfrep)) # two outliers 
weighth %>% 
  inner_join(bodymeasures, by = "SEQN") %>% # 89810
  filter(BMXBMI > 60 & BMI_selfrep < 45) # self rep kg = 118, meas kg = 181 
weighth %>% 
  inner_join(bodymeasures, by = "SEQN") %>% # 92377
  filter(BMXBMI < 30 & BMI_selfrep > 45) # self rep height = 1.19 m
weighth <- 
  weighth %>%
  filter(!SEQN %in% c(89810, 92377))
bodymeasures <-
  bodymeasures %>%
  filter(!SEQN %in% c(89810, 92377))

# Select cholesterol needed for analysis
cholesterol <-
  cholesterol_raw %>% 
  select(SEQN, LBXTC) # (mg/dL)
range(cholesterol$LBXTC, na.rm = TRUE)

# join tables
data <-
  demographics %>%
  inner_join(., bodymeasures,
             by = "SEQN") %>%
  inner_join(.,
             weighth, 
             by = "SEQN") %>%
  inner_join(.,
             cholesterol,
             by = "SEQN")
data %>% nrow()

# exclusion criteria
# exclude pregnant women
data <-
  data %>%
  filter(is.na(RIDEXPRG) | RIDEXPRG != 1)
data %>% nrow()
# exclude indiv of whom BMI_selfrep is not known
data <-
  data %>%
  filter(!is.na(BMI_selfrep))
# exclude indiv of whom wc is not known
data <- 
  data %>%
  filter(!is.na(BMXWAIST))
data %>% nrow()


# select variables needed for analysis
data <-
  data %>%
  select(SEQN, 
         wc = BMXWAIST,
         age = RIDAGEYR, 
         gender = RIAGENDR,
         bmi_selfrep = BMI_selfrep, 
         bmi = BMXBMI)
data <- 
  data %>%
  mutate(gender = case_when(gender == 2 ~ 1, 
                            gender == 1 ~ 0))
# remove labels 
data <- zap_label(data)

# delete a random set of the measured bmi measures
set.seed(20210219)
id <- sample(1:nrow(data), 0.75 * nrow(data))
data$bmi[id] <- NA

# save RDS file
saveRDS(data, file = paste0("./data/processed/bmi_wc_", 
                            Sys.Date(), 
                            ".RDS"))