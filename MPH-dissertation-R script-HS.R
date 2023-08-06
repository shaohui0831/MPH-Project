library(haven)
library(foreign)
library(dplyr)
library(descr)
library(tidyr)
library(mosaic)
library(ggplot2)
library(PMCMRplus)
library(forcats)
library(tableone)
library(knitr)
library(purrr)
library(survival)
library(broom)



mphproject <- read_dta("/Users/shaohui/Desktop/MPH\ Final\ Project/data/Reduced_UKBiobank_breast_cancer_Ahui_ST12.dta")

mphnew <- mphproject[, c("eid", "eid2", "_21003_0_0", "_31_0_0", "_6138_0_0", "_189_0_0", "_738_0_0", "_21000_0_0",
                         "_20116_0_0", "_894_0_0", "_914_0_0", "_2443_0_0",
                         "_21001_0_0", "_48_0_0", "_2724_0_0",
                         "_2784_0_0", "_2734_0_0",
                         "_3581_0_0", "n_3581_0_0", "n_3581_1_0", "n_3581_2_0", "n_3581_3_0", 
                         "n_3872_0_0", "n_3872_1_0", "n_3872_2_0", "n_3872_3_0", 
                         "_20110_0_0", "_20110_0_1", "_20110_0_2", "_20110_0_3", "_20110_0_4", "_20110_0_5", "_20110_0_6", "_20110_0_7", "_20110_0_8", "_20110_0_9", "_20110_0_10", 
                         "_20001_0_0", "_20001_0_1", "_20001_0_2", "_20001_0_3", "_20001_0_4", "_20001_0_5", 
                         "_20077_0_0", "assdate1", "dateIncidene_CANCERBREAST_f3", "CANCERBREAST_admit_f3",
                         "n_26011_0_0", "n_26011_1_0", "n_26011_2_0", "n_26011_3_0", "n_26011_4_0", 
                         "n_26013_0_0", "n_26013_1_0", "n_26013_2_0", "n_26013_3_0", "n_26013_4_0",
                         "n_26002_0_0", "n_26002_1_0", "n_26002_2_0", "n_26002_3_0", "n_26002_4_0", 
                         "n_26030_0_0", "n_26030_1_0", "n_26030_2_0", "n_26030_3_0", "n_26030_4_0", 
                         "n_26031_0_0", "n_26031_1_0", "n_26031_2_0", "n_26031_3_0", "n_26031_4_0", 
                         "n_26005_0_0", "n_26005_1_0", "n_26005_2_0", "n_26005_3_0", "n_26005_4_0", 
                         "n_26008_0_0", "n_26008_1_0", "n_26008_2_0", "n_26008_3_0", "n_26008_4_0", 
                         "n_26017_0_0", "n_26017_1_0", "n_26017_2_0", "n_26017_3_0", "n_26017_4_0",
                         "n_22038_0_0", "n_22039_0_0")]

mphnew <- rename(mphnew, sex = `_31_0_0`, age_baseline = `_21003_0_0`, ethnicity = `_21000_0_0`, education_qualifications = `_6138_0_0`, Townsend_index = `_189_0_0`, household_income = `_738_0_0`)
mphnew <- rename(mphnew, diabetes = `_2443_0_0`, smoking = `_20116_0_0`, mod_activity = `n_22038_0_0`, vig_activity = `n_22039_0_0`)
mphnew <- rename(mphnew, oralcpill = `_2784_0_0`, num_children = `_2734_0_0`)
mphnew <- rename(mphnew, age_primiparous_0 = `n_3872_0_0`, age_primiparous_1 = `n_3872_1_0`, age_primiparous_2 = `n_3872_2_0`, age_primiparous_3 = `n_3872_3_0`, )
mphnew <- rename(mphnew, menopause_age_00 = `_3581_0_0`, menopause_age_0 = `n_3581_0_0`, menopause_age_1 = `n_3581_1_0`, menopause_age_2 = `n_3581_2_0`, menopause_age_3 = `n_3581_3_0`) 

mphnew <- rename(mphnew, 
                 mother_illness_0 = `_20110_0_0`, 
                 mother_illness_1 = `_20110_0_1`, 
                 mother_illness_2 = `_20110_0_2`, 
                 mother_illness_3 = `_20110_0_3`, 
                 mother_illness_4 = `_20110_0_4`, 
                 mother_illness_5 = `_20110_0_5`, 
                 mother_illness_6 = `_20110_0_6`, 
                 mother_illness_7 = `_20110_0_7`, 
                 mother_illness_8 = `_20110_0_8`, 
                 mother_illness_9 = `_20110_0_9`, 
                 mother_illness_10 = `_20110_0_10`)

mphnew <- rename(mphnew, num_of_completed_questionnaires = `_20077_0_0`)
mphnew <- rename(mphnew, cancer_0 = `_20001_0_0`, cancer_1 = `_20001_0_1`, cancer_2 = `_20001_0_2`, cancer_3 = `_20001_0_3`, cancer_4 = `_20001_0_4`, cancer_5 = `_20001_0_5`)
                 
mphnew <- rename(mphnew, bmi = `_21001_0_0`, wc = `_48_0_0`, menopause_status = `_2724_0_0`)

mphnew <- rename(mphnew, sugar_baseline = `n_26011_0_0`, sugar_c1 = `n_26011_1_0`, sugar_c2 = `n_26011_2_0`, sugar_c3 = `n_26011_3_0`, sugar_c4 = `n_26011_4_0`)
mphnew <- rename(mphnew, carb_baseline = `n_26013_0_0`, carb_c1 = `n_26013_1_0`, carb_c2 = `n_26013_2_0`, carb_c3 = `n_26013_3_0`, carb_c4 = `n_26013_4_0`)

mphnew <- rename(mphnew, energy_baseline = `n_26002_0_0`, energy_c1 = `n_26002_1_0`, energy_c2 = `n_26002_2_0`, energy_c3 = `n_26002_3_0`, energy_c4 = `n_26002_4_0`)
mphnew <- rename(mphnew, starch_baseline = `n_26031_0_0`, starch_c1 = `n_26031_1_0`, starch_c2 = `n_26031_2_0`, starch_c3 = `n_26031_3_0`, starch_c4 = `n_26031_4_0`)
mphnew <- rename(mphnew, fat_baseline =`n_26008_0_0`, fat_c1 = `n_26008_1_0`, fat_c2 = `n_26008_2_0`, fat_c3 = `n_26008_3_0`, fat_c4 = `n_26008_4_0`)
mphnew <- rename(mphnew, protein_baseline = `n_26005_0_0`, protein_c1 = `n_26005_1_0`, protein_c2 = `n_26005_2_0`, protein_c3 = `n_26005_3_0`, protein_c4 = `n_26005_4_0`)
mphnew <- rename(mphnew, alcohol_baseline = `n_26030_0_0`, alcohol_c1 = `n_26030_1_0`, alcohol_c2 = `n_26030_2_0`, alcohol_c3 = `n_26030_3_0`, alcohol_c4 = `n_26030_4_0`)
mphnew <- rename(mphnew, fibre_baseline = `n_26017_0_0`, fibre_c1 = `n_26017_1_0`, fibre_c2 = `n_26017_2_0`, fibre_c3 = `n_26017_3_0`, fibre_c4 = `n_26017_4_0`)


########filter data###############
filter_sex <- mphnew %>%
  filter(sex == 0)

filter_quest <- filter_sex %>%
  filter(num_of_completed_questionnaires >= 1)

filter_energy <- filter_quest %>%
  filter(is.na(energy_baseline) | (energy_baseline >= 2510 & energy_baseline <= 16736))

filter_bcbaseline <- filter_energy %>%
  filter(!(cancer_0 == 1002 | cancer_1 == 1002 | cancer_2 == 1002 | cancer_3 == 1002 | cancer_4 == 1002 | cancer_5 == 1002))

filterdata_NAsugarcarb <- filter_bcbaseline %>%
  filter(!is.na(sugar_baseline) |
         !is.na(sugar_c1) |
         !is.na(sugar_c2) |
         !is.na(sugar_c3) | 
         !is.na(sugar_c4) |
         !is.na(carb_baseline) |
         !is.na(carb_c1) |
         !is.na(carb_c2) |
         !is.na(carb_c3) |
         !is.na(carb_c4))

filterdata_clean <- filterdata_NAsugarcarb %>%
  filter(!is.na(CANCERBREAST_admit_f3))



rm(filterdata_NAsugarcarb, filter_bcbaseline, filter_energy, filter_quest, filter_sex)


# names(filterdata_clean)


########clean data#####################
filterdata_clean <- filterdata_clean %>%
  mutate_at(vars(age_baseline, Townsend_index, 
                 bmi, wc,
                 mod_activity, vig_activity, 
                 menopause_age_00, menopause_age_0, menopause_age_1, menopause_age_2, menopause_age_3,
                 age_primiparous_0, age_primiparous_1, age_primiparous_2, age_primiparous_3,
                 sugar_baseline, sugar_c1, sugar_c2, sugar_c3, sugar_c4,
                 carb_baseline, carb_c1, carb_c2, carb_c3, carb_c4, 
                 energy_baseline, energy_c1, energy_c2, energy_c3,energy_c4, 
                 alcohol_baseline, alcohol_c1, alcohol_c2, alcohol_c3, alcohol_c4, 
                 starch_baseline, starch_c1, starch_c2, starch_c3, starch_c4, 
                 protein_baseline, protein_c1, protein_c2, protein_c3, protein_c4, 
                 fat_baseline, fat_c1, fat_c2, fat_c3, fat_c4,
                 fibre_baseline, fibre_c1, fibre_c2, fibre_c3, fibre_c4
                 ), as.numeric)


filterdata_clean <- filterdata_clean %>%
  mutate_at(vars(num_children, num_of_completed_questionnaires,), as.integer)


filterdata_clean <- filterdata_clean %>%
  mutate_at(vars(sex, education_qualifications, ethnicity, household_income,
                 smoking, diabetes, oralcpill, menopause_status, CANCERBREAST_admit_f3), as.factor)



filterdata_clean <- filterdata_clean %>%
  mutate(
    ethnicity = case_when(
      ethnicity %in% c('1', '1001', '1002', '1003') ~ 'White',
      ethnicity %in% c('2', '2001', '2002', '2003', '2004') ~ 'Mixed',
      ethnicity %in% c('3', '3001', '3002', '3003', '3004') ~ 'South Asian',
      ethnicity %in% c('4', '4001', '4002', '4003') ~ 'Black',
      ethnicity %in% c('5') ~ 'Chinese',
      ethnicity %in% c('6', '-1', '-3') ~ 'Others',
      TRUE ~ NA_character_))
    

# unique(filterdata_clean$ethnicity)



filterdata_clean <- filterdata_clean %>%
  mutate(education_category = case_when(
    education_qualifications == 1 ~ "College or University degree",
    education_qualifications == 2 ~ "High school education",
    education_qualifications %in% c(3, 4) ~ "Middle school education",
    education_qualifications %in% c(5, 6) ~ "Professional qualifications",
    education_qualifications == -7 ~ NA_character_,
    education_qualifications == -3 ~ NA_character_,
    TRUE ~ NA_character_))

# freq(filterdata_clean$education_category)


filterdata_clean$menopause_status <- ifelse(filterdata_clean$menopause_status == '0', 'No',
                                            ifelse(filterdata_clean$menopause_status %in% c('1', '2'), 'Yes',
                                                   ifelse(filterdata_clean$menopause_status %in% c('3', '-3'), NA,
                                                          NA)))

# freq(filterdata_clean$menopause_status)


filterdata_clean <- filterdata_clean %>%
  mutate(
    smoking = case_when(
      smoking == '0' ~ 'Never',
      smoking == '1' ~ 'Previous',
      smoking == '2' ~ 'Current',
      smoking == '-3' ~ NA_character_,
      TRUE ~ NA_character_))

# freq(filterdata_clean$smoking)


vars <- c('mother_illness_0', 'mother_illness_1', 'mother_illness_2', 'mother_illness_3',
          'mother_illness_4', 'mother_illness_5', 'mother_illness_6', 'mother_illness_7',
          'mother_illness_8', 'mother_illness_9', 'mother_illness_10')

for (var in vars) {
  filterdata_clean[[var]] <- case_when(
    filterdata_clean[[var]] %in% c('-11', '-13', '-17', '-21', '-23', '-27') ~ NA_character_,
    filterdata_clean[[var]] == '5' ~ 'Yes',
    TRUE ~ 'No'
  )
}

filterdata_clean$family_history_of_BC_Yes <- ifelse(apply(filterdata_clean[vars] == 'Yes', 1, any), 'Yes', 'No')

# freq(filterdata_clean$family_history_of_BC_Yes)


filterdata_clean <- filterdata_clean %>%
  mutate_at(vars(menopause_age_00, menopause_age_0, menopause_age_1, menopause_age_2, menopause_age_3), 
            ~na_if(., -1)) %>%
  mutate_at(vars(menopause_age_00, menopause_age_0, menopause_age_1, menopause_age_2, menopause_age_3), 
            ~na_if(., -3)) %>%
  mutate(age_at_menopause_first = coalesce(menopause_age_00, menopause_age_0, menopause_age_1, menopause_age_2, menopause_age_3))


filterdata_clean <- filterdata_clean %>%
  mutate_at(vars(age_primiparous_0, age_primiparous_1, age_primiparous_2, age_primiparous_3), 
            ~na_if(., -4)) %>%
  mutate_at(vars(age_primiparous_0, age_primiparous_1, age_primiparous_2, age_primiparous_3), 
            ~na_if(., -3)) %>%
  mutate(age_primiparous_min = pmin(age_primiparous_0, age_primiparous_1, age_primiparous_2, age_primiparous_3, na.rm = TRUE))


filterdata_clean <- filterdata_clean %>%
  mutate(oralcpill_category = case_when(
    oralcpill == 1 ~ "Yes",
    oralcpill == 0 ~ "No",
    oralcpill == -1 ~ "No",
    oralcpill == -3 ~ NA_character_,
    TRUE ~ NA_character_
  ))

# freq(filterdata_clean$oralcpill_category)



filterdata_clean <- filterdata_clean %>%
  mutate(diabetes_category = case_when(
    diabetes == 1 ~ "Yes",
    diabetes == 0 ~ "No",
    diabetes == -1 ~ "No",
    diabetes == -3 ~ NA_character_,
    TRUE ~ NA_character_
  ))

# freq(filterdata_clean$diabetes_category)



filterdata_clean <- filterdata_clean %>%
  mutate(household_income_category = case_when(
    household_income == 1 ~ "Less than 18,000",
    household_income == 2 ~ "18,000 to 30,999",
    household_income == 3 ~ "31,000 to 51,999",
    household_income == 4 ~ "52,000 to 100,000",
    household_income == 5 ~ "Greater than 100,000",
    household_income == -1 ~ NA_character_,
    household_income == -3 ~ NA_character_,
    TRUE ~ NA_character_
  ))

# freq(filterdata_clean$household_income_category)



filterdata_clean <- mutate(filterdata_clean, across(c("carb_baseline", "carb_c1", "carb_c2", "carb_c3", "carb_c4"), as.numeric))
filterdata_clean <- filterdata_clean %>%
  rowwise() %>%
  mutate(mean_carb = mean(c(carb_baseline, carb_c1, carb_c2, carb_c3, carb_c4), na.rm = TRUE))


filterdata_clean <- mutate(filterdata_clean, across(c("sugar_baseline", "sugar_c1", "sugar_c2", "sugar_c3", "sugar_c4"), as.numeric))
filterdata_clean <- filterdata_clean %>%
  rowwise() %>%
  mutate(mean_sugar = mean(c(sugar_baseline, sugar_c1, sugar_c2, sugar_c3, sugar_c4), na.rm = TRUE))


filterdata_clean <- mutate(filterdata_clean, across(c("energy_baseline", "energy_c1", "energy_c2", "energy_c3", "energy_c4"), as.numeric))
filterdata_clean <- filterdata_clean %>%
  rowwise() %>%
  mutate(mean_energy = mean(c(energy_baseline, energy_c1, energy_c2, energy_c3, energy_c4), na.rm = TRUE))


filterdata_clean <- mutate(filterdata_clean, across(c("starch_baseline", "starch_c1", "starch_c2", "starch_c3", "starch_c4"), as.numeric))
filterdata_clean <- filterdata_clean %>%
  rowwise() %>%
  mutate(mean_starch = mean(c(starch_baseline, starch_c1, starch_c2, starch_c3, starch_c4), na.rm = TRUE))


filterdata_clean <- mutate(filterdata_clean, across(c("fat_baseline", "fat_c1", "fat_c2", "fat_c3", "fat_c4"), as.numeric))
filterdata_clean <- filterdata_clean %>%
  rowwise() %>%
  mutate(mean_fat = mean(c(fat_baseline, fat_c1, fat_c2, fat_c3, fat_c4), na.rm = TRUE))


filterdata_clean <- mutate(filterdata_clean, across(c("protein_baseline", "protein_c1", "protein_c2", "protein_c3", "protein_c4"), as.numeric))
filterdata_clean <- filterdata_clean %>%
  rowwise() %>%
  mutate(mean_protein = mean(c(protein_baseline, protein_c1, protein_c2, protein_c3, protein_c4), na.rm = TRUE))


filterdata_clean <- mutate(filterdata_clean, across(c("fibre_baseline", "fibre_c1", "fibre_c2", "fibre_c3", "fibre_c4"), as.numeric))
filterdata_clean <- filterdata_clean %>%
  rowwise() %>%
  mutate(mean_fibre = mean(c(fibre_baseline, fibre_c1, fibre_c2, fibre_c3, fibre_c4), na.rm = TRUE))


filterdata_clean <- mutate(filterdata_clean, across(c("alcohol_baseline", "alcohol_c1", "alcohol_c2", "alcohol_c3", "alcohol_c4"), as.numeric))
filterdata_clean <- filterdata_clean %>%
  rowwise() %>%
  mutate(mean_alcohol = mean(c(alcohol_baseline, alcohol_c1, alcohol_c2, alcohol_c3, alcohol_c4), na.rm = TRUE))


filterdata_clean <- filterdata_clean %>%
  mutate(bmi_category = case_when(
    bmi < 25 ~ 'normal',
    TRUE ~ 'overweight'
  ))

filterdata_clean$bmi_category <- as.factor(filterdata_clean$bmi_category)
# freq(filterdata_clean$bmi_category)


filterdata_clean <- filterdata_clean %>%
  mutate(wc_category = case_when(
    wc < 88.9 ~ 'normal',
    TRUE ~ 'central adiposity'
  ))

filterdata_clean$wc_category <- as.factor(filterdata_clean$wc_category)
#freq(filterdata_clean$wc_category)


filterdata_clean <- filterdata_clean %>%
  mutate(physical_activity = case_when(
    is.na(mod_activity) & 
      is.na(vig_activity) ~ "inactive",
    mod_activity >= 600 | 
      vig_activity >= 600 ~ "active",
    TRUE ~ "inactive"
  ))

# freq(filterdata_clean$physical_activity)


# Define the minimum and maximum of Townsend index
min_townsend <- -6.26
max_townsend <- 11
# Define the breaks for each group
breaks <- c(min_townsend, -2.74, 0.78, 4.3, 7.82, max_townsend)
# Create the new variable
filterdata_clean$Townsend_index_category <- cut(filterdata_clean$Townsend_index, breaks = breaks, labels = c("Least deprived 1st", "2nd", "3rd", "4th", "Most deprived 5th"), include.lowest = TRUE)

filterdata_clean$Townsend_index_category <- as.factor(filterdata_clean$Townsend_index_category)

# freq(filterdata_clean$Townsend_index_category)
# summary(filterdata_clean$Townsend_index_category)


# freq(filterdata_clean$CANCERBREAST_admit_f3)



############description analysis################

filterdata_clean$carb_quintiles <- cut(filterdata_clean$mean_carb, 
                                       breaks = quantile(filterdata_clean$mean_carb, 
                                                         probs = seq(0, 1, by = 0.2), 
                                                         na.rm = TRUE), 
                                       include.lowest = TRUE, labels = FALSE)



carb_quintiles_values <- round(quantile(filterdata_clean$mean_carb, probs = seq(0, 1, by = 0.2), na.rm = TRUE), 1)
for (i in 1:5) {
  print(paste("[", carb_quintiles_values[i], ", ", carb_quintiles_values[i + 1], ")", sep = ""))
}


carb_quintiles_values <- round(quantile(filterdata_clean$mean_carb, probs = seq(0, 1, by = 0.2), na.rm = TRUE), 1)


filterdata_clean$carb_quintiles <- cut(filterdata_clean$mean_carb, 
                                       breaks = carb_quintiles_values, 
                                       include.lowest = TRUE, labels = FALSE)


carb_quintiles_counts <- table(filterdata_clean$carb_quintiles)


for (i in 1:5) {
  print(paste(carb_quintiles_values[i], "-", carb_quintiles_values[i + 1], " (n=", carb_quintiles_counts[i], ")", sep = ""))
}




sugar_quintiles_values <- round(quantile(filterdata_clean$mean_sugar, probs = seq(0, 1, by = 0.2), na.rm = TRUE), 1)


filterdata_clean$sugar_quintiles <- cut(filterdata_clean$mean_sugar, 
                                       breaks = sugar_quintiles_values, 
                                       include.lowest = TRUE, labels = FALSE)


sugar_quintiles_counts <- table(filterdata_clean$sugar_quintiles)


for (i in 1:5) {
  print(paste(sugar_quintiles_values[i], "-", sugar_quintiles_values[i + 1], " (n=", sugar_quintiles_counts[i], ")", sep = ""))
}




vars <- c("sex", "age_baseline", "ethnicity", "education_category", "Townsend_index_category", "household_income_category",
          "bmi", "wc", "diabetes_category","smoking", "mod_activity", "vig_activity", "menopause_status",
          "family history of BC_Yes", "CANCERBREAST_admit_f3", "num_children", "oralcpill_category","age_at_menopause_first",
          "mean_alcohol", "mean_energy", "mean_starch", "mean_fat", "mean_protein", "mean_fibre")

tableOne <- CreateTableOne(vars = vars, strata = "carb_quintiles", data = filterdata_clean, includeNA = TRUE,
                           factorVars = c("sex","ethnicity","education_category", "Townsend_index_category", "household_income_category", 
                                          "diabetes_category", "smoking", "menopause_status", "family history of BC_Yes", "CANCERBREAST_admit_f3",
                                          "oralcpill_category"))
print(tableOne, smd = TRUE)




vars <- c("sex", "age_baseline", "ethnicity", "education_category", "Townsend_index", "household_income_category",
          "bmi", "wc", "diabetes_category","smoking", "mod_activity", "vig_activity", "menopause_status",
          "family history of BC_Yes", "CANCERBREAST_admit_f3", "num_children", "oralcpill_category","age_at_menopause_first",
          "mean_alcohol", "mean_energy", "mean_starch", "mean_fat", "mean_protein", "mean_fibre")
tableTwo <- CreateTableOne(vars = vars, strata = "sugar_quintiles", data = filterdata_clean, includeNA = TRUE,
                           factorVars = c("sex","ethnicity","education_category", "household_income_category", 
                                          "diabetes_category", "smoking", "menopause_status", "family history of BC_Yes","CANCERBREAST_admit_f3",
                                          "oralcpill_category"))
print(tableTwo, smd = TRUE)



# use chi test carb

crosstab(filterdata_clean$ethnicity, filterdata_clean$carb_quintiles, chisq=TRUE, expected=FALSE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$education_category, filterdata_clean$carb_quintiles, chisq=TRUE, expected=FALSE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$household_income_category, filterdata_clean$carb_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$diabetes_category, filterdata_clean$carb_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$smoking, filterdata_clean$carb_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$menopause_status, filterdata_clean$carb_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$oralcpill_category, filterdata_clean$carb_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$CANCERBREAST_admit_f3, filterdata_clean$carb_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$bmi_category, filterdata_clean$carb_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$wc_category, filterdata_clean$carb_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$Townsend_index_category, filterdata_clean$carb_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$physical_activity, filterdata_clean$carb_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)



# use chi test sugar

crosstab(filterdata_clean$ethnicity, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=FALSE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$education_category, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=FALSE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$household_income_category, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$diabetes_category, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$smoking, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$menopause_status, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$oralcpill_category, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$CANCERBREAST_admit_f3, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$bmi_category, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$wc_category, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$Townsend_index_category, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = TRUE, total.r = TRUE, total.c = TRUE)

crosstab(filterdata_clean$physical_activity, filterdata_clean$sugar_quintiles, chisq=TRUE, expected=TRUE, plot=FALSE, prop.c = TRUE, prop.r = TRUE, missing.include = FALSE, total.r = TRUE, total.c = TRUE)



# cal mean(sd)
mean_age_baseline <- mean(filterdata_clean$age_baseline, na.rm = TRUE)
print(mean_age_baseline)
sd_age_baseline <- sd(filterdata_clean$age_baseline, na.rm = TRUE)
print(sd_age_baseline)
summary(filterdata_clean$age_baseline)
mean_age_baseline_by_carb <- aggregate(filterdata_clean$age_baseline ~ filterdata_clean$carb_quintiles, FUN = mean, na.rm = TRUE)
sd_age_baseline_by_carb <- aggregate(filterdata_clean$age_baseline ~ filterdata_clean$carb_quintiles, FUN = sd, na.rm = TRUE)


age_at_menopause_first <- mean(filterdata_clean$age_at_menopause_first, na.rm = TRUE)
print(age_at_menopause_first)
age_at_menopause_first <- sd(filterdata_clean$age_at_menopause_first, na.rm = TRUE)
print(age_at_menopause_first)
mean_age_at_menopause_first_by_carb <- aggregate(filterdata_clean$age_at_menopause_first ~ filterdata_clean$carb_quintiles, FUN = mean, na.rm = TRUE)
sd_age_at_menopause_first_by_carb <- aggregate(filterdata_clean$age_at_menopause_first ~ filterdata_clean$carb_quintiles, FUN = sd, na.rm = TRUE)

mean_age_at_menopause_first_by_sugar <- aggregate(filterdata_clean$age_at_menopause_first ~ filterdata_clean$sugar_quintiles, FUN = mean, na.rm = TRUE)
sd_age_at_menopause_first_by_sugar <- aggregate(filterdata_clean$age_at_menopause_first ~ filterdata_clean$sugar_quintiles, FUN = sd, na.rm = TRUE)



num_children <- mean(filterdata_clean$num_children, na.rm = TRUE)
print(num_children)
num_children <- sd(filterdata_clean$num_children, na.rm = TRUE)
print(num_children)

mod_activity <- mean(filterdata_clean$mod_activity, na.rm = TRUE)
print(mod_activity)
mod_activity <- sd(filterdata_clean$mod_activity, na.rm = TRUE)
print(mod_activity)
summary(filterdata_clean$mod_activity)

mean_vig_activity <- mean(filterdata_clean$vig_activity, na.rm = TRUE)
sd_vig_activity <- sd(filterdata_clean$vig_activity, na.rm = TRUE)
print(mean_vig_activity)
print(sd_vig_activity)

mean_vig_activity_by_carb <- aggregate(filterdata_clean$vig_activity ~ filterdata_clean$carb_quintiles, FUN = mean, na.rm = TRUE)
sd_vig_activity_by_carb <- aggregate(filterdata_clean$vig_activity ~ filterdata_clean$carb_quintiles, FUN = sd, na.rm = TRUE)

mean_energy <- mean(filterdata_clean$mean_energy, na.rm = TRUE)
print(mean_energy)
mean_energy <- sd(filterdata_clean$mean_energy, na.rm = TRUE)
print(mean_energy)

mean_starch <- mean(filterdata_clean$mean_starch, na.rm = TRUE)
print(mean_starch)
mean_starch <- sd(filterdata_clean$mean_starch, na.rm = TRUE)
print(mean_starch)

mean_fat <- mean(filterdata_clean$mean_fat, na.rm = TRUE)
print(mean_fat)
mean_fat <- sd(filterdata_clean$mean_fat, na.rm = TRUE)
print(mean_fat)

mean_protein <- mean(filterdata_clean$mean_protein, na.rm = TRUE)
print(mean_protein)
mean_protein <- sd(filterdata_clean$mean_protein, na.rm = TRUE)
print(mean_protein)

mean_fibre <- mean(filterdata_clean$mean_fibre, na.rm = TRUE)
print(mean_fibre)
mean_fibre <- sd(filterdata_clean$mean_fibre, na.rm = TRUE)
print(mean_fibre)

mean_alcohol <- mean(filterdata_clean$mean_alcohol, na.rm = TRUE)
print(mean_alcohol)
mean_alcohol <- sd(filterdata_clean$mean_alcohol, na.rm = TRUE)
print(mean_alcohol)
summary(filterdata_clean$mean_alcohol)


# continuous variables
favstats(filterdata_clean$age_baseline ~ filterdata_clean$carb_quintiles)
favstats(filterdata_clean$Townsend_index ~ filterdata_clean$carb_quintiles)



# use anova test carb
anova1 <- aov(filterdata_clean$age_baseline ~ filterdata_clean$carb_quintiles) 
summary(anova1)
anova2 <- aov(filterdata_clean$Townsend_index ~ filterdata_clean$carb_quintiles) 
summary(anova2)
anova3 <- aov(filterdata_clean$bmi ~ filterdata_clean$carb_quintiles) 
summary(anova3)
anova4 <- aov(filterdata_clean$wc ~ filterdata_clean$carb_quintiles) 
summary(anova4)
anova5 <- aov(filterdata_clean$mod_activity ~ filterdata_clean$carb_quintiles) 
summary(anova5)

kruskal.test(filterdata_clean$vig_activity ~ filterdata_clean$carb_quintiles)

anova7 <- aov(filterdata_clean$age_at_menopause_first ~ filterdata_clean$carb_quintiles) 
summary(anova7)
anova8 <- aov(filterdata_clean$num_children ~ filterdata_clean$carb_quintiles) 
summary(anova8)
anova9 <- aov(filterdata_clean$mean_energy ~ filterdata_clean$carb_quintiles) 
summary(anova9)
anova10 <- aov(filterdata_clean$mean_alcohol ~ filterdata_clean$carb_quintiles) 
summary(anova10)
anova11 <- aov(filterdata_clean$mean_starch ~ filterdata_clean$carb_quintiles) 
summary(anova11)
anova12 <- aov(filterdata_clean$mean_fat ~ filterdata_clean$carb_quintiles) 
summary(anova12)
anova13 <- aov(filterdata_clean$mean_protein ~ filterdata_clean$carb_quintiles) 
summary(anova13)
anova14 <- aov(filterdata_clean$mean_fibre ~ filterdata_clean$carb_quintiles) 
summary(anova14)
anova15 <- aov(filterdata_clean$mean_alcohol ~ filterdata_clean$carb_quintiles) 
summary(anova15)


histogram(filterdata_clean$bmi)
histogram(filterdata_clean$vig_activity)


# use anova test sugar
anova1 <- aov(filterdata_clean$age_baseline ~ filterdata_clean$sugar_quintiles) 
summary(anova1)
anova2 <- aov(filterdata_clean$Townsend_index ~ filterdata_clean$sugar_quintiles) 
summary(anova2)
anova3 <- aov(filterdata_clean$bmi ~ filterdata_clean$sugar_quintiles) 
summary(anova3)
anova4 <- aov(filterdata_clean$wc ~ filterdata_clean$sugar_quintiles) 
summary(anova4)
anova5 <- aov(filterdata_clean$mod_activity ~ filterdata_clean$sugar_quintiles) 
summary(anova5)

kruskal.test(filterdata_clean$vig_activity ~ filterdata_clean$sugar_quintiles)

anova7 <- aov(filterdata_clean$age_at_menopause_first ~ filterdata_clean$sugar_quintiles) 
summary(anova7)
anova8 <- aov(filterdata_clean$num_children ~ filterdata_clean$sugar_quintiles) 
summary(anova8)
anova9 <- aov(filterdata_clean$mean_energy ~ filterdata_clean$sugar_quintiles) 
summary(anova9)
anova10 <- aov(filterdata_clean$mean_alcohol ~ filterdata_clean$sugar_quintiles) 
summary(anova10)
anova11 <- aov(filterdata_clean$mean_starch ~ filterdata_clean$sugar_quintiles) 
summary(anova11)
anova12 <- aov(filterdata_clean$mean_fat ~ filterdata_clean$sugar_quintiles) 
summary(anova12)
anova13 <- aov(filterdata_clean$mean_protein ~ filterdata_clean$sugar_quintiles) 
summary(anova13)
anova14 <- aov(filterdata_clean$mean_fibre ~ filterdata_clean$sugar_quintiles) 
summary(anova14)
anova15 <- aov(filterdata_clean$mean_alcohol ~ filterdata_clean$sugar_quintiles) 
summary(anova15)



table(filterdata_clean$bmi_category, filterdata_clean$menopause_status)
table(filterdata_clean$bmi_category)



################################ Cox PH Model #########################################


# Cox models
follow_up_time <- as.numeric(filterdata_clean$dateIncidene_CANCERBREAST_f3 - filterdata_clean$assdate1)
filterdata_clean$follow_up_time <- follow_up_time
follow_up_length <- as.numeric(difftime(filterdata_clean$dateIncidene_CANCERBREAST_f3, filterdata_clean$assdate1, units = "days"))
favstats(follow_up_time)
histogram(follow_up_time)

# Calculate person-years
person_years <- sum(follow_up_time)/365.25
print(person_years)

freq(mphnew$sex, plot=FALSE)
mphnew$age_baseline <- as.numeric(mphnew$age_baseline)
summary(mphnew$age_baseline)
histogram(filterdata_clean$mean_alcohol)
histogram(filterdata_clean$mod_activity)


# create Surv
surv_obj <- Surv(follow_up_length, filterdata_clean$CANCERBREAST_admit_f3 == 1)


modeltest1 <- coxph(surv_obj ~ as.factor(carb_quintiles) , data = filterdata_clean)
summary(modeltest1)
cox.zph(modeltest1)

modeltest2 <- coxph(surv_obj ~ as.factor(sugar_quintiles) , data = filterdata_clean)
summary(modeltest2)
cox.zph(modeltest2)

modeltest3 <- coxph(surv_obj ~ as.factor(carb_quintiles) + bmi_category, data = filterdata_clean)
summary(modeltest3)
cox.zph(modeltest3)

modeltest4 <- coxph(surv_obj ~ as.factor(sugar_quintiles) + bmi_category, data = filterdata_clean)
summary(modeltest4)
cox.zph(modeltest4)

modeltest5 <- coxph(surv_obj ~ as.factor(carb_quintiles) + age_baseline, data = filterdata_clean)
summary(modeltest5)
cox.zph(modeltest5)

modeltest6 <- coxph(surv_obj ~ as.factor(sugar_quintiles) + age_baseline, data = filterdata_clean)
summary(modeltest6)
cox.zph(modeltest6)




model1a <- coxph(surv_obj ~ as.factor(carb_quintiles) + mean_energy, data = filterdata_clean)
summary(model1a)
res.zph <- cox.zph(model1a)
plot(res.zph)
cox.zph(model1a)

model1b <- coxph(surv_obj ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, data = filterdata_clean)
summary(model1b)
cox.zph(model1b)

model1c <- coxph(surv_obj ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                 + smoking + physical_activity + bmi_category + menopause_status + mean_alcohol 
                 + mean_energy, data = filterdata_clean)
summary(model1c)
cox.zph(model1c)



model2a <- coxph(surv_obj ~ as.factor(sugar_quintiles) + mean_energy, data = filterdata_clean)
summary(model2a)
cox.zph(model2a)

model2b <- coxph(surv_obj ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, data = filterdata_clean)
summary(model2b)
cox.zph(model2b)

model2c <- coxph(surv_obj ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                 + smoking + physical_activity + bmi_category + menopause_status + mean_alcohol 
                 + mean_energy, data = filterdata_clean)
summary(model2c)
cox.zph(model2c)


#############################################subgroup - menopausal status########


# create subgroup after follow_up_length is created
filterdata_clean_premenopausal <- subset(filterdata_clean, menopause_status == "No")
filterdata_clean_postmenopausal <- subset(filterdata_clean, menopause_status == "Yes")


follow_up_length <- as.numeric(difftime(filterdata_clean_premenopausal$dateIncidene_CANCERBREAST_f3, filterdata_clean_premenopausal$assdate1, units = "days"))
filterdata_clean_premenopausal$follow_up_length <- follow_up_length


# premenopausal model - carb
surv_premenopausal <- Surv(as.numeric(filterdata_clean_premenopausal$follow_up_length), filterdata_clean_premenopausal$CANCERBREAST_admit_f3 == 1)

modelpre1a <- coxph(surv_premenopausal ~ as.factor(carb_quintiles) + mean_energy, data = filterdata_clean_premenopausal)
summary(modelpre1a)
cox.zph(modelpre1a)

modelpre1b <- coxph(surv_premenopausal ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                 data = filterdata_clean_premenopausal)
summary(modelpre1b)
cox.zph(modelpre1b)

modelpre1c <- coxph(surv_premenopausal ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category
                    + smoking + physical_activity + bmi_category + mean_alcohol + mean_energy, 
                 data = filterdata_clean_premenopausal)
summary(modelpre1c)
cox.zph(modelpre1c)


# postmenopausal model - carb
follow_up_length <- as.numeric(difftime(filterdata_clean_postmenopausal$dateIncidene_CANCERBREAST_f3, filterdata_clean_postmenopausal$assdate1, units = "days"))
filterdata_clean_postmenopausal$follow_up_length <- follow_up_length

surv_postmenopausal <- Surv(as.numeric(filterdata_clean_postmenopausal$follow_up_length), filterdata_clean_postmenopausal$CANCERBREAST_admit_f3 == 1)

modelpost1a <- coxph(surv_postmenopausal ~ as.factor(carb_quintiles) + mean_energy, data = filterdata_clean_postmenopausal)
summary(modelpost1a)
cox.zph(modelpost1a)

modelpost1b <- coxph(surv_postmenopausal ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                 data = filterdata_clean_postmenopausal)
summary(modelpost1b)
cox.zph(modelpost1b)

modelpost1c <- coxph(surv_postmenopausal ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                     + smoking + physical_activity + bmi_category + mean_alcohol 
                     + mean_energy, data = filterdata_clean_postmenopausal)
summary(modelpost1c)
cox.zph(modelpost1c)


# premenopausal model - sugar
surv_premenopausal <- Surv(as.numeric(filterdata_clean_premenopausal$follow_up_length), filterdata_clean_premenopausal$CANCERBREAST_admit_f3 == 1)

modelpre2a <- coxph(surv_premenopausal ~ as.factor(sugar_quintiles) + mean_energy, data = filterdata_clean_premenopausal)
summary(modelpre2a)
cox.zph(modelpre2a)

modelpre2b <- coxph(surv_premenopausal ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                    data = filterdata_clean_premenopausal)
summary(modelpre2b)
cox.zph(modelpre2b)

modelpre2c <- coxph(surv_premenopausal ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                    + smoking + physical_activity + bmi_category + mean_alcohol 
                    + mean_energy, data = filterdata_clean_premenopausal)
summary(modelpre2c)
cox.zph(modelpre2c)

modelpre2atest <- coxph(surv_premenopausal ~ as.factor(sugar_quintiles), data = filterdata_clean_premenopausal)
summary(modelpre2atest)



# premenopausal model - sugar
surv_postmenopausal <- Surv(as.numeric(filterdata_clean_postmenopausal$follow_up_length), filterdata_clean_postmenopausal$CANCERBREAST_admit_f3 == 1)

modelpost2a <- coxph(surv_postmenopausal ~ as.factor(sugar_quintiles) + mean_energy, data = filterdata_clean_postmenopausal)
summary(modelpost2a)
cox.zph(modelpost2a)

modelpost2b <- coxph(surv_postmenopausal ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                     data = filterdata_clean_postmenopausal)
summary(modelpost2b)
cox.zph(modelpost2b)

modelpost2c <- coxph(surv_postmenopausal ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                     + smoking + physical_activity + bmi_category + mean_alcohol 
                     + mean_energy, data = filterdata_clean_postmenopausal)
summary(modelpost2c)
cox.zph(modelpost2c)

modelpost2atest <- coxph(surv_postmenopausal ~ as.factor(sugar_quintiles), data = filterdata_clean_postmenopausal)
summary(modelpost2atest)


############################################# subgroup - BMI ####

filterdata_clean_normal <- subset(filterdata_clean, bmi_category == "normal")
filterdata_clean_overweight <- subset(filterdata_clean, bmi_category == "overweight")


follow_up_length <- as.numeric(difftime(filterdata_clean_normal$dateIncidene_CANCERBREAST_f3, filterdata_clean_normal$assdate1, units = "days"))
filterdata_clean_normal$follow_up_length <- follow_up_length

surv_normal <- Surv(as.numeric(filterdata_clean_normal$follow_up_length), filterdata_clean_normal$CANCERBREAST_admit_f3 == 1)

modelnor1a <- coxph(surv_normal ~ as.factor(carb_quintiles) + mean_energy, data = filterdata_clean_normal)
summary(modelnor1a)
cox.zph(modelnor1a)

modelnor1b <- coxph(surv_normal ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                    data = filterdata_clean_normal)
summary(modelnor1b)
cox.zph(modelnor1b)

modelnor1c <- coxph(surv_normal ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                    + smoking + physical_activity + menopause_status + mean_alcohol 
                    + mean_energy, data = filterdata_clean_normal)
summary(modelnor1c)
cox.zph(modelnor1c)



follow_up_length <- as.numeric(difftime(filterdata_clean_overweight$dateIncidene_CANCERBREAST_f3, filterdata_clean_overweight$assdate1, units = "days"))
filterdata_clean_overweight$follow_up_length <- follow_up_length

surv_overweight <- Surv(as.numeric(filterdata_clean_overweight$follow_up_length), filterdata_clean_overweight$CANCERBREAST_admit_f3 == 1)

modelover1a <- coxph(surv_overweight ~ as.factor(carb_quintiles) + mean_energy, data = filterdata_clean_overweight)
summary(modelover1a)
cox.zph(modelover1a)

modelover1b <- coxph(surv_overweight ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                    data = filterdata_clean_overweight)
summary(modelover1b)
cox.zph(modelover1b)

modelover1c <- coxph(surv_overweight ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                     + smoking + physical_activity + menopause_status + mean_alcohol 
                     + mean_energy, data = filterdata_clean_overweight)
summary(modelover1c)
cox.zph(modelover1c)


# normal model - sugar

surv_normal <- Surv(as.numeric(filterdata_clean_normal$follow_up_length), filterdata_clean_normal$CANCERBREAST_admit_f3 == 1)
modelnor2a <- coxph(surv_normal ~ as.factor(sugar_quintiles) + mean_energy, data = filterdata_clean_normal)
summary(modelnor2a)
cox.zph(modelnor2a)

modelnor2b <- coxph(surv_normal ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                    data = filterdata_clean_normal)
summary(modelnor2b)
cox.zph(modelnor2b)

modelnor2c <- coxph(surv_normal ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                    + smoking + physical_activity + menopause_status + mean_alcohol 
                    + mean_energy, data = filterdata_clean_normal)
summary(modelnor2c)
cox.zph(modelnor2c)

# overweight model - sugar

surv_overweight <- Surv(as.numeric(filterdata_clean_overweight$follow_up_length), filterdata_clean_overweight$CANCERBREAST_admit_f3 == 1)
modelover2a <- coxph(surv_overweight ~ as.factor(sugar_quintiles) + mean_energy, data = filterdata_clean_overweight)
summary(modelover2a)
cox.zph(modelover2a)

modelover2b <- coxph(surv_overweight ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                     data = filterdata_clean_overweight)
summary(modelover2b)
cox.zph(modelover2b)

modelover2c <- coxph(surv_overweight ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                     + smoking + physical_activity + menopause_status + mean_alcohol 
                     + mean_energy, data = filterdata_clean_overweight)
summary(modelover2c)
cox.zph(modelover2c)


###########################################################combined groups##################################################


# create subgroup plus bmi
filterdata_clean_premenopausal_normal <- subset(filterdata_clean_premenopausal, bmi_category == "normal")
filterdata_clean_premenopausal_overweight <- subset(filterdata_clean_premenopausal, bmi_category == "overweight")
filterdata_clean_postmenopausal_normal <- subset(filterdata_clean_postmenopausal, bmi_category == "normal")
filterdata_clean_postmenopausal_overweight <- subset(filterdata_clean_postmenopausal, bmi_category == "overweight")


# premenopausal normal weight model - carb
follow_up_length <- as.numeric(difftime(filterdata_clean_premenopausal_normal$dateIncidene_CANCERBREAST_f3, filterdata_clean_premenopausal_normal$assdate1, units = "days"))
filterdata_clean_premenopausal_normal$follow_up_length <- follow_up_length

surv_premenopausal_normal <- Surv(as.numeric(filterdata_clean_premenopausal_normal$follow_up_length), filterdata_clean_premenopausal_normal$CANCERBREAST_admit_f3 == 1)

modelpre_normal_1a <- coxph(surv_premenopausal_normal ~ as.factor(carb_quintiles) + mean_energy, data = filterdata_clean_premenopausal_normal)
summary(modelpre_normal_1a)
cox.zph(modelpre_normal_1a)

modelpre_normal_1b <- coxph(surv_premenopausal_normal ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                            data = filterdata_clean_premenopausal_normal)
summary(modelpre_normal_1b)
cox.zph(modelpre_normal_1b)

modelpre_normal_1c <- coxph(surv_premenopausal_normal ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                            + smoking + physical_activity + mean_alcohol 
                            + mean_energy, data = filterdata_clean_premenopausal_normal)
summary(modelpre_normal_1c)
cox.zph(modelpre_normal_1c)


# premenopausal overweight model - carb
follow_up_length <- as.numeric(difftime(filterdata_clean_premenopausal_overweight$dateIncidene_CANCERBREAST_f3, filterdata_clean_premenopausal_overweight$assdate1, units = "days"))
filterdata_clean_premenopausal_overweight$follow_up_length <- follow_up_length

surv_premenopausal_overweight <- Surv(as.numeric(filterdata_clean_premenopausal_overweight$follow_up_length), filterdata_clean_premenopausal_overweight$CANCERBREAST_admit_f3 == 1)

modelpre_overweight_1a <- coxph(surv_premenopausal_overweight ~ as.factor(carb_quintiles) + mean_energy, data = filterdata_clean_premenopausal_overweight)
summary(modelpre_overweight_1a)
cox.zph(modelpre_overweight_1a)

modelpre_overweight_1b <- coxph(surv_premenopausal_overweight ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                                data = filterdata_clean_premenopausal_overweight)
summary(modelpre_overweight_1b)
cox.zph(modelpre_overweight_1b)

modelpre_overweight_1c <- coxph(surv_premenopausal_overweight ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                                + smoking + physical_activity + mean_alcohol 
                                + mean_energy, data = filterdata_clean_premenopausal_overweight)
summary(modelpre_overweight_1c)
cox.zph(modelpre_overweight_1c)

# postmenopausal normal weight model - carb
follow_up_length <- as.numeric(difftime(filterdata_clean_postmenopausal_normal$dateIncidene_CANCERBREAST_f3, filterdata_clean_postmenopausal_normal$assdate1, units = "days"))
filterdata_clean_postmenopausal_normal$follow_up_length <- follow_up_length

surv_postmenopausal_normal <- Surv(as.numeric(filterdata_clean_postmenopausal_normal$follow_up_length), filterdata_clean_postmenopausal_normal$CANCERBREAST_admit_f3 == 1)

modelpost_normal_1a <- coxph(surv_postmenopausal_normal ~ as.factor(carb_quintiles) + mean_energy, data = filterdata_clean_postmenopausal_normal)
summary(modelpost_normal_1a)
cox.zph(modelpost_normal_1a)

modelpost_normal_1b <- coxph(surv_postmenopausal_normal ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                             data = filterdata_clean_postmenopausal_normal)
summary(modelpost_normal_1b)
cox.zph(modelpost_normal_1b)

modelpost_normal_1c <- coxph(surv_postmenopausal_normal ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                             + smoking + physical_activity + mean_alcohol 
                             + mean_energy, data = filterdata_clean_postmenopausal_normal)
summary(modelpost_normal_1c)
cox.zph(modelpost_normal_1c)


# postmenopausal overweight model - carb
follow_up_length <- as.numeric(difftime(filterdata_clean_postmenopausal_overweight$dateIncidene_CANCERBREAST_f3, filterdata_clean_postmenopausal_overweight$assdate1, units = "days"))
filterdata_clean_postmenopausal_overweight$follow_up_length <- follow_up_length

surv_postmenopausal_overweight <- Surv(as.numeric(filterdata_clean_postmenopausal_overweight$follow_up_length), filterdata_clean_postmenopausal_overweight$CANCERBREAST_admit_f3 == 1)

modelpost_overweight_1a <- coxph(surv_postmenopausal_overweight ~ as.factor(carb_quintiles) + mean_energy, data = filterdata_clean_postmenopausal_overweight)
summary(modelpost_overweight_1a)
cox.zph(modelpost_overweight_1a)

modelpost_overweight_1b <- coxph(surv_postmenopausal_overweight ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                                 data = filterdata_clean_postmenopausal_overweight)
summary(modelpost_overweight_1b)
cox.zph(modelpost_overweight_1b)

modelpost_overweight_1c <- coxph(surv_postmenopausal_overweight ~ as.factor(carb_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                                 + smoking + physical_activity + mean_alcohol 
                                 + mean_energy, data = filterdata_clean_postmenopausal_overweight)
summary(modelpost_overweight_1c)
cox.zph(modelpost_overweight_1c)




# premenopausal normal weight model - sugar

modelpre_normal_1a <- coxph(surv_premenopausal_normal ~ as.factor(sugar_quintiles) + mean_energy, data = filterdata_clean_premenopausal_normal)
summary(modelpre_normal_1a)
cox.zph(modelpre_normal_1a)

modelpre_normal_1b <- coxph(surv_premenopausal_normal ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                            data = filterdata_clean_premenopausal_normal)
summary(modelpre_normal_1b)
cox.zph(modelpre_normal_1b)

modelpre_normal_1c <- coxph(surv_premenopausal_normal ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                            + smoking + physical_activity + mean_alcohol 
                            + mean_energy, data = filterdata_clean_premenopausal_normal)
summary(modelpre_normal_1c)
cox.zph(modelpre_normal_1c)

# premenopausal overweight model - sugar
modelpre_overweight_1a <- coxph(surv_premenopausal_overweight ~ as.factor(sugar_quintiles) + mean_energy, data = filterdata_clean_premenopausal_overweight)
summary(modelpre_overweight_1a)
cox.zph(modelpre_overweight_1a)

modelpre_overweight_1b <- coxph(surv_premenopausal_overweight ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                                data = filterdata_clean_premenopausal_overweight)
summary(modelpre_overweight_1b)
cox.zph(modelpre_overweight_1b)

modelpre_overweight_1c <- coxph(surv_premenopausal_overweight ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                                + smoking + physical_activity + mean_alcohol 
                                + mean_energy, data = filterdata_clean_premenopausal_overweight)
summary(modelpre_overweight_1c)
cox.zph(modelpre_overweight_1c)

# postmenopausal normal weight model - sugar
modelpost_normal_1a <- coxph(surv_postmenopausal_normal ~ as.factor(sugar_quintiles) + mean_energy, data = filterdata_clean_postmenopausal_normal)
summary(modelpost_normal_1a)
cox.zph(modelpost_normal_1a)

modelpost_normal_1b <- coxph(surv_postmenopausal_normal ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                             data = filterdata_clean_postmenopausal_normal)
summary(modelpost_normal_1b)
cox.zph(modelpost_normal_1b)

modelpost_normal_1c <- coxph(surv_postmenopausal_normal ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                             + smoking + physical_activity + mean_alcohol 
                             + mean_energy, data = filterdata_clean_postmenopausal_normal)
summary(modelpost_normal_1c)
cox.zph(modelpost_normal_1c)

# postmenopausal overweight model - sugar
modelpost_overweight_1a <- coxph(surv_postmenopausal_overweight ~ as.factor(sugar_quintiles) + mean_energy, data = filterdata_clean_postmenopausal_overweight)
summary(modelpost_overweight_1a)
cox.zph(modelpost_overweight_1a)

modelpost_overweight_1b <- coxph(surv_postmenopausal_overweight ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category + mean_energy, 
                                 data = filterdata_clean_postmenopausal_overweight)
summary(modelpost_overweight_1b)
cox.zph(modelpost_overweight_1b)

modelpost_overweight_1c <- coxph(surv_postmenopausal_overweight ~ as.factor(sugar_quintiles) + age_baseline + Townsend_index_category + household_income_category + education_category 
                                 + smoking + physical_activity + mean_alcohol 
                                 + mean_energy, data = filterdata_clean_postmenopausal_overweight)
summary(modelpost_overweight_1c)
cox.zph(modelpost_overweight_1c)


