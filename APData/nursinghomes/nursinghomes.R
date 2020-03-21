#import the data
library(readr)
library(dplyr)
deficiencies_basic <- read.csv("associatedpress_nursing_home_inspections_infection_control/KHN_SNF_Infection_Deficiencies.csv")
ar_deficiencies_basic <- filter(deficiencies_basic, state=="AR")
deficiencies_detail <- read.csv("associatedpress_nursing_home_inspections_infection_control/KHN_SNF_Inspection_Deficiencies_FullText.csv")
ar_deficiencies_detail <- filter(deficiencies_detail, state=="AR")

#join the data
deficiencies_all <- full_join(deficiencies_basic, deficiencies_detail, by = "provnum")
ar_deficiencies_all <- filter(deficiencies_all, state=="AR")

#analyze the data
scopesev <- group_by(deficiencies_all, Scope_and_Severity_Code) %>%
  summarize(type_harm=n())
ar_scopesev <- group_by(ar_deficiencies_all, Scope_and_Severity_Code) %>%
  summarize(type_harm=n())

group_by(ar_deficiencies_all, Year) %>%
  summarize(years=n())
