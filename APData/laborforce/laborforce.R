#import the data
library(readr)
library(dplyr)
state_prep <- read.csv("associatedpress_coronavirus_labor_impact/state_preparedness.csv")
county_indus_empl <- read.csv("associatedpress_coronavirus_labor_impact/county_industry_employment.csv")
msa_indus_empl <- read.csv("associatedpress_coronavirus_labor_impact/msa_industry_employment.csv")
us_indus_empl_by_size <- read.csv("associatedpress_coronavirus_labor_impact/national_industry_employment_by_size.csv")
state_biz_by_size <- read.csv("associatedpress_coronavirus_labor_impact/state_businesses_by_size.csv")
state_indus_empl <- read.csv("associatedpress_coronavirus_labor_impact/state_industry_employment.csv")

#find health care workers
#hospitals
state_hospital_empl <- filter(state_indus_empl, industry_code=="622") %>%
  mutate(pct_hosp_empl=employees/total_employees)
#nursing and residential
state_nursingres_empl <- filter(state_indus_empl, industry_code=="623") %>%
  mutate(pct_nursingres_empl=employees/total_employees)
#ambulatory, which includes A LOT
state_ambulatory_empl <- filter(state_indus_empl, industry_code=="621") %>%
  mutate(pct_ambulatory_empl=employees/total_employees)

#other affected employment
#food service
state_foodservice_empl <- filter(state_indus_empl, industry_code=="722") %>%
  mutate(pct_foodservice_empl=employees/total_employees)
#food stores
state_foodstores_empl <- filter(state_indus_empl, industry_code=="445") %>%
  mutate(pct_foodstores_empl=employees/total_employees)


