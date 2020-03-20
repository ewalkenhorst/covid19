#import the data
library(readr)
byhospital <- read.csv("associatedpress_us_hospital_capacity/1_occupancy_rates_at_hospital_level.csv")
bycounty <- read.csv("associatedpress_us_hospital_capacity/2_occupancy_rates_at_county_level.csv")
bystate <- read.csv("associatedpress_us_hospital_capacity/3_occupancy_rates_at_state_level.csv")
byruralurban <- read.csv("associatedpress_us_hospital_capacity/4_occupancy_rates_at_state_rural_urban_level.csv")

#analyze the by-hospital data
library(dplyr)
ar_hospitals <- filter(byhospital, state == "AR")
write.csv(ar_hospitals, "associatedpress_us_hospital_capacity/ar_hospitals.csv")
#average occupancy rate for general and icu beds -- by state, national
filter(ar_hospitals, !is.na(total_general_annual_occupancy_rate)) %>%
  summarize(., avg_gen_occupancy=mean(total_general_annual_occupancy_rate))
filter(byhospital, !is.na(total_general_annual_occupancy_rate)) %>%
  summarize(., avg_gen_occupancy=mean(total_general_annual_occupancy_rate))
filter(ar_hospitals, !is.na(intensive_care_annual_occupancy_rate)) %>%
  summarize(., avg_icu_occupancy=mean(intensive_care_annual_occupancy_rate))
filter(byhospital, !is.na(intensive_care_annual_occupancy_rate)) %>%
  summarize(., avg_icu_occupancy=mean(intensive_care_annual_occupancy_rate))
#total bed count, total empty bed, pct empty -- by state
filter(ar_hospitals, !is.na(total_general_bed_count), !is.na(total_general_number_of_empty_beds))%>%
  summarize(., total_ar_gen_beds=sum(total_general_bed_count), total_ar_gen_empty_beds=sum(total_general_number_of_empty_beds)) %>%
  mutate(ar_gen_empty_bed_ratio=total_ar_gen_empty_beds/total_ar_gen_beds)
filter(byhospital, !is.na(total_general_bed_count), !is.na(total_general_number_of_empty_beds))%>%
  summarize(., total_us_gen_beds=sum(total_general_bed_count), total_us_gen_empty_beds=sum(total_general_number_of_empty_beds)) %>%
  mutate(us_gen_empty_bed_ratio=total_us_gen_empty_beds/total_us_gen_beds)

#analyze the by-county data
library(dplyr)
ar_counties <- filter(bycounty, state == "AR")
write.csv(ar_counties, "associatedpress_us_hospital_capacity/ar_counties.csv")
#beds per elderly person
bycounty_bed_pop <- mutate(bycounty, bed_1000pop_ratio=((total_general_bed_count+intensive_care_bed_count)/total_population)*1000) %>%
  arrange(-bed_1000pop_ratio)
bycounty_bed_pop_60up <- mutate(bycounty_bed_pop, bed_1000pop60up_ratio=((total_general_number_of_empty_beds+intensive_care_number_of_empty_beds)/population_60_or_older)*1000) %>%
  arrange(-bed_1000pop60up_ratio)
#arkansas
ar_bycounty_bed_pop <- filter(bycounty_bed_pop, state=="AR")
ar_bycounty_bed_pop_60up <- filter(bycounty_bed_pop_60up, state=="AR")

#analyze the by-state data
library(dplyr)
bystate_bedpop <- mutate(bystate, bed_1000pop_ratio=((total_general_bed_count+intensive_care_bed_count)/total_population)*1000) %>%
  arrange(-bed_1000pop_ratio)


#analyze the by rural-urban data