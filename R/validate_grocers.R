# functions for validating grocer data

library(tidyverse)
library(onsr)


# load old data and geocode it
grocers_old <- read_csv("data/ONS Food Environment Mega Data - Food Env-edit.csv") %>%
  filter(Category2 == "grocery") %>%
  onsr::geocode_ottawa(var = "address",
                       verbose= TRUE) %>%
  select(name_old = Name,
         address_old = address,
         category_old = Category2,
         size_old = Category3,
         lat,
         lng)

#targets::tar_load(grocers_all)


# load new data and geocode it
grocers_all_handedits <- read_csv("data/grocers_all_handedits.csv")

grocers_new <- grocers_all_handedits %>%
  onsr::geocode_ottawa(var = "address") %>%
  mutate(category = "grocery") %>%
  select(name_new = name,
         address_new = address,
         address2_new = address2,
         category_new = category,
         size_new = size,
         lat, 
         lng)

# match old and new data by lat/lng
grocers_combined <- left_join(grocers_old, grocers_new, by = c("lat", "lng")) %>%
  drop_na(lat, lng) %>%
  distinct()


# find exact lat/lng matches: check to make sure they're sane
grocers_matched <- grocers_combined %>%
  drop_na(name_new)

# find old data that has no match: check to see what happened
# did it close? or did we miss it in our data collection?
grocers_old_unmatched <- grocers_combined %>%
  filter(is.na(name_new)) %>%
  select(name_old, address_old, category_old, size_old, lat, lng)

# new grocers with no match in old data--probably no extra validation needed, just
# new ones we found
grocers_new_unmatched <- left_join(grocers_new, grocers_old,  by = c("lat", "lng")) %>%
  drop_na(lat, lng) %>%
  distinct() %>%
  filter(is.na(name_old))


# what about the ones that the City of Ottawa's geocoding service couldn't find?
# confirmed by visual inspection that the ungeocodable old and new results
# are outside of ottawa (Smiths Falls, Renfrew, Prescott, etc.)
nogeocode_new <- grocers_new %>%
  filter(is.na(lat))

nogeocode_old <- grocers_old %>%
  filter(is.na(lat))


grocers_matched %>%
  write_csv("data/for_validation/grocers_old_matched.csv")

grocers_old_unmatched %>%
  write_csv("data/for_validation/grocers_old_unmatched.csv")

grocers_new_unmatched %>%
  write_csv("data/for_validation/grocers_new_unmatched.csv")
