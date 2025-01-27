# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  VLS calcs
# REF ID:   4d93fb4d 
# LICENSE:  MIT
# DATE:     2025-01-21
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(gagglr)
library(tidyverse)


# GLOBAL VARIABLES --------------------------------------------------------

# SI specific paths/functions  
load_secrets()

# Grab metadata
get_metadata(filepath) 

ref_id <- "4d93fb4d"

# IMPORT ------------------------------------------------------------------
#function to calculate age
calc_client_age <- function(birth_date, ref_date = Sys.Date()) {
  
  # Calculate the difference in years
  age <- as.numeric(difftime(ref_date, birth_date, units = "weeks")) %/% 52.5
  
  return(age)
  
}

#read in VL data
df_vl <- data_folder %>% 
  return_latest("Viral Load") %>% 
  readxl::read_excel(skip =4, col_names = T) 

#tidy 
df_vl_tidy <- df_vl %>% 
  mutate(year = 2024) %>% 
  mutate(Age = ifelse(Age == "U/K", NA, Age),
         Age = ifelse(Age == "`", NA, Age)) %>% 
  mutate(Age = as.numeric(Age)) %>% 
 # count(Age) %>% pull(Age)
  mutate(age_band = ifelse(Age <= 14, "0-14", "15+")) %>% 
  mutate(grp = case_when(age_band == "15+" & Sex == "F" ~ "Female 15+",
                         age_band == "15+" & Sex == "M" ~ "Male 15+",
                         age_band == "0-14" ~ "Children 0-14",
                         TRUE ~ "Unknown")) 


#address unknowns
df_vl_unknown <- df_vl_tidy %>% 
  #filter(age_band == "15+" & is.na(Sex))
  filter(grp == "Unknown") %>% 
  #count(grp, CODE, Sex, age_band, `Viral Load Count`) %>% 
  mutate(Sex = case_when(str_detect(CODE, "M") & is.na(Sex) ~ "M",
                         is.na(Sex) ~ "U/K",
                         TRUE ~ Sex)) %>% 
  mutate(date = str_sub(CODE, -6)) %>% 
  mutate(
    birthdate = ymd(ifelse(
      as.numeric(substr(date, 1, 2)) >= 50,
      paste0("19", date),  # Assume 1900s for years >= 50
      paste0("20", date)   # Assume 2000s for years < 50
    ))
  ) %>% 
  mutate(age = calc_client_age(birthdate, ref_date = "2024-12-01")) %>% 
  mutate(age_band = case_when(is.na(age_band) & age < 15 ~ "0-14",
                              is.na(age_band) & age >= 15 ~ "15+",
                              TRUE ~ age_band)) %>% 
  mutate(grp = case_when(age_band == "15+" & Sex == "F" ~ "Female 15+",
                         age_band == "15+" & Sex == "M" ~ "Male 15+",
                         age_band == "0-14" ~ "Children 0-14",
                         TRUE ~ grp)) 
  

df_vl %>% 
  count(`Viral Load Count`)

# quick count - 8 unknown age and sex
df_vl_tidy %>% 
  filter(grp != "Unknown") %>% 
  bind_rows(df_vl_unknown) %>%
  count(grp, `Viral Load Count`) %>% 
  filter(`Viral Load Count` == "<=1000")

df_vl_tidy %>% 
  filter(grp != "Unknown") %>% 
  bind_rows(df_vl_unknown) %>%
  count(grp)
