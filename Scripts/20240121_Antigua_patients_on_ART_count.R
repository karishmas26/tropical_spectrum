# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  
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
  
data_folder <- "Data"
  
df <- data_folder %>% 
  return_latest("OECS ART Report 2024 to send") %>% 
  readxl::read_excel(skip = 9)

df_2023 <- data_folder %>% 
  return_latest("OECS ART Report  2023") %>% 
  readxl::read_excel(skip = 9)

# recode sex
df_recode <- df_2023 %>%
  rename(client_code = `...1`,
         age = `...4`,
         date_started = `...11`,
         current_regimen = `...12`,
         qnt_arv_supplied = `...18`) %>% 
  mutate(unknown_sex = ifelse(is.na(Male) & is.na(Female), "*", NA)) %>% 
  pivot_longer(cols = c(Male,Female, unknown_sex), names_to = "Sex") %>% 
  relocate(Sex, .after = client_code) %>% 
  relocate(value, .after = Sex) %>% 
  filter(!is.na(value)) %>% 
  #mutate(Sex = ifelse(client_code == "CJMDF660827", "Female", Sex)) %>% 
  mutate(age = case_when(
                        age == "2 MONTHS" ~ "0",
                         age == "8 MONTHS" ~ "0",
                         age == "11 months" ~ "0",
                         age == "BABY" ~ "0",
                         age == "29*" ~ "29",
                         TRUE ~ age)) %>% 
  #mutate(age = str_remove(age, "*")) %>% 
  mutate(age = as.numeric(age))

#recode 
df_recode <- df_recode %>% 
  select(-value) %>% 
  pivot_longer(cols = c(New:Exist), names_to = "art_status") %>% 
  relocate(art_status, .after = age) %>% 
  relocate(value, .after = art_status) %>% 
  filter(!is.na(value)) 

df_final <- df_recode %>% 
  mutate(
    age_band = case_when(
      age >= 0 & age <= 4 ~ "0-4",
      age >= 5 & age <= 9 ~ "5-9",
      age >= 10 & age <= 14 ~ "10-14",
      age >= 15 & age <= 19 ~ "15-19",
      age >= 20 & age <= 24 ~ "20-24",
      age >= 25 & age <= 29 ~ "25-29",
      age >= 30 & age <= 34 ~ "30-34",
      age >= 35 & age <= 39 ~ "35-39",
      age >= 40 & age <= 44 ~ "40-44",
      age >= 45 & age <= 49 ~ "45-49",
      age >= 50 & age <= 54 ~ "50-54",
      age >= 55 & age <= 59 ~ "55-59",
      age >= 60 & age <= 64 ~ "60-64",
      age >= 65 & age <= 69 ~ "65-69",
      age >= 70 & age <= 74 ~ "70-74",
      age >= 75 & age <= 79 ~ "75-79",
      age >= 80 ~ "80+",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(
    gam_age_band = case_when(
      age >= 0 & age <= 4 ~ "0-4",
      age >= 5 & age <= 9 ~ "5-9",
      age >= 10 & age <= 14 ~ "10-14",
      age >= 15 & age <= 19 ~ "15-19",
      age >= 20 & age <= 24 ~ "20-24",
      age >= 25 & age <= 49 ~ "25-49",
      age >= 50 ~ "50+",
      TRUE ~ NA_character_
    )
  ) 

df_final %>% 
  select(client_code, Sex, age, art_status, contains("age_band")) %>% 
  count(Sex, age_band)



# MUNGE -------------------------------------------------------------------
