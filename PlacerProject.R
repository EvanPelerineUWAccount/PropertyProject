#Purpose; R-Project For Property 1
#Evan Pelerine
#3/3/2025

#installs packages
packages = c("tidyverse", "janitor","ggplot", "tigris","sf","mapview")
install.packages(packages)
library(tidyverse)
library(janitor)
library(ggplot2)
library(tigris)
library(sf)
library(mapview)

data <- read.csv("/Users/evanpelerine/Desktop/R-Folder/Visitors By Origin Law zipcode.csv")
View(data)

clean_data <- data %>% clean_names() %>% 
  mutate(p_visits = as.numeric(x_of_visits)/100,
         visits = as.numeric(visits)) %>%
  select(zipcode, city, state, p_visits, visits)

#counts by state
states_count <- clean_data %>% 
  filter(is.na(visits)==FALSE & visits >= 10) %>%
  group_by(state) %>%
  summarise(p_visits = sum(p_visits), visits = sum(visits)) %>%
  arrange(desc(visits))

#makes dumb chart from state count
states_count %>% ggplot(aes(x=state,y=p_visits))+
  geom_col()

#adds zipcode shape files
zip_list <- clean_data%>% filter(state == "WA") %>%
  mutate(zipcode = as.character(zipcode)) %>%
  select(zipcode) %>% pull()
zip_sf <- zctas(year = 2022, starts_with = zip_list) %>%
  mutate(zipcode = as.numeric(ZCTA5CE20)) %>%
  select(zipcode)

#joins shape files to clean_data
placer_zip <- clean_data%>%
  left_join(zip_sf, by="zipcode") %>%
  st_sf()

#builds map
Visits = placer_zip %>% select(p_visits) %>% na.omit()
mapview(Visits, col.regions = colorRampPalette(c("#ADD8E6", "#4B0082")), alpha = .9)

