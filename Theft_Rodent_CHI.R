#--------------------
# Objective: PPHA 30536 HW2
# Date: 23rd Oct, 2022
#--------------------

# Clear Global Environment
rm(list = ls())
options(
  scipen = 999,
  digits = 3
)
# Setting the Working Directory
setwd("~/Desktop/Fall Quarter/PPHA-30536/Week4/homework-2-weiluj")
# Load packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(spData)
library(scales)

#Q2.Create Choropleths with Chicago Data

#Q2.1.1 Load community shape file
# Load shapefile
community_path <- "~/Desktop/Fall Quarter/PPHA-30536/Week4/homework-2-weiluj/Boundaries_Community_Areas"
community_shape <- st_read(file.path(community_path, "geo_export_adaff510-6473-4c76-8bec-25fd53061673.shp"))
glimpse(community_shape)
# Clean shape dataset
community_shape <- community_shape %>%
  mutate(area_num = as.numeric(area_numbe)) %>% # convert to numeric format
  select(area_num, community, geometry)

#Q2.1.2 Load crime data from 2018 to present
crime <- read_csv("Crimes_2018_to_2022_Sept.csv")
glimpse(crime)
# Subset crime data related with theft
crime_type <- c("THEFT", "ROBBERY", "BURGLARY") # define interested crime type
theft <- crime %>%
  mutate(
    date = as.Date(`Date`, "%m/%d/%Y"), # convert to date format
    area = `Community Area`,
    year = year(date)
  ) %>%
  filter(date >= "2019-02-25" &
    `Primary Type` %in% crime_type) %>% # keep theft-related data only
  select(year, area)

# Merge with community shape data
merged_theft <- inner_join(theft, community_shape,
  by = c("area" = "area_num")
) %>% # join by community code
  group_by(area) %>%
  mutate(total_theft_case = n()) %>% # count total cases by community
  group_by(area, year) %>%
  mutate(annual_theft_case = n()) %>% # count cases by community and year
  distinct(area, year, .keep_all = TRUE) # remove duplicate rows
merged_theft_sf <- st_sf(merged_theft)
# Output and save to csv
write_csv(merged_theft_sf, "merged_theft.csv")
st_write(merged_theft_sf, "merged_theft_sf.shp")

#Q2.1.3 Load 311 service request on rodent complain data from 2018 to present
rodent_complain <-
  read_csv("311_Service_requests-Rodent_Baiting-2019_to_present.csv",
           show_col_types = FALSE)
glimpse(rodent_complain)
# Clean rodent complain data
rodent_complain <- rodent_complain %>%
  mutate(
    date = as.Date(CREATED_DATE, "%m/%d/%Y"), # convert to date format
    year = year(date), # generate year information
    area = COMMUNITY_AREA,
  ) %>%
  filter(DUPLICATE == FALSE & date <= "2022-09-30") %>% # remove repeated request
  select(area, year)
# Merge with community shape data
merged_rodent_complain <- inner_join(rodent_complain, community_shape,
  by = c("area" = "area_num")
) %>%
  group_by(area) %>%
  mutate(total_rodent_case = n()) %>% # count total cases by community
  group_by(area, year) %>%
  mutate(annual_rodent_case = n()) %>% # count cases by community and year
  distinct(area, year, .keep_all = TRUE) # remove duplicated rows
merged_rodent_complain_sf <- st_sf(merged_rodent_complain)
# Output and save to csv
write_csv(merged_rodent_complain, "merged_rodent_complain.csv")
st_write(merged_rodent_complain_sf, "merged_rodent_complain_sf.shp")

# Output combined merged csv
merged_theft_rodent <- inner_join(merged_theft, merged_rodent_complain, by = c("area", "year")) %>%
  select(-ends_with("y")) %>%
  select(ends_with("case"), everything()) %>%
  pivot_longer(total_theft_case:annual_rodent_case,
               names_to = "type",
               values_to = "case") %>%
  mutate(cal_type = ifelse( # mutate a new column detailing calculation method
    grepl("annual", type),
    "annual",
    "total"
  )) %>%
  mutate(type = ifelse( # mutate a new column detailing data type
    grepl("rodent", type),
    "311 service rodent complain",
    "theft"
  )
  )
names(merged_theft_rodent) <- gsub(".x","",names(merged_theft_rodent))

merged_theft_rodent_sf <- st_sf(merged_theft_rodent)
write_csv(merged_theft_rodent, "merged_theft_rodent.csv")
st_write(merged_theft_rodent_sf, "merged_theft_rodent_sf.shp")

# Note: For both data sets, we only keep data from 2018 to present, and since the rodent complain data
# starts from 2018-02-25, to make sure the alignment of date and comparison between the two data sets
# I filtered the crime data set to only keep data after 2018-02-25.
# Crime Data source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present-Dashboard/5cd6-ry5g
# 311 Rodent Complain Data source: https://dev.socrata.com/foundry/data.cityofchicago.org/28me-84fj


# Q2.2 Choropleth MAP

# Q2.2.1 Theft Crime Plot
theft_plt <- ggplot() +
  geom_sf(data = merged_theft_sf, aes(fill = total_theft_case)) +
  labs(
    title = "Accumulative Theft Crime Cases in Chicago",
    fill = element_blank(),
    caption = "Source: City of Chicago; Date range from 2019 to 2022"
  ) +
  scale_fill_distiller(
    palette = "RdPu",
    direction = 1
  ) + # darker color represents higher value
  theme_void() +
  theme(
    plot.title = element_text(
      size = 14, face = "bold",
      vjust = -0.5
    ),
    legend.text = element_text(size = 8),
    plot.caption = element_text(
      size = 6, face = "italic",
      vjust = 10, hjust = 1.5
    )
  )
ggplotly(theft_plt)

#Q2.2.2 Rodent Complain 311 Request Plot
rodent_plt <- ggplot() +
  geom_sf(data = merged_rodent_complain_sf, aes(fill = total_rodent_case)) +
  labs(
    title = "Accumulative Rodent Complain to Chicago 311 Serive",
    fill = element_blank(),
    caption = "Source: City of Chicago; Date range from 2019 to 2022"
  ) +
  scale_fill_distiller(
    palette = "RdPu",
    direction = 1
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 12, face = "bold",
      vjust = -1
    ),
    legend.text = element_text(size = 8),
    plot.caption = element_text(
      size = 6, face = "italic",
      vjust = 10, hjust = 1.5
    )
  )
  
ggplotly(rodent_plt)

#--------------------
# Answer: For the crime data, we can research on whether theft cases (burglary and robbery included)
# are concentrated in specific areas, especially areas with strong socioeconomic traits
# and further discover whether theft cases are correlated to wealth, transportation etc.
# For example, we can see from the plot that most theft cases occur around the loop and other downtown areas.
# For the rodent report data, we noticed a different distribution pattern among areas
# with cases converged in the north side but also in the west side. More research could be done to identify
# whether it's because people living in the north are more sensitive to rodent/aware of possible risks/311 service
# or it's because the north/west side do have more rodent than other areas
# and analysis of rodent complain cases by different season would also be interesting.
  
