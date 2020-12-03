# 1970 population demographics for cities
# source: Source: https://www.nhgis.org/about

library(tidyverse)
library(readxl)
library(sf)

# total population and white persons count for counties in 1970
pop_file <- 'census-data/nhgis0009_csv/nhgis0009_ts_nominal_county.csv'
pop_df <- read_csv(pop_file, col_types = 'cdcccccccdddddd')
pop_df <- pop_df %>% 
  dplyr::filter(YEAR == '1970')

# crosswalk between counties and 2019 L1 (metro) cbsas
# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html

cbsa <- readxl::read_excel('list1_2020.xls', skip = 2) %>%
  rename(geoid = 1, cbsa_name = 4, county = 8, state = 9, statefips = 10, countyfips = 11) %>%
  dplyr::select(geoid, cbsa_name, county, state, statefips, countyfips)
cbsa_shp <- "river-cities-data/tl_2019_us_cbsa.shp"
cbsa_sf <- sf::st_read(cbsa_shp) %>% dplyr::filter(LSAD == "M1")
cbsa_metro <- filter(cbsa, geoid %in% cbsa_sf$GEOID)

# join 1970 population data by county
# filter to counties within CBSAs
pop_join <- pop_df %>% 
  left_join(cbsa_metro, by = c("STATEFP" = "statefips", "COUNTYFP" = "countyfips")) %>%
  filter(!is.na(geoid))

# summarize total population and white by CBSA
# calculate percent white
cbsa1970 <- pop_join %>% 
  group_by(geoid, cbsa_name) %>%
  summarise(total1970 = sum(A00AA),
            totalwhite = sum(B18AA)) %>%
  mutate(percent_white = totalwhite/total1970)

cbsa1970 %>% write_csv('cbsa1970-percentwhite.csv')
