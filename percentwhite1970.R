# 1970 population demographics for cities
# source: Source: https://www.nhgis.org/about

library(tidyverse)
library(readxl)
library(sf)

# total population and white persons count for counties in 1970
pop_file <- 'data/nhgis/nhgis0009_ts_nominal_county.csv'
pop_df <- read_csv(pop_file, col_types = 'cdcccccccdddddd')
pop_df <- pop_df %>% 
  dplyr::filter(YEAR == '1970')

# crosswalk between counties and 2019 L1 (metro) cbsas
# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html

cbsa <- readxl::read_excel('data/reference-tables/list1_2020.xls', skip = 2) %>%
  rename(geoid = 1, cbsa_name = 4, county = 8, state = 9, statefips = 10, countyfips = 11) %>%
  dplyr::select(geoid, cbsa_name, county, state, statefips, countyfips)
cbsa_shp <- "data/tiger/tl_2019_us_cbsa.shp"
cbsa_sf <- sf::st_read(cbsa_shp) %>% dplyr::filter(LSAD == "M1")
cbsa_metro <- filter(cbsa, geoid %in% cbsa_sf$GEOID)

# join 1970 population data by county
# filter to counties within CBSAs
pop_join <- pop_df %>% 
  left_join(cbsa_metro, by = c("STATEFP" = "statefips", "COUNTYFP" = "countyfips")) %>%
  filter(!is.na(geoid))

# get dc value from places scale data
places_colnames <- read_csv('data/nhgis/nhgis0007_ts_nominal_place.csv', n_max = 1)
places_df <- read_csv('data/nhgis/nhgis0007_ts_nominal_place.csv', skip = 2, col_names = colnames(places_colnames))
dc_b18aa <- dplyr::filter(tracts_df, YEAR == 1970) %>% filter(STATE == "District Of Columbia") %>% pull(B18AA)

pop_join <- pop_join %>% mutate(B18AA = case_when(STATE == "District Of Columbia" ~ dc_b18aa, TRUE ~ B18AA))

# summarize total population and white by CBSA
# calculate percent white
cbsa1970 <- pop_join %>% 
  group_by(geoid, cbsa_name) %>%
  summarise(total1970 = sum(A00AA),
            totalwhite = sum(B18AA)) %>%
  mutate(percent_white = totalwhite/total1970)


cbsa1970 %>% write_csv('data/cbsa1970-percentwhite.csv')
cbsa_sf$NAME[!cbsa_sf$NAME %in% cbsa1970$cbsa_name]
