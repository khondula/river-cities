# River city population data
library(fs)
library(glue)
library(dplyr)
library(tidyr)
library(magrittr)

# location to download data
data_dir <- "data"
if(!fs::dir_exists(data_dir)){fs::dir_create(data_dir)}

## Census City and Town Intercensal Data
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/intercensal/cities/sub-est00int.pdf
# methodology https://www2.census.gov/programs-surveys/popest/technical-documentation/methodology/intercensal/2000-2010-intercensal-estimates-methodology.pdf

# download data
citytown_filepath <- "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/cities/sub-est00int.csv"
citytown_file_local <- glue::glue("{data_dir}/{basename(citytown_filepath)}")
download.file(citytown_filepath, destfile = citytown_file_local)

# read in
citytown_df <- readr::read_csv(citytown_file_local)

# SUMLEV == 040 states
# SUMLEV == 050 counties
# SUMLEV == 061 minor civil division
# SUMLEV == 071 minor civil division place part
# SUMLEV == 157 county place part
# SUMLEV == 162 incorporated place

# filter and convert from wide to long
citytown_long <- citytown_df %>%
  # filter out state and county level values
  filter(SUMLEV %in% c("061", "071", "157", "162")) %>%
  # keep grouping levels
  group_by(SUMLEV, STATE, COUNTY, PLACE, COUSUB, NAME, STNAME) %>%
  # convert columns to rows
  tidyr::pivot_longer(cols = ESTIMATESBASE2000:POPESTIMATE2010, 
                      names_to = "estimate", values_to = "population") %>%
  # separate year and estimate type into separate columns 
  # extract years as a the numbers in the estimate column
  mutate(year = stringr::str_extract(estimate, "\\d+")) %>%
  mutate(year = as.numeric(year)) %>%
  # extract letters
  mutate(estimate_type = stringr::str_extract(estimate, "[A-Z]+")) %>%
  dplyr::select(-estimate)
  
citytown_long %>% readr::write_csv("Subcounty_Intercensal_Pop_2000-2010.csv")

# 2010-2018 data
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2018/sub-est2018.pdf

# download data
citytown_filepath2 <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/cities/totals/sub-est2018_all.csv"
citytown_file2_local <- glue::glue("{data_dir}/{basename(citytown_filepath2)}")
download.file(citytown_filepath2, destfile = citytown_file2_local)

# read in
citytown2_df <- readr::read_csv(citytown_file2_local, col_types = "ccccccccccddddddddddd")

# The key for SUMLEV is as follows:
# 040 = State
# 050 = County
# 061 = Minor Civil Division
# 071 = Minor Civil Division place part
# 157 = County place part
# 162 = Incorporated place
# 170 = Consolidated city
# 172 = Consolidated city -- place within consolidated city

# filter and convert from wide to long
citytown2_long <- citytown2_df %>% 
  # filter out state and county level values
  filter(SUMLEV %in% c("061", "071", "157", "162", "170", "172")) %>%
  dplyr::select(-PRIMGEO_FLAG, -FUNCSTAT) %>% # dont think these are necessary
  # keep grouping levels
  group_by(SUMLEV, STATE, COUNTY, PLACE, COUSUB, CONCIT, NAME, STNAME) %>%
  # convert columns to rows
  tidyr::pivot_longer(cols = CENSUS2010POP:POPESTIMATE2018, 
                      names_to = "estimate", values_to = "population") %>%
  # separate year and estimate type into separate columns 
  # extract years as a the numbers in the estimate column
  mutate(year = stringr::str_extract(estimate, "\\d+")) %>%
  mutate(year = as.numeric(year)) %>%
  # extract letters
  mutate(estimate_type = stringr::str_extract(estimate, "[A-Z]+")) %>%
  dplyr::select(-estimate)

citytown2_long %>% readr::write_csv("Subcounty_Intercensal_Pop_2010-2018.csv")

# 1990 to 2000 data is here in worse format: 
# https://www.census.gov/data/tables/time-series/demo/popest/2000-subcounties-eval-estimates.html

# combine files
citytown_long %<>% mutate(CONCIT = NA, SUMLEV = as.character(SUMLEV))
citytown_all <- dplyr::bind_rows(citytown_long, citytown2_long)
readr::write_csv(citytown_all, "Subcounty_Intercensal_Pop_2000-2018.csv")
