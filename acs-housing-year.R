library(tidycensus)
library(dplyr)
library(rvest)

my_key <- scan("my-census-api-key", "")
census_api_key(my_key)

acs_vars_df <- tidycensus::load_variables(dataset = "acs5", year = "2018")
# get_acs() grants access to 5 year ACS APIs
# https://www.census.gov/data/developers/data-sets/acs-5year.html

cbsa_geograhpy <- "metropolitan statistical area/micropolitan statistical area"

# get variables from web
# vars_url <- "https://api.census.gov/data/2018/acs/acs5/profile/variables.html"
# acs_vars_page <- xml2::read_html(vars_url)
# acs_vars_list <- acs_vars_page %>% 
#   html_nodes("table") %>% 
#   html_table(fill = TRUE) 
# acs_vars_df <- acs_vars_list[[1]][-1,-9]
# acs_vars_df <- acs_vars_df[-c(1:13),] %>% 
#   mutate(variable = substr(Name, 1, nchar(Name)-2)) %>%
#   dplyr::select(Name, Label, Concept, variable)

# use table to get info about specific variable 
acs_vars_df %>%
  dplyr::filter(Name == "B07012_002E") %>%
  dplyr::select(Name, Label, Concept)

# string matching for set of variables
my_vars <- acs_vars_df %>%
  # dplyr::filter(Concept == "SELECTED HOUSING CHARACTERISTICS") %>%
  dplyr::filter(stringr::str_detect(label, "poverty")) %>% # string matching for "Built"
  dplyr::select(name, label, concept)

unique(my_vars$concept)

my_data <- get_acs(geography = cbsa_geograhpy,
                   variables = my_vars[["Name"]],
                   year = 2018, # default
                   survey = "acs5" # default
                   )
# join with var names
my_data %>% 
  left_join(acs_vars_df, by = "variable")


# add in a summary_var	
# Character string of a "summary variable" from the ACS 
# to be included in your output. Usually a variable 
# (e.g. total population) that you'll want to use as 
# a denominator or comparison.

my_data2 <- get_acs(geography = cbsa_geograhpy,
                   variables = my_vars[["Name"]],
                   summary_var = "DP04_0001", # variable without E added
                   year = 2018, # default
                   survey = "acs5" # default
                   )
