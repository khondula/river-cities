# update to get net migration variables for census CBSAs

# data available for 2010-2019 
# other factors to compare for figure 3
# to describe how sample of cities that met calibration
# differ or not from rest of cities

library(tidycensus)
library(tidyverse)
my_key <- scan("my-census-api-key", "")
census_api_key(my_key)
cbsa_geograhpy <- "metropolitan statistical area/micropolitan statistical area"

# The Population Estimates API is not available 
# in tidycensus for years prior to 2015
cbsa_components <- get_estimates(geography = cbsa_geograhpy, 
                                 year = 2019,
                                 time_series = TRUE,
                                 product = "components")


# ?get_estimates
# PERIOD column is 1-10
# https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2018.html
# PERIOD_CODE: Period of Change
# 1 = April 1, 2010 to June 30, 2010
# 2 = July 1, 2010 to June 30, 2011
# 3 = July 1, 2011 to June 30, 2012
# 4 = July 1, 2012 to June 30, 2013
# 5 = July 1, 2013 to June 30, 2014
# 6 = July 1, 2014 to June 30, 2015
# 7 = July 1, 2015 to June 30, 2016
# 8 = July 1, 2016 to June 30, 2017
# 9 = July 1, 2017 to June 30, 2018
# 10 = July 1, 2018 to June 30, 2019

periods <- data.frame(PERIOD = 1:10,
                      period_year = 2010:2019)

head(cbsa_components, 10)
unique(cbsa_components$variable)
# numbers for births, deaths, domestic migration,
# intl migration, natural increase (births minus deaths), 
# net migration
# definitions: https://www.census.gov/programs-surveys/popest/about/glossary.html
# Rates are expressed per 1,000 population.

# notes from documentation
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/cbsa-est2019-alldata.pdf
# Total population change includes a residual. This residual represents the change in population that cannot
# be attributed to any specific demographic component. See Population Estimates Terms and Definitions at
# http://www.census.gov/programs-surveys/popest/about/glossary.html.

# Net international migration in the United States includes the international migration of both native and
# foreign-born populations. Specifically, it includes: (a) the net international migration of the foreign born,
# (b) the net migration between the United States and Puerto Rico, (c) the net migration of natives to and
# from the United States, and (d) the net movement of the Armed Forces population between the United
# States and overseas.

# filter to just M1 metro areas (not micro)
# and join with dates for periods
cbsa_components <- cbsa_components %>% 
  filter(GEOID %in% cbsa_sf$GEOID) %>%
  left_join(periods)

cbsa_components %>% write_csv('cbsa-change-components.csv')

unique(cbsa_components$NAME) %>% length()

cbsa_components %>% 
  group_by(variable, period_year) %>%
  summarise(value_mean = mean(value, na.rm = TRUE),
            lwr = mean(value) - sd(value),
            upr = mean(value) + sd(value)) %>%
  ggplot(aes(x = period_year, y = value_mean)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) +
  geom_line(col = "red") +
  facet_wrap(vars(variable), scales = "free_y") +
  theme_bw() +
  ylab("mean +/- sd") +
  scale_x_continuous(labels=scales::number_format(accuracy = 1)) +
  ggtitle("Components of population change in 384 Metropolitan areas")

ggsave('change-timeseries.pdf', width = 10, height = 6)
