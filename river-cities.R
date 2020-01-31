library(sf)
library(maps)
library(dataRetrieval)
library(glue)
library(readr)

# The maps package has a dataset called `us.cities` that had 1005 US cities with 
# population of greater than approx 40,000 and state capitals regardless of size

us_cities_sf <- us.cities %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

my_city_sf <- us_cities_sf[11, ]

#######################################
get_usgs_sites <- function(my_city_sf){
  
  my_city_name <- my_city_sf$name
  my_city_pop <- my_city_sf$pop

  # make 5km bounding box around city coordinate
  mybbox <- my_city_sf %>% 
    st_transform(crs = 2163) %>%
    st_buffer(dist = 5000) %>% 
    st_transform(crs = 4326) %>%
    st_bbox(my_city_sf) %>% 
    as.vector() %>% round(digits = 2)
  
  # look for usgs sites with discharge within bbox
  city_sites <- tryCatch({
    dataRetrieval::whatNWISsites(parameterCd = "00060", 
                                             bBox = mybbox)},
    error = function(e) {return(NULL)})
  if(!is.null(city_sites)){
  
  # map
  # city_sites %>%
  #   st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326) %>%
  #   leaflet() %>%
  #   addTiles() %>%
  #   addMarkers()
  
  # get info about sites
  city_sites_info <- readNWISsite(city_sites[["site_no"]]) %>%
    dplyr::select(site_no, state_cd, drain_area_va)
  
  # get info about record length
  site_data_1980 <- whatNWISdata(parameterCd = "00060", 
                                 siteNumber = city_sites[["site_no"]]) %>%
    arrange(begin_date) %>%
    mutate(early_enough = begin_date < "1980-01-01") %>%
    dplyr::filter(early_enough, count_nu > 10)
  
  df <- site_data_1980 %>%
    dplyr::select(agency_cd, site_no, station_nm, dec_lat_va, dec_long_va,
                  huc_cd, data_type_cd, parm_cd, stat_cd, begin_date, end_date,
                  count_nu, early_enough) %>%
    left_join(city_sites_info) %>%
    mutate(city = my_city_name, city_pop = my_city_pop) %>%
    arrange(site_no)
  
  readr::write_csv(df, path = glue("data/{my_city_name}.csv"))
  # return(df)
  }
}
#################################

# get_usgs_sites(us_cities_sf[3,])

purrr::walk(1:nrow(us_cities_sf), ~get_usgs_sites(us_cities_sf[.x,]))
