# map
library(leaflet)
library(sf)
library(glue)
library(dplyr)

# make leaflet map showing census tract overlaid with census places
# reads files from public data directory

my_tract <- "01001020100" # example tract

leaflet_tract_map <- function(my_tract){
  
  state_fips <- substr(my_tract, 1, 2)
  # read in tracts spatial data and filter to tract
  tracts_dir <- "/nfs/public-data/census-tiger-2019/TRACT"
  tract_file <- glue::glue("{tracts_dir}/tl_2019_{state_fips}_tract.shp")
  tract_sf <- sf::st_read(tract_file) %>%
    dplyr::filter(GEOID == my_tract)
  
  # read in urban areas spatial data for nation
  # urban_dir <- "/nfs/public-data/census-tiger-2019/UAC/"
  # urban_file <- glue::glue("{urban_dir}/tl_2019_us_uac10.shp")
  # urban_sf <- sf::st_read(urban_file)
  
  data_dir <- "/nfs/khondula-data/projects/river-cities/data"
  places_lookup_dir <- glue("{data_dir}/census-lookups/tract-x-places")
  
  tract_places <- read_csv(glue("{places_lookup_dir}/tract_{my_tract}.csv"),
                           col_types = cols(.default = col_character()))
  place_GEOID <- tract_places$GEOID %>% unique()
  
  # read in places spatial data for state
  places_dir <- "/nfs/public-data/census-tiger-2019/PLACES"
  place_file <- glue::glue("{places_dir}/tl_2019_{state_fips}_place.shp")
  place_sf <- sf::st_read(place_file) %>%
    dplyr::filter(GEOID %in% place_GEOID)
  
  tract_bbox <- tract_sf %>% st_bbox()
  
  mm <- tract_sf %>% 
    leaflet() %>%
    fitBounds(lng1 = tract_bbox[["xmin"]],
              lng2 = tract_bbox[["xmax"]],
              lat1 = tract_bbox[["ymin"]],
              lat2 = tract_bbox[["ymax"]]) %>%
    addTiles() %>%
    # addPolygons(data = urban_sf, fillColor = "red", 
    #             opacity = 0,
    #             popup = ~UACE10, group = "uac") %>%
    addPolygons(opacity = 1, group = "tract") %>%
    addPolygons(data = place_sf, color = "purple", 
                popup = ~NAME, group = "places") %>%
    addLayersControl(overlayGroups = c("tract", "places"))
  
  return(mm)
}

leaflet_tract_map(my_tract)
