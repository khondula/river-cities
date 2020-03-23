library(dplyr)
library(data.table)
library(glue)
library(readr)
library(vroom)
library(leaflet)
library(sf)

data_dir <- "/nfs/khondula-data/projects/river-cities/data"
places_lookup_dir <- glue("{data_dir}/census-lookups/tract-x-places")
urban_lookup_dir <- glue("{data_dir}/census-lookups/tract-x-urban")

claims_tracts11 <- read_csv(glue::glue("{data_dir}/NFIP/claims_tracts11.csv"))
claims_data_file <- glue("{data_dir}/NFIP/openFEMA_claims20190831.csv")

head(claims_tracts11)

my_tract <- claims_tracts11[["tract"]][500]
my_tract

tract_places <- read_csv(glue("{places_lookup_dir}/tract_{my_tract}.csv"),
                         col_types = cols(.default = col_character()))
# tract_urbans <- read_csv(glue("{urban_lookup_dir}/tract_{my_tract}.csv"))

claims_df_tract <- read_csv(claims_data_file, 
                            col_types = cols(.default = col_character())) %>%
  dplyr::filter(censustract == my_tract)


nrow_tract_places <- nrow(tract_places)
place_name <- tract_places$NAME
place_GEOID <- tract_places$GEOID
claims_names <- claims_df_tract$reportedcity %>% unique()

message(nrow(claims_df_tract), " claims for census track ", my_tract)
message(nrow_tract_places, " place matches")
writeLines(place_name)
message(length(claims_names), " reported city names in claims")
writeLines(claims_names)

# Multiple place names and multiple reported cities

place_name_matches <- purrr::set_names(place_name) %>%
  purrr::map(~agrep(.x, claims_names,
                    ignore.case = TRUE,
                     value = TRUE,
                     max.distance = 0.25)) %>%
  purrr::map_df(~as_tibble(.x), .id = "place_name") %>%
  left_join(dplyr::select(tract_places, GEOID, NAME), 
            by = c("place_name" = "NAME"))

claims_df_tract_matches <- claims_df_tract %>%
  dplyr::select(censustract, reportedcity, countycode) %>%
  distinct() %>% 
  left_join(place_name_matches, by = c("reportedcity" = "value"))

claims_df_tract_matches

# map
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
  
  # read in places spatial data for state
  places_dir <- "/nfs/public-data/census-tiger-2019/PLACES"
  place_file <- glue::glue("{places_dir}/tl_2019_{state_fips}_place.shp")
  place_sf <- sf::st_read(place_file) %>%
    dplyr::filter(GEOID %in% place_GEOID)
  
  tract_bbox <- tract_sf %>%st_bbox()
  
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
