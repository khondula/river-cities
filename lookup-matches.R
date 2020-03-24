library(dplyr)
library(glue)
library(purrr)
library(readr)
library(tibble)
library(vroom)
library(leaflet)
library(sf)

data_dir <- "/nfs/khondula-data/projects/river-cities/data"
claims_tracts11 <- read_csv(glue::glue("{data_dir}/NFIP/claims_tracts11.csv"))
my_tract <- claims_tracts11[["tract"]][40]
my_tract


find_tractname_matches <- function(my_tract, agrep_dist = 0.35){
  
  data_dir <- "/nfs/khondula-data/projects/river-cities/data"
  claims_data_file <- glue("{data_dir}/NFIP/openFEMA_claims20190831.csv")
  
  places_lookup_dir <- glue("{data_dir}/census-lookups/tract-x-places")
  
  tract_places <- read_csv(glue("{places_lookup_dir}/tract_{my_tract}.csv"),
                           col_types = cols(.default = col_character()))

  if(nrow(tract_places) > 0){ # if there are places tract overlaps
    
  claims_df_tract <- vroom(claims_data_file, 
                              col_types = cols(.default = col_character())) %>%
    dplyr::filter(censustract == my_tract)
  
  place_name <- tract_places$NAME
  claims_names <- claims_df_tract$reportedcity %>% unique()

  # Create table of matches!! 
  place_name_matches <- purrr::set_names(place_name) %>%
    purrr::map(~agrep(.x, claims_names, ignore.case = TRUE,
                      value = TRUE, max.distance = agrep_dist)) %>%
    purrr::map_df(~as_tibble(.x), .id = "place_name") %>%
    left_join(dplyr::select(tract_places, GEOID, NAME), 
              by = c("place_name" = "NAME"))
  
  claims_df_tract_matches <- claims_df_tract %>%
    dplyr::select(censustract, reportedcity, countycode) %>%
    distinct() %>% 
    left_join(place_name_matches, by = c("reportedcity" = "value"))
  
  filepath1 <- glue::glue("{data_dir}/census-lookups/names-matching/tract_{my_tract}.csv")
  claims_df_tract_matches %>% readr::write_csv(filepath1)
  }
}

find_tractname_matches(my_tract)

claims_tracts11[["tract"]][1:10] %>% 
  purrr::walk(~find_tractname_matches(.x, agrep_dist = 0.35))

names_matching_dir <- glue::glue("{data_dir}/census-lookups/names-matching")
fs::dir_ls(names_matching_dir) %>% length()
  
fs::dir_ls(names_matching_dir) %>% 
  map_df(~read_csv(.x, col_types = c("ccccc"))) %>% View()

library(rslurm)
pars <- data.frame(my_tract = claims_tracts11[["tract"]],
                   agrep_dist = 0.35,
                   stringsAsFactors = FALSE)

sjob <- slurm_apply(find_tractname_matches, 
                    pars, 
                    jobname = 'tractnames',
                    # slurm_options = list(partition = "sesync"),
                    nodes = 20, 
                    cpus_per_node = 4,
                    submit = TRUE)

fs::dir_ls(names_matching_dir) %>% length()

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
