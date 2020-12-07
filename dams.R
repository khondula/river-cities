# update to get infrastructure data

library(tidyverse)
library(glue)
library(sf)
library(colorspace)
library(leaflet)
library(raster)
library(RStoolbox)
library(rosm)
library(dams)

dams <- dams::get_nid(overwrite = FALSE) # 91,457 records
dams <- dams %>%
  dplyr::select(recordid, dam_name, nidid,
                longitude, latitude, county, river, city, distance,
                year_completed,
                owner_type, dam_type, purposes, nid_height)

# filter to just flood control dams (18,304)
fc_dams <- dams %>% 
  dplyr::mutate(floodcontrol = str_detect(purposes, "C"),
         floodcontrol_primary = str_starts(purposes, "C")) %>%
  dplyr::filter(floodcontrol)
rm(dams)
# for visualization
# categorize dam heights
# and identify primary owner type 
fc_dams <- fc_dams %>% 
  mutate(height_category = case_when(nid_height == 0 ~ "A 0ft",
                                     nid_height > 0 & nid_height <= 15 ~ "B <15ft",
                                     nid_height > 15 & nid_height <= 50 ~ "C 15-30ft",
                                     nid_height > 30 & nid_height <= 30 ~ "D 30-50ft",
                                     nid_height > 50 & nid_height <=100 ~ "E 50-100ft",
                                     nid_height > 100 ~ "F >100ft")) %>%
  mutate(owner_type_first = str_sub(owner_type, 1, 1))

# convert to spatial object
# 10 without coordinates
# fc_dams %>% filter(is.na(longitude)) 
fc_dams_sf <- filter(fc_dams, !is.na(longitude)) %>%
                       st_as_sf(coords = c("longitude", "latitude")) %>%
                       st_set_crs(4269)

# metro areas shapefile
cbsa_shp <- "data/tiger/tl_2019_us_cbsa.shp"
cbsa_sf <- sf::st_read(cbsa_shp) %>% dplyr::filter(LSAD == "M1")

# my_geoid <- cbsa_sf$GEOID[101]
# cbsa_sf %>% dplyr::filter(GEOID == cbsa_sf$GEOID[101]) %>% pull(NAME)

# find dams within a metro area and save data and map
# include dams within a 30 km buffer on the map

fc_dams_dir <- 'cbsa-dams-files'
if(!dir.exists(fc_dams_dir)){fs::dir_create(fc_dams_dir)}
fc_dams_map_dir <- 'maps-dams'
if(!dir.exists(fc_dams_map_dir)){fs::dir_create(fc_dams_map_dir)}

save_dams_cbsa_map <- function(my_geoid, savemap = TRUE){
  # subset metro area to 1 city and make a buffer 
  my_cbsa_sf <- cbsa_sf %>% dplyr::filter(GEOID == my_geoid)
  my_cbsa_sf_buffer <- my_cbsa_sf %>% st_transform(5070) %>% 
    st_buffer(3e4) %>% st_transform(st_crs(fc_dams_sf))
  
  my_fc_dams_sf_buff <- fc_dams_sf %>% st_intersection(my_cbsa_sf_buffer)
  my_fc_dams_sf <- fc_dams_sf %>% st_intersection(my_cbsa_sf)
  my_fc_dams_df <- my_fc_dams_sf %>% st_drop_geometry() %>%
    dplyr::select(c(GEOID,NAME,NAMELSAD,LSAD,1:16))

  write_csv(my_fc_dams_df, glue('{fc_dams_dir}/flood-control-dams-in_{my_cbsa_sf$NAME}.csv'))
  
  if(savemap){
  if(nrow(my_fc_dams_sf) > 0){
  myextent <- sp::bbox(as(my_cbsa_sf_buffer, "Spatial"))
  osm1 <- osm.raster(myextent)
  my_cbsa_buff_prj <- sf::st_transform(my_cbsa_sf_buffer, 3857)
  my_cbsa_prj <- sf::st_transform(my_cbsa_sf, 3857)
  my_fc_dams_sf_prj <- sf::st_transform(my_fc_dams_sf_buff, 3857)
  my_fc_dams_sf_prj$lat <- st_coordinates(my_fc_dams_sf_prj) %>% as.data.frame() %>% pull(Y)
  my_fc_dams_sf_prj$lon <- st_coordinates(my_fc_dams_sf_prj) %>% as.data.frame() %>% pull(X)
  
  m1 <- ggRGB(osm1, ggObj = TRUE, r = 1, g = 2, b = 3)
  m2 <- m1 +
    geom_sf(data = my_cbsa_buff_prj, fill = NA) +
    geom_sf(data = my_cbsa_prj, 
            color = "purple",
            fill = "purple", 
            alpha = 0.05) +
    geom_sf(data = my_fc_dams_sf_prj, aes(fill = height_category, shape = owner_type_first), size = 4, alpha = 0.8) +
    theme_minimal() +
    scale_shape_manual(values = c(21:25,8)) +
    scale_fill_discrete_sequential("YlOrRd", rev = TRUE) +
    ggtitle(glue::glue("{my_cbsa_sf$NAME} (n = {nrow(my_fc_dams_sf)} in CBSA area)")) +
    xlab(element_blank()) +
    ylab(element_blank()) +
    coord_sf(xlim = c(st_bbox(my_cbsa_buff_prj)$xmin, st_bbox(my_cbsa_buff_prj)$xmax),
             ylim = c(st_bbox(my_cbsa_buff_prj)$ymin, st_bbox(my_cbsa_buff_prj)$ymax)) +
    guides(fill=guide_legend(override.aes=list(shape=21)))
  

  mapfile <- glue::glue("{fc_dams_map_dir}/flood-control-dams_{my_cbsa_sf$NAME}.pdf")
  ggsave(mapfile, m2, width = 8, height = 6)
  }
  }
}

# if slow internet, just do a few at a time
cbsa_sf$GEOID %>% walk(~save_dams_cbsa_map(.x, savemap = FALSE))

# read in all csvs and combine
fc_dams_cbsa_df <- list.files(fc_dams_dir, full.names = TRUE) %>% 
  map_df(~read_csv(.x, col_types = 'ccccccccccddcccdllcc'))

# summarise n dams and max dam height by cbsa
fc_dams_cbsa_summary <- fc_dams_cbsa_df %>% 
  group_by(GEOID, NAME) %>%
  summarise(n_dams = n(),
            max_dam_height = max(nid_height))

write_csv(fc_dams_cbsa_summary, 'data/flood-control-dams-summary.csv')
write_csv(fc_dams_cbsa_df, 'data/flood-control-dams-cbsa.csv')
