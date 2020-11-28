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
cbsa_shp <- "river-cities-data/tl_2019_us_cbsa.shp"
cbsa_sf <- sf::st_read(cbsa_shp) %>% dplyr::filter(LSAD == "M1")

my_geoid <- cbsa_sf$GEOID[101]
# cbsa_sf %>% dplyr::filter(GEOID == cbsa_sf$GEOID[101]) %>% pull(NAME)

# find dams within a metro area and save data and map
# include dams within a 30 km buffer on the map

save_dams_cbsa_map <- function(my_geoid, savemap = TRUE){
  # subset metro area to 1 city and make a buffer 
  my_cbsa_sf <- cbsa_sf %>% dplyr::filter(GEOID == my_geoid)
  my_cbsa_sf_buffer <- my_cbsa_sf %>% st_transform(5070) %>% 
    st_buffer(3e4) %>% st_transform(st_crs(fc_dams_sf))
  
  my_fc_dams_sf_buff <- fc_dams_sf %>% st_intersection(my_cbsa_sf_buffer)
  my_fc_dams_sf <- fc_dams_sf %>% st_intersection(my_cbsa_sf)
  my_fc_dams_df <- my_fc_dams_sf %>% st_drop_geometry() %>%
    dplyr::select(c(GEOID,NAME,NAMELSAD,LSAD,1:16))
  write_csv(my_fc_dams_df, glue('cbsa-dams-files/flood-control-dams-in_{my_cbsa_sf$NAME}.csv'))
  
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
  
  mapfile <- glue::glue("maps-dams/flood-control-dams_{my_cbsa_sf$NAME}.pdf")
  ggsave(mapfile, m2, width = 8, height = 6)
  }
  }
}

save_dams_cbsa_map(cbsa_sf$GEOID[142]) # stopped at.. with invalid geometry
save_dams_cbsa_map(cbsa_sf$GEOID[222]) # stopped at.. cant open file
save_dams_cbsa_map(cbsa_sf$GEOID[231]) # stopped at.. cant open file

# slow internet, just do a few at a time
# cbsa_sf$GEOID[1:10] %>% walk(~save_dams_cbsa_map(.x, savemap = FALSE))
# cbsa_sf$GEOID[11:30] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[31:60] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[61:90] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[91:100] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[101:130] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[131:141] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[143:160] %>% walk(~save_dams_cbsa_map(.x)) 
# cbsa_sf$GEOID[161:190] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[191:221] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[223:230] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[232:260] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[261:290] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[291:330] %>% walk(~save_dams_cbsa_map(.x))
# 
# cbsa_sf$GEOID[331:360] %>% walk(~save_dams_cbsa_map(.x))
# cbsa_sf$GEOID[361:392] %>% walk(~save_dams_cbsa_map(.x))

list.files('cbsa-dams-files/', full.names = TRUE)[1] %>% map(~read_csv(.x))
# some have 29 columns?
fc_dams_cbsa_df <- list.files('cbsa-dams-files/', full.names = TRUE) %>% 
  map_df(~read_csv(.x, col_types = 'ccccccccccddcccdllcc'))

head(fc_dams_cbsa_df)

fc_dams_cbsa_summary <- fc_dams_cbsa_df %>% group_by(GEOID, NAME) %>%
  summarise(n_dams = n(),
            max_dam_height = max(nid_height))
write_csv(fc_dams_cbsa_summary, 'flood-control-dams-summary.csv')
write_csv(fc_dams_cbsa_df, 'flood-control-dams-cbsa.csv')
########### leaflet map function ###############

# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery, 
#                    group = "Satellite Imagery") %>%
#   addTiles(group = "Open Street Map") %>%
#   addPolygons(data = my_cbsa_sf_buffer) %>%
#   addPolygons(data = my_cbsa_sf, popup = ~NAMELSAD,
#               fillOpacity = 0) %>%
#   addMarkers(data = fc_dams_sf, 
#              popup = ~glue("{dam_name}<br>{river}<br> 
#                {distance} miles from {city}<br>"))
# 
# leaflet_dams_map <- function(my_cbsa_geoid){
#   my_cbsa_sf <- cbsa_sf %>% dplyr::filter(GEOID == my_cbsa_geoid)
#   
# nhd <- "https://hydro.nationalmap.gov/arcgis/services/nhd/MapServer/WMSServer"
# wbd <- "https://hydro.nationalmap.gov/arcgis/services/wbd/MapServer/WMSServer"
# 
# ll <- leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery, 
#                    group = "Satellite Imagery") %>%
#   addTiles(group = "Open Street Map") %>%
#   addPolygons(data = my_cbsa_sf, popup = ~NAMELSAD,
#               fillOpacity = 0) %>%
#   addMarkers(data = fc_dams_sf, 
#              popup = ~glue("{dam_name}<br>{river}<br> 
#                {distance} miles from {city}<br>"))
#   # addWMSTiles(nhd, 
#               # layers = list("5", "6", "9", "10"),
#               # options = WMSTileOptions(format = "image/png",
#                                        # transparent = TRUE),
#               # group = "NHD") %>%
#   # addWMSTiles(wbd, layers = list("7"),
#               # options = WMSTileOptions(format = "image/png",
#                                        # transparent = TRUE),
#               # group = "watersheds") %>%
#   # addLayersControl(baseGroups = c("Open Street Map", "Satellite Imagery"),
#                    # overlayGroups = c("NHD", "watersheds")) %>%
#   # leaflet::hideGroup(c("watersheds"))
# 
# return(ll)}
# 
# leaflet_dams_map("10500")
