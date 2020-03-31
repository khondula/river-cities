library(leaflet)
library(sf)
library(glue)
library(dplyr)

# read in table with metropolitan and micropolitan area codes
# LSAD == M1 for metropolitan (> 50,000 population)
# LSAD == M2 for micropolitan (10-50K population)
cbsa_df <- readr::read_csv("/nfs/public-data/NFIP/cbsa-geoids.csv", col_types = "ccc")
head(cbsa_df)

leaflet_gauges_map("14540")

leaflet_gauges_map <- function(my_cbsa_geoid){
  
  # data on gauges with  begin_date < "1950-01-01"
  # end_date > "2019-01-01" and count_nu > 10
  gauges_df <- readr::read_csv("/nfs/public-data/NFIP/gauges-cbsa-M1.csv",
                               col_types = "cccddccccDDdcdcc")
  gauges_geoids <- gauges_df[["cbsa_geoid"]] %>% unique()
  if(!my_cbsa_geoid %in% gauges_geoids){stop("No good gauges")}
  
  good_sites_sf <- gauges_df %>%
    dplyr::filter(cbsa_geoid == my_cbsa_geoid) %>%
    dplyr::select(site_no, station_nm, dec_lat_va, dec_long_va, 
                  begin_date, end_date, count_nu, drain_area_va) %>%
    st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4269)
  
  # metro and micro areas
  cbsa_dir <- "/nfs/public-data/census-tiger-2019/CBSA"
  cbsa_file <- glue("{cbsa_dir}/tl_2019_us_cbsa.shp")
  my_cbsa_sf <- sf::st_read(cbsa_file) %>%
    dplyr::filter(GEOID == my_cbsa_geoid)
  my_bbox <- my_cbsa_sf %>% st_bbox()
  
  urban_dir <- "/nfs/public-data/census-tiger-2019/UAC/"
  urban_file <- glue::glue("{urban_dir}/tl_2019_us_uac10.shp")
  urban_sf <- sf::st_read(urban_file)
  urban_sf_clip <- st_crop(urban_sf, my_cbsa_sf)
  
  nhd <- "https://hydro.nationalmap.gov/arcgis/services/nhd/MapServer/WMSServer"
  wbd <- "https://hydro.nationalmap.gov/arcgis/services/wbd/MapServer/WMSServer"
  
  ll <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = my_cbsa_sf, popup = ~NAMELSAD,
                fillOpacity = 0) %>%
    addPolygons(data = urban_sf_clip, popup = ~NAMELSAD10,
                group = "Urbanized Area", 
                color = "gray", opacity = 1, weight = 1,
                fillOpacity = 0.5) %>%
    addMarkers(data = good_sites_sf, 
               popup = ~glue("{site_no}<br>{station_nm}<br> 
               {begin_date} to {end_date}<br>n={count_nu}<br>
                            {drain_area_va} sq mi")) %>%
    addWMSTiles(nhd, 
                layers = list("5", "6", "9", "10"),
                options = WMSTileOptions(format = "image/png",
                                         transparent = TRUE),
                group = "NHD") %>%
    addWMSTiles(wbd, layers = list("7"),
                options = WMSTileOptions(format = "image/png",
                                         transparent = TRUE),
                group = "watersheds") %>%
    addLayersControl(overlayGroups = c("NHD", "Urbanized Area", "watersheds")) %>%
    leaflet::hideGroup(c("watersheds"))
  
  return(ll)
}
