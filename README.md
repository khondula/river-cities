[![DOI](https://zenodo.org/badge/237534390.svg)](https://zenodo.org/badge/latestdoi/237534390)

# Cities on rivers

Code associated with data acquisition and cleaning for socio-hydrology flood risk model used in: *Flood Risk Behaviors of US Riverine Metropolitan Areas are driven by Local Hydrology and Shaped by Race* by James Knighton, Kelly Hondula, Cielo Sharkus, Christian Guzman, Rebecca Elliot

## List of Files

* `census-populations.Rmd` - population estimates for US metropolitan areas
* `gage-locations.Rmd` - query USGS NWIS for gages within bbox of metro areas, create static and interactive maps
* `dams.R` - calculate number and maximum height of dams within each metro area
* `percentwhite1970.R` - aggregate county level 1970 census data to metropolitan areas

## Packges and data sources

* USGS NWIS via dataRetrieval
* NID via dams
* NHGIS via web queries
* TIGER shapefiles from census FTP site
* Population estimates from census website
* American Community Survey via tidycensus

## Acknowledgements

This work was supported by the National Socio-Environmental Synthesis Center (SESYNC) under funding received from the National Science Foundation DBI-1639145.



