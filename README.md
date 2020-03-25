# river-cities

Socio-hydrology flood risk model

## Match city names in claims data to census locations with population info

> Data acquisition and cleaning

* `get-claims-data.Rmd` 

uses openFEMA_claims20190831.csv to create claims_tracts11.csv 
which is valid census tracts in claims data

* `get-census-spatial.Rmd`
* `get-usgs-sites.Rmd`
* `get_subcounty_intercensal.R`

> Spatial intersection

* `tract-x-spatial.Rmd`

Function *get_tract_joins* uses sf::st_contains to find polygon(s)
in census spatial data set of a given scale (places or cbsa) that
overlaps tract.

> Matching names

* `tract-x-names.R`

Function *find_tractname_matches* that saves table for each census tract
with census name for each tract + reportedcity combo in claims data.
For places, agrep_dist = 0.35, for CBSA, agrep_dist = 100

## Population data

`get-census-pops.Rmd` - CBSA populations for metro and micro areas

| SUMLEV | description | years |
|--------|-------------|-------|
| 040    | states | 2000-2010, 2010-2018 |
| 050    | counties | 2000-2010, 2010-2018 |
| 061    | minor civil division | 2000-2010, 2010-2018 |
| 071    | minor civil division place part | 2000-2010, 2010-2018 |
| 157    | county place part | 2000-2010, 2010-2018 |
| 162    | incorporated place | 2000-2010, 2010-2018 |
| 170    | Consolidated city | 2010-2018 |
| 172    | Consolidated city -- place within consolidated city | 2010-2018 |
| 310    | Metropolitan/Micropolitan Statistical Area | 2000, 2010-2018 |
| 314    | Metropolitan Division | 2010-2018? |
| 330    | Combined statistical area | 2010-2018? |


