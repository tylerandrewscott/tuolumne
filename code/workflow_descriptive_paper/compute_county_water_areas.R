library(tidyverse)
library(stringr)
library(tidyverse)
library(tigris)
library(sf)
library(maps)
library(sp)
library(rgdal)
library(pbapply)
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0")
fip_ref = tigris::fips_codes
fip_ref$CFIPS = paste0(fip_ref$state_code,fip_ref$county_code)
fip_ref$State_File = gsub('\\s','_',fip_ref$state_name)
fip_ref$dsn = paste0('spatial_input/nhd_states/NHD_H_',fip_ref$State_File,'_State_Shape/Shape/')
us_county = readOGR('../yampa/spatial_input/tl_2016_us_county/','tl_2016_us_county')
us_county = spTransform(us_county,TA)
us_county@data$CFIPS = paste0(us_county@data$STATEFP,us_county@data$COUNTYFP)
us_county@data$SUPS = fip_ref$state[match(us_county@data$GEOID,fip_ref$CFIPS)]
water = st_read('spatial_input/USA_Detailed_Water_Bodies.shp')

county_surface_area = pblapply(seq_along(us_county),function(x){
tcounty = st_as_sf(us_county[x,])
tcounty = st_transform(tcounty,st_crs(water))
tinter = st_intersection(tcounty,water)
data.frame(CFIPS = us_county@data$CFIPS[x],Surface_Water_Area_km2 = as.numeric(sum(st_area(tinter)))/1000000,stringsAsFactors = F)},cl = 4)

county_surface_area_df = do.call(rbind,county_surface_area)

saveRDS(county_surface_area_df,'scratch/descriptive_paper/county_surface_water_area.RDS')

