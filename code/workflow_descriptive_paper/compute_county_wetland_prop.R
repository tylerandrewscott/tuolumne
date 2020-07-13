library(tidyverse)
library(stringr)
library(tidyverse)
library(tigris)
library(sf)
library(maps)
library(sp)
library(rgdal)
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0")
fip_ref = tigris::fips_codes
fip_ref$CFIPS = paste0(fip_ref$state_code,fip_ref$county_code)
fip_ref$State_File = gsub('\\s','_',fip_ref$state_name)
fip_ref$dsn = paste0('spatial_input/nhd_states/NHD_H_',fip_ref$State_File,'_State_Shape/Shape/')
us_county = readOGR('../yampa/spatial_input/tl_2016_us_county/','tl_2016_us_county')
us_county = spTransform(us_county,TA)
us_county@data$CFIPS = paste0(us_county@data$STATEFP,us_county@data$COUNTYFP)
us_county@data$SUPS = fip_ref$state[match(us_county@data$GEOID,fip_ref$CFIPS)]



library(pbapply)
library(lwgeom)
county_wetlands_list = pblapply(seq_along(state.abb),function(x) {
  floc = paste0('spatial_input/nwi/',paste0(state.abb[x],'_geodatabase_wetlands.gdb'))
  layrs = st_layers(floc)
  layer = grep('_Wetlands$',layrs$name,value=T)
  state_wetlands = st_read(floc,layer)
  state_wetlands = st_make_valid(state_wetlands)
  temp_counties = us_county[grepl(state.abb[x],us_county@data$SUPS),]
  county_sf = st_as_sf(temp_counties)
  county_sf = st_transform(county_sf,st_crs(state_wetlands))
  tdf = data.frame(CFIPS = county_sf$CFIPS,Prop_Wetlands = pbsapply(1:nrow(county_sf),function(f) 
    as.numeric(sum(st_area(st_intersection(x = county_sf[f,],y = state_wetlands))) / st_area(county_sf[f,])),cl = 8))
  tdf
})

county_wetlands_df = do.call(rbind,county_wetlands_list)

saveRDS(county_wetlands_df,'scratch/descriptive_paper/county_wetlands_proportion.RDS')

