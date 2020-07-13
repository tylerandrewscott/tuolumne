library(tidyverse)
library(stringr)
library(tidyverse)
library(tigris)
library(sf)
library(maps)
library(sp)
library(rgdal)
TA <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0"

fip_ref = tigris::fips_codes
fip_ref$CFIPS = paste0(fip_ref$state_code,fip_ref$county_code)
fip_ref$State_File = gsub('\\s','_',fip_ref$state_name)
fip_ref$dsn = paste0('spatial_input/nhd_states/NHD_H_',fip_ref$State_File,'_State_Shape/Shape/')
us_county = st_read('../yampa/spatial_input/tl_2016_us_county/tl_2016_us_county.shp')
us_county = st_transform(us_county,TA)
us_county$CFIPS = paste0(us_county$STATEFP,us_county$COUNTYFP)
us_county$SUPS = fip_ref$state[match(us_county$GEOID,fip_ref$CFIPS)]

library(pbapply)
library(lwgeom)
library(gridExtra)
library(grid)
library(rgdal)
library(sp)
library(pbapply)

hab_poly = st_read('spatial_input/Critical_Habitat_USGS/CRITHAB_POLY.shp')
hab_poly = st_transform(hab_poly,TA)
hab_poly = st_make_valid(hab_poly)
hab_inters = st_intersects(us_county,hab_poly)

hab_inter_area = pbsapply(seq_along(hab_inters),function(x) sum(st_area(st_intersection(us_county[x,],hab_poly[hab_inters[[x]],]))),cl = 8)
us_county$Prop_CH_Poly = as.numeric(hab_inter_area / st_area(us_county))

hab_line = st_read('spatial_input/Critical_Habitat_USGS/CRITHAB_LINE.shp')
hab_line = st_transform(hab_line,TA)
hab_line_inters = st_intersects(us_county,hab_line)
hab_line_length = pbsapply(seq_along(hab_inters),function(x) sum(st_length(st_intersection(us_county[x,],hab_line[hab_line_inters[[x]],]))),cl = 8)
us_county$Length_CH_Line_M <- as.numeric(hab_line_length)

saveRDS(data.table(us_county)[,c("CFIPS","Length_CH_Line_M","Prop_CH_Poly")],'scratch/county_ch_data.RDS')





