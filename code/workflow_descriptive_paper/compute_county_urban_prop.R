library(data.table)
library(sf)
library(lwgeom)
library(pbapply)
albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
urban_area = st_read('spatial_input/cb_2016_us_ua10_500k.shp')
urban_area = st_transform(urban_area,albersNA)
counties = tigris::counties(class = 'sf')
counties = st_transform(counties,albersNA)

overs = st_intersects(counties,urban_area)
urban_list = pblapply(seq_along(overs),function(x) {
  if(length(overs[x])==0){Prop_Urban = 0}
  if(length(overs[x])!=0){
  Prop_Urban = sum(st_area(st_intersection(counties[x,],urban_area[overs[[x]],])))/st_area(counties[x,])
  }
  data.table(CFIPS = counties$GEOID[x],Prop_Urban = Prop_Urban)},cl = 8)

urban_prop_dt = rbindlist(urban_list)
saveRDS(urban_prop_dt,'scratch/descriptive_paper/county_urban_proportion.RDS')
