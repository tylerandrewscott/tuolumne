
library(data.table)
library(tidyverse)
library(lubridate)
library(sf)
library(lwgeom)
library(stringr)
library(pbapply)
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

bpa_url = 'https://opendata.arcgis.com/datasets/c3021ac75ef14dbbaf8dc4a38ac0e055_0.zip'
td = tempdir()
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(bpa_url, tf)
fname = unzip(tf, list=TRUE)

unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
bpa_land <- st_read(fpath)
bpa_land  <- st_transform(bpa_land ,crs = st_crs(albersNA))
bpa_land   <- st_make_valid(bpa_land )
ggplot() + geom_sf(data = bpa_land)

class(fb1)
summary(fb1)
class(fb1)
plot.nnet(fb1,pos.col='darkgreen',neg.col='darkblue',alpha.val=0.7,rel.rsc=15,
          circle.cex=10,cex=1.4,circle.col='brown',all.in='Sepal W.',all.out='v')
nnet::