

gc()
rm(list=ls())
library(ggplot2)
library(plyr)
library(dplyr)
library(sf)
blmdoeshape<-readRDS("blm_doe_shapes.rds")
blmdoeshape<-st_sf(blmdoeshape)
blmdoeshape<-st_make_valid(blmdoeshape[1:279,])
st_length(blmdoeshape)

redo_buffer<-function(blmdoeshape2,buffer){
blmdoeshape2<-st_buffer(blmdoeshape2,dist=buffer)
crithab<-read_sf("Downloads/routingengine/land_critical_habitat_area_v3/ez_gis.land_critical_habitat_area_v3.shp")
watercenter<-st_read("Downloads/routingengine/Nationwide Rivers Inventory/NRI.gdb","HYDRO_NationwideRiversInventory_ln")
watercenter2<-st_read("Downloads/routingengine/S_USA.WildScenicRiver_LN.gdb")
watercenter<-c(watercenter %>% st_geometry(),watercenter2 %>% st_geometry())
crithab<-st_intersects(blmdoeshape2,st_transform(crithab,st_crs(blmdoeshape2)))
blmdoeshape2$crithabcount<-sapply(crithab,length)
watercenter<-st_intersects(blmdoeshape2,st_transform(watercenter,st_crs(blmdoeshape2)))
blmdoeshape2$surfacewater.nri<-sapply(watercenter,length)
PADB<-read_sf("Downloads/routingengine/land_restriction_protected_areas_database_v3/ez_gis.land_restriction_protected_areas_database_v3.shp")
PADBi<-st_intersects(blmdoeshape2,st_transform(PADB,st_crs(blmdoeshape)))
PADBb<-PADB[unlist(PADBi) %>% unique(),]
ownlist<-lapply(1:nrow(blmdoeshape2), function(X) PADB$own_name[PADBi[[X]]])
blmdoeshape2$own.NPS<-sapply(ownlist, function(X) "NPS"%in%X)
blmdoeshape2$own.trib<-sapply(ownlist, function(X) "TRIB"%in%X)
blmdoeshape2$own.USFS<-sapply(ownlist, function(X) "USFS"%in%X)
blmdoeshape2$own.SLB<-sapply(ownlist, function(X) "SLB"%in%X)
blmdoeshape2$own.FWS<-sapply(ownlist, function(X) "FWS"%in%X)
blmdoeshape2$own.USBR<-sapply(ownlist, function(X) "USBR"%in%X)
#blmdoeshape2$maxcapacity<-sapply(gd1a$capacity, function(X) max(as.numeric(X),na.rm=T))
blmdoeshape2$newarea<-blmdoeshape2 %>% st_area()

tlines<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQck262IIZxf_o5q2Ci9gRlY2qTPzw0WqCsEch3RXNAqYVd-JKZgZLpSPGc3GxB3TsW0Dek2hd3_H6k/pub?gid=811733457&single=true&output=csv")
blmdoeshape2<-left_join(blmdoeshape2,tlines)
blmdoeshape2$YEAR<-ifelse(is.na(blmdoeshape2$YEAR),blmdoeshape2$ID %>% stringr::str_extract("-(2|1)(9|0)[0-9][0-9]-") %>% stringr::str_extract("[0-9]+"),blmdoeshape2$YEAR)
blmdoeshape2<-blmdoeshape2[blmdoeshape2$geometry %>% sapply(.,length)>0,]
library(raster)
#population
poptif1<-raster::raster("Downloads/usgrid_data_2000/geotiff/uspop00.tif")
poptif<-raster::raster("Downloads/usgrid_data_2010/geotiff/uspop10.tif")
poptif<-raster::projectRaster(poptif,crs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
poptif1<-raster::projectRaster(poptif1,crs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
gc()

ccv<-cellFromPolygon(poptif, p=as_Spatial(blmdoeshape2 %>% sf::st_geometry()))
ccv1<-cellFromPolygon(poptif1, p=as_Spatial(blmdoeshape2 %>% sf::st_geometry()))
names(ccv)<-1:length(ccv)
names(ccv1)<-1:length(ccv1)
ccv<-ccv[sapply(ccv,length)>0]
cellmelt<-reshape2::melt(ccv)
ccv1<-ccv1[sapply(ccv1,length)>0]
cellmelt1<-reshape2::melt(ccv1)
cellmelt$pop2010<-extract(poptif,cellmelt$value)
cellmelt1$pop2000<-extract(poptif1,cellmelt1$value)
cellmelt<-join(cellmelt,cellmelt1,type="full")
cellmelt<-ddply(cellmelt, .(L1),summarize, pop2010=sum(pop2010,na.rm=T), pop2000=sum(pop2000,na.rm=T))
blmdoeshape2$population10<-ifelse(as.numeric(blmdoeshape2$YEAR)>2009,cellmelt$pop2010,cellmelt$pop2000)


egsubregion<-read_sf("Downloads/egrid_subregions/eGRID2014_subregions.shp")
bmatch<-st_intersects(blmdoeshape2,egsubregion %>% st_transform(st_crs(blmdoeshape2)))
bmatch<-bmatch %>% lapply(.,function(X) egsubregion$zips_for_G[X]) %>% reshape2::melt()
table(bmatch$value)
bmatch<-ddply(bmatch,.(L1),summarize,region=ifelse(length(value)==1,as.character(value),"multiple"))
blmdoeshape2$Region<-NA
blmdoeshape2$Region[bmatch$L1]<-bmatch$region

blmdoeshape2$yearnum<-as.numeric(blmdoeshape2$YEAR)

st_write(blmdoeshape2 %>% dplyr::select(ID,yearnum),"blm_doe_shape.shp",append=FALSE)
blmshape<-st_read("blm_doe_shape.shp")
cvoteS<-readRDS("Downloads/countyVoteShare_3-2020_imputed.rds")
counties<-tigris::counties(class="sf")
cvoteS$FISCAL_YEAR %>% table()
counties<-counties %>% mutate(CFIPS=paste0(STATEFP,COUNTYFP)) %>% dplyr::select(CFIPS) 

counties<-counties %>% left_join(.,cvoteS)
counties<-na.omit(counties)
st_crs(counties)<-st_crs(4269)
counties<-st_transform(counties,st_crs(blmshape))

demlist<-lapply(2005:2018, function(i){
  #tempa<-st_transform(tempa,st_crs(counties))
  temp<-filter(blmshape, yearnum==i) %>% dplyr::select(ID) %>% st_make_valid() 
  Sys.sleep(1)
  if(i==2018){temp<-filter(blmshape, yearnum>2017) %>% dplyr::select(ID) %>% st_make_valid() }
  temp2<-filter(counties, FISCAL_YEAR==i) 
  if(sum(sapply(st_intersects(temp,temp2),length))==0){data.frame()} else {
    tempo<-st_interpolate_aw(temp2 %>% dplyr::select(percentD_H),temp,extensive=F)
    data.frame("ID"=temp$ID[tempo$Group.1],"DH"=tempo$percentD_H)}
})
blmdoeshape2<-blmdoeshape2 %>% left_join(.,demlist %>% bind_rows())

rebuildstatus<-readRDS("Box/tuolumne/scratch/transmission_lines/text_projtype_matches.rds")
blmdoeshape2<-left_join(blmdoeshape2,rebuildstatus)
blmdoeshape2$existing <-blmdoeshape2$sum_total>=3

blmdoeshape2$Office<-ifelse(stringr::str_detect(blmdoeshape2$ID,"BLM"),blmdoeshape2$ID %>% substring(.,1,10) %>% as.character(),blmdoeshape2$Office %>% as.character())
blmdoeshape2$Office<-substring(blmdoeshape2$Office,1,4) %>% gsub("\\W","",.)

blmdoeshape2$newarea <-blmdoeshape2$newarea %>% as.numeric(.)/1000000

blmdoeshape2$Doc_Type <-blmdoeshape2$Doc_Type %>% relevel(.,ref="EA")
blmdoeshape2$Office2<-c(blmdoeshape2$ID %>% substring(.,1,10) %>% .[1:193],blmdoeshape2$Office[194:279])
rebuildstatus<-readRDS("Box/tuolumne/scratch/transmission_lines/text_projtype_matches.rds")
blmdoeshape2<-left_join(blmdoeshape2,rebuildstatus)
blmdoeshape2$existing <-blmdoeshape2$sum_total>=3

turnout<-read.csv("Downloads/dataverse_files (2)/countypres_2000-2016.csv")
turnout<-turnout %>% dplyr::select(office,year,FIPS,totalvotes) %>% unique()

counties<-tigris::counties(class="sf")
head(counties)
counties$FIPS<-paste0(counties$STATEFP,counties$COUNTYFP) %>% as.numeric()
turnout<-merge(turnout,counties)

library(tidycensus)
#census_api_key("2b1ec2b4424bb192a30563a7c6e53e70a3ac5d31",install=T)
#readRenviron("~/.Renviron")
age10 <- get_decennial(geography = "county", variables = "P001001",year = 2010)
turnout<-left_join(turnout,age10 %>% dplyr::select(GEOID,value) %>% plyr::rename(.,c("GEOID"="FIPS","value"="Pop")) %>% mutate(FIPS=as.numeric(FIPS)))
turnout$voterate<-turnout$totalvotes/turnout$Pop

turnout<-turnout %>% st_sf() %>% st_transform(st_crs(blmdoeshape2))

vyt<-mutate(blmdoeshape2,voteyear=sapply(blmdoeshape2$yearnum,function(X) c(2000,2004,2008,2012,2016)[sort(X-c(2000,2004,2008,2012,2016),index.return=T)$ix] %>% .[.<=X] %>% .[1])) %>% dplyr::select(voteyear,ID) 
vyt$ilist<-vyt %>% st_intersects(st_transform(turnout,st_crs(blmdoeshape2)))
blmdoeshape2$VoterRate<-sapply(1:nrow(vyt),function(X) turnout[vyt$ilist[[X]],] %>% filter(year==vyt$voteyear[X]) %>% as.data.frame() %>%  .$voterate %>% mean())

saveRDS(blmdoeshape2,paste0("eis_ea_finaldataset",buffer,".rds"))
paste0("eis_ea_finaldataset",buffer,".rds")
}



