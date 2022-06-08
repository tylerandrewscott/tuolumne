library(maptools)
library(sf)
library(tigris)
library(readtext)
library(tokenizers)
library(spacyr)
library(acs)
library(tidyverse)
library(pbapply)
library(data.table)
library(lwgeom)
library(rgeos)

albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

blm_admin ="https://gis.blm.gov/EGISDownload/LayerPackages/BLM_National_Administrative_Units.zip"
temp <- tempfile()
download.file(blm_admin ,temp)
cat = unzip(temp)
layrs = st_layers('./BLM_National_Administrative_Units/admu.gdb')
fname <- (grep('gdb$',cat,value=T))

bound_list = lapply(layrs$name[-1],function(x) st_transform(st_read('./BLM_National_Administrative_Units/admu.gdb',x),albersNA))

bound_list[[1]]$Type = 'State Office'
bound_list[[1]]$ADMINISTRATIVE_NAME = bound_list[[1]]$ADMIN_ST
bound_list[[1]] = bound_list[[1]][bound_list[[1]]$ADMINISTRATIVE_NAME!='ES',]
bound_list[[2]]$Type = 'District Office'
bound_list[[2]]$ADMIN_NAME = toupper(bound_list[[2]]$PARENT_NAME)
bound_list[[2]]$ADMIN_NAME = gsub('\\s{2,}',' ',bound_list[[2]]$ADMIN_NAME)
bound_list[[2]]$ADMIN_NAME[grepl('DISTRICT$',bound_list[[2]]$ADMIN_NAME)] = paste0(bound_list[[2]]$ADMIN_NAME[grepl('DISTRICT$',bound_list[[2]]$ADMIN_NAME)],' OFFICE')
bound_list[[2]]$ADMIN_NAME[!grepl('OFFICE$|MONUMENT$',bound_list[[2]]$ADMIN_NAME)] = paste0(bound_list[[2]]$ADMIN_NAME[!grepl('OFFICE$|MONUMENT$',bound_list[[2]]$ADMIN_NAME)],' DISTRICT OFFICE')
bound_list[[2]]$ADMINISTRATIVE_NAME = gsub('\\s{2,}',' ',bound_list[[2]]$ADMIN_NAME)
bound_list[[3]]$Type = 'Field Office'
bound_list[[3]]$ADMU_NAME <- toupper(bound_list[[3]]$ADMU_NAME)
bound_list[[3]]$ADMU_NAME <- gsub('FO$|FIELD OFFI$|FIELD OFFIC$','FIELD OFFICE',bound_list[[3]]$ADMU_NAME)
bound_list[[3]]$ADMU_NAME <- gsub(' NM$',' NATIONAL MONUMENT',bound_list[[3]]$ADMU_NAME)
bound_list[[3]]$ADMU_NAME[!grepl('FIELD OFFICE$|NATIONAL MONUMENT$',bound_list[[3]]$ADMU_NAME)] <- paste0(bound_list[[3]]$ADMU_NAME[!grepl('FIELD OFFICE$|NATIONAL MONUMENT$',bound_list[[3]]$ADMU_NAME)],' FIELD OFFICE')
bound_list[[3]]$ADMINISTRATIVE_NAME = gsub('\\s{2,}',' ',bound_list[[3]]$ADMU_NAME)
bound_list[[4]]$Type = 'Other'
bound_list[[4]]$ADMU_NAME <- toupper(bound_list[[4]]$ADMU_NAME)
bound_list[[4]]$ADMU_NAME <- gsub(' NM$',' NATIONAL MONUMENT',bound_list[[4]]$ADMU_NAME)
bound_list[[4]]$ADMINISTRATIVE_NAME = gsub('\\s{2,}',' ',bound_list[[4]]$ADMU_NAME)

blm_ra = "https://www.blm.gov/or/gis/files/web_corp/district_ra_boundary.zip"
temp <- tempfile()
download.file(blm_ra ,temp)
cat = unzip(temp)
fname <- (grep('gdb$',cat,value=T))
fname <- gsub('\\/gdb$','',fname)
bound_list[[5]] <- st_transform(st_read(fname,'rab_poly'),albersNA)
bound_list[[5]]$Type = 'Resource Area'
bound_list[[5]]$RA_NAME <- toupper(bound_list[[5]]$RA_NAME)
bound_list[[5]]$RA_NAME <- gsub(' RA$',' RESOURCE AREA',bound_list[[5]]$RA_NAME)
bound_list[[5]]$ADMINISTRATIVE_NAME = gsub('\\s{2,}',' ',bound_list[[5]]$RA_NAME)

blm_ca = "https://gis.blm.gov/EGISDownload/LayerPackages/BLM_National_NLCS_NatlMonuments_NatlConservationAreas_poly.zip"
temp <- tempfile()
download.file(blm_ca ,temp)
cat = unzip(temp)
fname <- (grep('gdb$',cat,value=T))
fname <- gsub('\\/gdb$','',fname)

bound_list[[6]] <- st_transform(st_read(fname[2],'Monuments_NCAs_SimilarDesignation2015'),albersNA)
bound_list[[6]]$Type = 'Natural Area/Monument'
bound_list[[6]]$NCA_NAME = toupper(bound_list[[6]]$NCA_NAME)
bound_list[[6]]$NCA_NAME <- gsub(' NCA$',' NATIONAL CONSERVATION AREA',bound_list[[6]]$NCA_NAME)
bound_list[[6]]$NCA_NAME <- gsub(' NM$',' NATIONAL MONUMENT',bound_list[[6]]$NCA_NAME)
bound_list[[6]]$ADMINISTRATIVE_NAME = gsub('\\s{2,}',' ',bound_list[[6]]$NCA_NAME)

#bound_trans = pblapply(bound_list,function(x) st_make_valid(x),cl = 6)
bound_trans = bound_list
names(bound_trans) <- c('STATE','DISTRICT OFFICE','FIELD OFFICE','OTHER','RESOURCE AREA','NATIONAL MONUMENT/NATIONAL CONSERVATION AREA')
bound_trans$`NATIONAL MONUMENT/NATIONAL CONSERVATION AREA`$ADMINISTRATIVE_NAME[grepl('GRAND STAIRCASE',bound_trans$`NATIONAL MONUMENT/NATIONAL CONSERVATION AREA`$ADMINISTRATIVE_NAME)]<-"GRAND STAIRCASE-ESCALANTE NATIONAL MONUMENT"
bound_trans$`NATIONAL MONUMENT/NATIONAL CONSERVATION AREA`<-st_zm(bound_trans$`NATIONAL MONUMENT/NATIONAL CONSERVATION AREA`,drop = T)

#t1 = bound_trans$`NATIONAL MONUMENT/NATIONAL CONSERVATION AREA`[!bound_trans$`NATIONAL MONUMENT/NATIONAL CONSERVATION AREA`$ADMINISTRATIVE_NAME=="GRAND STAIRCASE-ESCALANTE NATIONAL MONUMENT",]
#t2 = bound_trans$`NATIONAL MONUMENT/NATIONAL CONSERVATION AREA`[bound_trans$`NATIONAL MONUMENT/NATIONAL CONSERVATION AREA`$ADMINISTRATIVE_NAME=="GRAND STAIRCASE-ESCALANTE NATIONAL MONUMENT",]
#bound_trans$`NATIONAL MONUMENT/NATIONAL CONSERVATION AREA` = t3

state_keep = c('02','04','06','08','15','16','20','30','31','32','35','38','40','41','46','48','49','53','56')
counties = tigris::counties(class ='sf')
counties = st_transform(counties, albersNA)
counties = counties[counties$STATEFP %in% state_keep,]


states= tigris::states(class ='sf')
states = st_transform(states, albersNA)
states = states[states$STATEFP%in% state_keep,]

bound_trans[[6]] <- st_make_valid(bound_trans[[6]])
county_overlap = lapply(seq_along(bound_trans),function(x) {print(x);st_intersects(bound_trans[[x]],counties)})
names(county_overlap) <- c('STATE','DISTRICT OFFICE','FIELD OFFICE','OTHER','RESOURCE AREA','NATIONAL MONUMENT/NATIONAL CONSERVATION AREA')


blm_nepa = readRDS('input/agency_nepa/blm/blm_scrape_dataset_update521.rds')
blm_nepa = as.data.table(blm_nepa)
blm_nepa$YEAR = str_extract(blm_nepa$NEPA..,paste(2000:2019,collapse='|'))

as.matrix(table(blm_nepa$YEAR,blm_nepa$Program))
table(blm_nepa$Program!='')
table(blm_nepa$Doc.Type,blm_nepa$YEAR)
co = as.matrix(table(blm_nepa$Program[blm_nepa$YEAR%in%2010:2018]))
htmlTable::htmlTable(data.table(co,keep.rownames = T)[order(V1,decreasing = T),])
blm_nepa$State_Search = pbsapply(seq_along(blm_nepa$Description),function(x) {
  sn = str_extract(blm_nepa$Description[[x]],states$NAME)
  if(any(!is.na(sn))){sn[!is.na(sn)]}else{NA}})
blm_nepa$Office.s. = as.character(blm_nepa$Office.s.)
blm_nepa$Office_List = str_split(blm_nepa$Office.s.,'\n')
blm_nepa$Office_List = sapply(blm_nepa$Office_List,toupper)
blm_nepa = blm_nepa[!unlist(sapply(blm_nepa$Office_List,function(x) any(grepl('WO-200|WO-350',x)),simplify = T)),]
blm_nepa$UNIT_LEVEL = NA
blm_nepa$Office_List <- sapply(blm_nepa$Office_List,function(x) gsub(' FO$',' FIELD OFFICE',x))
blm_nepa$Office_List <- sapply(blm_nepa$Office_List,function(x) gsub(' DO$',' DISTRICT OFFICE',x))
blm_nepa$Office_List <- sapply(blm_nepa$Office_List,function(x) gsub(' NMON$',' NATIONAL MONUMENT',x))
blm_nepa$Office_List <- sapply(blm_nepa$Office_List,function(x) gsub(' NCA$',' NATIONAL CONSERVATION AREA',x))
blm_nepa$Office_List <- sapply(blm_nepa$Office_List,function(x) gsub(' RA$',' RESOURCE AREA',x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub('ARCTIC FIELD OFFICE','ARCTIC DISTRICT OFFICE',x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub('ALTURAS FIELD OFFICE','APPLEGATE FIELD OFFICE',x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub('SURPRISE FIELD OFFICE','APPLEGATE FIELD OFFICE',x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub('EGAN FIELD OFFICE','BRISTLECONE FIELD OFFICE',x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub('SCHELL FIELD OFFICE','BRISTLECONE FIELD OFFICE',x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("RED ROCK - SLOAN CANYON NCA FIELD OFFICE","RED ROCK/SLOAN FIELD OFFICE",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("THREE RIVERS FIELD OFFICE","THREE RIVERS RESOURCE AREA",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("ANDREWS FIELD OFFICE","ANDREWS RESOURCE AREA",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub('^OLD_','',x))
#blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub('OLD_GRAND JUNCTION FIELD OFFICE','GRAND JUNCTION FIELD OFFICE',x))

blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("UMPQUA FIELD OFFICE","COOS BAY UMPQUA FIELD OFFICE",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("MYRTLEWOOD FIELD OFFICE","COOS BAY MYRTLEWOOD FIELD OFFICE",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("ASHLAND FIELD OFFICE","MEDFORD ASHLAND FIELD OFFICE",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("GRANTS PASS FIELD OFFICE","MEDFORD GRANTS PASS FIELD OFFICE",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("BUTTE FALLS FIELD OFFICE","MEDFORD BUTTE FALLS FIELD OFFICE",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("HANKSVILLE FIELD OFFICE","RICHFIELD FIELD OFFICE",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("WYOMING HIGH DESERT DISTRICT OFFICE","HIGH DESERT DISTRICT OFFICE",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("WYOMING HIGH PLAINS DISTRICT OFFICE","HIGH PLAINS DISTRICT OFFICE",x))

blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("LAKEVIEW FIELD OFFICE|KLAMATH FALLS FIELD OFFICE","LAKEVIEW KLAMATH FALLS FIELD OFFICE",x))

blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("SOUTH RIVER FIELD OFFICE","ROSEBURG DISTRICT SOUTH RIVER FIELD OFFICE",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("SWIFTWATER FIELD OFFICE","ROSEBURG DISTRICT SWIFTWATER FIELD OFFICE",x))

blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub('HOLLISTER FIELD OFFICE','CENTRAL COAST FIELD OFFICE',x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub('PALM SPRINGS.* COAST FIELD OFFICE',"PALM SPRINGS/S. COAST FIELD OFFICE",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("SANTA ROSA.*SAN JACINTO MOUNTAINS NATIONAL MONUMENT","SANTA ROSA-SAN JACINTO MOUNTAINS NATIONAL MONUMENT",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("EUGENE DISTRICT OFFICE|SALEM DISTRICT OFFICE" ,"NW OREGON DISTRICT OFFICE" ,x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("SIUSLAW FIELD OFFICE" ,"NW OREGON SIUSLAW FIELD OFFICE" ,x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("UPPER WILLAMETTE FIELD OFFICE" ,"NW OREGON UPPER WILLAMETTE FIELD OFFICE" ,x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("CASCADES FIELD OFFICE" ,"NW OREGON CASCADES FIELD OFFICE" ,x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("TILLAMOOK FIELD OFFICE" ,"NW OREGON TILLAMOOK FIELD OFFICE" ,x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("MARYS PEAK FIELD OFFICE" ,"NW OREGON MARYS PEAK FIELD OFFICE" ,x))

blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("SAN JUAN NATIONAL MONUMENT" ,"SAN JUAN ISLANDS NATIONAL MONUMENT",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("WIND RIVER - BIGHORN BASIN DISTRICT OFFICE" ,"WIND RIVER BIGHORN BASIN DISTRICT OFFICE",x))

blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("MALHEUR FIELD OFFICE" ,"VALE MALHEUR FIELD OFFICE" ,x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("BAKER CITY FIELD OFFICE" ,"VALE BAKER FIELD OFFICE" ,x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("CANYON OF THE ANCIENTS NATIONAL MONUMENT","CANYONS OF THE ANCIENTS NATIONAL MONUMENT",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("DOMINGUEZ-ESCALANTE NATIONAL CONSERVATION AREA","DOMINGUEZ/ESCALANTE NATIONAL CONSERVATION AREA",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("SNAKE RIVER BIRDS OF PREY NATIONAL CONSERVATION AREA","MORLEY NELSON SNAKE RIVER BIRDS OF PREY NATIONAL CONSERVATION AREA",x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("HILINE DISTRICT OFFICE|CENTRAL MONTANA DISTRICT OFFICE","NORTH CENTRAL MONTANA DISTRICT OFFICE" ,x))
blm_nepa$Office_List[blm_nepa$Office_List =="GREAT FALLS OIL & GAS FIELD OFFICE"] <- list(c("WESTERN MONTANA DISTRICT OFFICE","NORTH CENTRAL MONTANA DISTRICT OFFICE" ))

blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("NORTHWEST OREGON DISTRICT OFFICE" ,"NW OREGON DISTRICT OFFICE" ,x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("WENATCHEE FIELD OFFICE" ,"SPOKANE WENATCHEE FIELD OFFICE" ,x))
blm_nepa$Office_List = sapply(blm_nepa$Office_List,function(x) gsub("BORDER FIELD OFFICE" ,"SPOKANE BORDER FIELD OFFICE" ,x))

blm_nepa$UNIT_LEVEL[unlist(sapply(blm_nepa$Office_List,function(x) any(grepl('STATE OFFICE',x))))] <- 'STATE'
blm_nepa$UNIT_LEVEL[unlist(sapply(blm_nepa$Office_List,function(x) any(grepl('DISTRICT OFFICE',x))))] <- 'DISTRICT OFFICE'
blm_nepa$UNIT_LEVEL[unlist(sapply(blm_nepa$Office_List,function(x) any(grepl('NATIONAL MONUMENT',x))))] <- 'NATIONAL MONUMENT/NATIONAL CONSERVATION AREA'
blm_nepa$UNIT_LEVEL[unlist(sapply(blm_nepa$Office_List,function(x) any(grepl('RESOURCE AREA$',x))))] <- 'RESOURCE AREA'
blm_nepa$UNIT_LEVEL[unlist(sapply(blm_nepa$Office_List,function(x) any(grepl('FIELD OFFICE',x))))] <- 'FIELD OFFICE'
blm_nepa$UNIT_LEVEL[unlist(sapply(blm_nepa$Office_List,function(x) any(grepl('NATIONAL CONSERVATION AREA$',x))))] <- 'NATIONAL MONUMENT/NATIONAL CONSERVATION AREA'
blm_nepa <- blm_nepa[!is.na(blm_nepa$UNIT_LEVEL),]


blm_nepa$NEPA.. = as.character(blm_nepa$NEPA..)

counties$STATEABB = states$STUSPS[match(counties$STATEFP,states$STATEFP)]
blm_nepa$STATE_REGION = str_extract(blm_nepa$NEPA..,paste(state.abb,collapse='|'))
blm_nepa$STATE_REGION[blm_nepa$STATE_REGION=='OR'] <- list(c('OR','WA'))
blm_nepa$STATE_REGION[blm_nepa$STATE_REGION=='MT'] <- list(c('MT','SD',"ND"))
blm_nepa$STATE_REGION[blm_nepa$STATE_REGION=='NM'] <- list(c('TX','NM',"OK",'KS'))
blm_nepa$STATE_REGION[blm_nepa$STATE_REGION=='CO'] <- list(c('CO','NE'))
cfips_list = pblapply(1:nrow(blm_nepa),function(x){
  csub = counties[counties$STATEABB %in% blm_nepa$STATE_REGION[[x]],]
  County_Names = str_extract_all(blm_nepa$counties[x],paste(csub$NAME,collapse='|'))
  County_FIPS = csub$GEOID[match(County_Names[[1]],csub$NAME)]
  data.table(NEPA.. = blm_nepa$NEPA..[x],CFIPS = list(c(County_FIPS)))},cl = 8)
clist = rbindlist(cfips_list)
setkey(clist,'NEPA..')
setkey(blm_nepa,'NEPA..')
blm_nepa = blm_nepa[clist,]

blm_nepa$COUNTY_FIPS = list()
blm_nepa$COUNTY_WEIGHTS = list()
gc()

for(i in 1:nrow(blm_nepa)){
  #which(sapply(blm_nepa$COUNTY_FIPS,is.null))){#which(is.na(blm_nepa$CFIPS) & which(sapply(blm_nepa$COUNTY_FIPS,is.null)) {
  #if(i %in% seq(1,nrow(blm_nepa),100)){print(i)}
  print(i)
  if(all(blm_nepa$UNIT_LEVEL[[i]]=='STATE')&is.na(blm_nepa$CFIPS[i])){blm_nepa$COUNTY_FIPS[[i]] <- counties$GEOID[counties$STATEABB%in%blm_nepa$STATE_REGION[[i]]];blm_nepa$COUNTY_WEIGHTS[[i]] <- rep(1,length(counties$GEOID[counties$STATEABB%in%blm_nepa$STATE_REGION[[i]]]));next}
  if(all(blm_nepa$UNIT_LEVEL[[i]]=='STATE')&!is.na(blm_nepa$CFIPS[i])){blm_nepa$COUNTY_FIPS[[i]] <- blm_nepa$CFIPS[[i]];blm_nepa$COUNTY_WEIGHTS[[i]] <- rep(1,length(blm_nepa$CFIPS[[i]]));next}
  if(duplicated(blm_nepa$Office_List)[i]){
    blm_nepa$COUNTY_FIPS[[i]]<-blm_nepa$COUNTY_FIPS[[min(which(blm_nepa$Office_List[1:(i-1)] %in% blm_nepa$Office_List[i]))]];
    blm_nepa$COUNTY_WEIGHTS[[i]]<-blm_nepa$COUNTY_WEIGHTS[[min(which(blm_nepa$Office_List[1:(i-1)] %in% blm_nepa$Office_List[i]))]];
    next}
  if(all(blm_nepa$Office_List[[i]] == list('AMARILLO FIELD OFFICE'))){blm_nepa$COUNTY_FIPS[[i]]<-c('48375');blm_nepa$COUNTY_WEIGHTS[[i]]<-c(1);next}
  temp_overlap = county_overlap[blm_nepa$UNIT_LEVEL[i]]
  index = unique(c(unlist(sapply(blm_nepa$Office_List[[i]],function(x) which(x == bound_trans[[blm_nepa$UNIT_LEVEL[i]]]$ADMINISTRATIVE_NAME),simplify = T))))
  #index = which(blm_nepa$Office_List[[i]] == bound_trans[[blm_nepa$UNIT_LEVEL[i]]]$ADMINISTRATIVE_NAME)
  if(length(index)==0|all(is.na(index))){next}
  index = index[!is.na(index)]
  #if(!all(is.na(index))){
  temp_county_inters = st_area(st_intersection(st_union(bound_trans[[blm_nepa$UNIT_LEVEL[i]]][index,]),counties[unique(unlist(temp_overlap[[1]][index])),]))
  proj_area = sum(st_area(bound_trans[[blm_nepa$UNIT_LEVEL[i]]][index,]))
  tweights = data.table(Proj = blm_nepa$NEPA..[i],Prop = as.numeric(round(temp_county_inters/proj_area,2)),CFIPS = counties$GEOID[unique(unlist(temp_overlap[[1]][index]))])
  tweights = tweights[tweights$Prop>0.01,]
  blm_nepa$COUNTY_FIPS[[i]] <- formatC(tweights$CFIPS,width = 5,flag = 0)
  blm_nepa$COUNTY_WEIGHTS[[i]] <- tweights
  gc()
}


replace = which(sapply(blm_nepa$CFIPS,function(x) all(is.na(x))))
blm_nepa$CFIPS[replace] <- blm_nepa$COUNTY_FIPS[replace]

saveRDS(blm_nepa,'input/agency_nepa/blm/blm_scrape_dataset_cfips.RDS')

#which(sapply(blm_nepa$CFIPS,function(x) all(is.na(x)))&sapply(blm_nepa$COUNTY_FIPS,is.null))
#blm_nepa[sapply(blm_nepa$CFIPS,function(x) all(is.na(x)))&sapply(blm_nepa$COUNTY_FIPS,is.null),][4,]


#blm_nepa$STATES = str_extract_all(blm_nepa$Office_List,paste(bound_trans$STATE,collapse='|'))
#blm_nepa$FIELD_OFFICE = str_extract_all(blm_nepa$Office_List,paste(bound_trans$`FIELD OFFICE`$ADMINISTRATIVE_NAME,collapse='|'))
#blm_nepa$DISTRICT_OFFICE = str_extract_all(blm_nepa$Office_List,paste(bound_trans$`DISTRICT OFFICE`$ADMINISTRATIVE_NAME,collapse='|'))
#blm_nepa$OTHER_UNIT = str_extract_all(blm_nepa$Office_List,paste(bound_trans$OTHER$ADMINISTRATIVE_NAME,collapse='|'))
#blm_nepa$RESOURCE_AREA = str_extract_all(blm_nepa$Office_List,paste(bound_trans$`RESOURCE AREA`$ADMINISTRATIVE_NAME,collapse='|'))
#blm_nepa$NM_NCA = str_extract_all(blm_nepa$Office_List,paste(bound_trans$`NATIONAL MONUMENT/NATIONAL CONSERVATION AREA`$ADMINISTRATIVE_NAME,collapse='|'))

#bound_trans$STATE$STATE_SET = as.list(as.character(bound_trans$STATE$ADMIN_ST))
#bound_trans$STATE <- bound_trans$STATE[bound_trans$STATE$ADMIN_ST!='ES',]
#bound_trans$STATE$STATE_SET[[6]] <- c('MT','SD',"ND")
#bound_trans$STATE$STATE_SET[[11]] <- c('WA','OR')
#bound_trans$STATE$STATE_SET[[8]] <- c('TX','NM',"OK",'KS')
#bound_trans$STATE$STATE_SET[[10]] <- c('CO','NE')

#field_to_states = st_intersects(bound_trans$`FIELD OFFICE`,bound_trans$STATE)
