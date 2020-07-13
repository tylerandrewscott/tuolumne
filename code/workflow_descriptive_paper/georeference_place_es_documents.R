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


#es_df$text[es_df$EIS_Number=='20140062']
es_files = list.files('input/eis_es_text/',full.names = T)
eis_record = fread('input/epa_master_repository/eis_record_detail.csv')
eis_record = eis_record[!is.na(eis_record$EIS.Number),]
county_tigris = tigris::counties(class='sf')
state_tigris = tigris::states(class='sf')
setnames(county_tigris,'GEOID','CFIPS')
county_tigris$CFIPS = formatC(county_tigris$CFIPS,width=5,flag=0)
county_tigris$STATE_NAME = state_tigris$NAME[match(county_tigris$STATEFP,state_tigris$STATEFP)]
#county_tigris$NAME = ifelse(county_tigris$NAME != county_tigris$STATE_NAME,county_tigris$NAME,paste(county_tigris$NAME,'County'))
county_tigris$NAMELSAD[grepl("Doña Ana",county_tigris$NAMELSAD)] <- "Dona Ana County"

#ngaz2 = read_delim('input/gazetteers/NationalFile_20181001.txt',delim = '|')
#dim(ngaz2)
eis_record <- eis_record[eis_record$Document.Type=='Final',]
eis_record$YEAR = str_extract(eis_record$EIS.Number,'^[0-9]{4}')
eis_record <- eis_record[eis_record$YEAR>2011,]

ngaz = fread('input/gazetteers/NationalFile_20181001.txt',sep = '|')
places = ngaz[ngaz$FEATURE_CLASS=="Populated Place",]
urban = st_read('spatial_input/cb_2016_us_ua10_500k.shp')
ua_place = fread('https://www2.census.gov/geo/docs/maps-data/data/rel/ua_place_rel_10.txt')
setnames(ua_place,c('UA'),c('UA_FIPS'))
ua_place$UA_FIPS <- formatC(ua_place$UA_FIPS,width=5,flag=0)
setnames(ua_place,c('GEOID'),c('PLACE_FIPS'))

ua_county = fread('https://www2.census.gov/geo/docs/maps-data/data/rel/ua_county_rel_10.txt')
setnames(ua_county,'GEOID','CFIPS')
setnames(ua_county,'UA','UA_FIPS')

census_places = fread('spatial_input/2016_Gaz_place_national.txt')
setnames(census_places,c('GEOID'),c('PLACE_FIPS'))
census_places$PLACE_FIPS <-formatC(census_places$PLACE_FIPS,width=max(nchar(census_places$PLACE_FIPS)),flag=0)

census_places$UA_FIPS <- ua_place$UA_FIPS[match(census_places$PLACE_FIPS,ua_place$PLACE_FIPS)]
ua_county$UA <- formatC(ua_county$UA,width=5,flag=0)
ua_county$CFIPS = formatC(ua_county$CFIPS,width=5,flag=0)


#census_places <- census_places[census_places$UA!='99999',]
census_places$COMMON_NAME <- gsub(' CDP$| city$| town$','',census_places$NAME)

es_df = readtext::readtext(list.files('input/eis_es_text/',full.names = T))
es_df = data.table(es_df)
es_df$EIS_Number = str_extract(es_df$doc_id,'[0-9]{1,}')
es_df$text = gsub('NB(?=\\,|\\.)','NE',es_df$text,perl = T)
es_df$State.or.Territory = eis_record$State.or.Territory[match(es_df$EIS_Number,eis_record$EIS.Number)]
es_df$States <- str_split(es_df$State.or.Territory,'-')
#names(es_df_state_list) <- es_df$EIS_Number         
#es_df_state_list = lapply(es_df_state_list,function(x) gsub(' ','',x))
es_df = es_df[!is.na(es_df$State.or.Territory),]

#state_involvement_post_2012 = as.data.frame(table(unlist(str_split(final_sub$State.or.Territory[final_sub$YEAR>=2012],'-')))) %>% arrange(-Freq)
es_df$Agency = eis_record$Agency[match(es_df$EIS_Number,eis_record$EIS.Number)]

model = 'en_core_web_sm'
spacyr::spacy_initialize(model)

TA <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0"
county_tigris <- st_transform(county_tigris,TA)

county_neighbors <- st_intersects(county_tigris)
county_tigris$Neighbors_CFIPS <- sapply(county_neighbors,function(x) county_tigris$CFIPS[x])

es_df <- es_df[!grepl("PRO|NAT",es_df$States),]
#es_df <- es_df[es_df$EIS_Number %in% eis_record$EIS.Number[!grepl('PROGRAMMATIC',eis_record$Title)],]

census_places$COMMON_NAME <- str_conv(census_places$COMMON_NAME,'utf8')
#es_df = es_df[grepl('CA',es_df$States),]
census_places <- census_places[!grepl('CDP$',NAME),]
census_places <- census_places[!census_places$COMMON_NAME %in% month.name,]


fips_matches = pblapply(seq_along(es_df$text),function(x) {
  temp_text = es_df$text[x]
  eis_record[eis_record$EIS.Number== es_df$EIS_Number[x],]
  #state_eis = es_df_state_list[match(es_df$EIS_Number[x],names(es_df_state_list))]
  #pars = spacy_parse(temp_text,entity = T,dependency = F,full_parse = TRUE,nounphrase = T)
  place_match =   sapply(census_places$COMMON_NAME[census_places$USPS %in% es_df$States[[x]]],function(x) grepl(paste('\\b',x,'\\b'),temp_text))
  place_count =   sapply(census_places$COMMON_NAME[census_places$USPS %in% es_df$States[[x]]],function(x) str_count(temp_text,paste('\\b',x,'\\b')))
 if(length(place_count)){
 pmdf = as.data.frame(place_count[place_match])
 pmdf$PLACE_FIPS <- census_places$PLACE_FIPS[census_places$USPS %in% es_df$States[[x]]][place_match]
 pmdf$COMMON_NAME <- rownames(pmdf) 
 pmdf = pmdf %>% rename(count = `place_count[place_match]`)
  rm(temp_text);rm(pars);gc()
  if(nrow(pmdf)==0){
    df  = data.frame(EIS_Number = es_df$EIS_Number[x], count = NA,PLACE_FIPS =NA,COMMON_NAME=NA,stringsAsFactors = F)}
  if(nrow(pmdf)>0){
    df = data.frame(EIS_Number = es_df$EIS_Number[x], pmdf,stringsAsFactors = F)}
  rm(pars);rm(ents);rm(temp_text);rm(filter_ents);gc();
  data.table(df)}},cl = 10)


place_match_df = rbindlist(fips_matches)


# 
# 
# ngaz$CFIPS = ifelse(!is.na(ngaz$COUNTY_NUMERIC),paste0(formatC(ngaz$STATE_NUMERIC,width = 2,flag = '0'),formatC(ngaz$COUNTY_NUMERIC,width = 3,flag = '0')),NA)
# ngaz = ngaz[!ngaz$FEATURE_NAME %in% c('Pacific Ocean','South Pacific Ocean','Atlantic Ocean','Gulf of Mexico'),]
# ngaz = ngaz[!is.na(ngaz$FEATURE_CLASS),]
# ngaz = ngaz[ngaz$STATE_ALPHA %in% c(state.abb,'DC','PR'),]
# ngaz = ngaz[!is.na(ngaz$COUNTY_NUMERIC),]
# drop_types = c('Tower')
# ngaz = ngaz[!ngaz$FEATURE_CLASS %in% drop_types,]
# ngaz$COUNTY_NAME = county_tigris$NAME[match(ngaz$CFIPS,county_tigris$GEOID)]
# ngaz = ngaz[ngaz$CFIPS%in% county_tigris$GEOID,]
# ngaz$STATE_NAME = state_tigris$NAME[match(ngaz$STATE_ALPHA,state_tigris$STUSPS)]
# ngaz = ngaz[ngaz$STATE_NAME != ngaz$FEATURE_NAME,]
# ngaz = ngaz[!ngaz$FEATURE_NAME %in% c('River','Forest','Lake','Field','Beach','Mountain'),]
# gc()

saveRDS(file = 'scratch/descriptive_paper/place_matches.RDS',object = place_match_df)
  
  