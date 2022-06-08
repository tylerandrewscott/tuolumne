
library(readtext)
library(tokenizers)
library(spacyr)
library(tigris)
library(maptools)
library(acs)
library(tidyverse)
library(pbapply)
es_files = list.files('input/eis_es_text/',full.names = T)

eis_record = read_csv('input/epa_master_repository/eis_record_detail.csv')
eis_record = eis_record[!is.na(eis_record$EIS.Number),]
county_tigris = tigris::counties()
state_tigris = tigris::states()
county_tigris$STATE_NAME = state_tigris$NAME[match(county_tigris$STATEFP,state_tigris$STATEFP)]
county_tigris$NAME = ifelse(county_tigris$NAME != county_tigris$STATE_NAME,county_tigris$NAME,paste(county_tigris$NAME,'County'))

ngaz = read_delim('input/gazetteers/NationalFile_20181001.txt',delim = '|')
ngaz$CFIPS = ifelse(!is.na(ngaz$COUNTY_NUMERIC),paste0(formatC(ngaz$STATE_NUMERIC,width = 2,flag = '0'),formatC(ngaz$COUNTY_NUMERIC,width = 3,flag = '0')),NA)
ngaz = ngaz[!ngaz$FEATURE_NAME %in% c('Pacific Ocean','South Pacific Ocean','Atlantic Ocean','Gulf of Mexico'),]
ngaz = ngaz[!is.na(ngaz$FEATURE_CLASS),]
ngaz = ngaz[ngaz$STATE_ALPHA %in% c(state.abb,'DC','PR'),]
ngaz = ngaz[!is.na(ngaz$COUNTY_NUMERIC),]
drop_types = c('Tower')
ngaz = ngaz[!ngaz$FEATURE_CLASS %in% drop_types,]
ngaz$COUNTY_NAME = county_tigris$NAME[match(ngaz$CFIPS,county_tigris$GEOID)]
ngaz = ngaz[ngaz$CFIPS%in% county_tigris$GEOID,]
ngaz$STATE_NAME = state_tigris$NAME[match(ngaz$STATE_ALPHA,state_tigris$STUSPS)]
ngaz = ngaz[ngaz$STATE_NAME != ngaz$FEATURE_NAME,]
ngaz = ngaz[!ngaz$FEATURE_NAME %in% c('River','Forest','Lake','Field','Beach','Mountain'),]
gc()


es_df = readtext::readtext(list.files('input/eis_es_text/',full.names = T))
es_df$EIS_Number = str_extract(es_df$doc_id,'[0-9]{1,}')
es_df$text = gsub('NB(?=\\,|\\.)','NE',es_df$text,perl = T)
es_df$State.or.Territory = eis_record$State.or.Territory[match(es_df$EIS_Number,eis_record$EIS.Number)]
es_df_state_list = str_split(es_df$State.or.Territory,'-')
names(es_df_state_list) <- es_df$EIS_Number         
es_df_state_list = lapply(es_df_state_list,function(x) gsub(' ','',x))
es_df = es_df[!is.na(es_df$State.or.Territory),]

eis_record$YEAR = str_extract(eis_record$EIS.Number,'^[0-9]{4}')
final_sub = eis_record[eis_record$Document.Type == 'Final',]
state_involvement_post_2012 = as.data.frame(table(unlist(str_split(final_sub$State.or.Territory[final_sub$YEAR>=2012],'-')))) %>% arrange(-Freq)
es_df$Agency = eis_record$Agency[match(es_df$EIS_Number,eis_record$EIS.Number)]

#model = 'en_core_web_lg'
library(spacyr)
spacy_initialize()

#fips_matches = pblapply(seq_along(es_df$text),function(x) {
fips_matches = do.call(rbind,pblapply(seq_along(es_df$text),function(x) {
  temp_text = es_df$text[x]
  state_eis = es_df_state_list[match(es_df$EIS_Number[x],names(es_df_state_list))]
  pars = spacy_parse(temp_text,entity = T,dependency = F,full_parse = TRUE)
  ents = entity_extract(pars,type = 'all')
  filter_ents = ents[ents$entity_type%in%c('PERSON','GPE','LOC',"FAC") ,]
  state_ngaz_filter = ngaz[ngaz$STATE_ALPHA%in%unlist(state_eis),]
  found_ents = filter_ents[filter_ents$entity %in% state_ngaz_filter$FEATURE_NAME | filter_ents$entity %in% state_ngaz_filter$COUNTY_NAME,] 
  
  ngaz_found = state_ngaz_filter[ifelse(found_ents$entity %in% state_ngaz_filter$COUNTY_NAME,
         match(found_ents$entity,state_ngaz_filter$COUNTY_NAME),
         match(found_ents$entity,state_ngaz_filter$FEATURE_NAME)),]

  rm(temp_text);rm(pars);gc()
  if(nrow(ngaz_found)==0){
    df  = NULL}
  if(nrow(ngaz_found)>0){
    df = ngaz_found %>% mutate(EIS_Number = es_df$EIS_Number[x],Agency = es_df$Agency[x])}
  rm(pars);rm(ents);rm(temp_text);rm(filter_ents);gc();
  df},cl = 4))
  
  saveRDS(file = 'scratch/descriptive_paper/fips_matches_with_place_points.RDS',object = fips_matches)
  
  