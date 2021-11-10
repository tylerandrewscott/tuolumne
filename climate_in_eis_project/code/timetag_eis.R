pack = c('tokenizers','spacyr','tigris','maptools','acs','pbapply','data.table',
         'sf','doParallel')
need = pack[!pack %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(pack,require,character.only=T)

projects = readRDS('climate_in_eis_project/data_products/deis_metadata_with_covariates.RDS')
documents = readRDS('climate_in_eis_project/data_products/deis_doc_metadata.RDS')

#spacy_install(conda = "auto", version = "latest", lang_models = "en",
#              python_version = "3.6", envname = "spacy_condaenv", pip = FALSE,
#              python_path = NULL, prompt = TRUE)

#spacyr::spacy_download_langmodel(model = model,)
model = 'en_core_web_sm'
spacyr::spacy_initialize(model)

floc = 'scratch/'
#saveloc = 'temp_products/'

cors = detectCores() - 4
cl = makeCluster(cors)
registerDoParallel(cl)

corpus_files = list.files('climate_in_eis_project/input/',pattern = 'corpus',full.names = T)

year_stretches= lapply(seq_along(corpus_files),function(x) {
  x = 4
print(corpus_files[x])
temp = readRDS(corpus_files[x])
index = grep('project lifespan',temp$text)

test = str_extract(temp$text,'[0-9]{1,}-year.{30}')

str_extract(temp$text,'[0-9]{1,}-year life')
str_extract(temp$text,'[0-9]{1,}-year operations')
str_extract(temp$text,'.{10}[0-9]{1,}-year timespan.{10}')

test[!is.na(test)]
grep('period from',test[!is.na(test)],value = T)

temp$text[index]

period_extract = str_extract_all(temp$text,'\\b20[0-9]{2}(-|\\s(to|through)\\s)20[0-9]{2}\\b|(in|next|within) [0-9]{1,} years')
eis_extract = str_extract(temp$File,'^[0-9]{8}')
tt = lapply(seq_along(period_extract),function(i) {
  if(length(period_extract[[i]])>0){
    data.table(period_extract[[i]],eis_extract[i])
  }
})
tt1 = rbindlist(tt)
tt1$start = as.numeric(str_remove(tt1$V1,'(-|\\s(to|through)\\s).*'))
tt1$end = as.numeric(str_remove(tt1$V1,'.*(-|\\s(to|through)\\s)'))
tt1$year_pub = as.numeric(str_extract(tt1$V2,'^[0-9]{4}'))
tt1 <- tt1[start<=end,]
tt1 <- tt1[start>=year_pub,]
tt1$end = tt1$end - tt1$year_pub
tt1$start = tt1$start - tt1$year_pub
tt1})


year_stretch = rbindlist(year_stretches)
year_by_id = mapply(function(x,y,z) 
  {cbind(year = seq(x,y,1),id = z)},
  x = year_stretch$start,
  y = year_stretch$end,
  z = year_stretch$V2)

year_dt = rbindlist(lapply(year_by_id,data.table))
year_dt$year <- as.numeric(year_dt$year)


test = readRDS(corpus_files[9])
tt = test[grepl('20210011',File),]


tt[54,]

ss = str_extract_all(tt$text,'(in|next|within) [0-9]{1,} years')
ss[!is.na(ss)]

tt[Page==57,]
projects[!EIS.Number %in% year_dt[order(id),]$id,] 

projects[!EIS.Number %in% year_dt[order(id),]$id,] %>% tail()
projects[]


plot(density(year_dt[,mean(year),by=.(id)]$V1))

projects[1:10,]


sapply(1:nrow(year))
year_stretch$vec = year_stretch$start:year_stretch:end

year_stretch
tt1[,list(median(start),median(end)),by=.(V2)]



ggplot(tt1) + geom_point(aes(x= as.factor(V2),y = end))


vec = grep('2070-2099',temp$text)

temp$text[vec[4]]
temp$File[vec[4]]


ggplot(tt1[,list(mean = mean(end),median = median(end)),by=.(V2)]) + 
  geom_density(aes(x = median)) + geom_dotplot(aes(x = median),binwidth = 0.25)
tt



temp[grepl('20170000',temp$File)&grepl('2030',temp$text),]


unlist(period_extract)                  
                                 
                                 
                                 


unlist(period_extract)

date_ent_list = lapply(corpus_files,function(f) {
  f = corpus_files[5]
  print(f)
  temp <- readRDS(f)
  
  temp_split <- split(x = temp,f = str_extract(temp$File,'^[0-9]{8}'))


  sub_list = pblapply(seq_along(temp_split)[1:10],function(d){
    pars = spacy_parse(temp_split[[d]]$text,entity = T,dependency = T,full_parse = TRUE,lemma = F,pos = F,nounphrase = T)
    ents = entity_consolidate(pars)
    date_ents = ents[ents$entity_type == 'DATE',]
    if(nrow(date_ents)>0){
    date_ents$FILE <- names(temp_split)[d]
    date_ents}},cl = 4)
  sub_list})

test = rbindlist( sub_list)
extract_year = str_extract(test$token,'[^A-Za-z0-9][0-9]{4}[^A-Za-z0-9]')
which(!is.na(extract_yearextract_year=='2030')
which(str_extract(extract_year,'[0-9]{4}')=='2030')
test[2706,]
test %>% group_by(token) %>% tally()
table(extract_year)
library(lubridate)
extract_year




table(str_extract(test$FILE,'[0-9]{8}'))
top50pages <- empty_dt

county_tigris = tigris::counties(class = 'sf',year = '2017')
state_tigris = tigris::states(class = 'sf',year = '2017')
county_tigris$STATE_NAME = state_tigris$NAME[match(county_tigris$STATEFP,state_tigris$STATEFP)]
county_tigris$NAME = ifelse(county_tigris$NAME != county_tigris$STATE_NAME,county_tigris$NAME,paste(county_tigris$NAME,'County'))

clusterEvalQ(cl,{require(spacyr);require(data.table);require(stringr);
  model = 'en_core_web_sm'
  spacyr::spacy_initialize(model)
  #text_storage = 'input/filtered_text_files/'
  #ent_loc = 'scratch/boilerplate/project_place_entiities/'
})
gc()

td = tempdir()
url = 'https://geonames.usgs.gov/docs/stategaz/NationalFile.zip'
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('txt$',fname$Name,value=T))
ngaz = fread(fpath,sep = '|',fill = T,stringsAsFactors = F,quote = "")
ngaz$CFIPS = ifelse(!is.na(ngaz$COUNTY_NUMERIC),paste0(formatC(ngaz$STATE_NUMERIC,width = 2,flag = '0'),formatC(ngaz$COUNTY_NUMERIC,width = 3,flag = '0')),NA)
ngaz = ngaz[!ngaz$FEATURE_NAME %in% c('Pacific Ocean','South Pacific Ocean','Atlantic Ocean','Gulf of Mexico','Caribbean Sea'),]
ngaz = ngaz[!is.na(ngaz$FEATURE_CLASS),]
ngaz = ngaz[ngaz$STATE_ALPHA %in% c(state.abb,'DC','PR'),]
ngaz = ngaz[!is.na(ngaz$COUNTY_NUMERIC),]
drop_types = c('Tower')
ngaz = ngaz[!ngaz$FEATURE_CLASS %in% drop_types,]
ngaz$COUNTY_NAME = county_tigris$NAME[match(ngaz$CFIPS,county_tigris$GEOID)]
ngaz = ngaz[ngaz$CFIPS%in% county_tigris$GEOID,]
ngaz$STATE_NAME = state_tigris$NAME[match(ngaz$STATE_ALPHA,state_tigris$STUSPS)]
ngaz = ngaz[ngaz$STATE_NAME != ngaz$FEATURE_NAME,]
ngaz = ngaz[!ngaz$FEATURE_NAME %in% c('River','Wilderness','Forest','Lake','Field','Beach','Mountain','Pacific','Atlantic','Earth','Acres','North','South','East','West',state.name),]
gc()
ngaz$FEATURE_NAME2 = str_replace_all(ngaz$FEATURE_NAME,pattern = '\\s','_')

fls = unique(top50pages$File)
geo_file = 'climate_in_eis_project/data_products/geo_temp.rds'
if(file.exists(geo_file)){existing_geo = readRDS(geo_file)
multis = projects[projects$State=='Multi',]
existing_geo = existing_geo[!PROJECT_ID %in% multis$PROJECT_ID,]}else{existing_geo = NULL}

fls = fls[!fls %in% existing_geo$FILE]
text_sets = split(top50pages$text[top50pages$File %in% fls],top50pages$File[top50pages$File %in% fls])

fips_matches = foreach(i = text_sets,f = names(text_sets)) %dopar% {
  id = str_remove(f,'_.*')
  text = i
  pars = spacy_parse(text,entity = T,dependency = T,full_parse = TRUE,lemma = F,pos = F,nounphrase = T)
  ents = entity_consolidate(pars)
  filter_ents = ents[ents$entity_type%in%c('GPE','LOC',"FAC"),]
  if(nrow(filter_ents)>0){filter_ents$PROJECT_ID = id;filter_ents$FILE = f}
  filter_ents
}

require(pbapply)
filtered_fips_matches = pblapply(fips_matches,function(filter_ents) {
  states_proj = projects$State.or.Territory[unique(filter_ents$PROJECT_ID) == projects$EIS.Number]
  states_proj = unlist(str_split(states_proj,'-'))
  if(all(states_proj %in% state.abb)){sab = states_proj}else{
  states = unique(filter_ents$token[filter_ents$token %in% gsub('\\s','_',state.name)])
  if(length(states)==0){states = gsub("'",'',unique(filter_ents$token[gsub("'",'',filter_ents$token) %in% state.name]))}
  sab = state.abb[str_replace(state.name,'\\s','_') %in% states]
  }
  ngaz_filter = ngaz[ngaz$STATE_ALPHA %in% sab,]
  found_ents = filter_ents[filter_ents$token %in% ngaz_filter$FEATURE_NAME2,]
  if(nrow(found_ents)>0){
    found_ents$CFIPS <- ngaz_filter$CFIPS[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
    found_ents$FEATURE_ID <- ngaz_filter$FEATURE_ID[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
    found_ents$PRIM_LONG_DEC <- ngaz_filter$PRIM_LONG_DEC[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
    found_ents$PRIM_LAT_DEC <- ngaz_filter$PRIM_LAT_DEC[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
  }
  found_ents
},cl=cors)

new_geomatches = rbindlist(filtered_fips_matches,fill =T,use.names = T)
new_geomatches[,STATEFP := str_extract(CFIPS,'^[0-9]{2}')]
matches_by_state_project = new_geomatches[,.N,by = .(PROJECT_ID,STATEFP)]  
matches_by_state_project$STATE.ABB = fips.state$STUSAB[match(matches_by_state_project$STATEFP,formatC(fips.state$STATE,width = 2,flag = '0'))]
half_of_max = matches_by_state_project[,max(N)/2,by = .(PROJECT_ID)]
matches_by_state_project$half_of_max = half_of_max$V1[match(matches_by_state_project$PROJECT_ID,half_of_max$PROJECT_ID)]
projects$state_abb_list = str_split(projects$State.or.Territory,'-')
matches_by_state_project$listed_state = as.vector(mapply(function(x,y) x %in% y,x = matches_by_state_project$STATE.ABB,projects$state_abb_list[match(matches_by_state_project$PROJECT_ID,projects$EIS.Number)],SIMPLIFY = T))

good_states = matches_by_state_project[N>=half_of_max|listed_state,]
new_geomatches = new_geomatches[paste(PROJECT_ID,STATEFP) %in% paste(good_states$PROJECT_ID,good_states$STATEFP),] 
full_geo = rbind(existing_geo,new_geomatches,use.names = T,fill = T)
full_geo = full_geo[PRIM_LAT_DEC!=0,]

saveRDS(full_geo,geo_file)



