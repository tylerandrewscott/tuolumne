pack = c('tokenizers','spacyr','tigris','maptools','acs','pbapply','data.table','sf','doParallel')
need = pack[!pack %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(pack,require,character.only=T)

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
projects = fread('boilerplate_project/data_products/project_candidates_eis_only.csv')
full_tlist <- readRDS('boilerplate_project/input/feis_corpus_2013-2020.rds')

top40pages = full_tlist[Page %in% 1:50,]

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
adm_url = "https://geonames.usgs.gov/docs/stategaz/NationalFile.zip"
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(adm_url, tf)
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

fls = unique(top40pages$File)
geo_file = 'boilerplate_project/data_products/geo_temp.rds'
if(file.exists(geo_file)){existing_geo = readRDS(geo_file)}else{existing_geo = NULL}

multis = projects[projects$State=='Multi',]
existing_geo = existing_geo[!PROJECT_ID %in% multis$PROJECT_ID,]

fls = fls[!fls %in% existing_geo$FILE]
text_sets = split(top40pages$text[top40pages$File %in% fls],top40pages$File[top40pages$File %in% fls])

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

# 
# geomatches = readRDS(geo_file)
# projects = fread('boilerplate_project/data_products/project_candidates_eis_only.csv')
# projects = projects[Document=='Final',]
# projects = projects[grepl('^201[3-9]|^2020',PROJECT_ID),]
# projects = projects[!duplicated(EIS.Number),]
# documents = fread( 'boilerplate_project/data_products/document_candidates_eis_only.csv')
# documents = documents[PROJECT_ID %in% projects$PROJECT_ID,]
# 
# have_docs = projects$EIS.Number[projects$EIS.Number %in% str_remove(full_tlist$File,'_.*')]
# have_geo = projects$EIS.Number[projects$EIS.Number %in% geomatches$PROJECT_ID]


#keep_fips = state_tigris$STATEFP[!state_tigris$STUSPS%in%c('AK','HI','GU','MU','PR','VI','AS','MP')]

# ggplot() + ggtitle('Geotagged locations in EIS documents','(excluding AK and HI)') + 
#   geom_sf(data = state_tigris[state_tigris$STATEFP %in% keep_fips,],fill = 'white') + 
#   theme_map() + 
#   geom_point(pch = 21,data = geomatches[str_extract(geomatches$CFIPS,'^[0-9]{2}') %in% keep_fips,],aes(x = PRIM_LONG_DEC,y = PRIM_LAT_DEC),alpha = 0.15)
