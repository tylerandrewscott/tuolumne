
if(!require(tokenizers)){install.packages('tokenizers');require(tokenizers)}
if(!require(spacyr)){install.packages('spacyr');require(spacyr)}
if(!require(tigris)){install.packages('tigris');require(tigris)}
if(!require(maptools)){install.packages('maptools');require(maptools)}
if(!require(acs)){install.packages('acs');require(acs)}
if(!require(pbapply)){install.packages('pbapply');require(pbapply)}
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(sf)){install.packages('sf');require(sf)}
if(!require(doParallel)){install.packages('doParallel');require(doParallel)}

#spacy_install(conda = "auto", version = "latest", lang_models = "en",
#              python_version = "3.6", envname = "spacy_condaenv", pip = FALSE,
#              python_path = NULL, prompt = TRUE)

#spacyr::spacy_download_langmodel(model = model,)
model = 'en_core_web_sm'
spacyr::spacy_initialize(model)

floc = 'scratch/'
#saveloc = 'temp_products/'

cors = detectCores() - 3
cl = makeCluster(cors)
registerDoParallel(cl)

full_tlist <- readRDS('../bucket_mount/big_eis_text.rds')
top40pages = full_tlist[Page %in% 1:40,]


county_tigris = tigris::counties(class = 'sf',year = '2016')
state_tigris = tigris::states(class = 'sf',year = '2016')
county_tigris$STATE_NAME = state_tigris$NAME[match(county_tigris$STATEFP,state_tigris$STATEFP)]
county_tigris$NAME = ifelse(county_tigris$NAME != county_tigris$STATE_NAME,county_tigris$NAME,paste(county_tigris$NAME,'County'))

ngaz = fread('../bucket_mount/tuolumne/scratch/NationalFile_20191101.txt',sep = '|',fill = T,stringsAsFactors = F,quote = "")
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

clusterEvalQ(cl,{require(spacyr);require(data.table);
  model = 'en_core_web_sm'
  spacyr::spacy_initialize(model)
  #text_storage = 'input/filtered_text_files/'
  #ent_loc = 'scratch/boilerplate/project_place_entiities/'
})

clusterExport(cl , list('ngaz'))

clusterEvalQ(cl,{require(stringr)})

top40pages[grepl("^20190044",File),]

#table(file.exists(paste0(ent_loc,projects_used$PROJECT_ID,'.txt')))
fips_matches = foreach(i = top40pages$text[!str_remove(top40pages$File,'_.*') %in% geomatches$PROJECT_ID],j = top40pages$File[!str_remove(top40pages$File,'_.*') %in% geomatches$PROJECT_ID]) %dopar% {
   
  
  full_tlist[grepl('20160157',File),]
  
  oh 20160157
fips_matches = foreach(i = top40pages$text[!str_remove(top40pages$File,'_.*') %in% geomatches$PROJECT_ID],j = top40pages$File[!str_remove(top40pages$File,'_.*') %in% geomatches$PROJECT_ID]) %dopar% {
    i = top40pages$text[grepl('^20160157',top40pages$File)][1]
  
    top40pages$text[grepl('^20160157',top40pages$File)][1]
projects[PROJECT_ID=='20160157']
    id = str_remove(j,'_.*')
  pars = spacy_parse(i,entity = T,dependency = T,full_parse = TRUE,lemma = F,pos = F,nounphrase = T)
  ents = entity_consolidate(pars)
  filter_ents = ents[ents$entity_type%in%c('GPE','LOC',"FAC"),]
  states = unique(filter_ents$token[filter_ents$token %in% gsub('\\s','_',state.name)])
  if(length(states)==0){states = gsub("'",'',unique(filter_ents$token[gsub("'",'',filter_ents$token) %in% state.name]))}
  sab = state.abb[state.name %in% states]
  ngaz_filter = ngaz[ngaz$STATE_ALPHA %in% sab,]
  found_ents = filter_ents[filter_ents$token %in% ngaz_filter$FEATURE_NAME2,]
  if(nrow(found_ents)>0){
    found_ents$CFIPS <- ngaz_filter$CFIPS[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
    found_ents$FEATURE_ID <- ngaz_filter$FEATURE_ID[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
    found_ents$PRIM_LONG_DEC <- ngaz_filter$PRIM_LONG_DEC[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
    found_ents$PRIM_LAT_DEC <- ngaz_filter$PRIM_LAT_DEC[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
    found_ents$PROJECT_ID = id
  }
  found_ents
}


rbindlist(fips_matches,fill =T,use.names = T)  
geomatches = rbindlist(fips_matches,fill =T,use.names = T)  

old_fips_matches[!PROJECT_ID %in% geomatches$PROJECT_ID,][!duplicated(PROJECT_ID),]

old_fips_matches = readRDS('../bucket_mount/tuolumne/scratch/ngaz_matches.RDS')
old_fips_matches = old_fips_matches[PROJECT_ID %in% projects$PROJECT_ID,]

old_fips_matches[!PROJECT_ID %in% geomatches$PROJECT_ID,]

findset = which(!projects_used$PROJECT_ID %in% old_fips_matches$PROJECT_ID)

ent_loc = 'scratch/boilerplate/project_place_entiities/'
test = list.files(ent_loc)


#table(projects$AGENCY,projects$PROJECT_TYPE)
projects = fread('../bucket_mount/tuolumne/scratch/boilerplate/project_candidates_eis_only.csv')
projects = projects[Document=='Final',]
projects = projects[grepl('^201[3-9]|^2020',PROJECT_ID),]
documents = fread( '../bucket_mount/tuolumne/scratch/boilerplate/document_candidates_eis_only.csv')
documents = documents[PROJECT_ID %in% projects$PROJECT_ID,]

projects[!projects$PROJECT_ID %in% geomatches$PROJECT_ID,]

table(projects$PROJECT_ID %in% unique(str_remove(full_tlist$File,'_.*')))
full_tlist[grepl('20130090',File),]

id = projects_used$PROJECT_ID[i]
  
  
  if(!file.exists(paste0(ent_loc,id,'.txt'))){
    print(i)
    pfiles = files_used[PROJECT_ID == id,]
    if(nrow(pfiles)>0){
      temp_txt = rbindlist(lapply(1:nrow(pfiles),function(x) fread(paste0(text_storage,pfiles$text_name[x]),sep = '\t')))
      txt = substr(paste(temp_txt$text,collapse = ' '),1,400000)
      #state_eis = used_eis_state_list[match(dt$EIS[x],names(used_eis_state_list))]
      pars = spacy_parse(txt,entity = T,dependency = F,full_parse = TRUE,lemma = F,pos = F,nounphrase = T)
      ents = entity_consolidate(pars)
      filter_ents = ents[ents$entity_type%in%c('GPE','LOC',"FAC"),]
      states = unique(filter_ents$token[filter_ents$token %in% state.name])
      sab = state.abb[state.name %in% states]
      ngaz_filter = ngaz[ngaz$STATE_ALPHA %in% sab,]
      found_ents = filter_ents[filter_ents$token %in% ngaz_filter$FEATURE_NAME2,]
      if(nrow(found_ents)>0){
        found_ents$CFIPS <- ngaz_filter$CFIPS[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
        found_ents$FEATURE_ID <- ngaz_filter$FEATURE_ID[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
        found_ents$PRIM_LONG_DEC <- ngaz_filter$PRIM_LONG_DEC[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
        found_ents$PRIM_LAT_DEC <- ngaz_filter$PRIM_LAT_DEC[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
        found_ents$PROJECT_ID = id
      }
      rm(temp_text);rm(pars);gc(); rm(ents);rm(filter_ents);rm(txt);rm(ents);gc();
      if(nrow(found_ents)>0){found_ents}else{data.table(PROJECT_ID = id)}
      #cfreq = data.frame(table(state_ngaz_filter$CFIPS[ifelse(found_ents$entity %in% state_ngaz_filter$COUNTY_NAME,match(found_ents$entity,state_ngaz_filter$COUNTY_NAME),match(found_ents$entity,state_ngaz_filter$FEATURE_NAME))]),stringsAsFactors = F)
    }
  }
}


fips_matches_dt = rbindlist(fips_matches,use.names = T,fill = T)
fips_matches_dt[,STATEFP := str_extract(CFIPS,'^[0-9]{2}')]
matches_by_state_project = fips_matches_dt[,.N,by = .(PROJECT_ID,STATEFP)]  
half_of_max = matches_by_state_project[,max(N)/2,by = .(PROJECT_ID)]
matches_by_state_project$half_of_max = half_of_max$V1[match(matches_by_state_project$PROJECT_ID,half_of_max$PROJECT_ID)]
good_states = matches_by_state_project[N>=half_of_max,]

fips_matches_dt = fips_matches_dt[paste(PROJECT_ID,STATEFP) %in% paste(good_states$PROJECT_ID,good_states$STATEFP),] 
saveRDS(file = paste0(floc,'ngaz_matches.RDS'),object = fips_matches_dt)

stopCluster(cl)
stopImplicitCluster()

















text_storage = 'input/filtered_text_files/'
flist = list.files(text_storage)
projects_used = fread('scratch/boilerplate/projects_used.csv')
files_used = fread('scratch/boilerplate/documents_used.csv')
files_used$text_name = paste0(paste(files_used$PROJECT_ID,files_used$FILE_NAME,sep = '--'),'.txt')

ent_loc = 'scratch/boilerplate/project_place_entiities/'

geo_have = file.exists(paste0(floc,'ngaz_matches.RDS'))
if(!geo_have){findset = 1:nrow(projects_used);old_fips_matches = data.table()}
if(geo_have){
  old_fips_matches = readRDS(paste0(floc,'ngaz_matches.RDS'))
  findset = which(!projects_used$PROJECT_ID %in% old_fips_matches$PROJECT_ID)
}

clusterExport(cl , list('ngaz','projects_used','files_used'))


#table(file.exists(paste0(ent_loc,projects_used$PROJECT_ID,'.txt')))
fips_matches = foreach(i = findset) %dopar% {
 rm(df)
  id = projects_used$PROJECT_ID[i]
  if(!file.exists(paste0(ent_loc,id,'.txt'))){
    print(i)
    pfiles = files_used[PROJECT_ID == id,]
    if(nrow(pfiles)>0){
      temp_txt = rbindlist(lapply(1:nrow(pfiles),function(x) fread(paste0(text_storage,pfiles$text_name[x]),sep = '\t')))
      txt = substr(paste(temp_txt$text,collapse = ' '),1,400000)
      #state_eis = used_eis_state_list[match(dt$EIS[x],names(used_eis_state_list))]
      pars = spacy_parse(txt,entity = T,dependency = F,full_parse = TRUE,lemma = F,pos = F,nounphrase = T)
      ents = entity_consolidate(pars)
      filter_ents = ents[ents$entity_type%in%c('GPE','LOC',"FAC"),]
      states = unique(filter_ents$token[filter_ents$token %in% state.name])
      sab = state.abb[state.name %in% states]
      ngaz_filter = ngaz[ngaz$STATE_ALPHA %in% sab,]
      found_ents = filter_ents[filter_ents$token %in% ngaz_filter$FEATURE_NAME2,]
      if(nrow(found_ents)>0){
      found_ents$CFIPS <- ngaz_filter$CFIPS[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
      found_ents$FEATURE_ID <- ngaz_filter$FEATURE_ID[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
      found_ents$PRIM_LONG_DEC <- ngaz_filter$PRIM_LONG_DEC[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
      found_ents$PRIM_LAT_DEC <- ngaz_filter$PRIM_LAT_DEC[match(found_ents$token,ngaz_filter$FEATURE_NAME2)]
      found_ents$PROJECT_ID = id
      }
      rm(temp_text);rm(pars);gc(); rm(ents);rm(filter_ents);rm(txt);rm(ents);gc();
      if(nrow(found_ents)>0){found_ents}else{data.table(PROJECT_ID = id)}
      #cfreq = data.frame(table(state_ngaz_filter$CFIPS[ifelse(found_ents$entity %in% state_ngaz_filter$COUNTY_NAME,match(found_ents$entity,state_ngaz_filter$COUNTY_NAME),match(found_ents$entity,state_ngaz_filter$FEATURE_NAME))]),stringsAsFactors = F)
  }
  }
}


fips_matches_dt = rbindlist(fips_matches,use.names = T,fill = T)
fips_matches_dt[,STATEFP := str_extract(CFIPS,'^[0-9]{2}')]
matches_by_state_project = fips_matches_dt[,.N,by = .(PROJECT_ID,STATEFP)]  
half_of_max = matches_by_state_project[,max(N)/2,by = .(PROJECT_ID)]
matches_by_state_project$half_of_max = half_of_max$V1[match(matches_by_state_project$PROJECT_ID,half_of_max$PROJECT_ID)]
good_states = matches_by_state_project[N>=half_of_max,]

fips_matches_dt = fips_matches_dt[paste(PROJECT_ID,STATEFP) %in% paste(good_states$PROJECT_ID,good_states$STATEFP),] 
saveRDS(file = paste0(floc,'ngaz_matches.RDS'),object = fips_matches_dt)

  stopCluster(cl)
  stopImplicitCluster()
