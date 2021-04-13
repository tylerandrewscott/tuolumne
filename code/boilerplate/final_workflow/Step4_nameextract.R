if(!require(readtext)){install.packages('readtext');require(readtext)}
if(!require(tokenizers)){install.packages('tokenizers');require(tokenizers)}
if(!require(pbapply)){install.packages('pbapply');require(pbapply)}
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(doParallel)){install.packages('doParallel');require(doParallel)}
if(!require(stringr)){install.packages('stringr');require(stringr)}
if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(pdftools)){install.packages('pdftools');require(pdftools)}
if(!require(googleLanguageR)){install.packages('googleLanguageR')}
if(!require(rvest)){install.packages('rvest')}
#setwd('bucket_mount/tuolumne/')

project = 'scott-davis-remote'
zone = 'us-west1-a'
account_key = '../../../Box/tuolumne/scratch/scott-davis-remote-56e7dae929b7.json'

Sys.setenv(GCE_AUTH_FILE = account_key,
           GCE_DEFAULT_PROJECT_ID = project,
           GCE_DEFAULT_ZONE = zone)
library(googleLanguageR)
googleLanguageR::gl_auth(json_file = account_key)


pymat = fread('scratch/boilerplate/python_spacy_matcher_person_names.csv')
text_storage = 'input/filtered_text_files/'
flist = list.files(text_storage)
projects_used = fread('scratch/boilerplate/project_candidates_eis_only.csv')
files_used = fread('scratch/boilerplate/document_candidates_eis_only.csv')
tlist = list.files('../eis_documents/enepa_repository/text_as_datatable/',full.names = T, recursive = T)
#entity_file = 'scratch/boilerplate/entity_extraction_results.rds'
#if(!file.exists(entity_file)){ents =list()}else{ents = readRDS(entity_file)}
files_used$FILE_NAME <- str_replace(files_used$FILE_NAME,'\\.pdf$','\\.txt')
#files_used = files_used[!FILE_NAME %in% ents$FILE_NAME,]
keep_tlist = tlist[basename(tlist) %in% files_used$FILE_NAME]


pages_list = pblapply(keep_tlist,function(i) {
  print(i)
  base_file = basename(i)
  id = str_remove(basename(base_file),'_.*')
  #start_dt = data.table(FILE_NAME = base_file,PROJECT_ID = id,people = list(),orgs = list())
  temp_txt =  fread(i,sep = '\t')
  temp_txt$FILE = base_file
  temp_txt$keep = temp_txt$keep2 = 0
  prep_papers = grepl('Preparers|Contributors|PREPARERS|CONTRIBUTORS|Team Members|CONSULTANTS|PLANNING TEAM MEMBERS|Interdisciplinary Team|INTERDISCIPLINARY TEAM',temp_txt$text)&!grepl('\\.{7,}|TABLE OF CONTENTS|Table of Contents|^CONTENTS|Date Received|Response|RESPONSE',temp_txt$text)
  temp_txt$keep[prep_papers] <- 1
  prep_papers2 = grepl('^Preparers|^Contributors|^PREPARERS|^CONTRIBUTORS|^Team Members|^CONSULTANTS|^PLANNING TEAM MEMBERS|^Interdisciplinary Team|^INTERDISCIPLINARY TEAM',temp_txt$text)&!grepl('\\.{7,}|TABLE OF CONTENTS|Table of Contents|^CONTENTS|Date Received|Response|RESPONSE',temp_txt$text)
  temp_txt$keep2[prep_papers2] <- 1
  temp_txt = temp_txt[!duplicated(text),]
  temp_txt = temp_txt[!grepl('\\.{7,}',text),]
  if(any(temp_txt$keep!=0)){
    add = sort(unique(c(sapply(which(temp_txt$keep==1), function(x) x + 0:2))))
    add = add[add<=nrow(temp_txt)]
    temp_txt$keep[add]<-1
  }
  if(any(temp_txt$keep2!=0)){
    add = sort(unique(c(sapply(which(temp_txt$keep2==1), function(x) x + 0:2))))
    add = add[add<=nrow(temp_txt)]
    temp_txt$keep2[add]<-1
  }
    temp_txt = temp_txt[keep==1|keep2==1,]
    temp_txt = temp_txt[!grepl('\\.{8,}',text),]
    temp_txt},cl = 4)
  

pages_dt = rbindlist(pages_list,use.names = T)
pages_dt$USE_KEEP2 = pages_dt$FILE %in% pages_dt[,list(sum(keep),sum(keep2)),by=.(FILE)][V1>10&V2>0]$FILE

pages_dt = pages_dt[{!USE_KEEP2}|keep2==1,]



pages_by_file = split(pages_dt$Page,pages_dt$FILE)
pdf_files = list.files('../eis_documents/enepa_repository/documents/',full.names =T,recursive = T)
index = match(gsub('\\.txt$','.pdf',names(pages_by_file)),basename(pdf_files))

cluster = makeCluster(5)
registerDoParallel(cluster)
clusterEvalQ(cluster,require(data.table))
clusterEvalQ(cluster,require(pdftools))
clusterEvalQ(cluster,require(stringr))
clusterExport(cluster,varlist = list('pdf_files'))

clusterEvalQ(cluster,{
model = 'en_core_web_sm'
require(spacyr)
spacy_initialize(model)
})


# file_text = foreach(p = pages_by_file,f = names(pages_by_file),i = index) %dopar% {
# gc()
#   full_text = pdf_text(pdf_files[i])
#   tx_string = full_text[p]
#   tx_split_string = unlist(str_split(tx_string,'\\n'))
#   tx_split_string = gsub('(^[A-Z][a-z]+)\\,(\\s[A-Z][a-z]+)','\\1\\2',tx_split_string)
#   doc = spacyr::spacy_parse(tx_split_string)
#   spc = spacyr::entity_consolidate(doc)
#   if(nrow(spc)>0){
#   spc$FILE = f
#   spc}}
# 
# saveRDS(file_text,paste0('scratch/boilerplate/raw_entity_pull_',Sys.Date(),'.RDS'))

file_text = readRDS(sort(list.files('scratch/boilerplate/',pattern = 'raw_entity_pull_',full.names = T),decreasing = T)[1])
ent_dt = rbindlist(file_text)
ent_dt$token = str_remove(ent_dt$token,'\\s{4,}.*')
ent_dt = ent_dt[entity_type == 'PERSON',]
ent_dt = ent_dt[!grepl('et_al',token),]
ent_dt = ent_dt[grepl('_',token),]
ent_dt = ent_dt[grepl('[A-Z]',token),]
ent_dt$token <- str_remove(ent_dt$token,'\\s+$')
ent_dt$token <- str_remove_all(ent_dt$token,'\\s')
ent_dt$token <- str_remove(ent_dt$token,'_{1,}$')
ent_dt$token <- str_remove(ent_dt$token,'^_{1,}')
ent_dt = ent_dt[!grepl('^[A-Z]\\._[A-Z]',token),]
ent_dt = ent_dt[!grepl('^[A-Z]\\.[A-Z]\\._[A-Z]',token),]
ent_dt = ent_dt[grepl('_',token),]
ent_dt = ent_dt[!grepl('^[0-9]',token),]
ent_dt = ent_dt[!grepl('[0-9]{3,}',token),]
ent_dt = ent_dt[!grepl('County',token),]
ent_dt = ent_dt[!grepl('District',token),]
ent_dt = ent_dt[!grepl('^sandy_',token),]

ent_dt = ent_dt[!grepl('Biologist|Planner|Engineer|Environmental|Biology|Wildlife|Grazing|Canyon|Physical|Technician|Land_Use|Nez_Perce|Anchorage|Alaska|Pacific|Oxide|Harvest|Apache|Ranger|Reviewer|Landscape|Geography|Polytechnic|Shoshone|Draft|^Map_|Salmon|Stakeholder|Watershed|Scientist|Fisheries|Design|Specialist|Appendix|Riparian|EIS|Ecologist|University|Raw_Score',token),]


require(tigris)
ngaz = fread('scratch/NationalFile_20210101.txt',sep = '|',fill = T,stringsAsFactors = F,quote = "")
ngaz$CFIPS = ifelse(!is.na(ngaz$COUNTY_NUMERIC),paste0(formatC(ngaz$STATE_NUMERIC,width = 2,flag = '0'),formatC(ngaz$COUNTY_NUMERIC,width = 3,flag = '0')),NA)
ngaz = ngaz[!ngaz$FEATURE_NAME %in% c('Pacific Ocean','South Pacific Ocean','Atlantic Ocean','Gulf of Mexico','Caribbean Sea'),]
ngaz = ngaz[!is.na(ngaz$FEATURE_CLASS),]
ngaz = ngaz[ngaz$STATE_ALPHA %in% c(state.abb,'DC','PR'),]
ngaz = ngaz[!is.na(ngaz$COUNTY_NUMERIC),]
drop_types = c('Tower')
ngaz = ngaz[!ngaz$FEATURE_CLASS %in% drop_types,]
#ngaz = ngaz[!ngaz$FEATURE_NAME %in% c('River','Wilderness','Forest','Lake','Field','Beach','Mountain','Pacific','Atlantic','Earth','Acres','North','South','East','West',state.name),]
gc()
ngaz$FEATURE_NAME2 = str_replace_all(ngaz$FEATURE_NAME,pattern = '\\s','_')

ent_dt = ent_dt[!token %in% ngaz$FEATURE_NAME2,]
ent_dt = ent_dt[!grepl('[A-Z]{3,}',token),]
ent_dt = ent_dt[!grepl('[0-9]{2,}',token),]
ent_dt = ent_dt[!grepl('^([A-Z]\\.){2,}',token),]
ent_dt$token = str_remove(ent_dt$token,'_(–|-|—).*')
ent_dt$token = str_remove(ent_dt$token,'(__|_\\().*')


ent_dt = ent_dt[!grepl('_[a-z]',ent_dt$token),]
ent_dt = ent_dt[grepl('_',token),]
ent_dt$PROJECT_ID = str_remove(ent_dt$FILE,'_.*')
ent_dt= ent_dt[token %in% ent_dt[!duplicated(paste(token,PROJECT_ID)),][,.N,by=.(token)][order(-N)][N>1,]$token,]

bad_ents = c('QC_Reference','Date_Received','Wildlife_Biologist',"Environmental_Scientist" ,"Wetland_Geomorphic_Setting"   ,"Environmental_Science" ,
             "Land_Use"   , "Date_Received"   ,"QC_Reference"     , "Wetland_Site_Name" ,"Automatic_Hammer"  ,"Landscape_Architect"  ,"STLC_DI"  ,"Soil_Scientist" ,
             "CDOT_Geot","Wet_Sand_/_Muck_Flats" , "Landscape_Architecture" ,"Marine_Science"  ,"District_Ranger" ,"Colorado_Parks"   ,"Wildlife_Science" ,
             "Kootenai_Tribes" ,"YEH_ASSOCIATES_W_LONG", "Soil_Science", "Earthstar_Geographics"     ,  "Hydrologically_Disconnected","Park_Ranger" ,"STLC_DI__Complete"  ,
             "Endosulfan_II" ,"Endosulfan_Sulfate" ,"STLC_DI__Complete"  , "Douglas_-_fir" ,"Heptachlor_Epoxide","Wildlife_Biology","Land_Use_Permitting",
             "Noise_Specialist","Build_Alternatives","Appendix_D","Realty_Specialist","Virginia_Tech","Wild_Horses","Technical_Reviewer","Endrin_ketone__<","Reduce_Bat_Fatalities" ,
             "Medicine_Bow" ,   "Mark_Udall" , "Juris_Doctor","Planetary_Science"  , "Reducing_Avian_Collisions",    "P.E.__M.S."   , "Mule_Deer",
             "Date_Received__08/08/16","SANDY_CLAYSTONE","Tetra_Tech","Booz_Allen_Hamilton","NAVFAC_Northwest","Cardno_ENTRIX","Raw_Score_/36", "Technical_Memorandum" ,
             "Southeast_Alaska","I.__Soils","Soil_Profile","Hydrologically__Disconnected","Grassy_Forks","Stream_Photograph","Buffalo_PRMP","John_Hickenlooper",
             "Casa_Diablo_IV","Noise_Analyst","Stream_Photograph","Peckham_Ave","Map_ID_Site__Database(s","John_Barrasso","Mt._Baker","Groundwater_Resources",
             "Canada_Lynx","Ute_Mountain_Ute","Dianne_Feinstein","Northwest_Science","Kootenai_Tribe","Word_Processor","Graphic_Artist","Dick_Artley","Jeff_Merkley",
             "Noise_Analysis","Hazardous_Waste","Barbara_Boxer","Juris_Doctorate","Shoshone","Potomac_Yard","A-_Gil","Casa_Diablo_IV","Myotis_sodalis","Klamath_Tribes",
             "Burns_Paiute","Illustrated_Flora","Visual_Resources","Chief_Ranger","Indiana_Bat","PO_Box","PF_Doc","Arizona_Game","Forestry_Technician","P.E.__B.S.",
             "Montana_Fish","Suite_B__LOCATIONS","USDI_Fish","EIS_Reviewer","Page_1","Pool_Bypass","Sherman_Cattle","Good__","Airspeed_Altitude__SEL","SAND_FILL","CH2_M_Hill",
             "Jicarilla_Apache_Tribe","Marine_Mammal_Science"   ,"Mar._Res","Sand_Point_Way_NE"  ,
             "TPH_Gasoline_C4-C12" , "CH2_M_HILL","Louis_Berger" ,"Livestock_Grazing", "Endrin_Aldehyde" ,"Amec_Foster_Wheeler" ,"Wildlife_Biology","Bog__Fen","Boise_Idaho","BA_Geography" ,
             'Marl_Seeps')

ent_dt = ent_dt[!token%in% bad_ents,]

splts = str_split(ent_dt$token,'_',simplify = F)

pairs = sapply(splts,function(x) if(paste(x[2],x[1],sep = '_') %in% ent_dt$token){c(paste(x[2],x[1],sep = '_'),paste(x[1],x[2],sep = '_'))})
pairs = lapply(pairs[!sapply(pairs,is.null)],sort)

for(p in pairs){
  ent_dt$token[ent_dt$token %in% p] <- p[1]
}


person_dt = ent_dt


file_text = readRDS(sort(list.files('scratch/boilerplate/',pattern = 'raw_entity_pull_',full.names = T),decreasing = T)[1])
ent_dt = rbindlist(file_text,use.names = T,fill = T)

ent_dt$PROJECT_ID <- str_remove(ent_dt$FILE,'_.*')
ent_dt = ent_dt[grepl('[A-Za-z0-9]',token),]
ent_dt$token <- str_replace_all(ent_dt$token,'_{1,}',' ')
ent_dt$token = str_remove(ent_dt$token,'\\s{4,}.*')

#full_tlist <- readRDS('scratch/boilerplate/big_text_files/big_eis_text.rds')
ent_dt$token[grepl('STRATIFIED ENVIRONMENTAL (&|AND) ARCHAEOLOGICAL',toupper(ent_dt$token))] <- "STRATIFIED ENVIRONMENTAL & ARCHAEOLOGICAL SERVICES, LLC"
ent_dt$token[grepl('WALSH ENVIRON',toupper(ent_dt$token))] <- "WALSH ENVIRONMENTAL SCIENTISTS AND ENGINEERS, LLC"
ent_dt$token[grepl('KERLINGER',toupper(ent_dt$token))] <- "CURRY & KERLINGER, LLC"
ent_dt$token[grepl('SOLV LLC',toupper(ent_dt$token))|toupper(ent_dt$token)=='SOLV'] <- 'SOLVE, LLC'
ent_dt$token[grepl('GRETTE ASSOC',toupper(ent_dt$token))] <- "GRETTE ASSOCIATES, LLC"
ent_dt$token[grepl('^HELIA ENV',toupper(ent_dt$token))] <- "HELIA ENVIRONMENTAL, LLC"
ent_dt$token[grepl('PEAK ECOLOG',toupper(ent_dt$token))] <- "PEAK ECOLOGICAL SERVICES, LLC"
ent_dt$token[grepl('ALPINE GEOPHYSICS',toupper(ent_dt$token))] <- "ALPINE GEOPHYSICS, LLC"
ent_dt$token[grepl("HAYDEN(\\s|-)WING",toupper(ent_dt$token))]<- "HAYDEN-WING ASSOCIATES, LLC"
ent_dt$token[grepl("WILD TROUT ENTERPRISES",toupper(ent_dt$token))] <- "WILD TROUT ENTERPRISES, LLC"
ent_dt$token[grepl("ZEIGLER GEOLOGIC",toupper(ent_dt$token))] <- "ZEIGLER GEOLOGIC CONSULTING, LLC"
ent_dt$token[grepl("NORTHERN LAND USE",toupper(ent_dt$token))] <- "NORTHERN LAND USE RESEARCH ALASKA, LLC"
ent_dt$token[grepl("LEGACY LAND",toupper(ent_dt$token))] <- "LEGACY LAND & ENVIRONMENTAL SOLUTIONS, LLC"
ent_dt$token[grepl("SOUTHWEST GEOPHY",toupper(ent_dt$token))] <- 'SOUTHWEST GEOPHYSICAL CONSULTING, LLC'
ent_dt$token[grepl("SYRACUSE ENVIRONMENTAL RESEARCH ASSOCIATES",toupper(ent_dt$token))] <- "SYRACUSE ENVIRONMENTAL RESEARCH ASSOCIATES"
ent_dt$token[grepl("ANDERSON PERRY (&|AND) ASSOCIATES",toupper(ent_dt$token))] <- "ANDERSON PERRY & ASSOCIATES, INC." 
ent_dt$token[grepl("VEATCH",toupper(ent_dt$token))] <- "BLACK & VEATCH" 
ent_dt$token[grepl("TETRA TECH",toupper(ent_dt$token))] <- "TETRA TECH INC." 
ent_dt$token[grepl("FLUOR",toupper(ent_dt$token))] <- "FLUOR CORP." 
ent_dt$token[grepl('\\bERM\\b',ent_dt$token)] <- "ERM" 
ent_dt$token[grepl('\\bKiewit\\b',ent_dt$token)] <- "KIEWIT CORP." 
ent_dt$token[grepl('\\bArcadis\\b',ent_dt$token)] <- "ARCADIS NV" 
ent_dt$token[grepl('\\bECC\\b',ent_dt$token)] <- "ECC" 
ent_dt$token[grepl('CDM.*SMITH',toupper(ent_dt$token))] <- "CDM SMITH" 

ent_dt$token[grepl('Parsons Brinckerhoff',ent_dt$token)] <- "WSP" 

ent_dt$token[grepl('LOUIS.*BERGER',toupper(ent_dt$token))] <- "LOUIS BERGER" 
#note this next example is why toupper is not made permanent -- megawatt hours are written as MWh
ent_dt$token[grepl('\\bMWH\b',ent_dt$token)] <- "MWH CONSTRUCTORS, INC." 
ent_dt$token[grepl("MOTT MACDONALD",toupper(ent_dt$token))] <- "MOTT MACDONALD" 
ent_dt$token[grepl("AECOM",toupper(ent_dt$token))] <- "AECOM" 
ent_dt$token[grepl("RAMBOLL",toupper(ent_dt$token))] <- "RAMBOLL"
ent_dt$token[grepl("BATTELLE",toupper(ent_dt$token))] <- "BATTELLE"
ent_dt$token[grepl("GOLDER ASSOCIATES",toupper(ent_dt$token))] <-"GOLDER ASSOCIATES CORP."
other_consultants =  c("STRATIFIED ENVIRONMENTAL & ARCHAEOLOGICAL SERVICES, LLC",
                       "WALSH ENVIRONMENTAL SCIENTISTS AND ENGINEERS, LLC",
                       "CURRY & KERLINGER, LLC",
                       'SOLVE, LLC','LOUIS BERGER','CDM SMITH','BATTELLE',
                       "GRETTE ASSOCIATES, LLC",
                       "HELIA ENVIRONMENTAL, LLC",
                       "PEAK ECOLOGICAL SERVICES, LLC",
                       "ALPINE GEOPHYSICS, LLC",
                       "HAYDEN-WING ASSOCIATES, LLC",
                       "WILD TROUT ENTERPRISES, LLC",
                       "ZEIGLER GEOLOGIC CONSULTING, LLC",
                       "NORTHERN LAND USE RESEARCH ALASKA, LLC",
                       "LEGACY LAND & ENVIRONMENTAL SOLUTIONS, LLC",
                       'SOUTHWEST GEOPHYSICAL CONSULTING, LLC',
                       "ALPINE ENVIRONMENTAL CONSULTANTS, LLC",
                       "SYRACUSE ENVIRONMENTAL RESEARCH ASSOCIATES",
                       "NUTTER AND ASSOCIATES",
                       "GARCIA AND ASSOCIATES","HORIZON ENVIRONMENTAL SERVICES, INC.",
                       "WESTERN ECOSYSTEMS TECHNOLOGY, INC.",
                       "ANDERSON PERRY & ASSOCIATES, INC." )



### make consult matrix ####
 enr2019_1 = 'https://www.enr.com/toplists/2019-Top-200-Environmental-Firms-1'
 enr2019_2 = 'https://www.enr.com/toplists/2019-Top-200-Environmental-Firms-2'
 enr2018_1 = 'https://www.enr.com/toplists/2018-Top-200-Environmental-Firms-1'
 enr2018_2 = 'https://www.enr.com/toplists/2018-Top-200-Environmental-Firms-2'
 enr2017_1 = 'https://www.enr.com/toplists/2017-Top-200-Environmental-Firms-1'
 enr2017_2 = 'https://www.enr.com/toplists/2017-Top-200-Environmental-Firms-2'
 enr2015_1 = 'https://www.enr.com/toplists/2015_Top_200_Environmental_Firms1'
 enr2015_2 = 'https://www.enr.com/toplists/2015_Top_200_Environmental_Firms2'
 #
 
 
 des2019_1 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms1'
 des2019_2 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms2'
 des2019_3 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms3'
 des2019_4 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms4'
 des2019_5 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms5'
#
des2017_1 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms1'
 des2017_2 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms2'
 des2017_3 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms3'
 des2017_4 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms4'
 des2017_5 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms5'
#

 des2015_1 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms1'
 des2015_2 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms2'
 des2015_3 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms3'
 des2015_4 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms4'
 des2015_5 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms5'
#
 firm_lists = ls(pattern = 'des201[579]|enr201[579]')
#
 firm_lists = ls(pattern = 'des201[579]|enr201[579]')
 firm_names_css_2019 = 'td:nth-child(3)'
 firm_names_css_2017 = 'td:nth-child(3)'
 firm_names_css_2015 = 'td:nth-child(2)'
 
 firm_scrapes = pblapply(firm_lists,function(x) {
   if(grepl('2015',x)){
   get(x) %>% read_html() %>% html_nodes(firm_names_css_2015) %>% html_text(trim=T)
   }
   else if(grepl('2017',x)){
     get(x) %>% read_html() %>% html_nodes(firm_names_css_2017) %>% html_text(trim=T)
   }
   else if(grepl('2019',x)){
     get(x) %>% read_html() %>% html_nodes(firm_names_css_2019) %>% html_text(trim=T)
   }
 })


 names(firm_scrapes)<-firm_lists

saveRDS(firm_scrapes,'scratch/enr_firm_lists.RDS')

firm_scrapes = readRDS('scratch/enr_firm_lists.RDS')
nms = unlist(firm_scrapes)


clean_names = unique(str_remove(nms,'\\,.+'))
clean_names = unique(str_remove(clean_names,'\\s\\(.+'))
clean_names = gsub('|','\\|',clean_names,fixed = T)
clean_names = gsub('&','\\&',clean_names,fixed = T)

person_dt = person_dt[!str_replace_all(token,'_',' ') %in% clean_names,]

saveRDS(person_dt,'scratch/boilerplate/person_entities_extracted.RDS')


#length(unique(toupper(clean_names)))
#orgs$token = orgs$token
orgs = ent_dt


orgs$token[grep('CDM\\b|CDM SM',orgs$token)] <- 'CDM SMITH'
orgs = orgs[grepl('[A-Z]',token,perl = T),]
orgs = orgs[!grepl('http',token,perl = T),]
orgs = orgs[!grepl('\\=',token,perl = T),]
orgs = orgs[!token %in%state.name,]
#drop all tokens observed only 1 time
orgs = orgs[token %in% orgs[,.N,by=.(token)][N>1,]$token,]
# drop all tokens observed in more than 100 EISs
orgs = orgs[!token %in% orgs[!duplicated(paste(token,PROJECT_ID)),.N,by=.(token)][order(-N),][N>80,]$token,]
orgs = orgs[nchar(orgs$token)>1,]
orgs = orgs[str_count(orgs$token,'[0-9]')<=2,]
orgs = orgs[!tolower(orgs$token) %in% stopwords::stopwords(),]

orgs = orgs[!token%in%c('EIR',"NPS","INC","NP","RMP","CEQA","USCS","USGS",'Page','Little',"PAGE","England",'Pond',"Wood",'Sam','SAM','Johnson','Fullerton','Payette'),]
orgs = orgs[!token%in%state.abb,]

orgs$NAME = toupper(orgs$token)
orgs = orgs[!token %in% str_replace_all(person_dt$token,'_',' '),]
orgs = orgs[!grepl(paste(state.name,collapse = '|'),token,perl =T),]

orgs = orgs[token %in% orgs[!duplicated(paste(token,PROJECT_ID)),.N,by=.(token)][N>1,]$token,]

uq_tokens = unique(orgs$token);length(uq_tokens)

mcores = 4
 match1 = pblapply(clean_names,function(x) {grep(paste0('\\b',x,'\\b'),uq_tokens,perl = T)})
 match2 = pblapply(tm::removePunctuation(clean_names),function(x) grep(paste0('\\b',x,'\\b'),uq_tokens,perl = T),cl = mcores)
 match3 = pblapply(str_remove(clean_names,'\\s(Inc\\.$|LLC$|LLC\\.$|Corp\\.$)'),function(x) grep(paste0('\\b',x,'\\b'),uq_tokens,perl = T),cl = mcores)
 match1A = pblapply(toupper(clean_names),function(x) {grep(paste0('\\b',x,'\\b'),toupper(uq_tokens),perl = T)},cl = mcores)
 match2B = pblapply(tm::removePunctuation(toupper(clean_names)),function(x) grep(paste0('\\b',x,'\\b'),toupper(uq_tokens),perl = T),cl = mcores)
 match3C = pblapply(str_remove(toupper(clean_names),'\\s(INC\\.$|LLC$|LLC\\.$|CORP\\.$)'),function(x) grep(paste0('\\b',x,'\\b'),toupper(uq_tokens),perl = T),cl = mcores)

# # # # #

consult_matches = mapply(function(m1,m2,m3,m4,m5,m6) unique(c(m1,m2,m3,m4,m5,m6)),m1 = match1,m2 = match2,m3 = match3,m4 = match1A,m5 = match2B,m6 = match3C)

consult_dt = rbindlist(lapply(seq_along(clean_names),function(i) data.table(clean_names[i],uq_tokens[consult_matches[[i]]])),use.names = T,fill = T)[!is.na(V2),]

orgs$CONSULTANT <- consult_dt$V1[match(orgs$token,consult_dt$V2)]

orgs$entity_type[orgs$CONSULTANT %in% c("CDM Smith","Parametrix","MOTT MACDONALD","Leidos",'WSP',"HNTB","Tetra Tech Inc.","Louis Berger","RK\\&K","Michael Baker International",
"Black \\& Veatch","AECOM","ICF","GRAEF","Atkins North America","HDR")]<-'ORGANIZATION'
  
orgs[,.N,by=.(CONSULTANT,entity_type)][order(-N)][!is.na(CONSULTANT),][entity_type!='ORGANIZATION',][order(-N)][1:15,]
orgs[,.N,by=.(CONSULTANT)][order(-N)][!is.na(CONSULTANT),][order(-N)][1:15,]


fwrite(projects_used[,.(EIS.Number,AGENCY,EIS.Title)],'input/preparer_consultants.csv')
  




consultants = orgs[!is.na(CONSULTANT),]
consultants[,.N,by=.(CONSULTANT)][order(-N)][1:20,]

ent_dt[grepl("Payette",token),]
orgs[CONSULTANT=='Parsons',.(FILE,token)]

grep('20190276',keep_tlist,value = T)
stan_found = consultants[grepl('Stantec',CONSULTANT),]

CEAN AND COASTAL CONSULTANTS PARSONS BRINCKERHOFF
ARCADIS
SCAPE / LANDSCAPE ARCHITECTURE OCEAN AND COASTAL CONSULTANTS PARSONS BRINCKERHOFF
ARCADIS
SEAR


ent_dt[FILE=='20190276_Jordan_Cove_FEIS_Appendix_O.txt'][550:600,]

unique(stan_found$PROJECT_ID)

stantec = grep('Stantec',test$text)
st = test[stantec,.(File)][!duplicated(File),]
fl = str_remove(st$File,'_.*')

test[stantec,][!str_remove(File,'_.*') %in% stan_found$PROJECT_ID,]


consultants[PROJECT_ID=='20190276']

orgs[token=='Pond']
grep('POND',clean_names,value = T)
consultants


consultants


orgs[orgs$CONSULTANT=='Johnson',]

ent_dt[grepl('Cardno',token,perl = T),]

grep('DOWL',clean_names,value = T)

ent_dt[entity_type=='WORK']
ent_dt[grepl("GRAEF",toupper(token)),]


orgs[token=='Parsons']
grep('Johnson',clean_names,value = T)
orgs[CONSULTANT=='Page']

ent_dt[token=='Fullerton']
test = mapply(function(x,y) data.table(x,uq_tokens[y]), x=clean_names,y = consult_matches)

ent_dt[grepl('Sycamore',token),]
ent_dt[token=='Little']
rbindlist(test)

consult_matches[[2]]
lapply(consult_matches,function(x) uq_tokens[[x]])



lapply(uq_tokens,function(uq) data.table(orgs[token==uq,],)

test = pblapply(seq_along(uq),function(x) data.table(PROJECT_ID = orgs$PROJECT_ID[x],clean_names[consult_matches[[x]]]))




length(match1)
length(consult_matches)
length(uq_tokens)

uq_tokens[consult_matches[[10]]]


test2 = rbindlist(test,use.names = T,fill = T)
test2

consultant_project_matches = pblapply(seq_along(name_matches),function(x) {data.table(PROJECT_ID = orgs$PROJECT_ID[name_matches[[x]]],FIRM = clean_names[x])})
saveRDS(consultant_project_matches,'scratch/consultant_project_matches_V2.RDS')

consultant_project_matches = readRDS('scratch/consultant_project_matches_V2.RDS')

consults = rbindlist(consultant_project_matches)[!is.na(PROJECT_ID)]
consults$FIRM = toupper(consults$FIRM)
consults = consults[!duplicated(consults),]

handcoded_firms = orgs[NAME %in% other_consultants,.(PROJECT_ID,NAME)]
setnames(handcoded_firms,'NAME','FIRM')
autocoded_firms = orgs[grepl('CONSULTING|CONSULTANTS',NAME),.(PROJECT_ID,NAME)]
setnames(autocoded_firms,'NAME','FIRM')

consults = rbindlist(list(consults,handcoded_firms,autocoded_firms))
consults = consults[!duplicated(consults),]

consults = consults[!FIRM%in%c('NELSON','LITTLE','JOHNSON','WOOD','ENGLAND'),]
consults$FIRM[consults$FIRM=='ICF'] <- 'ICF INTERNATIONAL'
consults$FIRM[grepl('LEIDOS',consults$FIRM)] <- 'LEIDOS INC.'
consults$FIRM[grepl('PARSONS',consults$FIRM)] <- 'PARSONS CORP'
consults$FIRM[grepl('CARDNO',consults$FIRM)] <- 'LEIDOS INC.'
consults$FIRM[grepl('STANTEC',consults$FIRM)] <- 'STANTEC INC.'
consults$FIRM[grepl('PARAMETRIX',consults$FIRM)] <- 'PARAMETRIX INC.'
consults$FIRM[grepl('SWCA ENVIRONMENTAL',consults$FIRM)] <- 'SWCA ENVIRONMENTAL CONSULTANTS'
consults$FIRM[grepl('CH2M',consults$FIRM)] <- 'CH2M HILL'
consults = consults[FIRM!='WHITMAN',]
consults = consults[FIRM!='STEWART',]
consults = consults[FIRM!='PAGE',]
consult_Freq = consults[PROJECT_ID %in%projects_used$EIS.Number,][,.N,by = .(FIRM)][order(-N)][N>=5,]

consult_Freq

#consults[PROJECT_ID %in% epa_record$EIS.Number,][,.N,by = .(FIRM)][order(-N)][1:25,]

outdir.tables = "output/boilerplate/tables/" 
tableout <-htmlTable(consult_Freq)
outdir.tables = "output/boilerplate/tables/" 
sink(paste0(outdir.tables,"consultant_table.html"))
print(tableout,type="html",useViewer=F)
sink()



top100 = 'https://www.enr.com/toplists/2019-Top-200-Environmental-Firms-1'
top200 = 'https://www.enr.com/toplists/2019-Top-200-Environmental-Firms-2'
library(pdftools)
library(tabulizer)
library(rJava)
library(data.table)
library(rvest)
library(stringr)
tabs = '.flush-left td , th'
prepares = fread('../tuolumne/scratch/boilerplate/preparedocs.csv')

firms = rbindlist(list(read_html(top100) %>% html_nodes('table') %>% .[[2]] %>% html_table(trim=T,fill=T),
                       read_html(top200) %>% html_nodes('table') %>% .[[2]] %>% html_table(trim=T,fill=T)),use.names=T)

splits = str_split(firms$FIRM,',')
firms$NAME = sapply(splits,function(x) x[[1]])
firms$NAME[firms$NAME=="WHITMAN"] <- 'WHITMAN, REQUARDT & ASSOCIATES LLP'
firms$NAME[firms$NAME=="JOHNSON"] <- "JOHNSON, MIRMIRAN & THOMPSON INC."
firms$NAME[firms$NAME=="EA ENGINEERING"] <- "EA ENGINEERING, SCIENCE & TECHNOLOGY INC."



