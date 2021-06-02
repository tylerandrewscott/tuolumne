pack = c("tidyverse","Matrix","statnet","data.table","stringr","lolog","textreuse","gridExtra","sf","lubridate","doParallel","htmlTable",'treemapify',"ggridges","ggplot2","viridis","hrbrthemes","forcats",'ggthemes','tm','pbapply')
need = !pack %in% installed.packages()[,"Package"]
#sudo apt-get install libfontconfig1-dev
if(any(need)){sapply(pack[need],install.packages)}
sapply(pack,require,character.only=T)

require(forcats)


projects = fread('boilerplate_project/data_products/project_candidates_eis_only.csv')
full = c('Forest Service','Bureau of Land Management','Department of Commerce','National Park Service','Federal Energy Regulatory Commission','Department of Health and Human Services','National Oceanic and Atmospheric Administration','Bureau of Reclamation',"U.S. Army Corps of Engineers" ,'Department of Defense',"Department of Transportation (other)",'Department of Energy',"Bureau of Indian Affairs","Department of Homeland Security","Department of Interior (other)","Department of Housing and Urban Development","California Department of Transportation","Nuclear Regulatory Commission","Federal Highway Administration" ,"Fish and Wildlife Service","General Services Administration" ,"Tennessee Valley Authority" ,"USDA (non-FS)"  )
brev =c('FS','BLM','DOC','NPS','FERC','DHHS','NOAA','BR','ACOE','DOD','DOT (other)','DOE','BIA','DHS','DOI (other)','DHUD','CalDOT','NRC','FHWA','FWS','GSA','TVA','USDA (non-FS)')
nms = data.table(full,brev)
projects$AGENCY_SHORT <- nms$brev[match(projects$AGENCY,nms$full)]
projects$EIS.Number <- as.character(projects$EIS.Number)

docs = fread('boilerplate_project/data_products/document_candidates_eis_only.csv')

projects$Title <- gsub('\"',"",projects$Title,fixed = T)
projects$clean.title = str_replace(projects$Title,'\\s{1,}',' ')
projects$DECISION = NA
projects$DECISION[grepl('\\WAMP\\W|RMP|CCP|(CONSERVATION|RESOURCE|MANAGEMENT|MANGEMENT|RESTORATION|INVESTIGATION|LAND USE|FOREST|STEWARDSHIP|GRASSLAND|ACTIVITY|NOURISHMENT|ECOSYSTEMS|DEVELOPMENT|MASTER|TRAVEL|LONG-TERM|TRANSPORTATION|WATERCRAFT) PLAN|TRAVEL MANAGEMENT|SYSTEM PLANNING STUDY|GENERIC|PLAN REVISION|PLAN AMENDMENT|REFORMULATION|CONTROL MANUAL|HEEIA NATIONAL ESTUAR|WILDLIFE REFUGE COMPLEX|OSAGE COUNTY OIL AND GAS',toupper(projects$clean.title))] <- 'Plan'
projects$DECISION[grepl('REGULATION|STANDARDS|OMNIBUS|QUOTAS|NATIONAL APPROACH|LEGISLATIVE|DESIGNATION|ACTIVITIES|TRANSFERS|RANGELAND ANALYSIS|DETERMINATION|RESILIENCY INITIATIVES|FOREST-WIDE|BYCATCH|GRAZING ANALYSIS|COMBINE(D|) LICENSE|CATCH LIMITS|RANGELAND MANAGEMENT|CARCASS MANAGEMENT|PROPOSED STRATEGIES|SUSTAINABILITY INITIATIVES|REGULATORY|REGULATIONS|MANAGEMENT OPERATIONS|STRATEGY|VISITOR ACCESS|SITE-WIDE|ISSUANCE|RISK REDUCTION|OPERATIONAL EFFICIENCY|BASING|PROGRAMS|EXCHANGE|(READINESS|OIL AND GAS|GEOPHYSICAL) ACTIVITIES|RECAPITALIZATION|INVASIVE PLANT (TREATMENT|MANAGEMENT)|PROGRAMMATIC|(\\W|^)PROGRAM(\\W|$)|PERMIT RENEWAL|\\WUSE OF\\W|POLICY|CONTRACT|RULE|MEASURES|PERMIT TO RELEASE|RISK MANAGEMENT|FORESTWIDE|REZONING|MANAGEMENT RESPONSE|TRAINING AND TESTING|RESIDUAL MANAGEMENT|LEASING|NATIONWIDE|WESTERN STATES|SYSTEM OPERATIONS|AIRFIELD OPERATIONS|DAMAGE REDUCTION|TRAINING AND OPERATIONS',toupper(projects$clean.title))] <- 'Program/policy'
#note: after coding the other instances of word "management", remainder are actually forest service _projects_ that sound like plans
#NOTE: 'plan of operation" connotes a case where new thing is built, eis studies project+intended operations
projects$DECISION[grepl('(US|I|SR)(-|\\s)[0-9]{1,3}|\\WPROJECT(S|)(\\W|$)|NAVIGATION STUDY|BYPASS|MINING|RAILROAD|\\WLOOP\\W|HIGH SPEED|REDEVELOPMENT|HWY [0-9]|WIND FARM|EXPRESSWAY|SPECIAL-USE|RECREATION PERMIT|SITE WORK|SAFETY MODIFICATION|INTERMODAL|REGIONAL WATER SYSTEM|LAUNCH SITE|FACILITIES REMOVAL|ROADWAY|RAIL STUDY|INTERSTATE [0-9]|FEASIBILITY REPORT|FEASIBILITY STUDY|PROPOSED ESTABLISHMENT|POWER SYSTEM|HIGH-SPEED|RESERVOIR|SALVAGE|SITE PERMIT|LANDFILL|REPLACEMENT|CAMPUS|CONCEPT PLANS|POST 45|TUNNEL|TRAINING CENTER|CONSTRUCTION|EMBARKATION|MODIFICATIONS|DRAWDOWN|ACQUISITION|COMMUNICATION SITE|SPECIFIC PLAN|CHANNEL|BRIDGE|ECOSYSTEM RESTORATION|TAMARISK REMOVAL|PRODUCTION|TRANSMISSION|\\WLOCKS\\W|MODERNIZATION|PLAN OF OPERATION|OPERATIONAL PLAN|OPERATIONS PLAN|STATION|EXPERIMENTAL REMOVAL|\\WMINE$|FREEWAY|DREDGING|DEEPWATER PORT|REFORESTATION|EXTENSION|REBUILD|REHABILITATION|LIGHT RAIL|DISPOSAL|CROSSING|RELOCATION|REMEDIATION|LEVEES|RESTORATION$|RECONSTRUCTION|CONVERSION|CLOSURE|IMPROVEMENT|PARKWAY|HIGHWAY|\\WTRAIN\\W|CORRIDOR|EXPANSION|MAINTENANCE|UPGRADE|SALE|(FUELS|VEGETATION|RESTORATION) MANAGEMENT|DISPOSITION|FEE(-|\\s)TO(-|\\s)TRUST|BEDDOWN|EVALUATION|FACILITY|\\WMINE\\W|PASSAGE',toupper(projects$clean.title))&is.na(projects$DECISION)] <- 'Project'

projects$DECISION[projects$clean.title %in% c("Sabine Pass to Galveston Bay","Everglades Agricultural Area A-1 Shallow Flow Equalization Basin","Ochoco Summit Trail System","Malheur National Forest Site-Specific Invasive Plants Treatment", "Beasley Pond Analysis Area" ,"2016-2020 Fernow Experimental Forest","Lee Canyon EIS","Alton Coal Tract Lease By Application" , "Amoruso Ranch" , "Trout Creek","Rim Fire Recovery","North Fork Wells of Eagle Creek" ,"High Uintas Wilderness Colorado River Cutthroat Trout Habitat Enhancement","Beaver Creek","Sugarloaf Hazardous Fuels Reduction" , "Starry Goat","Little Boulder","Flat Country","South San Francisco Bay Shoreline Phase I" ,"Shasta Lake Water Resources Investigation","Ambler Road Final Environmental Impact Statement","Adams and Denver Counties, Colorado General Investigation Study","Tollgate Fuels Reduction", "Gold Butterfly" ,"RES Americas Moapa Solar Energy Center" ,'Cordova Hills','Riley Ridge to Natrona',"Restoration of the Mariposa Grove of Giant Sequoias" ,"Lassen Lodge Final Environmental Impact Statement","I69 Section 6 Martinsville to Indianapolis","NIH Bethesda Surgery, Radiology, And Lab Medicine Building","Continental United States (CONUS) Interceptor Site")]<-'Project'
projects$DECISION[projects$clean.title %in% c("FEIS to Address the Presence of Wolves at Isle Royale National Park","BEH Rangeland Allotments","Shoreline II Outfitter/Guide"  ,"Designated Routes and Areas for Motor Vehicle Use (DRAMVU)", "Southwest Coastal Louisiana" , "Central and Southern Florida, Everglades Agricultural Area (EAA), Florida","Previously Issued Oil and Gas Leases in the White River National Forest" , "Powder River Training Complex Ellsworth Air Force Base"  ,"Four-Forest Restoration Initiative  Coconino and Kaibab National Forests" ,"The Management of Conflicts Associated with Double-crested Cormorants","Gulf Regional Airspace Strategic Initiative Landscape Initiative")]<-'Program/policy'
projects$DECISION[projects$clean.title %in% c("Rocky Mountain Arsenal National Wildlife Refuge", "Antelope Grazing Allotments AMP","Ringo FEIS & FPA")]<-'Plan'
projects$AGENCY <- fct_infreq(projects$AGENCY)
projects$AGENCY_SHORT <- fct_infreq(projects$AGENCY_SHORT)



if(Sys.info()[['sysname']]== 'Linux'){flist_dt = readRDS('../bucket_mount/tuolumne/scratch/boilerplate/big_text_files/big_eis_text.rds')}
if(Sys.info()[['sysname']]!= 'Linux'){flist_dt = readRDS('scratch/boilerplate/big_text_files/big_eis_text.rds')}

flist_dt = flist_dt[str_replace(flist_dt$File,'txt$','pdf') %in% docs$FILE_NAME,]
source('boilerplate_project/code/functions/cleanText.R')
flist_dt <- cleanText(flist_dt)

meta_dt = flist_dt[,.(Page,File,EIS.Number)]
page_counts = meta_dt[,.N,by=.(EIS.Number)]
setnames(page_counts,'N','total_pages')

projects[,.N,by=.(AGENCY)][order(-N)]
projects[,USE_DOCS:=EIS.Number %in% meta_dt$EIS.Number]


lda = readRDS('boilerplate_project/data_products/eis_page_scores_scratch.rds')
lda$score = as.numeric(lda$score)
lda = lda[!duplicated(lda),]
lda = lda[score>=300,]
lda$a = gsub('^(((?!--).)+-{2})\\1','\\1',lda$a,perl = T)
lda$b = gsub('^(((?!--).)+-{2})\\1','\\1',lda$b,perl = T)
lda = lda[a!=b,]
lda = lda[!duplicated(lda),]
gc()

lda$a_page = as.numeric(str_remove(lda$a,'.*_'))
lda$b_page = as.numeric(str_remove(lda$b,'.*_'))
lda$a_id = (str_remove(lda$a,'_.*'))
lda$b_id = (str_remove(lda$b,'_.*'))

lda$a_file = (str_remove(lda$a,'_[0-9]{1,}$'))
lda$b_file = (str_remove(lda$b,'_[0-9]{1,}$'))

lda <- lda[a_file %in% str_replace(docs$FILE_NAME,'pdf$','txt'),]
lda <- lda[b_file %in% str_replace(docs$FILE_NAME,'pdf$','txt'),]



lda_within = lda[a_id == b_id,]
lda_solo = rbind(lda_within[,.(a_id,score,a,a_file,a_page)],lda_within[,.(b_id,score,b,b_file,b_page)],use.names = F)
lda_solo$AGENCY = projects$AGENCY[match(lda_solo$a_id,projects$EIS.Number)]
#lda_solo = lda_solo[!a_file %in% badfiles,]
lda_solo$a_file = gsub('\\s','_',lda_solo$a_file)
lda_solo$a_file = gsub('\\.pdf\\.txt$','.txt',lda_solo$a_file)
lda_solo$a = gsub('\\.pdf\\.txt','.txt',lda_solo$a)

over300 = lda_solo[order(-score),][!duplicated(a),]
countover300 = over300[,.N,by = .(a_id,AGENCY)]
setnames(countover300,'a_id','EIS.Number')
setnames(countover300,'N','over300')
page_counts = page_counts[page_counts$EIS.Number %in% projects$EIS.Number,]
page_counts$AGENCY = projects$AGENCY[match(page_counts$EIS.Number,projects$EIS.Number)]
countover300 = merge(countover300,page_counts[EIS.Number %in% projects$EIS.Number,],all = T)
countover300$AGENCY_SHORT = projects$AGENCY_SHORT[match(countover300$AGENCY,projects$AGENCY)]
countover300$over300[is.na(countover300$over300)] <- 0
projects$within_prop = {countover300$over300/countover300$total_pages}[match(projects$EIS.Number,countover300$EIS.Number)]



lda_between = lda[a_id != b_id,]
lda_solo = rbind(lda_between[,.(a_id,score,a,a_file,a_page)],lda_between[,.(b_id,score,b,b_file,b_page)],use.names = F)
lda_solo$AGENCY = projects$AGENCY[match(lda_solo$a_id,projects$EIS.Number)]
#lda_solo = lda_solo[!a_file %in% badfiles,]
lda_solo$a_file = gsub('\\s','_',lda_solo$a_file)
lda_solo$a_file = gsub('\\.pdf\\.txt$','.txt',lda_solo$a_file)
lda_solo$a = gsub('\\.pdf\\.txt','.txt',lda_solo$a)

over300 = lda_solo[order(-score),][!duplicated(a),]
countover300 = over300[,.N,by = .(a_id,AGENCY)]
setnames(countover300,'a_id','EIS.Number')
setnames(countover300,'N','over300')
page_counts = page_counts[page_counts$EIS.Number %in% projects$EIS.Number,]
page_counts$AGENCY = projects$AGENCY[match(page_counts$EIS.Number,projects$EIS.Number)]
countover300 = merge(countover300,page_counts[EIS.Number %in% projects$EIS.Number,],all = T)
countover300$AGENCY_SHORT = projects$AGENCY_SHORT[match(countover300$AGENCY,projects$AGENCY)]
countover300$over300[is.na(countover300$over300)] <- 0
projects$between_prop = {countover300$over300/countover300$total_pages}[match(projects$EIS.Number,countover300$EIS.Number)]

projects = projects[USE_DOCS==T&!duplicated(EIS.Number),]

projects$total_pages = page_counts$total_pages[match(projects$EIS.Number,page_counts$EIS.Number)]

(compare_between_within = ggplot(projects,aes(x = between_prop,y = within_prop,size = total_pages)) + 
  geom_point(alpha = 0.5,pch = 21) + 
  scale_x_continuous('Proportion of pages w/ between-EIS LDA score > 300') +
  scale_y_continuous('Proportion of pages w/ within-EIS LDA score > 300') +
  theme_bw() + 
  theme(legend.position = c(0.8,0.7),legend.background = element_rect(fill = alpha('white',0))) + 
  scale_size_binned_area(breaks=c(100,500,1000,10000),name = '# EIS pages') + 
  ggtitle('Between-project vs. within-project text reuse'))
ggsave(compare_between_within,filename = 'boilerplate_project/output/figures/compare_within_between.png',dpi = 500,width = 7,height = 6,units = 'in')


