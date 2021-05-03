pack = c("tidyverse","Matrix","statnet","data.table","stringr","lolog","textreuse","gridExtra","sf","lubridate","doParallel","htmlTable",'treemapify',"ggridges","ggplot2","viridis","hrbrthemes","forcats",'ggthemes','tm','pbapply')
need = !pack %in% installed.packages()[,"Package"]
#sudo apt-get install libfontconfig1-dev
if(any(need)){sapply(pack[need],install.packages)}
sapply(pack,require,character.only=T)

#update.packages(ask = FALSE, checkBuilt = TRUE)
runEISnet = TRUE
mcores = detectCores() - 2
samples = 10000
iters = 40
stepsize = 50
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

flist_dt = readRDS('scratch/boilerplate/big_text_files/big_eis_text.rds')
flist_dt = flist_dt[str_replace(flist_dt$File,'txt$','pdf') %in% docs$FILE_NAME,]
flist_dt$EIS.Number <- str_remove(flist_dt$File,'_.*')
flist_dt <- flist_dt[EIS.Number %in% projects$EIS.Number,]
flist_dt$text = gsub('\"\"','',flist_dt$text,fixed = T)
chars = nchar(flist_dt$text)
periods = stringr::str_count(flist_dt$text,"\\.")
numbers = stringr::str_count(flist_dt$text,"[0-9]")
caps = stringr::str_count(flist_dt$text,'[A-Z]')
tildes = stringr::str_count(flist_dt$text,'~')
quotes = stringr::str_count(flist_dt$text,'\\"')
spaces = stringr::str_count(flist_dt$text,'\\s')

cut = 0.1
flist_dt  = flist_dt[chars>400&{periods/chars}<cut&{quotes/chars}<cut&{tildes/chars}<cut&{numbers/chars}<cut&{caps/chars}<cut&{spaces/chars}<{cut*2},]




require(sylcount)
doc_texts = lapply(unique(flist_dt$EIS.Number),function(x) paste0(flist_dt$text[flist_dt$EIS.Number==x],collapse = ' '))
names(doc_texts) <- unique(flist_dt$EIS.Number)

meta_dt = flist_dt[,.(Page,File,EIS.Number)]
page_counts = meta_dt[,.N,by=.(EIS.Number)]
setnames(page_counts,'N','total_pages')

projects[,.N,by=.(AGENCY)][order(-N)]
projects[,USE_DOCS:=EIS.Number %in% meta_dt$EIS.Number]

length(unique(fread('../eis_documents/enepa_repository/meta_data/extra_docs.csv')$EIS.Number))
table(str_extract(unique(fread('../eis_documents/enepa_repository/meta_data/extra_docs.csv')$EIS.Number),'^[0-9]{4}'))


tb = htmlTable(projects[,.(sum(USE_DOCS),.N),by=.(AGENCY)][order(-V1),])
outdir.tables = "boilerplate_project/output/tables/" 
sink(paste0(outdir.tables,"sample_by_agency.html"))
print(tb,type="html",useViewer=F)
sink()

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

# test = lda[score>300&score<350,]
# test[agency1=='Federal Energy Regulatory Commission'&agency2 == 'Federal Energy Regulatory Commission']
aa = flist_dt$text[flist_dt$File=='20130046_Mid_Fork_American_FEIS_FERC_2079.txt'&flist_dt$Page==308]
bb = flist_dt$text[flist_dt$File=='20180175_Final_Environmental_Impact_Statement_for_the_Lassen_Lodge_Hydroelectric_Project-P-12496-002.txt'&flist_dt$Page==177]
align_local(aa,bb)
# 
# test = lda[score>600&score<750,]
# test[agency1=='Federal Energy Regulatory Commission'&agency2 == 'Federal Energy Regulatory Commission']
aa = flist_dt$text[flist_dt$File=='20170148_VolumeI_FEIS_MXPGXP_20170713.txt'&flist_dt$Page==551]
bb = flist_dt$text[flist_dt$File=='20180144_MIDSHIP_Project_Final_EIS_Vol_I.txt'&flist_dt$Page==295]
align_local(aa,bb)


lda_eis = lda[a_id != b_id,]
lda_solo = rbind(lda_eis[,.(a_id,score,a,a_file)],lda_eis[,.(b_id,score,b,b_file)],use.names = F)
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

agency_summary_stats = countover300[,list(mean(over300/total_pages),median(over300/total_pages)),by = .(AGENCY_SHORT)]
names(agency_summary_stats) <- c('AGENCY_SHORT','mean','median')
agency_summary_stats$mean = round(agency_summary_stats$mean,3)
agency_summary_stats$median = round(agency_summary_stats$median,3)


(gg_pages_over_300 = ggplot() + 
    #  geom_jitter(data = countover300,aes(x = fct_rev(AGENCY_SHORT),y = over300/total_pages),pch = 21,colour = 'grey50') + 
    #geom_boxplot(data = countover300,aes(x = fct_rev(AGENCY_SHORT),y = over300/total_pages),fill = NA) + 
    geom_jitter(data = countover300,aes(x = fct_rev(AGENCY_SHORT),y = over300/total_pages),alpha = 0.5,fill = NA,height = 0,pch = 21) + 
    geom_point(data = countover300[,median(over300/total_pages),by=.(AGENCY_SHORT)],
               aes(x = fct_rev(AGENCY_SHORT),y = V1,col ='red'),pch = '|',size = 5)+
    coord_flip() + scale_color_manual(values = 'red',name = 'Agency median',labels = 'Agency median')+
    #    ggplot2::annotate("label",x = fct_rev(agency_summary_stats$AGENCY_SHORT),y = agency_summary_stats$median*1.5,label = agency_summary_stats$median) + 
    theme_bw() + scale_y_continuous(name = '# pages w/ LDA>300 / total pages by project') + 
                                    #limits=c(0,round(max(countover300$over300/countover300$total_pages) ,1))) + 
    theme(legend.title = element_blank(),axis.title.y = element_blank(),legend.position = c(0.8,0.25),axis.text = element_text(size = 12)) + ggtitle("Text reuse between EISs by project and agency"))

ggsave(gg_pages_over_300,filename = 'boilerplate_project/output/figures/agencies_lda_over_300.png',dpi = 300,units = 'in',height = 4.5,width = 6)

eis_ids = unique(projects$EIS.Number[projects$USE_DOCS])
n_eis = length(eis_ids)
eis_blank_mat = matrix(0,ncol = n_eis,nrow = n_eis,dimnames = list(eis_ids,eis_ids))

geo = readRDS('boilerplate_project/data_products/geo_temp.rds')
geo = geo[!is.na(PRIM_LONG_DEC)&!is.na(PRIM_LAT_DEC),]
geo = geo[PRIM_LAT_DEC>16,]
geo$PRIM_LONG_DEC[geo$PRIM_LONG_DEC>170] <- geo$PRIM_LONG_DEC[geo$PRIM_LONG_DEC>170] * -1
geo = geo[geo$PROJECT_ID %in% eis_ids,]
project_centroids = geo[,list(mean(PRIM_LONG_DEC,na.rm = T),mean(PRIM_LAT_DEC,na.rm = T)),by = .(PROJECT_ID)]

geo$AGENCY = projects$AGENCY_SHORT[match(geo$PROJECT_ID,projects$EIS.Number)]
agency_centroids = geo[,list(mean(PRIM_LONG_DEC),mean(PRIM_LAT_DEC)),by = .(AGENCY)]
setnames(project_centroids,'PROJECT_ID','EIS.Number')
us_states  = tigris::states(class = 'sf',year = 2015)
us_states = us_states[!us_states$STUSPS %in%  c('VI','MP','GU','AS'),]
# 
# ggplot() + geom_sf(data = us_states,fill = NA,lwd = 0.2) + 
#   scale_y_continuous(expand = c(0,0)) + 
#   scale_x_continuous(limits=c(-180,-50),expand = c(0,0)) + 
#   geom_point(data = blm_ea_centroids,aes(x = V1,y = V2,color = 100 * N/total_pages),alpha = 0.5) +
#   scale_color_viridis_c(name = '% high reuse',direction = -1) + theme_map()
# 

# 
gg_centroid = ggplot() + 
  geom_sf(data = us_states,fill = NA,lwd = 0.2) + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(limits=c(-180,-50),expand = c(0,0)) + 
  geom_point(pch = 19,alpha=0.2,size = 0.5,data = project_centroids,aes(x = V1,y = V2)) + theme_map() +
  theme_map() +
  ggtitle("Estimated project centroids from geotagging") + theme(text = element_text(size = 12)) +  
  NULL
gg_centroid

estimate_loc = eis_ids[!eis_ids %in% project_centroids$EIS.Number]
### add in agency-wide replacements for four missing locations ####
project_centroids = rbind(project_centroids,
data.table(EIS.Number= estimate_loc,
           agency_centroids[match(projects$AGENCY_SHORT[match(estimate_loc,projects$EIS.Number)],agency_centroids$AGENCY),][,2:3])
)
project_dist = dist(as.matrix(project_centroids[order(EIS.Number),.(V1,V2)]),upper = T,diag = F)
dist_mat = log(as.matrix(project_dist)+0.001)
project_centroids$EIS.Number -> rownames(dist_mat) -> colnames(dist_mat)

people = readRDS('boilerplate_project/data_products/person_entities_extracted.RDS')
author_matrix = as.matrix(table(people$name,people$PROJECT_ID))
notin = projects$EIS.Number[!projects$EIS.Number%in% colnames(author_matrix)]
addin = matrix(0,nrow = nrow(author_matrix),ncol = length(notin),dimnames = list(rownames(author_matrix),sort(notin)))
author_matrix_full = cbind(author_matrix,addin)
author_matrix_full = author_matrix_full[rowSums(author_matrix_full)>1,]
author_triplet =  slam::as.simple_triplet_matrix(author_matrix_full)
project_to_project_author_matrix = slam::crossprod_simple_triplet_matrix(author_triplet)


consults = readRDS('boilerplate_project/data_products/consultant_project_matches.RDS')
setnames(consults,'PROJECT_ID','EIS.Number')
consult_Freq = consults[EIS.Number %in% projects$EIS.Number[projects$USE_DOCS],][,.N,by = .(FIRM)][order(-N)][N>=10,]

#consults[PROJECT_ID %in% epa_record$EIS.Number,][,.N,by = .(FIRM)][order(-N)][1:25,]
require(htmlTable)
tableout <-htmlTable(consult_Freq)
outdir.tables = 'boilerplate_project/output/tables/'
sink(paste0(outdir.tables,"consultant_table.html"))
print(tableout,type="html",useViewer=F)
sink()


consult_matrix = as.matrix(table(consults$FIRM,consults$EIS.Number))
notin = projects$EIS.Number[!projects$EIS.Number%in% colnames(consult_matrix)]
addin = matrix(0,nrow = nrow(consult_matrix),ncol = length(notin),dimnames = list(rownames(consult_matrix),sort(notin)))

consult_matrix_full = cbind(consult_matrix,addin)
consult_triplet =  slam::as.simple_triplet_matrix(consult_matrix_full)
project_to_project_consult_matrix = slam::crossprod_simple_triplet_matrix(consult_triplet)

lit_dt = fread('boilerplate_project/data_products/nepa_litigation_by_agency_year.csv')
ldt_long = melt(lit_dt,id.vars = 'recode',measure.vars = patterns('y[0-9]'))
ldt_long$year <- as.numeric(str_remove(ldt_long$variable,'[a-z]'))
ltd_total = ldt_long[year<2013,][,sum(value,na.rm = T),by=.(recode)]

tots = projects[,.N,by=.(AGENCY_SHORT)]

ltd_total$total_eis = tots$N[match(ltd_total$recode,tots$AGENCY_SHORT)]
ltd_total$litigation_per_eis = ltd_total$V1/ltd_total$total_eis



ideology_sheet = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSOSX--qpOSyBZcW3OWeWRmiC2-eC74un7fZYXFCzrW8FNB1FOQFMaIq-CW8hMIoBqtZMXYR05UA7Lu/pub?output=csv'
ideol = fread(ideology_sheet)
subs = str_split(ideol$epaStandIn,', ')
projects$index = match(projects$Lead.Agency,ideol$epaName)
projects = data.table(projects,ideol[unlist(sapply(projects$Lead.Agency,function(x) c(grep(x,ideol$epaName),grep(x,ideol$epaStandIn)))),])

projects$skills_rating[is.na(projects$skills_rating)] <- 0

library(lubridate)
#if(runEISnet){ 
#eis_p_list = pblapply(seq(2,20,2),function(p) {

  # ######### EIS analysis #########
  s = 300
  tcounts = lda_eis[score>s & a_id!=b_id,.N,by = .(a_id,b_id)]
  eis_nodes = sort(eis_ids)
  eis_net = network.initialize(n = length(eis_nodes),directed = F,bipartite = F,hyper = F,multiple = F,loops = F)
  network.vertex.names(eis_net) <- eis_nodes
  edgelist = tcounts
  edgelist = edgelist[N>=0,]
  network::add.edges.network(eis_net,tail = match(edgelist$a_id,network.vertex.names(eis_net)),
                             head = match(edgelist$b_id,network.vertex.names(eis_net)),names.eval = 'Score_Count',vals.eval = edgelist$N)
  eis_net %v% 'Agency' <- as.character(projects$AGENCY[match(network.vertex.names(eis_net),projects$EIS.Number)])
  eis_net %v% 'Agency_Short' <- as.character(projects$AGENCY_SHORT[match(network.vertex.names(eis_net),projects$EIS.Number)])
  eis_net %v% 'Fed_Reg_Date' <- decimal_date(mdy(projects$Federal.Register.Date[match(network.vertex.names(eis_net),projects$EIS.Number)]))
  eis_net %v% 'Year' <- (eis_net %v% 'Fed_Reg_Date')-2013
  eis_net %v% 'EIS' <- network.vertex.names(eis_net)
  eis_net %v% 'ln_Pages_Hashed' <- log(page_counts$total_pages[match(eis_net%v%'vertex.names',page_counts$EIS.Number)])
  eis_net %v% 'NEPA_litigation_per_FEIS_2001_2012' = ltd_total$litigation_per_eis[match(eis_net %v% 'Agency_Short', ltd_total$recode)]
  
  eis_net %v% 'Ideo_Rating' <- projects$ideo_rating[match(network.vertex.names(eis_net),projects$EIS.Number)]
  eis_net %v% 'Skill_Rating' <- projects$skills_rating[match(network.vertex.names(eis_net),projects$EIS.Number)]
  # eis_net %v% 'sqrt_NEPA_litigation_2001_2012'  <- sqrt(eis_net %v% 'NEPA_litigation_2001_2012' )
  eis_net %v% 'Consultant' <- network.vertex.names(eis_net) %in% consults$EIS.Number
  eis_distance_matrix = abs(dist_mat[rownames(dist_mat) %in% {eis_net %v% 'vertex.names'},colnames(dist_mat) %in% {eis_net %v% 'vertex.names'}])
  
  
  
  readab = readRDS('boilerplate_project/data_products/readability_scores_by_project.rds')

  eis_net %v% 'ln(Readability score)' <- (log(readab$re[match(eis_net %v% 'vertex.names',readab$EIS.Number)]))
  
  eis_net %v% 'Output' <- projects$DECISION[match(eis_net %v% 'vertex.names',projects$EIS.Number)]

  eis_consult_matrix = project_to_project_consult_matrix[rownames(project_to_project_consult_matrix) %in%  eis_ids,colnames(project_to_project_consult_matrix) %in%  eis_ids]
  eis_consult_index = match(eis_net %v% 'vertex.names',rownames(eis_consult_matrix))
  eis_consult_matrix = eis_consult_matrix[eis_consult_index,eis_consult_index]

  
  eis_author_matrix = project_to_project_author_matrix[rownames(project_to_project_author_matrix) %in% eis_ids,colnames(project_to_project_author_matrix) %in% eis_ids]
  eis_author_index = match(eis_net %v% 'vertex.names',rownames(eis_author_matrix))
  eis_author_matrix = eis_author_matrix[eis_author_index,eis_author_index]

  eis_order = rank(eis_net %v% 'EIS')
  

diag(eis_consult_matrix)<- NA
diag(eis_author_matrix)<- NA
diag(eis_distance_matrix)<- NA
mean(eis_consult_matrix,na.rm = T)
mean(eis_author_matrix,na.rm = T)
mean(eis_distance_matrix,na.rm = T)


eis_p_list = pblapply(seq(2,50,1),function(p){
    print(p)
  # ##### fit eis_net lolog model ####
  # cl = makeCluster(mcores)
  #  registerDoParallel(cl)
  temp_eis_net = eis_net
  delete.edges(temp_eis_net,  which(get.edge.value(temp_eis_net,'Score_Count')<p))
  if(p == 5){eis_master_net <<- temp_eis_net}
  
  mod_temp_eis = lolog(temp_eis_net ~ edges +       
                         nodeCov('ln_Pages_Hashed') +
                         nodeMatch('Agency_Short') +
                         nodeFactor('Consultant')+
                         nodeCov('NEPA_litigation_per_FEIS_2001_2012') +
                         nodeCov('Skill_Rating') + nodeCov('Ideo_Rating') + 
                         absDiff('Fed_Reg_Date') +
                        nodeFactor('Output') +
                         nodeCov('ln(Readability score)') +
                         edgeCov(eis_consult_matrix>0,name = 'Same_Consultant') +
                         edgeCov(eis_author_matrix>0,name = 'Preparers') +
                         edgeCov(eis_distance_matrix,name = 'Distance')|eis_order,
                       nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)
  m1 = (summary(mod_temp_eis)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 1)

  eis_mresults = m1
  eis_mresults$DV = 'EIS'
  
  #eis_mresults$coef = gsub('.*\\.','',eis_mresults$coef)
  eis_mresults$coef = fct_inorder(eis_mresults$coef)
  eis_mresults$coef = fct_rev(eis_mresults$coef)
  eis_mresults$p = p
  eis_mresults
},cl = 2)



eis_dt = rbindlist(eis_p_list)
eis_dt$coef = fct_recode(eis_dt$coef,'litigations per FEIS, 2001-2012' = 'nodecov.NEPA_litigation_per_FEIS_2001_2012','Used consultants' = 'nodeFactor.Consultant.1',
                               '|publication date diff.|' = 'absDiff.Fed_Reg_Date','ln(pages)' = 'nodecov.ln_Pages_Hashed','Same consultant(s)' = 'edgeCov.Same_Consultant',
                         "Same preparer(s)" = "edgeCov.Preparers","Distance" = "edgeCov.Distance",'ln(readability)' = "nodecov.ln(Readability score)",
                         'Same agency'='nodematch.Agency_Short', "Program/policy" = "nodeFactor.Output.1", "Project" = "nodeFactor.Output.2",
                         'Ideol. rating' = 'nodecov.Ideo_Rating','Skill rating' = 'nodecov.Skill_Rating')

eis_dt$coef <- fct_rev(eis_dt$coef)
eis_dt[,sig:=ifelse(theta-1.96 * se < 0 & theta+1.96*se > 0,0,1)]


gg_sensitivity = ggplot(eis_dt[coef!='edges'&mod==1,][order(-coef)]) + 
  geom_errorbar(aes(x = p,ymin = theta-1.96 * se,ymax = theta+1.96*se)) + 
  geom_point(aes(x = p,y = theta,fill = as.factor(sig),size = as.factor(sig)),pch = 21,size = 0.5) + 
  facet_wrap(~coef,scales = 'fixed') + 
  scale_fill_manual(values = c('white','black'),name = '95% interval spans 0',labels=c('yes','no')) + 
  theme_bw() + geom_vline(aes(xintercept = 10,col = 'thresh'),lty = 2) + geom_hline(yintercept = 0,lty = 3) + 
  theme(text = element_text(family = 'Times',size = 10),legend.position = c(0.7,0.1),
        legend.box = "horizontal") + 
  scale_color_manual(labels = 'threshold for main models',name = '',values = 'black') + 
  scale_size_manual(values = c(0.5,1))+
  scale_x_continuous(name = '# pages where LDA > 300',breaks = c(0,10,25,50,75,100)) +
  scale_y_continuous(name = '95% confidence interval (additive log-odds')+
  ggtitle('Parameter estimates by threshold used to assign tie between projects')
gg_sensitivity
ggsave(gg_sensitivity,filename = 'boilerplate_project/output/figures/threshold_sensitivity.png',dpi = 300,width = 7,height =  6,units = 'in')



p = 5
temp_eis_net = eis_net

delete.edges(temp_eis_net,  which(get.edge.value(temp_eis_net,'Score_Count')<p))
?lolog
mod_temp_eis = lolog(temp_eis_net ~ edges +  twoPath + 
                       nodeCov('ln_Pages_Hashed') +
                       nodeMatch('Agency_Short') +
                       nodeFactor('Consultant')+
                       nodeCov('Skill_Rating') + nodeCov('Ideo_Rating') + 
                       nodeCov('NEPA_litigation_per_FEIS_2001_2012') +
                       absDiff('Fed_Reg_Date') +
                       nodeFactor('Output')+
                       nodeCov('ln(Readability score)') +
                       edgeCov(eis_consult_matrix>0,name = 'Same_Consultant') +
                       edgeCov(eis_author_matrix>0,name = 'Preparers') +
                       edgeCov(eis_distance_matrix,name = 'Distance')|eis_order,
                     nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)
  summary(mod_temp_eis)
m1 = (summary(mod_temp_eis)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 1)
  
  eis_mresults = m1
  eis_mresults$DV = 'EIS'
  eis_mresults$coef = fct_recode(eis_mresults$coef,'litigations per FEIS, 2001-2012' = 'nodecov.NEPA_litigation_per_FEIS_2001_2012','Used consultants' = 'nodeFactor.Consultant.1',
                           '|publication date diff.|' = 'absDiff.Fed_Reg_Date','ln(pages)' = 'nodecov.ln_Pages_Hashed','Same consultant(s)' = 'edgeCov.Same_Consultant',
                           "Same preparer(s)" = "edgeCov.Preparers","Distance" = "edgeCov.Distance",'ln(readability)' = "nodecov.ln(Readability score)",
                           'Same agency'='nodematch.Agency_Short', "Program/Policy" = "nodeFactor.Output.1", "Project" = "nodeFactor.Output.2",
                           'Ideol. rating' = 'nodecov.Ideo_Rating','Skill rating' = 'nodecov.Skill_Rating')
  
  eis_mresults$coef = fct_inorder(eis_mresults$coef)
  eis_mresults$coef = fct_rev(eis_mresults$coef)
eis_mresults <- data.table(eis_mresults)
eis_mresults[,sig:=ifelse(theta - 1.96 * se <0 & theta + 1.96 * se>0,0,1)]
eis_mresults$mod_sig = paste0(eis_mresults$mod,eis_mresults$sig)


(eis_grob = ggplot(eis_mresults[coef!='edges'&p==5&mod==1,],aes(group = as.factor(mod))) + 
    geom_hline(yintercept = 0,lty = 2,col = 'grey50') + 
    geom_errorbar(aes(ymin = theta - 1.96 * se,ymax = theta + 1.96 * se,x = coef,col = as.factor(mod)),
                  width = 0.2,lwd = .75,position = position_dodge(width = .3)) + 
    geom_point(aes(y = theta,x= coef,
                   col = as.factor(mod)),position = position_dodge2(width = .3),shape = 19,size = .75) + 
    # geom_point(data = eis_dt[coef!='edges'&p==5&mod==1&sig==0,],fill = 'white',
    #            aes(y = theta,x= coef,
    #                col = as.factor(mod)),position = position_dodge2(width = .3),shape = 21,size = .75) + 
    theme(axis.text.y = element_blank()) + ggtitle('Predicting high reuse between EISs') + 
    ylab(expression(`less likely` %<-% `boilerplate` %->% `more likely`))+
    coord_flip() + theme_bw() +
    scale_color_colorblind(name = 'model',labels = c('95% confidence interval')) + 
    scale_fill_manual(values = c(NA,'white',NA))+
    theme(legend.position = c(0.7,0.075),legend.text = element_text(size = 10),
          axis.title.y = element_blank(),legend.background = element_rect(fill = NA),
          text = element_text(size = 10,family = 'Times'),legend.title = element_blank(),axis.title.x = element_text(size = 10)) +
    guides(shape = FALSE,fill = F) + 
    NULL)


ggsave(plot = eis_grob,filename = 'boilerplate_project/output/figures/eis_lolog.png',width = 5,height = 3.4,units = 'in',dpi = 300)

#modlist = list(mod_temp_eis,mod_temp_eis_dist,mod_temp_eis_dist_interaction )



coef_dt = eis_dt[p==5&mod==1,]
coef_dt[,CI:=paste0(sprintf("%.3f", round(theta,3)),' (',sprintf("%.3f", round(se,3)),')')]
coef_dt$coef = fct_drop(coef_dt$coef,'DistanceXAuthor')
coef_dtcoef = fct_inorder(coef_dt$coef)

coef_dt$stars = ifelse(coef_dt$pvalue <0.001,"***",ifelse(coef_dt$pvalue<0.01,"**",ifelse(coef_dt$pvalue<0.05,"*",'')))
coef_dt$CI  =  paste0(coef_dt$CI,coef_dt$stars)

coef_cast =  dcast(coef_dt[,.(mod,coef,CI)],coef~mod)
coef_cast = coef_cast[order(fct_rev(coef)),]
outdir.tables = "boilerplate_project/output/tables/" 
tableout <-htmlTable(coef_cast)
outdir.tables = "boilerplate_project/output/tables/" 
sink(paste0(outdir.tables,"eis_lolog_coefs.html"))
print(tableout,type="html",useViewer=F)
sink()





blm_eis_projects = projects[AGENCY == 'Bureau of Land Management'&projects$PROJECT_TYPE=='EIS']
lda_blm = lda[a_id %in% blm_eis_projects$EIS.Number & b_id %in% blm_eis_projects$EIS.Number,]
lda_blm_highreuse = lda_blm[score>300,.N,by= .(a_id,b_id)]
pgrid = expand.grid(a_id = blm_eis_projects$EIS.Number,b_id = blm_eis_projects$EIS.Number)

blm_grid = left_join(pgrid,lda_blm_highreuse)
blm_grid$N[is.na(blm_grid$N)] <- 0
blm_grid = data.table(blm_grid)
blm_sagegrouse= c(20180294,20180293,20180295,20180296,20180297,20180298)


pdata = blm_grid[grepl('^2018',a_id) & grepl('^2018',b_id),]
pdata = pdata[as.numeric(a_id)<=as.numeric(b_id),]

(gg_sagegrouse = ggplot() +
    geom_tile(data= pdata[a_id!=b_id,],aes(x = a_id,y = b_id,fill = N)) + 
    geom_tile(data= pdata[a_id==b_id,],aes(x = a_id,y = b_id),fill = 'grey50') + 
    scale_fill_viridis_c(name = '# pages LDA > 300',option = 'B',direction = -1) + 
    ggtitle('# high reuse pages shared between 2018 BLM EISs','red = regional sagegrouse habitat plan') + theme_bw() + 
    theme(axis.title = element_blank(),axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 45,colour = ifelse(sort(unique(pdata$a_id)) %in% blm_sagegrouse,'red','black')),
          axis.text.y = element_text(colour = ifelse(sort(unique(pdata$a_id)) %in% blm_sagegrouse,'red','black')),
          legend.position = c(0.8,0.3)) )

ggsave(gg_sagegrouse,units = 'in',width = 6,height = 6,dpi = 600,filename = 'boilerplate_project/output/figures/sagegrouse_blm.png')


