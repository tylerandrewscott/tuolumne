

pack = c('data.table','stringr','tidyverse','doParallel','pdftools','lubridate','htmlTable')
need = pack[!pack %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(pack,require,character.only=T)

#### check for necessary input data and download from DataDryad if not found ####
  ##### NOTE THIS TAKES A WHILE -- 1.9GB CORPUS FILE WILL NEED TO DOWNLOAD IF NOT FOUND -- 

source('boilerplate_project/code/functions/getInputData.R')


empty_project_record = data.table(PROJECT_ID = character(),YEAR = numeric(),PROJECT_TYPE = character(),AGENCY = character())
empty_doc_dt = data.table(YEAR = numeric(),FILE_NAME = character(), FILE_LOC = character(), PROJECT_TYPE = character(),AGENCY = character())

###############
###############

epa = fread('boilerplate_project/input/feis_record_detail.csv')

epa$Title = iconv(epa$Title,'utf8')
epa$Year = str_extract(epa$EIS.Number,'^[0-9]{4}')


epa  = epa[!grepl('ADOPTION|WITHDRAWN|^Withdrawn|^Adoption',Title),]
#adoptions not coded as such
epa = epa[!EIS.Number%in% c('20170008','20170006','20200182','20150181'),]
epa = epa[Agency!='California Department of Transportation',]

#epa = epa[!EIS.Number%in%c(20170006,20170006),]

epa$Agency[epa$Agency %in% c('Bonneville Power Administration','Western Area Power Administration','National Nuclear Security Administration')] <- 'Department of Energy'
epa$Agency[grepl('National Marine Fisheries',epa$Agency)] <- 'National Oceanic and Atmospheric Administration'
usda_non_fs = c('Rural Utilities Service',
                'Animal and Plant Health Inspection Service',
                'Agriculture Research Service',
                'Natural Resource Conservation Service',
                'Department of Agriculture')
epa$Agency[epa$Agency %in% usda_non_fs]<- 'USDA (non-FS)'
epa$Agency[epa$Agency %in% c('National Geospatial-Intelligence Agency','Department of Defense','United States Marine Corps',
                             'National Security Agency',
                             'United States Navy','United States Army','United States Air Force')] <- 'Department of Defense'

epa$Agency[epa$Agency %in%c('Department of the Interior',
                            'Office of Surface Mining',
                            'Bureau of Ocean Energy Management')]<- 'Department of Interior (other)'

epa$Agency[epa$Agency %in% c('U.S. Customs and Border Protection','Federal Emergency Management Agency','U.S. Coast Guard')] <- 'Department of Homeland Security'

epa$Agency[epa$Agency %in%c('Federal Transit Administration','Federal Aviation Administration','Department of Transportation',
                            'Surface Transportation Board','Maritime Administration','National Highway Traffic Safety Administration',
                            'Federal Railroad Administration')]<- 'Department of Transportation (other)'

epa$Agency[epa$Agency%in%c('Department of Health and Human Services','Food and Drug Administration','National Institute of Health')]<-'Department of Health and Human Services'


epa = epa[Agency %in% epa[,.N,Agency][order(N),][N>=5,]$Agency,]



doc_url = 'boilerplate_project/input/feis_document_record.csv'
epa_docs = fread(doc_url)


epa_docs = epa_docs[EIS.Number %in% epa$EIS.Number,]
epa_docs = epa_docs[!grepl('(CEQ|)[0-9]{8,}_(CEQ|)[0-9]{8,}\\.(pdf|PDF)',epa_docs$File_Name),]
epa_docs = epa_docs[!is.na(File_Name),]
epa_docs = epa_docs[grepl("pdf$",File_Name),]
epa_docs = epa_docs[!duplicated(epa_docs)]


###agencies posted duplicates ####
epa_docs = epa_docs[!File_Name %in% c('20130252_HSTT_EIS_Appendices_A-G_VOL_III_08-13.pdf',
  '20130252_HSTT_EIS_Exec_Sum-CH_3.4_VOL_I_08-13.pdf',
 '20130252_HSTT_EIS_CH_3.5-7_VOL_II_08-13.pdf'),]



epa$YEAR = epa$Year
epa$AGENCY = epa$Agency
epa_docs$YEAR = epa$YEAR[match(epa_docs$EIS.Number,epa$EIS.Number)]
epa_docs$AGENCY = epa$AGENCY[match(epa_docs$EIS.Number,epa$EIS.Number)]
epa$PROJECT_ID = epa$EIS.Number

epa_docs$PROJECT_ID = epa_docs$EIS.Number
text_meta = readRDS('boilerplate_project/input/feis_corpus_2013-2020.rds')
#text_flist = list.files('../eis_documents/enepa_repository/text_as_datatable/',recursive = T)


ex = gsub('pdf$|PDF$','txt',epa_docs$File_Name) %in% basename(unique(text_meta$File))

epa_sub_docs = epa_docs[ex,]
epa_sub = epa
epa_sub$HAVE_DOCS = epa_sub$EIS.Number %in% epa_sub_docs$EIS.Number

projects = epa_sub
documents = epa_sub_docs

documents$FILE_NAME <- gsub('\\s{1,}',' ',documents$File_Name)
documents$FILE_NAME <- gsub('\\s{1,}','_',documents$File_Name)

projects = projects[!duplicated(projects),]

full = c('Forest Service','Bureau of Land Management','Department of Commerce','National Park Service','Federal Energy Regulatory Commission','Department of Health and Human Services','National Oceanic and Atmospheric Administration','Bureau of Reclamation',"U.S. Army Corps of Engineers" ,'Department of Defense',"Department of Transportation (other)",'Department of Energy',"Bureau of Indian Affairs","Department of Homeland Security","Department of Interior (other)","Department of Housing and Urban Development","California Department of Transportation","Nuclear Regulatory Commission","Federal Highway Administration" ,"Fish and Wildlife Service","General Services Administration" ,"Tennessee Valley Authority" ,"USDA (non-FS)"  )
brev =c('FS','BLM','DOC','NPS','FERC','DHHS','NOAA','BR','ACOE','DOD','DOT (other)','DOE','BIA','DHS','DOI (other)','DHUD','CalDOT','NRC','FHWA','FWS','GSA','TVA','USDA (non-FS)')
nms = data.table(full,brev)
projects$AGENCY_SHORT <- nms$brev[match(projects$AGENCY,nms$full)]
projects$EIS.Number <- as.character(projects$EIS.Number)
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

projects = projects[order(-mdy(Federal.Register.Date)),][!duplicated(EIS.Number),]

documents <- documents[!duplicated(documents),]
documents = documents[!{grepl('\\(',documents$FILE_NAME) &!grepl('\\)',documents$FILE_NAME)},]

fwrite(projects,file = 'boilerplate_project/data_products/project_candidates_eis_only.csv')
fwrite(documents,file = 'boilerplate_project/data_products/document_candidates_eis_only.csv')

htmlTable::htmlTable(projects[,list(sum(PROJECT_ID %in% documents$PROJECT_ID|PROJECT_ID %in% documents$PROJECT_ID),.N),by = .(AGENCY)][order(-N)])

