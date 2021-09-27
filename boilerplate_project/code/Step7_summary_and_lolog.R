pack = c("tidyverse","Matrix","statnet","data.table","stringr","lolog","ggrepel","textreuse","gridExtra","sf","lubridate","doParallel","htmlTable","ggplot2","viridis","forcats",'ggthemes','tm','pbapply','htmlTable')
need = !pack %in% installed.packages()[,"Package"]
#sudo apt-get install libfontconfig1-dev
if(any(need)){sapply(pack[need],install.packages)}
sapply(pack,require,character.only=T)

run_basemod = F
REWRITE = T
RERUN = T
#update.packages(ask = FALSE, checkBuilt = TRUE)
runEISnet = TRUE
mcores = detectCores() - 2
samples = 10000
iters = 40
stepsize = 500


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




if(RERUN){
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
  
  
  if(REWRITE){
    tb = htmlTable(projects[,.(sum(USE_DOCS),.N),by=.(AGENCY)][order(-V1),])
    outdir.tables = "boilerplate_project/output/tables/" 
    sink(paste0(outdir.tables,"sample_by_agency.html"))
    print(tb,type="html",useViewer=F)
    sink()
  }
  
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
  
  if(REWRITE){
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
  }
  
  lda_eis = lda[a_id != b_id,]
  lda_solo = rbind(lda_eis[,.(a_id,score,a,a_file,a_page)],lda_eis[,.(b_id,score,b,b_file,b_page)],use.names = F)
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
  
  if(REWRITE){
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
    
    ggsave(gg_pages_over_300,filename = 'boilerplate_project/output/figures/Figure3_agencies_lda_over_300.png',dpi = 300,units = 'in',height = 4.5,width = 6)
  }
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
  # # 
  
  # 
  if(REWRITE){
    gg_centroid = ggplot() +
      geom_sf(data = us_states,fill = NA,lwd = 0.2,col ='grey50') +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(limits=c(-180,-50),expand = c(0,0)) +
      geom_point(pch = 19,alpha=0.2,size = 0.75,data = project_centroids,aes(x = V1,y = V2)) + theme_map() +
      theme_map() +
      ggtitle("Estimated project centroids from geotagging") + theme(text = element_text(size = 12)) +
      NULL
    ggsave(plot = gg_centroid,filename = 'boilerplate_project/output/figures/FigureA5_estimated_centroids.png',dpi = 300,width = 7,height =  6,units = 'in')
  }
  estimate_loc = eis_ids[!eis_ids %in% project_centroids$EIS.Number]
  ### add in agency-wide replacements for four missing locations ####
  project_centroids = rbind(project_centroids,
                            data.table(EIS.Number= estimate_loc,
                                       agency_centroids[match(projects$AGENCY_SHORT[match(estimate_loc,projects$EIS.Number)],agency_centroids$AGENCY),][,2:3])
  )
  project_dist = dist(as.matrix(project_centroids[order(EIS.Number),.(V1,V2)]),upper = T,diag = F)
  dist_mat = as.matrix(project_dist)
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
  if(REWRITE){
    #consults[PROJECT_ID %in% epa_record$EIS.Number,][,.N,by = .(FIRM)][order(-N)][1:25,]

    tableout <-htmlTable(consult_Freq)
    outdir.tables = 'boilerplate_project/output/tables/'
    sink(paste0(outdir.tables,"consultant_table.html"))
    print(tableout,type="html",useViewer=F)
    sink()
  }
  
  consult_matrix = as.matrix(table(consults$FIRM,consults$EIS.Number))
  notin = projects$EIS.Number[!projects$EIS.Number%in% colnames(consult_matrix)]
  addin = matrix(0,nrow = nrow(consult_matrix),ncol = length(notin),dimnames = list(rownames(consult_matrix),sort(notin)))
  
  consult_matrix_full = cbind(consult_matrix,addin)
  consult_triplet =  slam::as.simple_triplet_matrix(consult_matrix_full)
  project_to_project_consult_matrix = slam::crossprod_simple_triplet_matrix(consult_triplet)
  
  lit_dt = fread('boilerplate_project/data_products/nepa_litigation_by_agency_year.csv')
  ldt_long = reshape2::melt(lit_dt,id.vars = 'recode',measure.vars = patterns('y[0-9]'))
  ldt_long$year <- as.numeric(str_remove(ldt_long$variable,'[a-z]'))
  ltd_total = ldt_long[year<2013,][,sum(value,na.rm = T),by=.(recode)]
  
  tots = projects[,.N,by=.(AGENCY_SHORT)]
  
  ltd_total$total_eis = tots$N[match(ltd_total$recode,tots$AGENCY_SHORT)]
  ltd_total$litigation_per_eis = ltd_total$V1/ltd_total$total_eis
  

  ideology_sheet = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSOSX--qpOSyBZcW3OWeWRmiC2-eC74un7fZYXFCzrW8FNB1FOQFMaIq-CW8hMIoBqtZMXYR05UA7Lu/pub?output=csv'
  ideol = fread(ideology_sheet)
  subs = str_split(ideol$epaStandIn,', ')
  ideol[ideol$agency=="Department of Health and Human Services" ]
  index = match(projects$Lead.Agency, ideol$epaName)
  index[is.na(index)] <- as.vector(sapply(projects$Lead.Agency[is.na(index)],function(y) {
 which(sapply(subs,function(x) y %in% x))}))
 
  projects = data.table(projects,ideol[index,])
  


  
  projects[is.na(skills_rating)]$skills_rating<-0
gg_agency = ggplot(projects[!duplicated(acr),]) + 
    geom_point(aes(x = skills_rating,y = ideo_rating)) + 
    geom_label_repel(aes(x = skills_rating,y = ideo_rating,label = acr),max.overlaps = 15) + 
    theme_bw() +
    xlab('lower <-- Perceived workforce skill --> higher') +
   ylab('left <-- Perceived political ideology --> right') +
    ggtitle('Perceived agency workforce skill and ideology') +
    labs(caption = "(data from Richardson et al. 2018)")
  
ggsave(filename = 'boilerplate_project/output/figures/A6_agency_attributes.png',plot = gg_agency,dpi=500,width = 6,height = 6,units = 'in')
  
 
  #if(runEISnet){ 
  #eis_p_list = pblapply(seq(2,20,2),function(p) {
  
  # ######### EIS analysis #########
  s = 300
  #tcounts = lda_eis[score>s & a_id!=b_id,.N,by = .(a_id,b_id)] 

  # edge_counts = rbind(lda[a_id!=b_id,.N,by=.(a_id,b_id,a)][,.N,b=.(a_id,b_id)],
  #                              lda[a_id!=b_id,.N,by=.(b_id,a_id,b)][,.N,b=.(b_id,a_id)],use.names = F)
  # 
  
  lda_eis$a_count = page_counts$total_pages[match(lda_eis$a_id,page_counts$EIS.Number)]
  lda_eis$b_count = page_counts$total_pages[match(lda_eis$b_id,page_counts$EIS.Number)]
  
  edge_counts = lda_eis[score>s & a_id!=b_id,.N,by = .(a_id,b_id)]

  eis_nodes = sort(eis_ids)
  eis_init = network.initialize(n = length(eis_nodes),directed = F,bipartite = F,hyper = F,multiple = F,loops = F)
  network.vertex.names(eis_init) <- eis_nodes
  
  x_vals = 1:20
  dns = sapply(x_vals,function(x) { 
    eis_net <- eis_init;
    edge_counts_sub = edge_counts[N>=x,]
    network.density(network::add.edges.network(eis_net,tail = match(edge_counts_sub$a_id,network.vertex.names(eis_net)),
                                               head = match(edge_counts_sub$b_id,network.vertex.names(eis_net)),names.eval = 'Score_Count',
                                               vals.eval = edge_counts_sub$N))})
  
  
  (gg_threshold = ggplot() + geom_vline(xintercept = 3,lty = 2,col = 'grey50') + geom_path(aes(x = x_vals,y= dns)) + geom_point(aes(x = x_vals,y= dns)) + theme_bw() + 
      xlab('threshold for y_ij = 1') + ylab('graph density') + ggtitle('Graph density vs. threshold for shared pages'))
  ggsave(plot = gg_threshold,filename = 'boilerplate_project/output/figures/FigureA3_tiethreshold_density.png',height = 4,width = 6,units = 'in',dpi = 500)
  
  
  
  final_edge_list = edge_counts[N>=3,]
  eis_net <- eis_init
  network::add.edges.network(eis_net,tail = match( final_edge_list$a_id,network.vertex.names(eis_net)),
                             head = match( final_edge_list$b_id,network.vertex.names(eis_net)),names.eval = 'Score_Count',
                             vals.eval =  final_edge_list$N)

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
  
  
  eis_net %v% 'Year' <- as.character(floor(2013 + eis_net %v% 'Year'))
  
  saveRDS(eis_net,'boilerplate_project/data_products/network_object.rds')
  
  
  eis_consult_matrix = project_to_project_consult_matrix[rownames(project_to_project_consult_matrix) %in%  eis_ids,colnames(project_to_project_consult_matrix) %in%  eis_ids]
  eis_consult_index = match(eis_net %v% 'vertex.names',rownames(eis_consult_matrix))
  eis_consult_matrix = eis_consult_matrix[eis_consult_index,eis_consult_index]
  
  
  eis_author_matrix = project_to_project_author_matrix[rownames(project_to_project_author_matrix) %in% eis_ids,colnames(project_to_project_author_matrix) %in% eis_ids]
  eis_author_index = match(eis_net %v% 'vertex.names',rownames(eis_author_matrix))
  eis_author_matrix = eis_author_matrix[eis_author_index,eis_author_index]
  saveRDS(list('author' = eis_author_matrix,'consult' = eis_consult_matrix,'distance' = eis_distance_matrix),'boilerplate_project/data_products/edgecov_list.rds')
}


eis_net <- readRDS('boilerplate_project/data_products/network_object.rds')
mat_list = readRDS('boilerplate_project/data_products/edgecov_list.rds')
eis_author_matrix <- mat_list$author
eis_consult_matrix <- mat_list$consult
eis_distance_matrix <- mat_list$distance
eis_order = rank(eis_net %v% 'EIS')


temp_eis_net <- eis_net
 
table(temp_eis_net %v% 'Output')

if(run_basemod){
  
  cores = detectCores()-5
  cl = makeCluster(cores)
  registerDoParallel(cl)

# mod_lolog = lolog(temp_eis_net ~ edges + gwdegree(1) + gwesp(1) + 
#                     constraint(boundedDegree(0L,max(degree(temp_eis_net))))|eis_order,nsamp = 10e3,
#                   maxIter = 80,cluster = cl,theta = mod_lolog$theta)
# saveRDS(object = mod_lolog,'boilerplate_project/data_products/base_lolog_nocovariates.rds')

mod_lolog_cov2 = lolog(temp_eis_net ~ edges + 
                       # degree(0) + 
                       # esp(0) + esp(13) + 
                       gwdegree(1) + 
                       gwesp(2) + 
                    #constraint(boundedDegree(0L,max(degree(temp_eis_net)))) +
                    nodeFactor('Year') +
                    nodeCov('ln_Pages_Hashed') +
                     # nodeFactor('Agency_Short')+
                    nodeMatch('Agency_Short') +

                    nodeFactor('Consultant')+
                    nodeCov('Skill_Rating') + 
                    nodeCov('Ideo_Rating') + 
                    nodeCov('NEPA_litigation_per_FEIS_2001_2012') +
                    absDiff('Fed_Reg_Date') +
                    nodeFactor('Output')+
                    nodeCov('ln(Readability score)') +
                    edgeCov(eis_author_matrix,'Shared_Author') + 
                   edgeCov(eis_consult_matrix,'Shared_Consultant') + 
                    edgeCov(eis_distance_matrix,'Centroid_Distance')|eis_order,
        nsamp = 5e3,
                  maxIter = 40,cluster = cl)

gof_deg_object2 = gofit(object = mod_lolog_cov2,formula = temp_eis_net ~ degree(0:50),nsim = 500,cluster = cl)
gof_esp_object2 = gofit.lolog(object = mod_lolog_cov2,formula = temp_eis_net ~ esp(0:25),nsim = 500,cluster = cl)


saveRDS(object = mod_lolog_cov2,'boilerplate_project/data_products/lolog_withcovariates.rds')
saveRDS(object = summary(mod_lolog_cov2),'boilerplate_project/data_products/lolog_summary_withcovariates.rds')
#gof_deg_object = gofit(object = mod_lolog_cov,formula = temp_eis_net ~ degree(0:50),nsim = 1000)
#gof_esp_object = gofit(object = mod_lolog_cov,formula = temp_eis_net ~ esp(0:25),nsim = 1000)

saveRDS(object = list(gof_deg_object2,gof_esp_object2),'boilerplate_project/data_products/lolog_gof.rds')

}



gof = readRDS('boilerplate_project/data_products/lolog_gof.rds')
dof = reshape2::melt(gof[[1]]$stats)
obs = reshape2::melt( gof[[1]]$ostats)
obs$degree = as.numeric(str_extract(rownames(obs),'[0-9]{1,}'))
g1 = ggplot() + geom_path(data = dof,aes(x = Var2-1,group = Var1,y = value),alpha = 0.2,lwd = 0.1) + 
  theme_bw() + geom_path(data = obs,aes(x = degree,y= value),lwd = 1,col = 'red') + xlab('# degrees') + 
  ylab('# of structures observed or simulated') 

dof2 = reshape2::melt(gof[[2]]$stats)
obs2 = reshape2::melt( gof[[2]]$ostats)
obs2$degree = as.numeric(str_extract(rownames(obs2),'[0-9]{1,}'))

g2 = ggplot() + geom_path(data = dof2,aes(x = Var2-1,group = Var1,y = value,col ='black'),alpha = 0.3,lwd = 0.1) + 
  theme_bw() + geom_path(data = obs2,aes(x = degree,y= value,col = 'red'),lwd = 1) + 
  xlab('# edgewise shared partners') + ylab('# of structures observed or simulated') +
  scale_color_manual(values= c('black','red'),labels = c('simulated graph','observed graph')) + 
 # guides(color = guide_legend(override.aes = list(lwd = c(1,1),labels= c('A','B'),col = c('1','2'))))
  theme(legend.position = c(0.7,0.9),legend.title = element_blank(),legend.background = element_rect(fill = alpha('white',0)))

ggsave(grid.arrange(g1,g2,ncol = 2,top = 'Model goodness-of-fit: observed vs. simulated structures'),filename = 'boilerplate_project/output/figures/FigureA4_appendix_gof_plots.png',dpi = 500, width = 7,height = 3.5, units = 'in')

if(!run_basemod){
  #mod_lolog = readRDS('boilerplate_project/data_products/base_lolog_nocovariates.rds')
  mod_lolog_cov = readRDS('boilerplate_project/data_products/lolog_withcovariates.rds')
}


msum = readRDS('boilerplate_project/data_products/lolog_summary_withcovariates.rds')

m1 = (msum[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.))

eis_mresults = m1
eis_mresults$DV = 'EIS'
eis_mresults$coef = fct_recode(eis_mresults$coef,'litigations per FEIS, 2001-2012' = 'nodecov.NEPA_litigation_per_FEIS_2001_2012','Used consultants' = 'nodeFactor.Consultant.1',
                               '|publication date diff.|' = 'absDiff.Fed_Reg_Date','ln(pages)' = 'nodecov.ln_Pages_Hashed','Same consultant(s)' = 'edgeCov.Shared_Consultant',
                               "Same preparer(s)" = "edgeCov.Shared_Author","Distance" = "edgeCov.Centroid_Distance",'ln(readability)' = "nodecov.ln(Readability score)",
                               'Same agency'='nodematch.Agency_Short', "Program/Policy" = "nodeFactor.Output.1", "Project" = "nodeFactor.Output.2",
                               'Ideol. rating' = 'nodecov.Ideo_Rating','Skill rating' = 'nodecov.Skill_Rating','GWESP (decay=2)'='gwesp.2','GW degree (decay=1)'='gwdegree.1',
                               'Program/policy' = 'nodeFactor.Output.1', 'Project' = 'nodeFactor.Output.2',
                               'year 2014' = 'nodeFactor.Year.1','year 2015'='nodeFactor.Year.2','year 2016'= 'nodeFactor.Year.3',
                               'year 2017' = 'nodeFactor.Year.4','year 2018'='nodeFactor.Year.5','year 2019'='nodeFactor.Year.6','year 2020'='nodeFactor.Year.7')

eis_mresults$coef = fct_inorder(eis_mresults$coef)
eis_mresults$coef = fct_rev(eis_mresults$coef)
eis_mresults <- data.table(eis_mresults)
eis_mresults[,sig:=ifelse(theta - 1.96 * se <0 & theta + 1.96 * se>0,0,1)]
eis_mresults$mod_sig = paste0(eis_mresults$mod,eis_mresults$sig)


eis_mresults[,CI:=paste0(sprintf("%.3f", round(theta,3)),' (',sprintf("%.3f", round(se,3)),')')]

eis_mresultscoef = fct_inorder(eis_mresults$coef)

eis_mresults$stars = ifelse(eis_mresults$pvalue <0.001,"***",ifelse(eis_mresults$pvalue<0.01,"**",ifelse(eis_mresults$pvalue<0.05,"*",'')))
eis_mresults$CI  =  paste0(eis_mresults$CI,eis_mresults$stars)


coef_cast =  eis_mresults[,.(coef,CI)]
coef_cast = coef_cast[order(fct_rev(coef)),]

outdir.tables = "boilerplate_project/output/tables/" 
tableout <-htmlTable(coef_cast)
outdir.tables = "boilerplate_project/output/tables/" 
sink(paste0(outdir.tables,"eis_lolog_coefs.html"))
print(tableout,type="html",useViewer=F)
sink()



#gof_deg_object = gofit(object = mod_lolog,formula = temp_eis_net ~ degree(0:50),nsim = 1000)
#gof_esp_object = gofit(object = mod_lolog,formula = temp_eis_net ~ esp(0:25),nsim = 1000)
gof_deg_object2 = gofit.lolog(object = mod_lolog_cov[[2]],formula = temp_eis_net ~ degree(0:50),nsim = 1000)
gof_esp_object2 = gofit(object = mod_lolog_cov,formula = temp_eis_net ~ esp(0:25),nsim = 1000)


# NA to summarize
diag(eis_consult_matrix)<- NA
diag(eis_author_matrix)<- NA
diag(eis_distance_matrix)<- NA
mean(eis_consult_matrix,na.rm = T)
mean(eis_author_matrix,na.rm = T)
mean(eis_distance_matrix,na.rm = T)
#add back in so ergm works
diag(eis_consult_matrix)<- 1
diag(eis_author_matrix)<- 1
diag(eis_distance_matrix)<- 0

gc()




