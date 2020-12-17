

pack = c('data.table','stringr','tidyverse','doParallel','pdftools','lubridate')
need = pack[!pack %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(pack,require,character.only=T)

empty_project_record = data.table(PROJECT_ID = character(),YEAR = numeric(),PROJECT_TYPE = character(),AGENCY = character())
empty_doc_dt = data.table(YEAR = numeric(),FILE_NAME = character(), FILE_LOC = character(), PROJECT_TYPE = character(),AGENCY = character())

###############
###############


epa = fread('../eis_documents/enepa_repository/meta_data/eis_record_detail.csv')
epa = epa[Document=='Final',]

epa = epa[grepl('^201[3-9]',epa$EIS.Number),]

#epa = epa[Agency=='Bureau of Land Management',]

epa$Title = iconv(epa$Title,'utf8')
epa$Year = str_extract(epa$EIS.Number,'^[0-9]{4}')
epa  = epa[!grepl('ADOPTION|WITHDRAWN|^Withdrawn|^Adoption',Title)&!grepl('PRO',State.or.Territory),]
epa = epa[!EIS.Number%in% c('20170008','20170006'),]
#epa = epa[!EIS.Number%in%c(20170006,20170006),]



epa$Agency[epa$Agency %in% c('Bonneville Power Administration','Western Area Power Administration')] <- 'Department of Energy'

epa = epa[Agency %in% epa[,.N,Agency][order(N),][N>=10,]$Agency,]

doc_url = '../eis_documents/enepa_repository/meta_data/eis_document_record.csv'
epa_docs = fread(doc_url)

doc_url2 = '../eis_documents/enepa_repository/meta_data/extra_docs.csv'
epa_docs2 = fread(doc_url2)
epa_docs = rbindlist(list(epa_docs,epa_docs2),fill=T)

epa_docs = epa_docs[EIS.Number %in% epa$EIS.Number,]

epa_docs = epa_docs[!grepl('(CEQ|)[0-9]{8}_(CEQ|)[0-9]{8}\\.(pdf|PDF)',epa_docs$File_Name),]

epa_docs = epa_docs[!is.na(File_Name),]
epa_docs = epa_docs[grepl("pdf$",File_Name),]
epa_docs = epa_docs[!duplicated(epa_docs)]




#epa_docs$File_Name <- textclean::replace_non_ascii(epa_docs$File_Name)

epa_docs= epa_docs[EIS.Number  %in% epa_docs[,.N,by=.(EIS.Number)][N==1,]$EIS.Number | !grepl('^[0-9]{8}(\\s|_)(APPENDIX|APPX|APPENDICES)|BIOP|SURVEY|SUPPLEMENT|COMMENT REPORT|EVALUATION|CONCEPT PLANS|DELINEATION|LETTER|DECISION REPORT|INITIAL SITE|PRELIMINARY|HEARING|APPENDIX\\.PDF$|EXHIBITS|DEIS|DRAFT|MAP|COMMENTS|RESPONSE|RESOURCEREPORT|ATTACHMENT|POWERPOINT|STUDY|BUDGET|MEETING|MINUTES|ENVIRONMENTAL(\\s|_)ASSESSMENT|FONSI|FIGURE|TECHNICAL(\\s|_)MEMORANDUM|FINDING(\\s|_)OF(\\s|_)NO(\\s|_)SIGNIFICANT|(?<!AND_)RECORD(\\s|_)OF(\\s|_)DECISION',toupper(File_Name),perl = T)&!grepl('GRSA DEIS|_EIR\\+EIS\\+EIS_508',toupper(File_Name)),]


#epa_docs = epa_docs[grep('App[A-Z]',File_Name,invert = T),]


app_drops = grepl('Appendix|Appendices|appendix|appendices|appx|final_biological_opinion|Traffic_Analysis_Addendum|Aerial_Atlas|BiologicalAssessment|Appendice|Applendix|Appenices|APPENDIX|APPENDICES|Appedicies',epa_docs$File_Name)&!grepl('and Appendix|to Appendix|Chapter|Front|Cover|\\& Append',epa_docs$File_Name)
epa_docs = epa_docs[EIS.Number  %in% epa_docs[,.N,by=.(EIS.Number)][N==1,]$EIS.Number|!app_drops,]

app_drops = grepl('App([A-Z]|\\.[1-9])|App\\s[A-Z]|(App|APP|Appx|Appndx|Appnd)(\\s|_|\\.|-)[A-Z1-9]|APX(\\s|_|\\.|-)[A-Z1-9]|Apps[A-Z]|Appdx|Appedix|Appdcs(_|\\.|\\s)[0-9A-Z]|(A|a)ppend[A-Z]',epa_docs$File_Name)&!grepl('Front.*to_App\\.1|withAppA|and_App(\\s|_|)A|Chapter|Cover|wApp|Front',epa_docs$File_Name)
epa_docs = epa_docs[EIS.Number  %in% epa_docs[,.N,by=.(EIS.Number)][N==1,]$EIS.Number|!app_drops,]
epa_nodoc = epa[!EIS.Number %in% epa_docs$EIS.Number,]
epa$PROJECT_TYPE = 'EIS'
epa_docs$PROJECT_TYPE = 'EIS'


epa$YEAR = epa$Year
epa$AGENCY = epa$Agency
epa_docs$YEAR = epa$YEAR[match(epa_docs$EIS.Number,epa$EIS.Number)]
epa_docs$AGENCY = epa$AGENCY[match(epa_docs$EIS.Number,epa$EIS.Number)]
epa$PROJECT_ID = epa$EIS.Number
epa_sub = epa[,.(PROJECT_ID,AGENCY,YEAR,PROJECT_TYPE)]

epa_docs$File_Name = gsub('\\.pdf\\.pdf$','.pdf',epa_docs$File_Name)

epa_docs$PROJECT_ID = epa_docs$EIS.Number
epa_docs$FILE_NAME = paste0(epa_docs$PROJECT_ID,'--',gsub('\\.PDF$|\\.pdf$','.txt',epa_docs$File_Name))

epa_docs$FILE_LOC = paste0('../eis_documents/enepa_repository/text_as_datatable/',epa_docs$YEAR)
epa_sub_docs = epa_docs[,.(PROJECT_ID,AGENCY,YEAR,PROJECT_TYPE,FILE_NAME,FILE_LOC)]
epa_sub_docs$FILE_NAME = str_remove(epa_sub_docs$FILE_NAME,'.+--')

ex = file.exists(paste(epa_sub_docs$FILE_LOC,epa_sub_docs$FILE_NAME,sep='/'))
epa_sub_docs = epa_sub_docs[ex,]

epa_sub = epa_sub[PROJECT_ID %in% epa_sub_docs$PROJECT_ID,]
epa_sub$MASTER_ID = epa_sub$PROJECT_ID
epa_sub_docs$MASTER_ID = epa_sub_docs$PROJECT_ID
library(htmlTable)


###############
###############
usfs = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_project_detail.csv')
usfs$`Expected Analysis Type` = gsub('\\r|\\n|\\t','',usfs$`Expected Analysis Type`)
usfs$`Current Status` = gsub('\\r|\\n|\\t','',usfs$`Current Status`)
usfs = usfs[order(Project_Num,Project_Status),]
usfs[,Proj_Num:=NULL]
setnames(usfs,'Project_Num','PROJECT_ID')

#usfs_overview = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_project_overview.csv')
#setnames(usfs_overview,c('Page',c('Project_Page')))

not_in_epa_record = epa[AGENCY=='Forest Service'&!PROJECT_ID %in% epa_sub$PROJECT_ID,]

crosswalk = data.table(EPA_ID = c(
                         20150029,  
                       20140238,
                         20140214,
                      20140173,
                      20140186,
                        20130380,
                       20130362,
                      20130364,
                      20130333,
                    20130287,
                   20130177,
                       20130013), USFS_ID = c(40683,40682,33418,41946,NA,22287,14392,28356,41238,894,34158,17267))


usfs = usfs[`Expected Analysis Type` %in% c('Environmental Assessment')|usfs$PROJECT_ID %in% crosswalk$USFS_ID,]

usfs = usfs[`Current Status`=='Analysis Completed',]
#usfs = usfs[`Decision Signed Date`!='',]
usfs$YEAR = year(ymd(usfs$`Decision Signed Date`))
usfs$YEAR[is.na(usfs$YEAR)|usfs$YEAR==''] <- year(mdy(usfs$`Last Updated:`[is.na(usfs$YEAR)|usfs$YEAR=='']))


usfs_docs = fread('../eis_documents/agency_nepa_libraries/usfs/metadata/forest_service_document_record.csv')
setnames(usfs_docs,c('Project_Num','File_Name'),c('PROJECT_ID','FILE_NAME'))
usfs_docs = usfs_docs[PROJECT_ID %in% usfs$PROJECT_ID ,]


usfs_docs = usfs_docs[Stage!='Supporting',]
usfs_docs = usfs_docs[Stage!='Scoping',]
usfs_docs = usfs_docs[Stage!='Litigation',]
usfs_docs = usfs_docs[Stage!='Pre-Scoping',]
usfs_docs = usfs_docs[Stage!='Forest Plan',]
usfs_docs = usfs_docs[Stage!='Post-Decision',]


dropstring = 'Record of Decision|Decision(\\s|_|)Notice|^Binder|Report|(L|l)egal|Preliminary|Opportunity|PublicLetter|LegalNotice|Affidavit|Receipt|Objection|Letter|letter|Map\\W|MAP|Figure|Appendix|Appx|Appdx|Scoping|Appeal|Administrative Review Response|Opposing Views'
usfs_docs = usfs_docs[!grepl(dropstring,Document_Name)|grepl('FEIS|Final',Document_Name),]

usfs_docs = usfs_docs[!grepl('Map$',Document_Name),]
usfs_docs$Analysis_Type = usfs$`Expected Analysis Type`[match(usfs_docs$PROJECT_ID,usfs$PROJECT_ID)]
usfs_docs$Document_Name = gsub('"\"{2,}','',usfs_docs$Document_Name)
usfs_docs = usfs_docs[!duplicated(paste(toupper(usfs_docs$Document_Name),usfs_docs$PROJECT_ID)),]
usfs_docs = usfs_docs[!duplicated(paste(str_remove(Document_Name,'[0-9]{4}(-|_)[0-9]{2}(-|_)[0-9]{2}'),PROJECT_ID)),]
usfs_docs = usfs_docs[order(PROJECT_ID,-Document_File),][!duplicated(paste(PROJECT_ID,str_remove_all(Document_Name,paste(month.name,collapse='|')))),]


usfs_docs = usfs_docs[PROJECT_ID!='53498'|Document_Name=='NNIP_EA_FONSI']
usfs_docs = usfs_docs[PROJECT_ID!='40179'|Document_Name=='FINAL_BearCreekWatershedRestorationEA_July2015']
usfs_docs = usfs_docs[PROJECT_ID!='51590'|Document_Name=="East Chula EA August 2018"]
usfs_docs = usfs_docs[PROJECT_ID!='15870'|Document_Name== "FINAL Environmental Assessment with Appendices"]
usfs_docs = usfs_docs[PROJECT_ID!='43836'|Document_Name== "Final EA_UnauthRouteDecom_with maps" ]
usfs_docs = usfs_docs[PROJECT_ID!='38112'|Document_Name==  "Final_EA_MV-22_without_FONSI" ]
usfs_docs = usfs_docs[!grepl('^Map[^a-z]',Document_Name),]

usfs_docs = usfs_docs[PROJECT_ID!='46306'|grepl('^Final',Document_Name),]
usfs_docs[,.N,by=.(PROJECT_ID)][order(-N)]

test = usfs_docs[,list(sum(grepl('(Final|FINAL|final|Updated|updated)(\\s|_|)(EA|Environmental(\\s|_|)Assessment)',Document_Name)),
      sum(grepl('(Draft|DRAFT|draft)(\\s|_|)(EA|Environmental(\\s|_|)Assessment)',Document_Name))),by=.(PROJECT_ID)]
test = test[V1>0&V2>0,]

for(i in unique(test$PROJECT_ID)){
  {
    usfs_docs = usfs_docs[PROJECT_ID!=i|!grepl('(Draft|DRAFT|draft)(\\s|_|)(EA|Environmental(\\s|_|)Assessment)',Document_Name),]
  }
}


test = usfs_docs[,list(sum(grepl('(Final|FINAL|final|Updated|updated).*(EA|Environmental(\\s|_|)Assessment)',Document_Name)),
                       sum(grepl('(Draft|DRAFT|draft|PRELIM|prelim|Prelim).*(EA|Environmental(\\s|_|)Assessment)',Document_Name)&!grepl('(Final|FINAL|final|Updated|updated)',Document_Name))),by=.(PROJECT_ID)]
test = test[V1>0&V2>0,]
for(i in unique(test$PROJECT_ID)){
  {
    usfs_docs = usfs_docs[PROJECT_ID!=i|(!grepl('(Draft|DRAFT|draft|PRELIM|prelim|Prelim).*(EA|Environmental(\\s|_|)Assessment)',Document_Name)&grepl('(Final|FINAL|final)',Document_Name)),]
  }
}


test = usfs_docs[,list(sum(grepl('(Final|FINAL|final|Updated|updated).*(EA|Environmental(\\s|_|)Assessment)',Document_Name)),
                       sum(grepl('(EA|Environmental(\\s|_|)Assessment)',Document_Name)&!grepl('(Final|FINAL|final|Updated|updated)',Document_Name))),by=.(PROJECT_ID)]
test = test[V1>0&V2>0,]
for(i in unique(test$PROJECT_ID)){
  {
    usfs_docs = usfs_docs[PROJECT_ID!=i|(grepl('(EA|Environmental(\\s|_|)Assessment)',Document_Name)&grepl('(Final|FINAL|final|Updated|updated)',Document_Name)),]
  }
}



test = usfs_docs[,list(sum(grepl('(EA|Environmental(\\s|_|)Assessment).*(Final|FINAL|final|Updated|updated)',Document_Name)),
                       sum(grepl('(EA|Environmental(\\s|_|)Assessment).*(Draft|DRAFT|draft|PRELIM|prelim|Prelim)',Document_Name)&!grepl('(Final|FINAL|final|Updated|updated)',Document_Name))),by=.(PROJECT_ID)]
test = test[V1>0&V2>0,]
for(i in unique(test$PROJECT_ID)){
  {
    usfs_docs = usfs_docs[PROJECT_ID!=i|(!grepl('(EA|Environmental(\\s|_|)Assessment).*(Draft|DRAFT|draft|PRELIM|prelim|Prelim)',Document_Name)&grepl('(Final|FINAL|final)',Document_Name)),]
  }
}



test = usfs_docs[,list(sum(grepl('(EA|Environmental(\\s|_|)Assessment).*(Final|FINAL|final|Updated|updated)',Document_Name)),
                       sum(grepl('(EA|Environmental(\\s|_|)Assessment)',Document_Name)&!grepl('(Final|FINAL|final|Updated|updated)',Document_Name))),by=.(PROJECT_ID)]
test = test[V1>0&V2>0,]
for(i in unique(test$PROJECT_ID)){
  {
    usfs_docs = usfs_docs[PROJECT_ID!=i|(grepl('(EA|Environmental(\\s|_|)Assessment)',Document_Name)&grepl('(Final|FINAL|final)',Document_Name)),]
  }
}

test = usfs_docs[,list(sum(grepl('(EA|Environmental(\\s|_|)Assessment)',Document_Name[Stage=='Decision'])),
                       sum(grepl('(EA|Environmental(\\s|_|)Assessment)',Document_Name[Stage=='Analysis']))),by=.(PROJECT_ID)]
test = test[V1>0&V2>0,]

for(i in unique(test$PROJECT_ID)){
  {
    usfs_docs = usfs_docs[PROJECT_ID!=i|((grepl('(EA|Environmental(\\s|_|)Assessment)',Document_Name)&Stage=='Decision')),]
  }
}



usfs_eis_docs = usfs_docs[PROJECT_ID %in% crosswalk$USFS_ID & Stage %in%c('Analysis','Decision')&grepl('FEIS|Final',toupper(Document_Name)),]

usfs_eis_docs = usfs_eis_docs[!grepl(dropstring,Document_Name),]

usfs_eis_docs = usfs_eis_docs[!grepl("APPENDICES|APPENDIX",toupper(Document_Name))|PROJECT_ID%in%usfs_eis_docs[,.N,by=.(PROJECT_ID)][N==1,]$PROJECT_ID,]


ea_search_string = 'Environmental Assesment|FEA|[0-9]{3,}ea|EA[0-9]{8}|^EA|\\sEA$|\\sEA\\s|(_|[a-z])EA$|(-|\\s|_)FinalEA(-|\\s|_)|Final-EA|final-EA|EAVol|\\.EA\\.|[A-Z]EA_|_EA\\s|([a-z]|_)EA_|\\sEA_|_ea$|^ea|_ea_|\\sea\\s|_ea$|EAfinal|EAFinal|Final for|Final EA|\\sEA\\,|EAFONSI|Environmental(\\s|_|)Assessment|ENVIRONMENTAL(\\s|_|)ASSESSMENT|Environmental(\\s|_|)Analysis|environmental(\\s|_|)assessment|EAFINAL|EaFinal|Eafinal|Final Chapter|CHAPTER_[0-9]_Final|EnviormentalAssessment|EA_cover|^[a-z]_(?!APPENDIX)|Chapter\\s[0-9]_FINAL|EAandappendices|\\([0-9]{1,2}\\)\\s(cover|contents|CHAPTER)|[0-9a-z]EA[0-9a-z]|signed_reduced'
usfs_exdocs = usfs_docs[!grepl(ea_search_string,Document_Name,perl = T),]
usfs_eadocs = usfs_docs[grepl(ea_search_string,Document_Name,perl  =  T),]
adins = list(
  usfs_exdocs[!PROJECT_ID  %in%  usfs_eadocs$PROJECT_ID,][grepl('Chap(\\s|_)[0-9]',Document_Name),],
  usfs_exdocs[!PROJECT_ID  %in%  usfs_eadocs$PROJECT_ID,][grepl('Chapter(\\s|_|)[0-9]',Document_Name),],
  usfs_exdocs[!PROJECT_ID  %in%  usfs_eadocs$PROJECT_ID,][grepl('EAFor',Document_Name),],
  usfs_exdocs[!PROJECT_ID  %in%  usfs_eadocs$PROJECT_ID,][grepl('_final_',Document_Name),],
  usfs_exdocs[!PROJECT_ID  %in%  usfs_eadocs$PROJECT_ID,][grepl('(\\s|[a-z0-9])(EA|FEA)(\\s|[a-z0-9]|$)',Document_Name)&!grepl('Draft|DRAFT|Map',Document_Name),])
usfs_eadocs = rbind(usfs_eadocs,rbindlist(adins))

usfs_docs = rbind(usfs_eadocs,usfs_eis_docs,use.names = T)

usfs_docs = usfs_docs[PROJECT_ID!='37159'|FILE_NAME=='37159_82381_FSPLT3_1455561.pdf',]
usfs_docs = usfs_docs[PROJECT_ID!='40028'|FILE_NAME=='40028_90675_FSPLT3_4668375.pdf',]
usfs_docs = usfs_docs[PROJECT_ID!='50578'|FILE_NAME=='50578_105465_FSPLT3_4483293.pdf',]
usfs_docs = usfs_docs[PROJECT_ID!='47798'|FILE_NAME=='47798_102553_FSPLT3_3864370.pdf',]
usfs_docs = usfs_docs[PROJECT_ID!='41284'|FILE_NAME=='41284_94137_FSPLT3_4537667.pdf',]
usfs_docs = usfs_docs[PROJECT_ID!='44969'|FILE_NAME=='44969_99420_FSPLT3_3811726.pdf',]
usfs_docs = usfs_docs[PROJECT_ID!='37887'|FILE_NAME=='37887_84513_FSPLT2_261596.pdf',]
usfs_docs = usfs_docs[PROJECT_ID!='40790'|FILE_NAME=='40790_92768_FSPLT3_1607609.pdf',]
usfs_docs = usfs_docs[PROJECT_ID!='50871'|FILE_NAME=='50871_105786_FSPLT3_4448309.pdf',]
usfs_docs = usfs_docs[FILE_NAME!='48884_103699_FSPLT3_4406530.pdf',]
usfs_docs = usfs_docs[PROJECT_ID!='29099'|FILE_NAME=='29099_59579_FSPLT3_2067673.pdf',]


temp_excludes = c('PROJECT_ID','Project_Status','Project_Page')
usfs = usfs[!duplicated(usfs[,-temp_excludes,with=F]),]
usfs = usfs[YEAR %in% 2013:2019,]
usfs = usfs[PROJECT_ID %in% usfs_docs$PROJECT_ID,]
usfs_docs = usfs_docs[PROJECT_ID %in% usfs$PROJECT_ID,]
usfs_docs$AGENCY= 'Forest Service'
usfs$AGENCY = 'Forest Service'
usfs_docs$YEAR = usfs$YEAR[match(usfs_docs$PROJECT_ID,usfs$PROJECT_ID)]
usfs_docs$FILE_LOC = paste0('../eis_documents/agency_nepa_libraries/usfs/text_as_datatable/',usfs_docs$YEAR)



usfs$MASTER_ID = usfs$PROJECT_ID
usfs_docs$MASTER_ID = usfs_docs$PROJECT_ID
usfs$MASTER_ID[usfs$MASTER_ID %in% crosswalk$USFS_ID] <- crosswalk$EPA_ID[match(usfs$MASTER_ID[usfs$MASTER_ID %in% crosswalk$USFS_ID],crosswalk$USFS_ID)]
usfs_docs$MASTER_ID[usfs_docs$MASTER_ID %in% crosswalk$USFS_ID] <- crosswalk$EPA_ID[match(usfs_docs$MASTER_ID[usfs_docs$MASTER_ID %in% crosswalk$USFS_ID],crosswalk$USFS_ID)]
usfs_docs$PROJECT_TYPE = ifelse(usfs_docs$MASTER_ID %in% epa$EIS.Number,'EIS', 'EA')
usfs$PROJECT_TYPE = ifelse(usfs$MASTER_ID %in% epa$EIS.Number,'EIS', 'EA')

usfs_docs = usfs_docs[!grepl('^[0-9]{8}(\\s|_)(APPENDIX|APPX|APPENDICES)|BIOP|SURVEY|SUPPLEMENT|EVALUATION|CONCEPT PLANS|DELINEATION|LETTER|DECISION REPORT|INITIAL SITE|PRELIMINARY|HEARING|APPENDIX\\.PDF$|EXHIBITS|DEIS|DRAFT|MAP|COMMENTS|RESPONSE|RESOURCEREPORT|ATTACHMENT|POWERPOINT|STUDY|BUDGET|MEETING|MINUTES|FONSI|FIGURE|TECHNICAL(\\s|_)MEMORANDUM|FINDING(\\s|_)OF(\\s|_)NO(\\s|_)SIGNIFICANT|RECORD(\\s|_)OF(\\s|_)DECISION|TABLE(\\s|_)OF(\\s|_)CONTENTS',toupper(Document_Name))&!grepl('GRSA DEA|_EIR\\+EA\\+EA_508',toupper(Document_Name)),]
#usfs_docs = usfs_docs[grep('App[A-Z]',FILE_NAME,invert = T),]
app_drops = which(grepl('Appendix|Appendices|appendix|appendices|appx|Appendice|Appenices|Applendix|Appendices|APPENDIX|APPENDICES|Appedicies',usfs_docs$Document_Name)&!grepl('and Appendix|to Appendix|Chapter|Front|Cover|FEIS__Appendices',usfs_docs$Document_Name))
usfs_docs = usfs_docs[-app_drops,]
app_drops = which(grepl('App([A-Z]|\\.[1-9])|App\\s[A-Z]|(App|APP|Appx|Appndx|Appnd)(\\s|_|\\.|-)[A-Z1-9]|APX(\\s|_|\\.|-)[A-Z1-9]|Apps[A-Z]|Appdx|Appedix|Appdcs(_|\\.|\\s)[0-9A-Z]|(A|a)ppend[A-Z]',usfs_docs$Document_Name)&!grepl('^22287|Front.*to_App\\.1|withAppA|and_App(\\s|_|)A|Chapter|Cover|wApp|Front',usfs_docs$Document_Name))
usfs_docs = usfs_docs[-app_drops,]

usfs_docs = usfs_docs[!duplicated(usfs_docs),]

usfs_docs = usfs_docs[order(PROJECT_ID,Document_Name),][!duplicated(paste(str_remove_all(Document_Name,'[0-9]'),PROJECT_ID))]



fl = '../eis_documents/agency_nepa_libraries/usfs/text_as_datatable'
usfs_docs$FILE_LOC = paste(fl,usfs_docs$YEAR,sep = '/')
usfs_docs$FILE_NAME = paste0(usfs_docs$PROJECT_ID,'--',gsub('\\.PDF$|\\.pdf$','.txt',usfs_docs$FILE_NAME))


usfs_sub = usfs[,.(MASTER_ID,PROJECT_ID,YEAR,PROJECT_TYPE,AGENCY)]
usfs_sub_docs = usfs_docs[,.(YEAR,FILE_NAME, FILE_LOC,PROJECT_TYPE,AGENCY,PROJECT_ID,MASTER_ID)]


ex = file.exists(paste(usfs_sub_docs$FILE_LOC,usfs_sub_docs$FILE_NAME,sep = '/'))


usfs_sub_docs = usfs_sub_docs[ex,]


require(pbapply)
usfs_docs_split = split(usfs_sub_docs,usfs_sub_docs$PROJECT_ID)
usfs_docs_split  = usfs_docs_split[sapply(usfs_docs_split ,nrow)>1]
drop_list = pblapply(seq_along(usfs_docs_split),function(f){
bos = combn(paste(usfs_docs_split[[f]]$FILE_LOC,usfs_docs_split[[f]]$FILE_NAME,sep ='/'),2,simplify = F)
text_list = lapply(bos,function(a) lapply(a,function(b) fread(b)$text))
for(t in seq_along(text_list)){
distmat = stringdist::stringdistmatrix(text_list[[t]][[1]],text_list[[t]][[2]],method = 'qgram')
min_loc =apply(distmat,1,which.min)
run = rle(diff(min_loc)==1)
if(any(run$values)){
props = max(run$lengths[run$values])/c(length(text_list[[t]][[1]]),length(text_list[[t]][[2]]))
if(sum(props>0.5)==1){return(bos[[t]][props>0.5])}
if(sum(props>0.5)==2){return(bos[[t]][which.min(c(length(text_list[[t]][[1]]),length(text_list[[t]][[2]])))])}
else{return(NULL)}
}
}
},cl = 5)
gc()
usfs_sub_docs = usfs_sub_docs[!FILE_NAME %in% basename(unlist(drop_list)),]



  


###############
###############

blm = fread('../eis_documents/agency_nepa_libraries/blm/metadata/project_record.csv')
blm2 = fread('../eis_documents/agency_nepa_libraries/blm/metadata/project_record_lup.csv')
setnames(blm2,c('Type of Planning Effort','Plan Name'),c('Doc Type','Project Name'))
blm = rbind(blm,blm2,fill = T)


blm = blm[!`Project Status` %in% c('Withdrawn','Public Scoping','Paused'),]
blm = blm[!`NEPA #`%in% c('DOI-BLM-MT-C010-2019-0037-EA','DOI-BLM-WY-D030-2019-0067-EA'),]

blm_docs = fread('../eis_documents/agency_nepa_libraries/blm/metadata/document_record.csv')
blm_docs2 = fread('../eis_documents/agency_nepa_libraries/blm/metadata/document_lup_record.csv')
blm_docs = rbind(blm_docs,blm_docs2,fill = T)


blm_docs = blm_docs[!grepl('Files',`Available Formats`),]
raw_docs = blm_docs
blm_docs = blm_docs[!grepl('^Appendix',`Document Name`),]
blm_docs = blm_docs[!grepl('^Appendix',File_Name),]
blm_docs = blm_docs[!grepl('jpg$|png$|txt$|zip$',File_Name,perl = T),]

#blm[`NEPA #`=='DOI-BLM-NM-P020-2016-1249-EA',]
#blm_docs[NEPA_ID=='DOI-BLM-NM-P020-2016-1249-EA',]
# 
# blm_eis = blm[blm$`Doc Type` == 'EIS',]
# blm_eis_docs = blm_docs[NEPA_ID %in% blm_eis$`NEPA #`,]
# blm_eis$`Project Name` = gsub('\\s\\(.*','',blm_eis$`Project Name`)
# blm_eis$`Project Name`[blm_eis$`Project Name`=="Greater Phoenix Mine Project"] <- "Greater Phoenix Project"  
# blm_eis$`Project Name`[blm_eis$`Project Name`=="Monument Butte Oil and Gas Development Project" ] <- "Monument Butte Area Oil and Gas Development Project, Duchesne and Uintah County, UtahImpact Statement for Newfield Exploration Corporation Monument Butte Oil and Gas Development Project in Uintah and Duchesne Counties"
blm_docs = blm_docs[!duplicated(blm_docs),]



epa$Title[grepl('Monument Butte Oil and Gas Development',epa$Title)] <- "Monument Butte Oil and Gas Development Project" 
epa$Title[grepl("Vegetation Treatments Using Aminopyralid, Fluroxypyr, and Rimsulfuron on Bureau of Land Management Lands in 17 Western States",epa$Title)] <- "Vegetation Treatments Using Aminopyralid Fluroxypyr and Rimsulfuron on BLM Lands in 17 Western States PEIS"
epa$Title[grepl("Kremmling Proposed Resource Management Plan",epa$Title)] <- "Kremmling Resource Management Plan"
epa$Title[grepl("Jarbidge Proposed Resource Management Plan" ,epa$Title)] <- "Revised Jarbidge Resource Management Plan (2010)"
epa$Title[grepl("Lander Field Office Planning Area Project  Proposed Resource Management Plan",epa$Title)] <- "Lander Resource Management Plan Revision"
blm = blm[`Doc Type` == 'EA'|`Project Name` %in% epa[AGENCY=='Bureau of Land Management',][!(PROJECT_ID %in% epa_sub_docs$PROJECT_ID),]$Title,] 
blm_eis = blm$`NEPA #`[grep('EIS',blm$`Doc Type`)]


blm$`Decision Date`[blm$`Decision Date`=='02/25/2109']<-'02/25/2019'
blm$Year <- (str_extract(gsub('^[A-Z0-9]{3,}-','',gsub('DOI-BLM-([A-Za-z0-9]|\\s)+-','',blm$`NEPA #`)),'^[0-9]{4}'))
blm <- blm[!grepl('CX',blm$`NEPA #`),]
blm = blm[Year %in% 2013:2019 | `Doc Type`!='EA',]

blm_docs = blm_docs[!grepl('Files',`Available Formats`),]
blm_docs = blm_docs[File_Name != 'FONSI.pdf',]

blm_docs = blm_docs[!grepl('^Fig[^h]',`Document Name`),]

blm_docs$Year = blm$Year[match(blm_docs$NEPA_ID,blm$`NEPA #`)]
blm_docs = blm_docs[NEPA_ID%in%blm$`NEPA #`,]


name_is_file = blm_docs[gsub('\\.pdf','',blm_docs$File_Name)==blm_docs$NEPA_ID]$NEPA_ID
blm_docs = blm_docs[!NEPA_ID%in% name_is_file|gsub('\\.pdf','',File_Name)==NEPA_ID,]


blm_docs = blm_docs[!grepl('Scoping',Set_Type),]
blm_docs = blm_docs[!grepl('^APPENDIX',toupper(File_Name)),]
blm_docs = blm_docs[!grepl('^FONSI',toupper(File_Name)),]
blm_docs = blm_docs[!grepl('MAP_PACKET',toupper(File_Name)),]
blm_docs = blm_docs[!grepl('ATTACHMENT',toupper(File_Name)),]
blm_docs = blm_docs[!grepl('PRELIMINARY(\\s|_|-)EA$|EA(\\s|_|-)PRELIMINARY$|FONSI_PRELIMINARY$',toupper(`Document Name`)),]
blm_docs = blm_docs[!grepl('^FINDING OF NO SIG',toupper(`Document Name`)),]
blm_docs = blm_docs[!grepl('^(UNSIGNED|APPROVED) FINDING OF NO SIG',toupper(`Document Name`)),]
blm_docs = blm_docs[!grepl('^DEAR READER',toupper(`Document Name`)),]
blm_docs = blm_docs[!grepl('DEAR READER',toupper(`Document Name`)),]
blm_docs = blm_docs[!grepl('SOIL_REPORT',toupper(`Document Name`)),]

blm_docs = blm_docs[!grepl('_Eval\\.pdf',File_Name),]

blm_docs = blm_docs[!grepl('NOTICE OF REALTY',toupper(`Document Name`)),]
blm_docs = blm_docs[toupper(`Document Name`)!='FONSI',]



blm_docs$TAG = grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',blm_docs$File_Name,perl = T)

blm_docs = blm_docs[!grepl('Scoping',`Document Name`),]
blm_docs = blm_docs[!grepl('^Notice of',`Document Name`),]

blm_docs = blm_docs[!Set_Name %in% c('Lease Sale Summary Cultural Report','Scoping Meeting Posters','Public Comments Received','Standards Assessment',
                                    'Public Scoping Meeting (3/21/18) Posters and Presentation','Notice of Competitive Lease Sale'),]


drops = unique(blm_docs[Set_Name=='Final EA',]$NEPA_ID)
blm_docs = blm_docs[Set_Name=='Final EA' | !NEPA_ID %in% drops,]

blm_docs = blm_docs[`Document Name`!='DR',]

blm_docs = blm_docs[!grepl('App',File_Name),]
blm_docs = blm_docs[!grepl('Final_EA_Figures',File_Name),]

blm_docs = blm_docs[!grepl('^Figure',File_Name),]
blm_docs = blm_docs[!grepl('_MAP\\.pdf$',File_Name),]
blm_docs = blm_docs[!grepl('DECISION_RECORD',toupper(File_Name)),]
blm_docs = blm_docs[!grepl('^NEWS',toupper(File_Name)),]
blm_docs = blm_docs[!grepl('ISSUE_MAPS',toupper(File_Name)),]


blm_docs = blm_docs[NEPA_ID!='DOI-BLM-CO-N050-2019-0040-EA'|File_Name=='NW_District_EA_September2019.pdf',]
blm_docs = blm_docs[NEPA_ID!='DOI-BLM-CA-C060-2018-0064-EA'|File_Name=='2018-0064-EA_MOC,_Round_Mountain;_Three_Applications_for_Permit_to_Drill.pdf',]




blm_docs = blm_docs[!NEPA_ID%in%blm_docs[,list(sum(grepl('EA\\.pdf$',File_Name)),sum(grepl('EA_(Final|FINAL)\\.pdf',File_Name))),by=.(NEPA_ID)][V1>0&V2>0,]$NEPA_ID|
           grepl('EA_(Final|FINAL)\\.pdf',File_Name),]

blm_docs = blm_docs[!NEPA_ID%in%blm_docs[,list(sum(grepl('^DR',File_Name)),sum(grepl('EA-FONSI',File_Name)),sum(grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',File_Name)&!grepl('^DR|EA-FONSI',File_Name))),by=.(NEPA_ID)][{V1>0|V2>0}&V3>0,]$NEPA_ID|
                      grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',File_Name)&!grepl('^DR|EA-FONSI',File_Name),]


blm_docs = blm_docs[toupper(`Document Name`)!='DECISION RECORD',]
blm_docs = blm_docs[toupper(`Document Name`)!='TECHNICAL RESOURCES REPORT',]
blm_docs = blm_docs[toupper(`Document Name`)!='SCOPING INFORMATION PACKAGE',]
blm_docs = blm_docs[toupper(`Document Name`)!='DECISION DOCUMENT',]
blm_docs = blm_docs[toupper(`Document Name`)!='DECISION RECORD\\.PDF',]
blm_docs = blm_docs[toupper(`Document Name`)!='DR\\.PDF',]
blm_docs = blm_docs[toupper(`Document Name`)!='PROJECT MAP',]
blm_docs = blm_docs[toupper(`Document Name`)!='SCOPING NOTICE',]
blm_docs = blm_docs[toupper(`Document Name`)!='SCOPING LETTER',]
blm_docs = blm_docs[toupper(`Document Name`)!='DRSIGN',]
blm_docs = blm_docs[toupper(`Document Name`)!='SIGNED_FONSI',]
blm_docs = blm_docs[toupper(`Document Name`)!='SIGNED FONSI',]
blm_docs = blm_docs[toupper(`Document Name`)!='UNSIGNED FONSI',]
blm_docs = blm_docs[toupper(`Document Name`)!='SIGNED_DR',]
blm_docs = blm_docs[toupper(`Document Name`)!='MAP',]
blm_docs = blm_docs[!grepl('Draft_Environmental_Assessment',File_Name),]
blm_docs = blm_docs[toupper(`Document Name`)!='APPENDICES',]
blm_docs = blm_docs[toupper(`Document Name`)!='ADDENDUM',]
blm_docs = blm_docs[toupper(`Document Name`)!='NEWS RELEASE',]
blm_docs = blm_docs[toupper(`Document Name`)!='PRESS RELEASE',]
blm_docs = blm_docs[toupper(`Document Name`)!='DECISION',]
blm_docs = blm_docs[!grepl('LETTER',toupper(`Document Name`)),]
blm_docs = blm_docs[!grepl('^DRAFT',toupper(`Document Name`)),]
blm_docs = blm_docs[!grepl('^MAPS',toupper(`Document Name`)),]
blm_docs = blm_docs[!grepl('^MAP',toupper(File_Name)),]
blm_docs = blm_docs[!grepl('^PRELIMINARY',toupper(`Document Name`)),]
blm_docs = blm_docs[!grepl('^APPLICATION FOR PERMIT',toupper(`Document Name`)),]

blm_docs = blm_docs[!grepl('Version(|_|\\s)1',Set_Name),]
blm_docs = blm_docs[grepl('Version(|_|\\s)3' ,Set_Name)| !NEPA_ID %in% blm_docs[,list(sum(grepl('Version(|_|\\s)2',Set_Name)),sum(grepl('Version(|_|\\s)3',Set_Name))),by=.(NEPA_ID)][V1>0&V2>0,]$NEPA_ID,]

blm_docs = blm_docs[order(-mdy(`Release Date`)),][!duplicated(paste(NEPA_ID,`Document Name`)),]
blm_docs = blm_docs[order(-mdy(`Release Date`)),][!duplicated(paste(NEPA_ID,File_Name)),]

blm_docs = blm_docs[`Document Name`!='Environmental Assessment Maps',]
blm_docs = blm_docs[!grepl('Allotment(s|) RHA$',`Document Name`),]

FID = blm_docs$NEPA_ID[toupper(blm_docs$`Document Name`)%in%c('FINAL EA','FINAL ENVIRONMENTAL ASSESSMENT')]
DID = blm_docs$NEPA_ID[toupper(blm_docs$`Document Name`)%in%c('EA','ENVIRONMENTAL ASSESSMENT','DRAFT EA','DRAFT ENVIRONMENTAL ASSESSMENT')]
blm_docs = blm_docs[!NEPA_ID %in% intersect(FID,DID)|toupper(blm_docs$`Document Name`)%in%c('FINAL EA','FINAL ENVIRONMENTAL ASSESSMENT'),]


blm_docs = blm_docs[!grepl('ROUTE_REPORT|RTE_RPT',toupper(File_Name)),]
blm_docs = blm_docs[!grepl('SURVEY',toupper(File_Name)),]
blm_docs = blm_docs[!grepl('INITIAL',toupper(File_Name)),]
blm_docs = blm_docs[!grepl('HEARING',toupper(File_Name)),]
blm_docs = blm_docs[!grepl('EXHIBITS',toupper(File_Name)),]

#blm_docs = blm_docs[!grepl('^[0-9]{8}(\\s|_)(APPENDIX|APPX|APPENDICES)|BIOP|SURVEY|SUPPLEMENT|EVALUATION|CONCEPT PLANS|DELINEATION|LETTER|DECISION REPORT|INITIAL SITE|RTE_RPT|HEARING|APPENDIX\\.PDF$|EXHIBITS|DEIS|DRAFT|MAP|COMMENTS|RESPONSE|RESOURCEREPORT|POWERPOINT|STUDY|BUDGET|MEETING|MINUTES|FONSI|FIGURE|TECHNICAL(\\s|_)MEMORANDUM|FINDING(\\s|_)OF(\\s|_)NO(\\s|_)SIGNIFICANT|RECORD(\\s|_)OF(\\s|_)DECISION|TABLE(\\s|_)OF(\\s|_)CONTENTS',toupper(FILE_NAME))&!grepl('GRSA DEA|_EIR\\+EA\\+EA_508',toupper(FILE_NAME)),]

blm_docs$kbsize = as.numeric(str_remove_all(blm_docs$`Available Formats`,'\\(|\\)|\\sKB'))
blm_docs$kbsize[!is.na(blm_docs$kbsize)&blm_docs$kbsize==0] <- NA
blm_docs$kbsize[grepl('Volume|Chapter',blm_docs$`Document Name`)] <- NA

small = blm_docs[!is.na(kbsize)&kbsize<100&!grepl('EIS',`NEPA_ID`),]$NEPA_ID
small_with_big = small[small %in% blm_docs[!is.na(kbsize)&kbsize>100&!grepl('EIS',`NEPA_ID`),]$NEPA_ID]
blm_docs = blm_docs[is.na(kbsize)|!NEPA_ID %in% small_with_big|{NEPA_ID %in% small_with_big & kbsize>100},]

small = blm_docs[!is.na(kbsize)&kbsize<300&!grepl('EIS',`NEPA_ID`),]$NEPA_ID
small_with_big = small[small %in% blm_docs[!is.na(kbsize)&kbsize>1000&!grepl('EIS',`NEPA_ID`),]$NEPA_ID]
blm_docs = blm_docs[is.na(kbsize)|!NEPA_ID %in% small_with_big|{NEPA_ID %in% small_with_big & kbsize>1000},]


solo_docs = blm_docs[NEPA_ID %in% blm_docs[,.N,by=.(NEPA_ID)][N==1,]$NEPA_ID|NEPA_ID %in% blm_docs$NEPA_ID[grepl('Volume|Chapter',blm_docs$`Document Name`)],]
multi_docs = blm_docs[NEPA_ID %in% blm_docs[,.N,by=.(NEPA_ID)][N>1,]$NEPA_ID,]
one_EA = multi_docs[grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`)),][,.N,by=.(NEPA_ID)][N==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% one_EA & grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]

one_EA = multi_docs[grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(File_Name)),][,.N,by=.(NEPA_ID)][N==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% one_EA & grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(File_Name)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]


one_revisedEA = multi_docs[grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&grepl('REVISED',toupper(`Document Name`)),][,.N,by=.(NEPA_ID)][N==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% one_revisedEA  & grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&grepl('REVISED',toupper(`Document Name`)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]


one_finalEA = multi_docs[grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&grepl('FINAL',toupper(`Document Name`)),][,.N,by=.(NEPA_ID)][N==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% one_finalEA & grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&grepl('FINAL',toupper(`Document Name`)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]


one_finalEA = multi_docs[grepl('FINAL',toupper(File_Name))&grepl('(EA|FEA)',File_Name),][,.N,by=.(NEPA_ID)][N==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% one_finalEA & grepl('FINAL',toupper(File_Name))&grepl('(EA|FEA)',File_Name),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]


one_nondecEA = multi_docs[grepl('(^|_|-|\\s)(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&!grepl('DECISION',toupper(`Document Name`)),][,.N,by=.(NEPA_ID)][N==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% one_nondecEA & grepl('(^|_|-|\\s|FINAL)(EA|FEA)($|_|-|\\s)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&!grepl('DECISION',toupper(`Document Name`)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]

one_docsEA = multi_docs[grepl('(^|_|-|\\s)(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&!grepl('DOCX',toupper(File_Name)),][,.N,by=.(NEPA_ID)][N==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% one_docsEA  & grepl('(^|_|-|\\s)(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&!grepl('DOCX',toupper(File_Name)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]


one_nondrEA = multi_docs[grepl('(^|_|-|\\s)(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&!grepl('(^|_)DR(_|-)',toupper(File_Name)),][,.N,by=.(NEPA_ID)][N==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% one_nondrEA  & grepl('(^|_|-|\\s)(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&!grepl('(^|_)DR(_|-)',toupper(File_Name)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]

one_nonfonsiEA = multi_docs[grepl('(^|_|-|\\s)(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&!grepl('FONSI',toupper(File_Name)),][,.N,by=.(NEPA_ID)][N==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% one_nonfonsiEA & grepl('(^|_|-|\\s)(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))&!grepl('FONSI',toupper(File_Name)),])

dropFONSI = multi_docs[,list(sum(grepl('FONSI$',`Document Name`)),sum(grepl('EA$',`Document Name`))),by=.(NEPA_ID)][V1==1&V2==1,]$NEPA_ID
solo_docs = rbind(solo_docs,multi_docs[NEPA_ID %in% dropFONSI & grepl('EA$',`Document Name`),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]


solo = multi_docs[,sum(grepl('(^|_|-|\\s)(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`))),by=.(NEPA_ID)][V1==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% solo  & grepl('(^|_|-|\\s)(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(`Document Name`)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]


solo = multi_docs[,sum(grepl('(^|_|-|\\s)(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(File_Name))),by=.(NEPA_ID)][V1==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% solo  & grepl('(^|_|-|\\s)(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(File_Name)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]


v3 = multi_docs[,sum(grepl('(^|_|-|\\s|[0-9])(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(File_Name))&grepl('V3',toupper(File_Name))),by=.(NEPA_ID)][V1==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% v3  & 
          grepl('(^|_|-|\\s|[0-9])(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(File_Name))&grepl('V3',toupper(File_Name)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]



v2 = multi_docs[,sum(grepl('(^|_|-|\\s|[0-9])(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(File_Name))&grepl('V2',toupper(File_Name))),by=.(NEPA_ID)][V1==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% v2  & 
                               grepl('(^|_|-|\\s|[0-9])(EA|FEA)($|_|-|\\s|\\.|FINAL)|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT',toupper(File_Name))&grepl('V2',toupper(File_Name)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]


oneEA = multi_docs[,sum(grepl('(^|_|-|\\s|[0-9])(EA|FEA|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT)($|_|-|\\s|\\.|[0-9])',toupper(File_Name))),by=.(NEPA_ID)][V1==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% oneEA & grepl('(^|_|-|\\s|[0-9])(EA|FEA|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT)($|_|-|\\s|\\.|[0-9])',toupper(File_Name)),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]

eaUpdate = multi_docs[grepl('UPDATED',toupper(`File_Name`))&grepl('EA',File_Name)&!grepl('DR|FONSI|Decision|DECISON',File_Name),.N,by=.(NEPA_ID)][N==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% eaUpdate  & grepl('UPDATED',toupper(`File_Name`))&grepl('EA',File_Name)&!grepl('DR|FONSI|Decision|DECISON',File_Name),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]


ea508 = multi_docs[grepl('508',`File_Name`)&grepl('EA',File_Name)&!grepl('DR|FONSI|Decision|DECISON',File_Name),.N,by=.(NEPA_ID)][N==1,]$NEPA_ID
solo_docs = rbind(solo_docs,
                  multi_docs[NEPA_ID %in% ea508   & grepl('508',toupper(`File_Name`))&grepl('EA',File_Name)&!grepl('DR|FONSI|Decision|DECISON',File_Name),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]


eaNOAPP = multi_docs[!grepl('APPEND',`File_Name`)&grepl('(^|_|-|\\s|[0-9])(EA|FEA|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT)($|_|-|\\s|\\.|[0-9])',File_Name)&!grepl('DR|FONSI|Decision|DECISON',File_Name),.N,by=.(NEPA_ID)][N==1,]$NEPA_ID

solo_docs = rbind(solo_docs,
          multi_docs[!grepl('APPEND',`File_Name`)&grepl('(^|_|-|\\s|[0-9])(EA|FEA|ENVIRONMENTAL(_|-|\\s|)ASSESSMENT)($|_|-|\\s|\\.|[0-9])',File_Name)&!grepl('DR|FONSI|Decision|DECISON',File_Name),])
multi_docs = multi_docs[!NEPA_ID %in% solo_docs$NEPA_ID,]



##############################################

blm_docs = rbind(solo_docs,multi_docs,use.names = T)


blm_docs$`Document Name` <- gsub('--','-',blm_docs$`Document Name`)
blm_docs = blm_docs[!duplicated(paste(blm_docs$File_Name,blm_docs$NEPA_ID)),]


blm$PROJECT_TYPE = ifelse(blm$`Doc Type`=='EA','EA','EIS')
blm_docs$PROJECT_TYPE = blm$PROJECT_TYPE[match(blm_docs$NEPA_ID,blm$`NEPA #`)]
blm$AGENCY = 'Bureau of Land Management'
blm_docs$AGENCY = 'Bureau of Land Management'

setnames(blm,c("NEPA #",'Year'),c('PROJECT_ID','YEAR'))
setnames(blm_docs,c('NEPA_ID','File_Name'),c('PROJECT_ID','FILE_NAME'))




keep_second_doc = intersect(blm_docs$PROJECT_ID[grepl('V1|v1|V\\.1|v\\.1',blm_docs$`Document Name`)],blm_docs$PROJECT_ID[grepl('V2|v2|V\\.2|v\\.2',blm_docs$`Document Name`)])
blm_docs = blm_docs[!PROJECT_ID %in% keep_second_doc|!grepl('V1|v1|V\\.1|v\\.1',`Document Name`),]

keep_third_doc = intersect(blm_docs$PROJECT_ID[grepl('V2|v2|V\\.2|v\\.2',blm_docs$`Document Name`)],blm_docs$PROJECT_ID[grepl('V3|v3|V\\.3|v\\.3',blm_docs$`Document Name`)])
blm_docs = blm_docs[!PROJECT_ID %in% keep_third_doc|!grepl('V2|v2|V\\.2|v\\.2',`Document Name`),]

drop_prelim = intersect(blm_docs$PROJECT_ID[grepl('PRELIM',toupper(blm_docs$`Document Name`))],blm_docs$PROJECT_ID[grepl('FINAL',toupper(blm_docs$`Document Name`))])
blm_docs = blm_docs[!PROJECT_ID %in% drop_prelim|!grepl('PRELIM',toupper(`Document Name`)),]

keep_second_doc = intersect(blm_docs$PROJECT_ID[grepl('V1|v1|V\\.1|v\\.1',blm_docs$FILE_NAME)],blm_docs$PROJECT_ID[grepl('V2|v2|V\\.2|v\\.2',blm_docs$FILE_NAME)])
blm_docs = blm_docs[!PROJECT_ID %in% keep_second_doc|!grepl('V1|v1|V\\.1|v\\.1',FILE_NAME),]

keep_third_doc = intersect(blm_docs$PROJECT_ID[grepl('V2|v2|V\\.2|v\\.2',blm_docs$FILE_NAME)],blm_docs$PROJECT_ID[grepl('V3|v3|V\\.3|v\\.3',blm_docs$FILE_NAME)])
blm_docs = blm_docs[!PROJECT_ID %in% keep_third_doc|!grepl('V2|v2|V\\.2|v\\.2',FILE_NAME),]

drop_prelim = intersect(blm_docs$PROJECT_ID[grepl('PRELIM',toupper(blm_docs$FILE_NAME))],blm_docs$PROJECT_ID[grepl('FINAL',toupper(blm_docs$FILE_NAME))])
blm_docs = blm_docs[!PROJECT_ID %in% drop_prelim|!grepl('PRELIM',toupper(FILE_NAME)),]


blm_docs$YEAR = blm$YEAR[match(blm_docs$PROJECT_ID,blm$PROJECT_ID)]


blm_docs = blm_docs[PROJECT_ID!='DOI-BLM-AZ-A030-2018-0002-EA'|FILE_NAME=='Mt._Logan_GPR_EA_post_comment_analysis_Final_508C.pdf',]
blm_docs = blm_docs[PROJECT_ID!='DOI-BLM-CA-C060-2017-0067-EA',]

blm_sub = blm[,.(PROJECT_ID,YEAR,AGENCY,PROJECT_TYPE)]

fl = '../eis_documents/agency_nepa_libraries/blm/text_as_datatable'
blm_docs$FILE_LOC = paste(fl,blm_docs$YEAR,sep = '/')
blm_docs$FILE_NAME = paste0(blm_docs$PROJECT_ID,'--',gsub('\\.PDF$|\\.pdf$|\\.docx$|\\.doc$','.txt',blm_docs$FILE_NAME))
#blm_docs$FILE_LOC = paste('../eis_documents/agency_nepa_libraries/blm/nepa_documents',blm_docs$YEAR,sep = '/')
#doe_docs$FILE_NAME = paste0(doe_docs$PROJECT_ID,'--',doe_docs$FILE_NAME,'.txt')

blm_sub_docs = blm_docs[,.(PROJECT_ID,YEAR,FILE_NAME,FILE_LOC,PROJECT_TYPE,AGENCY)]

ex = file.exists(paste(blm_sub_docs$FILE_LOC,blm_sub_docs$FILE_NAME,sep = '/'))

blm_sub_docs = blm_sub_docs[ex,]
require(pbapply)
blm_docs_split = split(blm_sub_docs,blm_sub_docs$PROJECT_ID)
blm_docs_split  = blm_docs_split[sapply(blm_docs_split ,nrow)>1]
drop_list = pblapply(seq_along(blm_docs_split),function(f){
  bos = combn(paste(blm_docs_split[[f]]$FILE_LOC,blm_docs_split[[f]]$FILE_NAME,sep ='/'),2,simplify = F)
  text_list = lapply(bos,function(a) lapply(a,function(b) fread(b)$text))
  for(t in seq_along(text_list)){
    distmat = stringdist::stringdistmatrix(text_list[[t]][[1]],text_list[[t]][[2]],method = 'qgram')
    min_loc =apply(distmat,1,which.min)
    run = rle(diff(min_loc)==1)
    if(any(run$values)){
      props = max(run$lengths[run$values])/c(length(text_list[[t]][[1]]),length(text_list[[t]][[2]]))
      if(sum(props>0.5)==1){return(bos[[t]][props>0.5])}
      if(sum(props>0.5)==2){return(bos[[t]][which.min(c(length(text_list[[t]][[1]]),length(text_list[[t]][[2]])))])}
      else{return(NULL)}
    }
  }
},cl = 5)
gc()


blm_sub_docs = blm_sub_docs[!FILE_NAME %in% basename(unlist(drop_list)),]



blm_sub$MASTER_ID = blm_sub$PROJECT_ID
blm_sub_docs$MASTER_ID = blm_sub_docs$PROJECT_ID

eis_blm = blm[blm$`Project Name` %in% epa[AGENCY=='Bureau of Land Management',][!(PROJECT_ID %in% epa_sub_docs$PROJECT_ID),]$Title,] 
eis_blm$EIS.Number = epa$EIS.Number[match(eis_blm$`Project Name`,epa$Title)]

blm_sub$MASTER_ID[blm_sub$PROJECT_ID %in% eis_blm$PROJECT_ID] <- eis_blm$EIS.Number[match(blm_sub$MASTER_ID[blm_sub$PROJECT_ID %in% eis_blm$PROJECT_ID],eis_blm$PROJECT_ID)]

blm_sub_docs$MASTER_ID <- blm_sub$MASTER_ID[match(blm_sub_docs$PROJECT_ID,blm_sub$PROJECT_ID)]



# ###############
# ###############
# 
# # doe_doc_file = '../eis_documents/agency_nepa_libraries/doe/metadata/doe_nepa_document_record.RDS'
# # doe_docs = readRDS(doe_doc_file)
# # doe_proj_file = '../eis_documents/agency_nepa_libraries/doe/metadata'
library(data.table)
doe = readRDS('../eis_documents/agency_nepa_libraries/doe/metadata/doe_nepa_record.RDS')
doe = doe[!duplicated(doe),]
setnames(doe,c('Date','Project_Type'),c('YEAR','PROJECT_TYPE'))

doe = doe[!is.na(YEAR)&YEAR>=2013,]

doe_docs = readRDS('../eis_documents/agency_nepa_libraries/doe/metadata/doe_nepa_document_record.RDS')
doe_docs$FILE_NAME <- basename(doe_docs$DOCUMENT_URL)
setnames(doe,'NEPA_ID','PROJECT_ID')
setnames(doe_docs,'NEPA_ID','PROJECT_ID')

keep_eis = c( "EIS-0400")
doe = doe[PROJECT_TYPE=='EA'|PROJECT_ID %in% keep_eis,]
doe_docs = doe_docs[PROJECT_ID %in% doe$PROJECT_ID,]

doe = doe[!grepl('adopted the EA|DOE was.a cooperating agency|DOE was a cooperating agency|DOE participated as a cooperating agency|DOE as cooperating agencies|DOE is participating as a cooperating agency|DOE is a cooperating agency|DOE as a cooperating agency|DOE was a cooperating|DOE\\, a cooperating|DOE\\, Office of Fossil Energy\\, was a cooperating agency|DOE\\’s National Nuclear Security Administration \\(NNSA\\) as cooperating agency',project_description),]
doe_docs = doe_docs[PROJECT_ID %in% doe$PROJECT_ID,]

doe_docs$YEAR = doe$YEAR[match(doe_docs$PROJECT_ID,doe$PROJECT_ID)]
doe_docs$PROJECT_TYPE = ifelse(doe_docs$PROJECT_ID %in% keep_eis,'EIS','EA')
doe_docs = doe_docs[FILE_NAME!='EA-1849-S-1--EA-1849-S-1-2014.txt',]

doe_docs = doe_docs[grepl('[A-Za-z]',str_remove(str_remove(doe_docs$FILE_NAME,'.*--'),'\\.txt$')),]

doe_docs = doe_docs[!grepl('Appendix|FONSI|-CX-|Notice of Availability|Notice of Intent|DEA|(Draft|DRAFT|DEIS|draft)(_|\\s|-|)(EA|ea|Environmental|ENVIRONMENTAL|environmental)',DOCUMENT_TITLE),]

doe_docs= doe_docs[!(!grepl('Final Environmental|FEA|(FINAL|final|Final)(\\s|-|_|)(EA|ea|Environmental|environnmental)',DOCUMENT_TITLE)&grepl('FONSI|fonsi|Finding of No|Finding Of No',DOCUMENT_TITLE)),]

doe = doe[PROJECT_ID %in% doe_docs$PROJECT_ID,]

doe$AGENCY = 'Department of Energy'
doe_docs$AGENCY = 'Department of Energy'

fl = '../eis_documents/agency_nepa_libraries/doe/text_as_datatable'

doe_docs$FILE_LOC = paste(fl,doe_docs$YEAR,sep = '/')
doe_docs$FILE_NAME = paste0(doe_docs$PROJECT_ID,'--',gsub('PDF$|pdf$|docx$|doc$','txt',doe_docs$FILE_NAME))

#doe$PROJECT_ID = paste0('DOE-',doe$PROJECT_ID)
#doe_docs$PROJECT_ID = paste0('DOE-',doe_docs$PROJECT_ID)
doe_docs$MASTER_ID = doe_docs$PROJECT_ID
doe$MASTER_ID = doe$PROJECT_ID
doe$MASTER_ID[doe$MASTER_ID=='EIS-0436'] <- 20160029
doe$MASTER_ID[doe$MASTER_ID=='EIS-0400'] <- 20130186


doe_docs$MASTER_ID = doe$MASTER_ID[match(doe_docs$PROJECT_ID,doe$PROJECT_ID)]

doe_sub = doe[,.(PROJECT_ID,YEAR,PROJECT_TYPE,AGENCY,MASTER_ID)]
doe_sub_docs = doe_docs[,.(PROJECT_ID,YEAR,FILE_NAME,FILE_LOC,PROJECT_TYPE,AGENCY,MASTER_ID)]

# blm_sub = blm[,.(PROJECT_ID,YEAR,AGENCY,PROJECT_TYPE)]
# blm_docs$FILE_LOC = paste('../eis_documents/agency_nepa_libraries/blm/nepa_documents',blm_docs$YEAR,sep = '/')
# blm_sub_docs = blm_docs[,.(PROJECT_ID,YEAR,FILE_NAME,FILE_LOC,PROJECT_TYPE,AGENCY)]
# doe_docs$DOCUMENT_TITLE

#   
projects = rbindlist(list(epa_sub,usfs_sub,blm_sub,doe_sub),use.names=T,fill=T)
documents = rbindlist(list(epa_sub_docs,usfs_sub_docs,blm_sub_docs,doe_sub_docs),use.names=T,fill=T)


projects$PROJECT_ID <- gsub('\\s{1,}',' ',projects$PROJECT_ID)
projects$PROJECT_ID <- gsub('\\s{1,}','_',projects$PROJECT_ID)
documents$PROJECT_ID <- gsub('\\s{1,}',' ',documents$PROJECT_ID)
documents$PROJECT_ID <- gsub('\\s{1,}','_',documents$PROJECT_ID)


documents$FILE_NAME <- gsub('\\s{1,}',' ',documents$FILE_NAME)
documents$FILE_NAME <- gsub('\\s{1,}','_',documents$FILE_NAME)

projects = projects[!duplicated(projects),]

documents <- documents[!duplicated(documents),]

fwrite(projects,file = 'scratch/boilerplate/project_candidates.csv')
fwrite(documents,file = 'scratch/boilerplate/document_candidates.csv')

htmlTable(epa[,list(sum(EIS.Number %in% documents$MASTER_ID),.N),by = .(Agency)][order(-N)])
htmlTable(projects[,list(sum(PROJECT_ID %in% documents$PROJECT_ID|MASTER_ID %in% documents$PROJECT_ID),.N),by = .(AGENCY,PROJECT_TYPE)][order(-N)])

