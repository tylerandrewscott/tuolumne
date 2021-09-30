require(data.table)
require(stringr)
proj.dir = 'climate_in_eis_project/'

epa = fread(paste0(proj.dir,'input/eis_record_detail.csv'))
epa = epa[Document=='Draft',]
#epa = epa[Agency=='Bureau of Land Management',]

epa$Title = iconv(epa$Title,'utf8')
epa$Year = str_extract(epa$EIS.Number,'^[0-9]{4}')
epa = epa[as.numeric(Year) %in% 2013:2020,]
epa  = epa[!grepl('ADOPTION|WITHDRAWN|^Withdrawn|^Adoption',Title),]
epa = epa[!EIS.Number%in% c('20170008','20170006'),]
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


doc_url = paste0(proj.dir,'input/eis_document_record.csv')
epa_docs = fread(doc_url)
doc_url2 = paste0(proj.dir,'input/extra_docs.csv')
epa_docs2 = fread(doc_url2)
epa_docs = rbindlist(list(epa_docs,epa_docs2),fill=T)
epa_docs = epa_docs[EIS.Number %in% epa$EIS.Number,]
epa_docs = epa_docs[!grepl('(CEQ|)[0-9]{8}_(CEQ|)[0-9]{8}\\.(pdf|PDF)',epa_docs$File_Name),]
epa_docs = epa_docs[!is.na(File_Name),]
epa_docs = epa_docs[grepl("pdf$",File_Name),]
epa_docs = epa_docs[!duplicated(epa_docs)]
epa_docs= epa_docs[EIS.Number  %in% epa_docs[,.N,by=.(EIS.Number)][N==1,]$EIS.Number | !grepl('^[0-9]{8}(\\s|_)(APPENDIX|APPX|APPENDICES)|BIOP|SURVEY|SUPPLEMENT|COMMENT REPORT|EVALUATION|CONCEPT PLANS|DELINEATION|LETTER|DECISION REPORT|INITIAL SITE|PRELIMINARY|HEARING|APPENDIX\\.PDF$|EXHIBITS|MAP|COMMENTS|RESPONSE|RESOURCEREPORT|ATTACHMENT|POWERPOINT|STUDY|BUDGET|MEETING|MINUTES|ENVIRONMENTAL(\\s|_)ASSESSMENT|FONSI|FIGURE|TECHNICAL(\\s|_)MEMORANDUM|FINDING(\\s|_)OF(\\s|_)NO(\\s|_)SIGNIFICANT|(?<!AND_)RECORD(\\s|_)OF(\\s|_)DECISION',toupper(File_Name),perl = T)&!grepl('GRSA DEIS|_EIR\\+EIS\\+EIS_508',toupper(File_Name)),]
app_drops = grepl('Appendix|Appendices|appendix|appendices|appx|final_biological_opinion|Traffic_Analysis_Addendum|Engineer_Report|Aerial_Atlas|BiologicalAssessment|Appendice|Applendix|Appenices|APPENDIX|APPENDICES|Appedicies',epa_docs$File_Name)&!grepl('and Appendix|to Appendix|Chapter|Front|Cover|\\& Append',epa_docs$File_Name)
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


saveRDS(epa,paste0(proj.dir,'data_products/eis_metadata.RDS'))
saveRDS(epa_docs,paste0(proj.dir,'data_products/eis_doc_metadata.RDS'))

library(htmlTable)


