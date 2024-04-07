require(data.table)
require(stringr)

#### this is the raw core data, from eis_documents repository ###

epa = readRDS('../eis_documents/enepa_repository/metadata/eis_record_detail.rds')
docs = readRDS('../eis_documents/enepa_repository/metadata/eis_document_record.rds')
proj.dir = 'climate_in_eis_project/'

#epa = fread(paste0(proj.dir,'input/deis_project_record.csv'))
epa$Title = iconv(epa$Title,'utf8')
epa$Year = str_extract(epa$EIS.Number,'^[0-9]{4}')
epa = epa[as.numeric(Year) %in% 2013:2022,]
epa  = epa[!grepl('ADOPTION|WITHDRAWN|^Withdrawn|^Adoption',Title),]
epa = epa[!EIS.Number%in% c('20170008','20170006'),]
#epa = epa[!EIS.Number%in%c(20170006,20170006),]
epa = epa[grepl('Draft',epa$Document.Type),]
epa = epa[{!grepl(paste(state.name,collapse='|'),epa$Agency)} | grepl('Tennessee Valley',epa$Agency),]

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


#doc_url = 'climate_in_eis_project/input/deis_document_record.csv'
#epa_docs = fread(doc_url)
epa_docs = docs[docs$EIS.Number %in% epa$EIS.Number,]

epa_docs = epa_docs[!grepl('(CEQ|)[0-9]{8,}_(CEQ|)[0-9]{8,}\\.(pdf|PDF)',epa_docs$File_Name),]
epa_docs = epa_docs[!is.na(File_Name),]
epa_docs = epa_docs[grepl("pdf$",File_Name),]
epa_docs = epa_docs[!duplicated(epa_docs)]

epa$YEAR = epa$Year
epa$AGENCY = epa$Agency
epa_docs$YEAR = epa$YEAR[match(epa_docs$EIS.Number,epa$EIS.Number)]
epa_docs$AGENCY = epa$AGENCY[match(epa_docs$EIS.Number,epa$EIS.Number)]

epa_sub = epa[,.(EIS.Number,AGENCY,YEAR)]

epa_docs$File_Name = gsub('\\.pdf\\.pdf$','.pdf',epa_docs$File_Name)

epa_docs$EIS.Number = epa_docs$EIS.Number
epa_docs$FILE_NAME = paste0(epa_docs$EIS.Number,'--',gsub('\\.PDF$|\\.pdf$','.txt',epa_docs$File_Name))


saveRDS(epa,paste0(proj.dir,'data_products/deis_metadata.RDS'))
saveRDS(epa_docs,paste0(proj.dir,'data_products/deis_doc_metadata.RDS'))

