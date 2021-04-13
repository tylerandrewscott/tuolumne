

pack = c('data.table','stringr','tidyverse','doParallel','pdftools','lubridate')
need = pack[!pack %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(pack,require,character.only=T)

empty_project_record = data.table(PROJECT_ID = character(),YEAR = numeric(),PROJECT_TYPE = character(),AGENCY = character())
empty_doc_dt = data.table(YEAR = numeric(),FILE_NAME = character(), FILE_LOC = character(), PROJECT_TYPE = character(),AGENCY = character())

###############
###############


require(data.table)
require(stringr)
epa = fread('../eis_documents/enepa_repository/meta_data/eis_record_detail.csv')
#epa = epa[Agency=='Forest Service',]

epa = epa[Document=='Final',]
#epa = epa[Agency=='Bureau of Land Management',]

epa$Title = iconv(epa$Title,'utf8')
epa$Year = str_extract(epa$EIS.Number,'^[0-9]{4}')
epa = epa[as.numeric(Year) %in% 2013:2020,]

grep('PROG',epa$Title,value = T)
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


doc_url = '../eis_documents/enepa_repository/meta_data/eis_document_record.csv'
epa_docs = fread(doc_url)

doc_url2 = '../eis_documents/enepa_repository/meta_data/extra_docs.csv'
epa_docs2 = fread(doc_url2)

epa_docs = rbindlist(list(epa_docs,epa_docs2),fill=T)

epa_docs = epa_docs[EIS.Number %in% epa$EIS.Number,]
epa_docs = epa_docs[!grepl('(CEQ|)[0-9]{8,}_(CEQ|)[0-9]{8,}\\.(pdf|PDF)',epa_docs$File_Name),]
epa_docs = epa_docs[!is.na(File_Name),]
epa_docs = epa_docs[grepl("pdf$",File_Name),]
epa_docs = epa_docs[!duplicated(epa_docs)]

epa$YEAR = epa$Year
epa$AGENCY = epa$Agency
epa_docs$YEAR = epa$YEAR[match(epa_docs$EIS.Number,epa$EIS.Number)]
epa_docs$AGENCY = epa$AGENCY[match(epa_docs$EIS.Number,epa$EIS.Number)]
epa$PROJECT_ID = epa$EIS.Number

epa_docs$PROJECT_ID = epa_docs$EIS.Number
text_flist = list.files('../eis_documents/enepa_repository/text_as_datatable/',recursive = T)


ex = gsub('pdf$|PDF$','txt',epa_docs$File_Name) %in% basename(text_flist)

epa_sub_docs = epa_docs[ex,]
epa_sub = epa[PROJECT_ID %in% epa_sub_docs$PROJECT_ID,]
epa_sub$MASTER_ID = epa_sub$PROJECT_ID
epa_sub_docs$MASTER_ID = epa_sub_docs$PROJECT_ID
library(htmlTable)


projects = epa_sub
documents = epa_sub_docs

projects$PROJECT_ID <- gsub('\\s{1,}',' ',projects$PROJECT_ID)
projects$PROJECT_ID <- gsub('\\s{1,}','_',projects$PROJECT_ID)
documents$PROJECT_ID <- gsub('\\s{1,}',' ',documents$PROJECT_ID)
documents$PROJECT_ID <- gsub('\\s{1,}','_',documents$PROJECT_ID)


documents$FILE_NAME <- gsub('\\s{1,}',' ',documents$File_Name)
documents$FILE_NAME <- gsub('\\s{1,}','_',documents$File_Name)

projects = projects[!duplicated(projects),]

documents <- documents[!duplicated(documents),]

documents = documents[!{grepl('\\(',documents$FILE_NAME) &!grepl('\\)',documents$FILE_NAME)},]
fwrite(projects,file = 'scratch/boilerplate/project_candidates_eis_only.csv')
fwrite(documents,file = 'scratch/boilerplate/document_candidates_eis_only.csv')

htmlTable(epa[,list(sum(PROJECT_ID %in% epa_sub$PROJECT_ID),.N),by=.(Agency)][order(-N)])
#htmlTable(projects[,list(sum(PROJECT_ID %in% documents$PROJECT_ID|MASTER_ID %in% documents$PROJECT_ID),.N),by = .(AGENCY,PROJECT_TYPE)][order(-N)])

