

pack = c('data.table','stringr','tidyverse','doParallel','pdftools','lubridate')
need = pack[!pack %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(pack,require,character.only=T)

empty_project_record = data.table(PROJECT_ID = character(),YEAR = numeric(),PROJECT_TYPE = character(),AGENCY = character())
empty_doc_dt = data.table(YEAR = numeric(),FILE_NAME = character(), FILE_LOC = character(), PROJECT_TYPE = character(),AGENCY = character())

###############
###############


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

