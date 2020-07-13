library(data.table)
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(stringr)){install.packages('stringr');require(stringr)}
if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(doParallel)){install.packages('doParallel');require(doParallel)}
if(!require(pdftools)){install.packages('pdftools');require(pdftools)}

epa = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vR8zbAEoQLFnaTdE-dtNVGMXNizTEpjhHBHyOs6md_E55xxDLlvDGfwJfugY_-UXTWTo5p4Bmd2nmRo/pub?gid=1613953881&single=true&output=csv')
epa = epa[grepl('^201[3-9]',epa$EIS.Number),]
#epa = epa[Agency=='Bureau of Land Management',]
epa = epa[Document=='Final',]
epa$Year = str_extract(epa$EIS.Number,'[0-9]{4}')
epa  = epa[!grepl('ADOPTION',Title),]
#epa = epa[!EIS.Number%in%c(20170006,20170006),]
epa = epa[Agency %in% epa[,.N,Agency][order(N),][N>=10,]$Agency,]
epa = epa[Agency == 'Federal Energy Regulatory Commission',]
#htmlTable(epa[,.N,Agency][order(-N),])

#epa = epa[epa$Agency %in%c('Bureau of Land Management','Federal Highway Administration', "U.S. Army Corps of Engineers",'Federal Energy Regulatory Commission'),]

doc_url = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vTwMVW7wWjxKcohX5wVDdrgB1ZkBFJjSfZ-UwLWbZMmOz7BwVbMLVTvWHgRvYQFzID_gObxJV6BpaQ-/pub?gid=1065482997&single=true&output=csv'
epa_docs = fread(doc_url)
epa_docs = epa_docs[EIS.Number %in% epa$EIS.Number,]
epa_docs = epa_docs[!grepl('(CEQ|)[0-9]{8}_(CEQ|)[0-9]{8}',epa_docs$File_Name),]
epa_docs = epa_docs[!is.na(File_Name),]
epa_docs = epa_docs[grepl("pdf$",File_Name),]
epa_docs = epa_docs[!duplicated(epa_docs)]
epa_docs= epa_docs[grep('^[0-9]{8}(\\s|_)(APPENDIX|APPX|APPENDICES)|BIOP|SURVEY|SUPPLEMENT|EVALUATION|CONCEPT PLANS|DELINEATION|REPORT|LETTER|DECISION REPORT|INITIAL SITE|PRELIMINARY|HEARING|APPENDIX\\.PDF$|EXHIBITS|DEIS|DRAFT|MAP|COMMENTS|RESPONSE|RESOURCEREPORT|ATTACHMENT|POWERPOINT|STUDY|ERRATA|BUDGET|MEETING|MINUTES|ENVIRONMENTAL(\\s|_)ASSESSMENT|FONSI|FIGURE|TECHNICAL(\\S|_)MEMORANDUM|FINDING(\\s|_)OF(\\s|_)NO(\\s|_)SIGNIFICANT|RECORD(\\s|_)OF(\\s|_)DECISION|TABLE(\\s|_)OF(\\s|_)CONTENTS',toupper(File_Name),invert = T),]
#epa_docs = epa_docs[grep('App[A-Z]',File_Name,invert = T),]
#app_drops = which(grepl('Appendix|Appendices|appendix|appendices|appx|Appendice|Appenices|APPENDIX|APPENDICES|Appedicies',epa_docs$File_Name)&!grepl('and Appendix|to Appendix|Chapter|Front|Cover',epa_docs$File_Name))
#epa_docs = epa_docs[-app_drops,]
#app_drops = which(grepl('App([A-Z]|\\.[1-9])|App\\s[A-Z]|(App|APP|Appx|Appndx|Appnd)(\\s|_|\\.|-)[A-Z1-9]|APX(\\s|_|\\.|-)[A-Z1-9]|Apps[A-Z]|Appdx|Appedix|Appdcs(_|\\.|\\s)[0-9A-Z]|(A|a)ppend[A-Z]',epa_docs$File_Name)&!grepl('Front.*to_App\\.1|withAppA|and_App(\\s|_|)A|Chapter|Cover|wApp|Front',epa_docs$File_Name))
#epa_docs = epa_docs[-app_drops,]
fwrite(epa,'../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/test_ferc_eis_used.csv')
fwrite(epa_docs, '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/test_ferc_documents_used.csv')
storage = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/eis_documents/'
#flist = list.files(storage)

library(doParallel)
mcoptions <- list(preschedule=FALSE, set.seed=T,cleanup = T)
cl <- makeCluster(10)
registerDoParallel(cl)

epa_docs = epa_docs[order(EIS.Number,File_Name),]

temp_text_list = foreach(i=seq_along(epa_docs$File_Name)) %dopar% {
  rm(nepa);rm(temp_text);rm(temp_page)
  nepa = epa_docs$EIS.Number[i]
  fname = epa_docs$File_Name[i]
  temp_text = tryCatch({pdftools::pdf_text(paste0(storage,fname))},error = function(e) NULL)
  if(length(temp_text)>0){
  temp_page = unlist(sapply(temp_text,function(x) x))
  temp_page = gsub('\\s{1,}',' ',temp_page)
  data.table::data.table(EIS = nepa,File = fname,Paragraph = seq_along(temp_page),text = temp_page,stringsAsFactors = F)
}
}

all_page_dt = rbindlist(temp_text_list)
#all_page_dt = readRDS(paste0(floc,'tokenized_page_epa_repo.RDS'))
all_page_dt = all_page_dt[nchar(all_page_dt$text)<10000,]
all_page_dt = all_page_dt[nchar(all_page_dt$text)>750,]
all_page_dt[,EIS_File_Par:=paste(EIS,File,Paragraph,sep = '_')]
all_page_dt$index = 1:nrow(all_page_dt)
all_page_dt$text = textclean::replace_non_ascii(all_page_dt$text)
all_page_dt = all_page_dt[!grepl('^Figure [0-9]',all_page_dt$text),]
all_page_dt$text = gsub('\\s{1,}$','',all_page_dt$text)

#saveRDS(all_page_dt,paste0(floc,'tokenized_page_epa_repo_750-10000chars.RDS'))
floc = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/'
#saveloc = 'temp_products/'
saveRDS(all_page_dt,paste0(floc,'ferc_test_tokenized_page_epa_repo_750-10000chars.RDS'))
parallel::stopCluster(cl)





