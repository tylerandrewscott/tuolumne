library(data.table)
library(rvest)
library(curl)
library(tidyverse)
library(textreuse)
library(stringr)
library(doParallel)
library(pdftools)
library(pdfsearch)

epa = fread('input/epa_master_repository/eis_record_detail.csv')
epa = epa[grepl('^201[3-9]',epa$EIS.Number),]
#epa = epa[Agency=='Bureau of Land Management',]
epa = epa[Document=='Final',]
epa$Year = str_extract(epa$EIS.Number,'[0-9]{4}')
epa  = epa[!grepl('ADOPTION',Title),]


epa_docs = fread('input/epa_master_repository/eis_document_record.csv')
epa_docs = epa_docs[EIS.Number %in% epa$EIS.Number,]
epa_docs = epa_docs[!grepl('(CEQ|)[0-9]{8}_(CEQ|)[0-9]{8}',epa_docs$File_Name),]
epa_docs = epa_docs[!is.na(File_Name),]

i = 1


#average is 17.3 per year
#mean(table(epa$Year))

storage = '../../../../net/tmp/tscott1/tuolumne_scratch/eis_main_pdfs/'
floc = 'https://eplanning.blm.gov'
eis_ref = fread('input/agency_nepa/blm/blm_post2000_eis.csv')
eis_ref2 = fread('input/agency_nepa/blm/blm_post2000_rmp_eis.csv')
eis_ref = rbindlist(list(eis_ref,eis_ref2),fill = T)

eis_ref = eis_ref[`Project Status` %in% c('Completed','Final EIS (non LUP)','Decision and Protest',"Decision and Appeal", "PRMP - Protest and Resolution" ),]
eis_ref = eis_ref[eis_ref$`NEPA #`!='DOI-BLM-AK-0000-2011-0001-RMP-EIS',]

flist = list.files(storage)
all_doc_set = fread('input/agency_nepa/blm/blm_nepa_document_record.csv')
draft_set = all_doc_set[grepl('DRAFT|DEIS',toupper(File)),]
doc_set = all_doc_set[!grepl('Scoping|Draft|DRAFT|Maps|Append|Figur|Meeting|Environmental Assessments|Comment| EA$|Presentation|Transcripts|Studies|Materials|Record of Decision|Data Sheets|Maps',Volume),]
doc_set = doc_set[grep('DEIS|DRAFT|MAP|APPENDIX|COMMENTS|APPENDICES|RESPONSE|RESOURCEREPORT|ATTACHMENT|POWERPOINT|ERRATA|BUDGET|MEETING|MINUTES|ENVIRONMENTAL(\\s|_)ASSESSMENT|FONSI|FIGURE|FINDING(\\s|_)OF(\\s|_)NO(\\s|_)SIGNIFICANT|RECORD(\\s|_)OF(\\s|_)DECISION|TABLE(\\s|_)OF(\\s|_)CONTENTS',toupper(File),invert = T),]
doc_set = doc_set[grep('App[A-Z]',File,invert = T),]


library(doParallel)
mcoptions <- list(preschedule=FALSE, set.seed=T,cleanup = T)
cl <- makeCluster(10)
registerDoParallel(cl)
library(pdfsearch)
library(tidytext)


phrase_bank <- list(
  summary = c('Executive Summary','Summary','ES-[0-9]{1,3}'),
  intro = c('Introduction','Purpose and Need'),
  impacts = c('Environmental Consequences','Analysis of Environmental Consequences','Environmental Impacts'),
  setting = c('Affected Environment'),
  cumulative = c('Cumulative Impacts','Cumulative Impacts Analysis','Cumulative Effects'),
  consultation = c('Consultation and Coordination'),
  preparers = c('List of Preparers'))


toc_list = foreach(i=seq_along(eis_ref$`NEPA #`),.options.multicore=mcoptions) %dopar% {
  print(i) 

  require(data.table)
eis_files = sort(grep(eis_ref$`NEPA #`[i],doc_set$File,value=T))
if(length(eis_files)>0){
#draft_files = sort(grep(eis_ref$`NEPA #`[i],draft_set$File,value=T))
eis_text = sapply(sort(eis_files),function(x) {pd = tryCatch({pdftools::pdf_text(paste0(storage,x))},error = function(e) '');pd})
eis_text = unlist(eis_text)
eis_text = gsub('^\\s+','',eis_text)
total_pages = sapply(eis_files,function(x) pdftools::pdf_info(paste0(storage,x))$pages)
total_pages = total_pages[total_pages!=0]
page_count = data.table::rbindlist(lapply(seq_along(total_pages),function(x) data.table::data.table(Iterator = rep(1,total_pages[x]),Page_In_File = 1:total_pages[x],File = names(total_pages)[x],stringsAsFactors = F)))
page_count$Page = cumsum(page_count$Iterator)


chapter_page_extracts = str_extract_all(eis_text,'[1-7]-[1-9][0-9]{0,3}\\n')
uses_chapter_specific_pagenums = sum(sapply(chapter_page_extracts,length)==1) / length(eis_text) > 0.7

chapter_page_extracts[sapply(chapter_page_extracts,length)!=1] <- NA

p = 2
phrase_search = paste(paste(paste0('^',c(phrase_bank[[p]],toupper(phrase_bank[[p]]))),collapse='|'),paste(paste0(c(phrase_bank[[p]],toupper(phrase_bank[[p]])),'\\n'),collapse='|'),sep='|')
if(names(phrase_bank)[p]=='intro'
  
  
}


grep(phrase_search,eis_text)




for(p in seq_along(phrase_bank)){
  #print(p)
  if(names(phrase_bank)[p]!='preparers'){
phrase_search = paste(paste(paste0('^',c(phrase_bank[[p]],toupper(phrase_bank[[p]]))),collapse='|'),paste(paste0(c(phrase_bank[[p]],toupper(phrase_bank[[p]])),'\\n'),collapse='|'),sep='|')
page_candidates <- grep(phrase_search,eis_text)
if(identical(integer(0),page_candidates)){next}
diff_from_last_1 <- cumsum(c(1, diff(page_candidates) - 1))
diff_from_last_2 <- cumsum(c(1, diff(page_candidates) - 2))
diff_from_last = list(diff_from_last_1,diff_from_last_2)
run_length_1 <- rle(diff_from_last_1)
run_length_2 <- rle(diff_from_last_2)
which_version = which.max(c(max(run_length_1$lengths),max(run_length_2$lengths)))
run_length <- list(run_length_1,run_length_2)[[which_version]]
chapter_pages = page_candidates[which(diff_from_last[[which_version]] == with(run_length, values[which.max(lengths)]))]

if(length(chapter_pages)<2 & !names(phrase_bank)[p]%in%c('consultation','cumulative') & length(unique(page_count$File[chapter_pages])==1)  & grepl('Ch_[1-6]|Chapter|CHAPTER',unique(page_count$File[chapter_pages]))){
  chapter_pages = min(page_count$Page_In_File[chapter_pages]):max(page_count$Page[page_count$File==unique(page_count$File[chapter_pages])])}



page_count[[names(phrase_bank)[p]]] <- {1:nrow(page_count)} %in%chapter_pages + 0
spread = min(page_candidates):max(page_candidates)
add_pages = setdiff(spread[spread < max(chapter_pages) & spread > {min(chapter_pages)-2}],chapter_pages)
add_pages <- add_pages[page_count$V2[add_pages] %in% unique(page_count$V2[chapter_pages])]
page_count[[names(phrase_bank)[p]]][add_pages] <- 1
  }
  if(names(phrase_bank)[p]=='preparers'){
page_count[[names(phrase_bank)[p]]] <-0 
page_count[[names(phrase_bank)[p]]][intersect(grep('List of Preparers',eis_text),
               grep('DOCUMENT CONTENTS|TABLE OF CONTENTS|Table of Contents|^CONTENTS|DOCUMEN T CONTENTS',eis_text,invert=T))] <- 1
  }
}
found = names(phrase_bank)[names(phrase_bank) %in% names(page_count)]
page_count_keep = page_count[rowSums(page_count[,found,with=F])>0,]
page_index = data.table::melt(page_count_keep,id.vars = c('File','Page','Page_In_File','Iterator'),stringsAsFactors = F)[value>0,]
page_index$text = eis_text[page_index$Page]
page_index[,Iterator:=NULL]
page_index
dim(page_index)

colSums(page_count_keep[,found,with=F])
page_count_keep[impacts==1,]
page_index}
}

parallel::stopCluster(cl)

blm_tagged_sections = rbindlist(toc_list,fill = T)



str(toc_list)
