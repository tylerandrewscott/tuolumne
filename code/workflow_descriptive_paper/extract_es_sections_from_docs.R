library("tabulizer")
library(pdftools)
library(stringr)
library(tidyverse)
#nohup R --no-save <win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.R >& win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.Rout &
library(tm)
library(SnowballC)
library(stringr)
library(RCurl)
library(lubridate)
library(pbapply)
library(jsonlite)
library(pdfsearch) 
require(data.table)
recs = fread('exec_summary_locations.csv')
recs[9,]
texts = pblapply(1:nrow(recs),function(i) {
  i = 10
  if(is.na(recs$Exec_Summary_Start_Page[i])){
  tabulizer::extract_text(file = recs$Exec_Summary_File[i])}else{
  tabulizer::extract_text(file = recs$Exec_Summary_File[i],pages = recs$Exec_Summary_Start_Page[i]:recs$Exec_Summary_End_Page[i])
  }
  },cl = 1)


test = pdf_text(recs$Exec_Summary_File[i])

flist = list.files('scratch/eis_documents/','pdf$',full.names = F)
drecord = read_csv('input/epa_master_repository/eis_document_record.csv')


erecord = read_csv('input/epa_master_repository/eis_record_detail.csv') 
erecord = erecord[!is.na(erecord$EIS.Number),]

erecord$Year = year(mdy(erecord$Federal.Register.Date))
erecord$EYEAR = str_extract(erecord$EIS.Number,'^[0-9]{4}')
erecord = erecord[erecord$EYEAR>=2013,]
erecord <- erecord[erecord$Document=='Final' & !grepl('ADOPTION',erecord$Title),]
erecord = erecord[!duplicated(erecord$EIS.Number),]
drecord = drecord[drecord$EIS.Number %in% erecord$EIS.Number,]
drecord = drecord[grepl('pdf',drecord$File_Name),]
drecord$DAMAGED = 0
drecord$DAMAGED[grepl('20150138',drecord$EIS.Number)] <- 1
crecord = drecord[!grepl('^APPENDIX|ATTACHMENT|COMMENT|^APPENDICE|^FIGURE|FIGURE|MAP[0-9]|ADDENDUM|^COVER SHEET',toupper(drecord$Original_File_Name)) & drecord$DAMAGED==0,]
#crecord = crecord[!grepl('_App',crecord$File_Name),]

crecord = crecord[!grepl('^[0-9]{8}_[0-9]{8}.pdf',crecord$File_Name),]
#crecord = crecord[!grepl('Vol2|Vol3|Vol4|VolII|VolIII|VolIV',crecord$File_Name),]
#crecord = crecord[!grepl('Volume(_|\\s)[2-9]|Vol(_|\\s)[2-9]|Volume[2-9]|Vol[2-9]',crecord$File_Name),]
#crecord = crecord[!grepl('Volume(II|III|IV|V)|Vol(II|III|IV|V)|Vol(_|\\s)(II|III|IV|V)|Volume(\\s|_)(II|III|IV|V)',crecord$File_Name),]
#crecord = crecord[!grepl('Part ([2-9]|[0-9]{2,})',crecord$File_Name),]
#crecord = crecord[!grepl('Part([2-9]|[0-9]{2,})',crecord$File_Name),]
#crecord = crecord[!grepl('Technical Report|Letter|Evaluation|Report|EPA comments|Appendices',crecord$File_Name),]
#crecord = crecord[!grepl('Ch(\\s|_)([2-9]|[1-9]{2})|Ch\\. ([2-9]|[1-9]{2}||0[2-9])',crecord$File_Name),]
#crecord = crecord[!grepl('Chapter(\\s|_)([2-9]|[1-9]{2}|0[2-9])|Chapter([2-9]|[1-9]{2}|0[2-9])',crecord$File_Name),]
#crecord = crecord[!grepl('Annex',crecord$File_Name),]
#crecord = crecord[!grepl('\\WApp\\W|\\WApdx\\W',crecord$File_Name),]
#crecord = crecord[!grepl('\\WApp[A-Z]',crecord$File_Name),]

extra_files = list.files('input/nepa_feis_externalfinds',full.names = F)
extra_files_df = data.frame(FLOC = 'input/nepa_feis_externalfinds/',EIS.Number = str_extract(extra_files,'[0-9]{8}'), Original_File_Name = extra_files,File_Name = extra_files,stringsAsFactors = F,DAMAGED = 0)
crecord$FLOC = 'scratch/eis_documents/'
crecord= rbind(extra_files_df,crecord)
crecord = crecord[!grepl('\\/',crecord$File_Name),]

nope = erecord$EIS.Number[!erecord$EIS.Number%in% unique(crecord$EIS.Number)]
#table(nope %in% drecord$EIS.Number)
#erecord[erecord$EIS.Number%in%nope,] %>% select(EIS.Number,Title) %>% .[11:20,]
saveRDS(object = nope,file = 'scratch/nodocumentrecord.RDS')

#test = tabulizer::extract_text(paste0('scratch/eis_documents/',f))
#tt = tokenizers::tokenize_sentences(test)

readPDF2<-function (engine = c("xpdf", "Rpoppler", "ghostscript", "Rcampdf","custom"), control = list(info = NULL, text = NULL)) {
  stopifnot(is.character(engine), is.list(control))
  engine <- match.arg(engine)
  pdf_info <- switch(engine, xpdf = function(x) tm:::pdf_info_via_xpdf(x,control$info), Rpoppler = Rpoppler::PDF_info, ghostscript = tm:::pdf_info_via_gs,Rcampdf = Rcampdf::pdf_info, custom = control$info)
  pdf_text <- switch(engine, xpdf = function(x) system2("pdftotext", c(control$text, shQuote(x), "-"), stdout = TRUE), Rpoppler = Rpoppler::PDF_text, ghostscript = pdf_text_via_gs, Rcampdf = Rcampdf::pdf_text, custom = control$text)
  if (!is.function(pdf_info) || !is.function(pdf_text)) 
    stop("invalid function for PDF extraction")
  function(elem, language, id) {
    uri <- processURI2(elem$uri)
    #meta <- pdf_info(uri)
    meta<-list()
    content <- pdf_text(uri)
    content<-iconv(enc2utf8(content), sub = "byte")
    tm::PlainTextDocument(content, meta$Author, meta$CreationDate, 
                          meta$Subject, meta$Title, basename(elem$uri), language, 
                          meta$Creator)}}
processURI2 <-function(uri) {
  uri <- as.character(uri)
  if (identical(substr(uri, 1, 7), "file://"))
    uri <- substr(uri, 8, nchar(uri))
  uri
}

read_engine <- readPDF2(engine=c("Rpoppler"),control = list(info = '-f'))



es_regex = '^Summary of the Final EIS|^Summary$|^SUMMARY$|^Executive Summary|^EXECUTIVE SUMMARY|\nSummary$|\nSUMMARY$|\nAbstract|\nABSTRACT|\nExecutive Summary|\nEXECUTIVE SUMMARY|Summary of Plan|Executive Summary$|EXECUTIVE SUMMARY$'
es_regex_exclude = 'Summary Report|Summary of (?!the Final EIS|Plan)|INDEX|Index|Table of Contents|TABLE OF CONTENTS|THIS PAGE INTENTIONALLY LEFT BLANK|LIST OF FIGURES|This Page Intentionally Left Blank|This page intentionally left blank|CONTENTS|^Contents|Summary of Comment|Summary of comments|LIST OF TABLES|Summary of Content|Summary of Public Comments'
es_regex_abstract = '^ABSTRACT|^Abstract'
#floc = 'scratch/eis_documents/'
#readable = list.files('scratch/eis_documents_plaintext/')
library(pdftools)
regex_line_number_string = "([0-9]{1,2}\\s){3,}"

fle = list.files('input/eis_es_text/')
flen = str_extract(fle,'[0-9]{8}')
need_still = erecord$EIS.Number[!erecord$EIS.Number %in% flen]

matches = pblapply(seq_along(erecord$EIS.Number),function(i) {
  temp_flist  = crecord[crecord$EIS.Number == erecord$EIS.Number[i],]
  if(any(grepl('Executive_Summary|Executive_summary|Exec_Summary|Exec Summary',temp_flist$File_Name)&!grepl("Summary Report|Summary Table|Meetings Summary|Hearing Summary|Summary of|Scoping",temp_flist$File_Name)))
  {
    f = which(grepl('Executive_Summary|Executive_summary|Exec_Summary|Exec Summary',temp_flist$File_Name))
    dfile   = paste0(temp_flist$FLOC[f],temp_flist$File_Name[f])
    temp_read <- tryCatch(read_engine(elem = list(uri = dfile), language = "en"))
    temp_text  = temp_read$content[!grepl(es_regex_exclude,temp_read$content)][1:min(50,length(temp_read$content))]
    if(!any(grepl('Executive',temp_read$content))){temp_text = tabulizer::extract_text(dfile)}
    temp_read_list = list(temp_text)
  }

if(!any(grepl('Executive_Summary|Executive_summary|Exec_Summary|Exec Summary',temp_flist$File_Name)&!grepl("Summary Report|Summary Table|Meetings Summary|Hearing Summary|Summary of|Scoping",temp_flist$File_Name))){
  #flist = flist[gsub('.pdf','.txt',flist) %in% readable]
  temp_read_list = lapply(seq_along(temp_flist$File_Name),function(f){
    dfile   = paste0(temp_flist$FLOC[f],temp_flist$File_Name[f])
  temp_read <- tryCatch(read_engine(elem = list(uri = dfile), language = "en"),error = function(e) NULL)

  if(!is.null(temp_read)){
  es_pages_orig = which(grepl(es_regex,temp_read$content,perl = T)&!grepl(es_regex_exclude,temp_read$content,perl = T))
  es_pages = es_pages_orig[es_pages_orig < 100 | (es_pages_orig + 2) %in% es_pages_orig | (es_pages_orig - 2) %in% es_pages_orig |
                             (es_pages_orig + 1) %in% es_pages_orig | (es_pages_orig - 1) %in% es_pages_orig]
  es_pages = sort(c(es_pages,which(((1:length(temp_read$content)) + 1) %in% es_pages & ((1:length(temp_read$content)) -1) %in% es_pages)))

  pages = lapply(temp_read$content,function(page) tail(stringi::stri_split_lines(page,omit_empty = T)[[1]],3))
  
  es_page_numbers = which(sapply(pages,function(x)  {any(grepl('^(E|)S(-|\\s)[0-9]{1,3}',x,perl=T)|(grepl('Page',x,perl=T)&grepl('s',x,perl=T) & grepl('[0-9]',x,perl=T)))}) & 
                            !grepl(es_regex_exclude,temp_read$content,perl=T) & 
                            sapply(pages,function(x) {all(!grepl("[^\\x00-\\x7F]",x,perl=T))|any(grepl('Page',x,perl=T)&grepl('s',x,perl=T) & grepl('[0-9]',x,perl=T))}))
  es_page_numbers = es_page_numbers[(es_page_numbers + 1) %in% es_page_numbers | (es_page_numbers - 1) %in% es_page_numbers]
  es_page_numbers = es_page_numbers[es_page_numbers<200]
  es_page_numbers = es_page_numbers[es_page_numbers<=length(temp_read$content)]
  es_pages = unique(sort(c(es_pages,es_page_numbers)))
  case_page_numbers = which(sapply(pages,function(x) any(!grepl('[^ivxlc]', x,perl=T))) & 
                              !grepl(es_regex_exclude,temp_read$content,perl=T) & 
                              sapply(pages,function(x) all(!grepl("[^\\x00-\\x7F]",x,perl=T))))
  case_page_numbers = case_page_numbers[(case_page_numbers + 1) %in% case_page_numbers | (case_page_numbers - 1) %in% case_page_numbers]
  if(length(es_pages)>0){case_page_numbers <- case_page_numbers[case_page_numbers>=min(es_pages)]}
  temp_text = NULL
  if(length(es_pages)==1){es_pages = es_pages + c(0:3)}
  if(length(es_pages)!=0 | length(case_page_numbers)!=0){
  temp_text <- stringr::str_replace_all(unlist(temp_read)[sort(union(es_pages,case_page_numbers))],"[\\s]+", " ")}
  
  if(length(es_pages)==0 & length(case_page_numbers)==0 & length(es_pages_orig)>0){
    headings= sapply(seq_along(temp_read$content),function(p){
      content = unlist(stringi::stri_split_lines(temp_read$content[p]))
      content[!grepl('[a-z]',content) &
                grepl('[A-Z]{2,}',content) & !grepl('[0-9]{2,}',content)]})
    summary_page_num = which(grepl("EXECUTIVE SUMMARY",headings))
    summary_page_num = summary_page_num[!summary_page_num %in% which(grepl(es_regex_exclude,temp_read$content,perl=T))]
    if(length(summary_page_num)!=0){
    final = min(which(sapply(headings,length)>0 & seq_along(headings)>summary_page_num))
  temp_text = temp_read$content[summary_page_num:final]}}

  if(length(es_pages)==0 & length(case_page_numbers)==0 & length(es_pages_orig)==0){
    last_ditch_effort = which(sapply(pages,function(x) any(grepl('(Page\\s|)1-[0-9]{1,3}$',x,perl=T))))
    pages_top3 = lapply(temp_read$content,function(page) head(stringi::stri_split_lines(page,omit_empty = T)[[1]],3))
    intro_pages = which(grepl('INTRODUCTION|Introduction',pages_top3)| sapply(pages,function(x) any(grepl('(Page\\s|)1-[0-9]{1,3}$',x,perl=T)))) 
  temp_text = temp_read$content[intro_pages]}
rm(temp_read);rm(dfile)
  if(length(temp_text)>0){
  temp_text}}
  })
  }
  
  
  if(length(temp_read_list)>0){
  temp_read_list = lapply(temp_read_list,function(x) x[!is.na(x)])
  temp_read_all = unlist(temp_read_list[!is.na(temp_read_list) & !sapply(temp_read_list,is.null)])
  if(sum(nchar(temp_read_all))>500000){print(paste('Bad find job for EIS#',erecord$EIS.Number[i]))}
  if(sum(nchar(temp_read_all))<=500000){
    temp_read_all = temp_read_all[!duplicated(temp_read_all)]
  temp_read_all = gsub(regex_line_number_string,"",temp_read_all)
  if(length(temp_read_all)>0)
  {write.table(temp_read_all, file=paste0('input/eis_es_text/',gsub('pdf','txt',paste0('ES_',erecord$EIS.Number[i],'.txt'))),
              quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")}}}
  rm(temp_read_all);rm(temp_read_list);rm(temp_read);rm(temp_flist)
},cl = 4)


