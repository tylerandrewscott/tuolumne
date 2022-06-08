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
flist = list.files('scratch/eis_documents/','pdf$',full.names = F)
drecord = read_csv('input/epa_master_repository/eis_document_record.csv')
erecord = read_csv('input/epa_master_repository/eis_record_detail.csv') 
erecord$Year = year(mdy(erecord$Federal.Register.Date))
erecord = erecord[erecord$Year>=2000,]
drecord = drecord[drecord$EIS.Number %in% erecord$EIS.Number,]
drecord = drecord[grepl('pdf',drecord$File_Name),]

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

floc = 'scratch/eis_documents/'
nloc = 'scratch/eis_documents_plaintext/'

for(f in drecord$File_Name){
  dfile   = paste0(floc,f)
  new_name = paste0(nloc,gsub('pdf','txt',f))
  if(file.exists(new_name)){next}
  print(f)
  temp_read <-tryCatch(read_engine(elem=list(uri = dfile),language='en'), error = function(e) NA)
  if(!is.na(temp_read)){
  temp_read <- stringr::str_replace_all(unlist(temp_read),"[\\s]+", " ")
   write.table(temp_read, file=new_name,
              quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
  rm(temp_read)}
}
