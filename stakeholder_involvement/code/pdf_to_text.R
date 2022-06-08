
library(pdftools)
library(stringr)
library(tidyverse)
#nohup R --no-save <win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.R >& win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.Rout &
library(tm)
library(SnowballC)
library(stringr)
library(RCurl)
library(data.table)
reader = "Rpoppler"

readPDF2<-function(engine = c("xpdf", "Rpoppler", "ghostscript", "Rcampdf","custom"), control = list(info = NULL, text = NULL)) {
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
read_engine <- readPDF2(engine=reader,control = list(info = '-f')) 
lines = fread('pretrained_wordvecs/glove.840B.300d.txt')
word = lines$V1
# 
# #source('code/denny_clean_functions.R')
# subd = 'input/eis_clean_pdfs/'
# output_subd = 'input/eis_text/'
# flist = list.files(subd,'*.pdf$')
# read_engine <- readPDF2(engine=reader,control = list(info = '-f'))
# 
# 
# for(i in flist){
#   print(i)
#   dfile <- paste0(subd,i)
#   temp_read <- read_engine(elem = list(uri = dfile), language = "en")
#   temp_read <- stringr::str_replace_all(unlist(temp_read),"[\\s]+", " ")
#   write.table(temp_read, file=paste0(output_subd,gsub('pdf','txt',i)),
#               quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
#   rm(temp_read)
# }
# 
# #source('code/denny_clean_functions.R')
# subd = 'input/eis_comment_feis_pdfs/'
# output_subd = 'input/eis_comment_feis_text/'
# flist = list.files(subd,'*.pdf$')
# for(i in flist){
#   print(i)
#   dfile <- paste0(subd,i)
#   temp_read <- read_engine(elem = list(uri = dfile), language = "en")
#   temp_read <- stringr::str_replace_all(unlist(temp_read),"[\\s]+", " ")
#   write.table(temp_read, file=paste0(output_subd,gsub('pdf','txt',i)),
#               quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
#   rm(temp_read)
# }
# 
# ##### too many image files, need text


bad_text = c('Contra_Loma_Reservoir_Project_FEIS.pdf',
             'Shasta_Lake_Water_Resources_FEIS.pdf',
             'Revision_9B_Regulations_Governing_Oil_Gas_FEIS.pdf')
output_subd = 'input/eis_comment_feis_text/'
library(tesseract)
library(hunspell)
eng <- tesseract("eng")
for (tf in bad_text){
  temp_file = paste0('input/eis_comment_feis_pdfs/',tf)
  results <- tesseract::ocr_data(temp_file, engine = eng)
  #temp_read <- tesseract::ocr(tf, engine = eng)
  temp_text = paste(results$word[results$confidence>75],collapse=' ')
  ad <- hunspell(temp_text)
  for (bad in ad[[1]])
  {
    if (bad %in% word){next}
    find_replace = hunspell_suggest(bad)[[1]][1]
    if(!is.na(find_replace))
    {
      temp_text = gsub(bad,find_replace,temp_text,fixed = T)
    }
  }
  temp_text <- stringr::str_replace_all(temp_text,"[\\s]+", " ")
  
  write.table(temp_text, file=paste0(output_subd,gsub('pdf','txt',tf)),
              quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
  rm(temp_text)
}






