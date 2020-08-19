library(data.table)
library(tidytext)
library(tm)
file_storage = 'scratch/epa_comment_letters'
record_df = fread('input/epa_master_repository/eis_record_detail.csv',stringsAsFactors = F)
file.rename(from = list.files(file_storage,pattern='PDF$|Pdf$',full.names = T), gsub('PDF$|Pdf$','pdf',list.files(file_storage,pattern='PDF$|Pdf$',full.names = T)))
file.rename(from = list.files(file_storage,pattern='WPD$',full.names = T), gsub('WPD$','wpd',list.files(file_storage,pattern='WPD$',full.names = T)))
file.rename(from = list.files(file_storage,pattern='DOC$',full.names = T), gsub('DOC$','doc',list.files(file_storage,pattern='DOC$',full.names = T)))


lc_files = list.files(file_storage,pattern='pdf$',full.names = T)

library(readtext)
library(tabulizer)

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

library(tesseract)
library(hunspell)
library(base64enc)
library(pbapply)



lc_files[!file.exists(paste0('input/epa_comment_txt/', basename(lc_files)))]
pblapply(lc_files[!file.exists(paste0('input/epa_comment_txt/', basename(lc_files)))],function(i) {
 print(i)
 temp_read <- tryCatch({read_engine(elem = list(uri = i), language = "en")},error = function(e) NULL)
 if(!is.null(temp_read)&sum(nchar(temp_read$content))>50){
   temp_read_unlist <- stringr::str_replace_all(unlist(temp_read),"[\\s]+", " ")
 write.table(temp_read_unlist, file=paste0('input/epa_comment_txt/',gsub('pdf$|PDF$','txt', basename(i))),
             quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")}
 if(!is.null(temp_read)&sum(nchar(temp_read$content))<=50){
 temp_ocr = tesseract::ocr(i)
 temp_ocr =  stringr::str_replace_all(unlist(temp_ocr),"[\\s]+", " ")
 write.table(temp_ocr, file=paste0('input/epa_comment_txt/',gsub('pdf$|PDF$','txt', basename(i))),
             quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
 rm(temp_ocr)}
 rm(temp_read)
  },cl = 10)


# Load required libraries
library(tesseract)
library(base64enc)
input <- "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"
input <- i

x <- base64decode(input)
fileConn<-file(tf <- tempfile(fileext = ".png"), "wb")
writeBin(x, fileConn)
close(fileConn)
ocr(tf)



wpd_files = list.files(file_storage,pattern='wpd',full.names = T)
wpd_files[1]
pblapply(wpd_files, function(f){ })

soffice --headless --convert-to txt:Text --outdir ../../input/epa_comment_txt *.wpd 

