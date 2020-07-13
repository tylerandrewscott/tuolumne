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

library(tidyverse)
library(lubridate)

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
floc = '../../../../net/tmp/tscott1/manitou_scratch/scratch/usfs_documents_plaintext/'
flist = list.files(floc,recursive = T)
#flist_sample = sample(flist,10)

library(data.table)
library(textreuse)
tdf = readtext::readtext(paste0(floc,flist))
tdt = data.table(tdf)
tdt = tdt[nchar(tdt$text)>500,]

tdt$gram5 = lapply(tdt$text,textreuse::tokenize_ngrams,n = 5)
tdt$gram_hash = lapply(tdt$gram5,textreuse::hash_string)


doc_gram_hash = lapply(seq_along(tdt$gram5),function(x) 
  data.table(doc = tdt$doc_id[x],index = 1:length(tdt$gram5[[x]]), gram5 = tdt$gram5[[x]],hash_code = tdt$gram_hash[[x]]))

dgh = rbindlist(doc_gram_hash)


hash_more_than_once = dgh[,.N,by = .(hash_code)][N>1,]




tdt$gram5_hash_crossref = list(data.table())




nchar(tdt$text)
gc()
head(tdt)
sapply(tdt$text,nchar)
tdt$gram5 <- lapply(tdt$text,textreuse::tokenize_ngrams,n = 5)
?tokenize_ngrams()


hash_string(tdt$text[1])

s <- c("How", "many", "roads", "must", "a", "man", "walk", "down")

citation('textreuse')

tokenizers::
library(tokenizers)
library(data.table)
par_list = lapply(flist,function(x){
f = x
dfile   = paste0(floc,f)
#new_name = paste0(nloc,gsub('pdf','txt',f))
  #if(file.exists(new_name)){next}
temp_read <-tryCatch(read_engine(elem=list(uri = dfile),language='en'), error = function(e) NULL)
pars = tokenize_paragraphs(temp_read$content)
par_temp = data.table(file = f,par_id = 1:length(pars),par_text = pars)
par_temp})

par_dt = rbindlist(par_list)

library(textreuse)

a <- textreuse::tokenize_(paste("How does it feel, how does it feel?",
                          "To be without a home",
                          "Like a complete unknown, like a rolling stone"))
b <- textreuse::tokenize_ngrams(paste("How does it feel, how does it feel?",
                          "To be on your own, with no direction home",
                          "A complete unknown, like a rolling stone"))

jaccard_similarity(a, b)


jaccard_similarity(textreuse::tokenize_ngrams(tolower(par_dt$par_text[[2]]),n = 5),
                   textreuse::tokenize_ngrams(tolower(par_dt$par_text[[3]]),n = 5))


sapply(par_dt$par_text,length)
_?jaccard_similarity
jaccard_similarity(par_dt$par_text[[10]], par_dt$par_text[[2]])

par_dt$par_text[[10]]
par_dt$par_text[[1]]


dim(par_dt)
if(!is.null(temp_read)){
    temp_read <- stringr::str_replace_all(unlist(temp_read),"[\\s]+", " ")
    write.table(temp_read, file=new_name,
                quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
    rm(temp_read)}
}
