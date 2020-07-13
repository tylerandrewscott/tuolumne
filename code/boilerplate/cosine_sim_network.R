library(textreuse)
library(data.table)
library(tokenizers)
library(rvest)
library(readtext)
library(pdftools)
library(stringr)
library(Matrix)
library(Matrix.utils)
#library(pbapply)
data.table::setDTthreads(10)
floc = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/'
dt = readRDS(paste0(floc,'tokenized_page_epa_repo.RDS'))

dt = dt[nchar(dt$text)<15000,]
dt = dt[nchar(dt$text)>600,]

#dt = dt[1:100,]
#toc = pdf_toc(paste0(fdir,flist[f]))
#string_contents = sapply(toc$children,function(x) x$title)
#test = heading_search(paste0(fdir,flist[f]), headings = string_contents,ignore_case = T,
 #                     path = TRUE)
library(parallel)
library(doParallel)
library(zoo)
library(dplyr)
library(pbapply)
library(quanteda)
ngram = 5
doc_list_dt = dt

uq_eis = unique(doc_list_dt$EIS)



dfm_list = lapply(seq_along(uq_eis),function(i) {
print(i)
tok = tokens( doc_list_dt$text[doc_list_dt$EIS==uq_eis[i]], remove_numbers = T, remove_punct = T, remove_separators = T)
tok_ngram = tokens_ngrams(tok,n = 5)
tok_dfm = quanteda::dfm(tok_ngram)
rownames(tok_dfm) <- rep(uq_eis[i],nrow(tok_dfm))
eis_dfm = dfm_compress(tok_dfm ,margin = 'both')
eis_dfm})

gram_dfm_sparse = do.call(rbind,dfm_list)

saveRDS(gram_dfm_sparse ,'../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/gram_dfm_sparse.RDS')

 