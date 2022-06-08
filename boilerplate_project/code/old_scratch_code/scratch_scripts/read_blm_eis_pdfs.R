library(textreuse)
library(data.table)
library(tokenizers)
library(rvest)
library(readtext)
library(pdftools)
library(stringr)
library(pdfsearch)
#library(pbapply)


eis_ref = fread('input/agency_nepa/blm/blm_post2000_eis.csv')
fdir = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/agency_nepa_docs/blm/'
flist = list.files(fdir)

eis = eis_ref$`NEPA #`[10]
eis_ref[10,]

test = pdf_text(paste0(fdir,"DOI-BLM-WY-P000-2007-0003-EISvol1.pdf" ))

pdf_toc(test)

pdfsearch::heading_search(test,c('Executive Summary','Introduction','Proposed Action and Alternatives',
                                 'AFFECTED ENVIRONMENT AND ENVIRONMENTAL CONSEQUENCES',
                                 'CUMULATIVE ENVIRONMENTAL CONSEQUENCES',
                                 
                                 
                                 
                                 ),full_line = F,ignore_case = T)


#toc = pdf_toc(paste0(fdir,flist[f]))
#string_contents = sapply(toc$children,function(x) x$title)
#test = heading_search(paste0(fdir,flist[f]), headings = string_contents,ignore_case = T,
 #                     path = TRUE)

library(doParallel)
mcoptions <- list(preschedule=FALSE, set.seed=T,cleanup = T)
cl <- makeCluster(8)
registerDoParallel(cl)

base_text_list = foreach(f=flist,.options.multicore=mcoptions) %dopar% {
  pdftools::pdf_text(paste0(fdir,f))}

names(base_text_list) <- flist
loc = fdir
no_chapters = numeric(0)

toc_list = lapply(seq_along(base_text_list)[1:2],function(i){
  print(i)
  i = 1

fname = names(base_text_list)[i]
temp_tx = base_text_list[[i]]
temp_tx = gsub('^\\s{1,}','',temp_tx)
chapter_extract= (as.numeric(str_extract(toupper(str_extract(temp_tx ,'(CHAPTER|Chapter|S E C T I O N)\\s[0-9]{1,2}')),'[0-9]{1,}')))

sum(is.na(chapter_extract))
table(is.na(chapter_extract))


if(length(chapter_extract)/length(temp_tx)>0.1){

chapter_nums = chapter_extract[chapter_extract %in% names(table(chapter_extract))[table(chapter_extract)>1]]
chapter_nums <- sort(unique(chapter_nums))
if(!identical(no_chapters,chapter_nums)){
chapter_from_last <- cumsum(c(1, diff(chapter_nums) - 1))
chapter_run_length <- rle(chapter_from_last)
chapter_nums = chapter_nums[which(chapter_from_last == with(chapter_run_length , values[which.max(lengths)]))]

toc = data.table(Chapter = c('TOC','ES',chapter_nums),Start = NA,End = NA,Name = NA)

for(j in 1:nrow(toc)){
  #print(j)
  chapter = toc$Chapter[j]
  if(chapter == 'TOC'){first_pass = grep('TABLE OF CONTENTS|CONTENTS',toupper(temp_tx))}
  if(chapter == 'ES'){first_pass = grep('EXECUTIVE SUMMARY',toupper(temp_tx))}
  #else if(chapter == 'APPX'){first_pass = grep('APPENDIX',toupper(temp))}
  if(!chapter %in% c('TOC','ES')){
    string_search = paste0('CHAPTER ',chapter,'[^0-9]')
    first_pass = grep(string_search,toupper(temp_tx))
    first_pass = first_pass[first_pass>toc$End[(j-1)]]
  }

diff_from_last <- cumsum(c(1, diff(first_pass) - 1))
run_length <- rle(diff_from_last)
chapter_pages = first_pass[which(diff_from_last == with(run_length, values[which.max(lengths)]))]
spread = min(first_pass):max(first_pass)
add_pages = setdiff(spread[spread < max(chapter_pages) & spread %in% first_pass[first_pass>max(toc$End[1:(j-1)])]],chapter_pages)
chapter_pages = sort(c(add_pages,chapter_pages))
if(length(chapter_pages)<=1){toc = toc[1:(j-1),];break}
toc$Start[j] <- min(chapter_pages)
toc$End[j] <- max(chapter_pages)
if(chapter %in% c('TOC','ES')){toc$Name[j]<-chapter}
if(!chapter %in% c('TOC','ES')){
name_candidates = gsub('Chapter[^A-Z]+','',str_extract(temp_tx[chapter_pages],paste0('Chapter ',chapter,'[^\\n]+')))
name = names(table(name_candidates))[table(name_candidates)==max(table(name_candidates))]
toc$Name[j] <- name
if(name == 'Index'){toc = toc[1:j,];break}
rm(first_pass);rm(chapter)
}
toc$File = fname
}}
toc
}})


toc_list
#else{
  
#str_extract(temp_tx,'[1-9]\\.0(\\W|\\D)+[A-Z][^\\n]+')
  
#}
# 
# 
# sections = rbindlist(toc_list)
# sections$text = sapply(1:nrow(sections),function(t) paste(base_text_list[[sections$File[t]]][sections$Start[t]:sections$End[t]],collapse=' '))
# enviro_consequences = sections[Name == 'Environmental Consequences',]
# 
# library(zoo)
# library(pbapply)
# ngram = 10
# doc_list_dt = enviro_consequences
# ngram_hash_dt = rbindlist(lapply(seq_along(doc_list_dt$text),function(x) {
#   gram <- tokenize_ngrams(doc_list_dt$text[x],n = ngram)
#   gram_hash <- hash_string(unlist(gram))
#   td = data.table(gram_hash,ngram = unlist(gram),index = 1:length(gram_hash))
#   td$doc = doc_list_dt$File[x]
#   td$section = doc_list_dt$Name[x]
#   rm(gram);rm(gram_hash)
#   td}))
# 
# 
# mult_grams = ngram_hash_dt[!duplicated(paste(gram_hash,doc,sep='_')),][,.N,by = gram_hash][N>1,]
# ngram_mult_dt = ngram_hash_dt[gram_hash %in% mult_grams$gram_hash,]
# false_positives = ngram_mult_dt[,length(unique(ngram)),by=.(gram_hash)][V1>1,]
# 
# ngram_mult_dt <- ngram_mult_dt[!ngram_mult_dt$gram_hash%in%false_positives$gram_hash,]
# 
# candidate_pairs <- ngram_mult_dt
# candidate_pairs$uid = paste(candidate_pairs$doc,candidate_pairs$section,sep='_')
# 
# 
# doc_pairs = candidate_pairs[, {i1 <-  combn(, 2)
# list(i1[1,], i1[2,]) }, by = gram_hash]
# doc_pairs = doc_pairs[V1!=V2,]
# freq_pairs = doc_pairs[,.N,by=gram_hash][order(-N),][N>5,]
# doc_pairs = doc_pairs[!gram_hash %in% freq_pairs$gram_hash,]
# 
# doc_pairs = doc_pairs[!duplicated(doc_pairs),]
# combos_to_search = doc_pairs[!duplicated(paste0(V1,V2)),.(V1,V2)]
# 
# doc_list_dt$uid = paste(doc_list_dt$File,doc_list_dt$Name,sep='_')
# 
# library(doParallel)
# mcoptions <- list(preschedule=FALSE, set.seed=T,cleanup = T)
# cl <- makeCluster(8)
# registerDoParallel(cl)
# 
# local_align_list = foreach(i=1:nrow(combos_to_search),.options.multicore=mcoptions) %dopar% {
#  textreuse::align_local(a = doc_list_dt$text[doc_list_dt$uid==combos_to_search$V1[i]],b = doc_list_dt$text[doc_list_dt$uid==combos_to_search$V2[i]],progress = F)
# }
# 
# stopCluster(cl)
# 
# saveRDS('scratch/reuse/')
# 
# 
# 
# align_local(a = 'what is going on here, i dont understand the problem',b = 'why are we measuring this I dont think there is an issue')
# doc_list_dt$text[doc_list_dt$uid==combos_to_search$V1[i]]
# 
# enviro_corpus = TextReuseCorpus(text = enviro_consequences$text,meta = list(id = enviro_consequences$File,chapter = enviro_consequences$Name))
# 
# combos = combn(x =names(enviro_corpus),2,simplify = F)
# 
# 
# 
# test = lapply(seq_along(combos),function(x) align_local(
#   a = enviro_corpus[[combos[[x]][1]]],
#   b = enviro_corpus[[combos[[x]][2]]]))
# 
# 
# choose(2,names(enviro_corpus))
# enviro_align = align_local(enviro_corpus,)
# 
# 
# 
# sections$Start[t]:sections$End[t]
# test = paste(base_text_list[[sections$File[t]]][sections$Start[t]:sections$End[t]],collapse=' ')
# 
# nchar(test)
# 
