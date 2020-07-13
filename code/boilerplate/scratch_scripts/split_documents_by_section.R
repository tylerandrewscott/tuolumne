library(textreuse)
library(data.table)
library(tokenizers)
library(rvest)
library(readtext)
library(pdftools)
library(stringr)
#library(pbapply)

fdir = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/leverage_points/ca_water_energy_pdfs/'
sdir = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/leverage_points/ca_water_energy_raw_text/'

flist = list.files(fdir)
hydro_files = grep('Hydro',flist,value=T)
#toc = pdf_toc(paste0(fdir,flist[f]))
#string_contents = sapply(toc$children,function(x) x$title)
#test = heading_search(paste0(fdir,flist[f]), headings = string_contents,ignore_case = T,
 #                     path = TRUE)

library(doParallel)
mcoptions <- list(preschedule=FALSE, set.seed=T,cleanup = T)
cl <- makeCluster(8)
registerDoParallel(cl)

base_text_list = foreach(f=hydro_files,.options.multicore=mcoptions) %dopar% {
  pdftools::pdf_text(paste0(fdir,f))}

names(base_text_list) <- hydro_files
loc = fdir

hydro_tocs = lapply(seq_along(base_text_list),function(f) {
temp = base_text_list[[f]]
toc = data.table(chapter = c('ES','Introduction','Proposed_Action','Environmental_Analysis','Developmental_Analysis',
                             'Conclusions','Cited_Literature'),
                 start = c(grep('EXECUTIVE SUMMARY\\n',temp),
grep('1.0\\s{1,}INTRODUCTION\\n',temp),
grep('2.0\\s{1,}PROPOSED ACTION AND ALTERNATIVES\\n',temp),
grep('3.0\\s{1,}ENVIRONMENTAL ANALYSIS\\n',temp),
grep('4.0\\s{1,}DEVELOPMENTAL ANALYSIS\\n',temp),
grep('5.0\\s{1,}CONCLUSIONS AND RECOMMENDATIONS\\n',temp),
grep('6.0\\s{1,}LITERATURE CITED\\n',temp)))
toc$end = c(toc$start[-1] - 1,Inf)
toc$file = names(base_text_list)[f]
toc})

hydro_splits = rbindlist(hydro_tocs)
hydro_splits = hydro_splits[chapter!='Cited_Literature',]
for(i in 1:nrow(hydro_splits)){
hydro_splits$text[i] <- paste(base_text_list[[hydro_splits$file[i]]][hydro_splits$start[i]:hydro_splits$end[i]],collapse=' ')
}

library(zoo)
library(pbapply)
ngram = 10
doc_list_dt = hydro_splits
ngram_hash_dt = rbindlist(lapply(seq_along(doc_list_dt$text),function(x) {
 gram <- tokenize_ngrams(doc_list_dt$text[x],n = ngram)
   gram_hash <- hash_string(unlist(gram))
   td = data.table(gram_hash,ngram = unlist(gram),index = 1:length(gram_hash))
   td$doc = doc_list_dt$file[x]
   td$section = doc_list_dt$chapter[x]
   rm(gram);rm(gram_hash)
   td}))

mult_grams = ngram_hash_dt[!duplicated(paste(gram_hash,doc,sep='_')),][,.N,by = gram_hash][N>1,]
ngram_mult_dt = ngram_hash_dt[gram_hash %in% mult_grams$gram_hash,]
false_positives = ngram_mult_dt[,length(unique(ngram)),by=.(gram_hash)][V1>1,]
ngram_mult_dt <- ngram_mult_dt[!ngram_mult_dt$gram_hash%in%false_positives$gram_hash,]
candidate_pairs <- ngram_mult_dt
candidate_pairs$uid = paste(candidate_pairs$doc,candidate_pairs$section,sep='_')

doc_pairs = candidate_pairs[, {i1 <-  combn(uid, 2)
list(i1[1,], i1[2,]) }, by = gram_hash]
doc_pairs = doc_pairs[V1!=V2,]
freq_pairs = doc_pairs[,.N,by=gram_hash][order(-N),][N>5,]
doc_pairs = doc_pairs[!gram_hash %in% freq_pairs$gram_hash,]
 
doc_pairs = doc_pairs[!duplicated(doc_pairs),]
combos_to_search = doc_pairs[!duplicated(paste0(V1,V2)),.(V1,V2)]

 doc_list_dt$uid = paste(doc_list_dt$file,doc_list_dt$chapter,sep='_')
 
library(doParallel)
mcoptions <- list(preschedule=FALSE, set.seed=T,cleanup = T)
cl <- makeCluster(8)
registerDoParallel(cl)

local_align_list = foreach(i=1:nrow(combos_to_search),.options.multicore=mcoptions) %dopar% {
textreuse::align_local(a = doc_list_dt$text[doc_list_dt$uid==combos_to_search$V1[i]],b = doc_list_dt$text[doc_list_dt$uid==combos_to_search$V2[i]],progress = F)
}

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









temp_toc = pdf_toc(paste0(fdir,hydro_files[1]))
sapply(temp_toc$children,function(x) x$title)

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


toc_list
#else{
  
#str_extract(temp_tx,'[1-9]\\.0(\\W|\\D)+[A-Z][^\\n]+')
  
#}
# 
# 
# sections = rbindlist(toc_list)
# sections$text = sapply(1:nrow(sections),function(t) paste(base_text_list[[sections$File[t]]][sections$Start[t]:sections$End[t]],collapse=' '))
# enviro_consequences = sections[Name == 'Environmental Consequences',]

