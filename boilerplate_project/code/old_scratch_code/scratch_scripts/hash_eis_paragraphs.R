library(textreuse)
library(data.table)
library(tokenizers)
library(rvest)
library(readtext)
library(pdftools)
library(stringr)
#library(pbapply)

fdir = 'scratch/reuse/tokenized_pars_epa_repo.csv'
dt = fread(fdir)

#dt = dt[1:100,]
#toc = pdf_toc(paste0(fdir,flist[f]))
#string_contents = sapply(toc$children,function(x) x$title)
#test = heading_search(paste0(fdir,flist[f]), headings = string_contents,ignore_case = T,
 #                     path = TRUE)

library(doParallel)
library(zoo)
library(pbapply)
ngram = 10
doc_list_dt = dt
doc_list_dt = doc_list_dt[sample(1:nrow(doc_list_dt),size = 100000),]
ngram_hash_dt = rbindlist(lapply(seq_along(doc_list_dt$text),function(x) {
 gram <- tokenize_ngrams(doc_list_dt$text[x],n = ngram)
   gram_hash <- hash_string(unlist(gram))
   td = data.table(gram_hash,ngram = unlist(gram))
   td = td[grepl('[A-Za-z]',ngram),]
   td$Gram = 1:nrow(td)
   td$EIS = doc_list_dt$EIS[x]
   td$File = doc_list_dt$File[x]
   td$Par = doc_list_dt$Paragraph[x]
   rm(gram);rm(gram_hash)
   td}))

#find and filter out just ngrams that show up in more than 1 paragraph
mult_grams = ngram_hash_dt[!duplicated(paste(gram_hash,EIS,File,Par,sep='_')),][,.N,by = .(gram_hash)][N>1,]
ngram_mult_dt = ngram_hash_dt[gram_hash %in% mult_grams$gram_hash,]

#drop ngrams that are just numbers or NA
ngram_mult_dt = ngram_mult_dt[grepl('[A-Za-z]',ngram),]

#find and filter out mistake hashes where underlying grams are different
false_positives = ngram_mult_dt[,length(unique(ngram)),by=.(gram_hash)][V1>1,]
ngram_mult_dt <- ngram_mult_dt[!ngram_mult_dt$gram_hash%in%false_positives$gram_hash,]

candidate_pairs <- ngram_mult_dt
candidate_pairs[,UID:=paste(EIS,File,Par,Gram,sep = '_')]

candidate_pairs[,.N,by = .(gram_hash)][N<2,]

test_hash = sample(candidate_pairs$gram_hash,10)
test_pairs = candidate_pairs[gram_hash %in% test_hash,]


test_pairs[,{cb = combn(UID,2); list(cb[1,],cb[2,])},by=gram_hash]


dim(test_pairs)
test_pairs


pairings = candidate_pairs[,combn(UID,2),by=gram_hash]
str(pairings)



combos = combn(test$UID,2)

str(combos)

list(i1[1,], i1[2,]) }, by = gram_hash]

doc_pairs = candidate_pairs[, {i1 <-  combn(UID, 2)
list(i1[1,], i1[2,]) }, by = gram_hash]

dim(test)
#filter out self comparisons
doc_pairs = doc_pairs[V1!=V2,]

#count up frequency of pairs
freq_pairs = doc_pairs[,.N,by=gram_hash][order(-N),]
#drop pairs that aren't frequent enough
freq_pairs = freq_pairs[N>5&N<4000,]


#drop pairs that are too frequent
doc_pairs = doc_pairs[gram_hash %in% freq_pairs$gram_hash,]
combos_to_search = doc_pairs[,.(V1,V2)]
combos_to_search[,V1:=gsub('_[0-9]{1,}$','',V1)]
combos_to_search[,V2:=gsub('_[0-9]{1,}$','',V2)]
combos_to_search = combos_to_search[!duplicated(combos_to_search)]                 

doc_list_dt$EIS_File_Par = paste(doc_list_dt$EIS,doc_list_dt$File,doc_list_dt$Paragraph,sep = '_')
library(doParallel)
mcoptions <- list(preschedule=FALSE, set.seed=T,cleanup = T)
cl <- makeCluster(8)
registerDoParallel(cl)
local_align_list = foreach(i=1:nrow(doc_pairs),.options.multicore=mcoptions) %dopar% {
textreuse::align_local(a = doc_list_dt$text[doc_list_dt$EIS_File_Par==combos_to_search$V1[i]],b = doc_list_dt$text[doc_list_dt$EIS_File_Par==combos_to_search$V2[i]],progress = F)
}
stopCluster(cl)


saveRDS(local_align_list,'scratch/reuse/local_aling_list.RDS')
saveRDS(combos_to_search,'scratch/reuse/combos_searched.RDS')
 
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

