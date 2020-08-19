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
total_chars = nchar(doc_list_dt$text)
doc_list_dt = doc_list_dt[total_chars>300,]
#doc_list_dt = doc_list_dt[EIS %in% sample(unique(doc_list_dt$EIS),10),]
#doc_list_dt = doc_list_dt[sample(1:nrow(doc_list_dt),size = 100000),]
ngram_hash_dt = rbindlist(mclapply(seq_along(doc_list_dt$text),function(x) {
   gram <- tokenize_ngrams(doc_list_dt$text[x],n = ngram)
   gram_hash <- hash_string(unlist(gram))
   td = data.table(gram_hash,ngram = unlist(gram))
   td = td[grepl('[A-Za-z]',ngram),]
   td$Gram = 1:nrow(td)
   td$EIS = doc_list_dt$EIS[x]
   td$File = doc_list_dt$File[x]
   td$Par = doc_list_dt$Paragraph[x]
   rm(gram);rm(gram_hash)
   td},mc.cores = 8,mc.preschedule = T,mc.cleanup = T))


#find and filter out just ngrams that only occur 1
more_than_once = ngram_hash_dt[,.N,by=.(gram_hash)][N>1,]
ngram_hash_dt = ngram_hash_dt[gram_hash %in% more_than_once$gram_hash,]


#filter out ngram paris that only occur that only occur in across EISs
more_than_once_in_same_eis = ngram_hash_dt[,.N,by =.(gram_hash,EIS)][N>1,]
ngram_hash_dt = ngram_hash_dt[gram_hash %in% more_than_once_in_same_eis$gram_hash & EIS %in% more_than_once_in_same_eis$EIS,]


#this one takes a while becuase it has to actually read the ngrams
#ngram_hash_dt[,UID:=paste(EIS,File,Par,Gram,sep = '_')]
#find and filter out mistake hashes where underlying grams are different
false_positives = ngram_hash_dt[,length(unique(ngram)),by=.(gram_hash)][V1>1,]
ngram_hash_dt <- ngram_hash_dt[!ngram_hash_dt$gram_hash%in%false_positives$gram_hash,]

#2nd filter out ngram paris that only occur that only occur in across EISs after removing false positives
more_than_once_in_same_eis_again = ngram_hash_dt[,.N,by =.(gram_hash,EIS)][N>1,]
ngram_hash_dt = ngram_hash_dt[paste(gram_hash,EIS) %in% paste(more_than_once_in_same_eis_again$gram_hash,more_than_once_in_same_eis_again$EIS),]

#drop ngrams that are just numbers or NA
ngram_hash_dt = ngram_hash_dt[grepl('[A-Za-z]',ngram),]
candidate_pairs <- ngram_hash_dt
#candidate_pairs[,UID:=paste(EIS,File,Par,Gram,sep = '_')]
candidate_pairs[,EIS_File_Par:=paste(EIS,File,Par,sep = '_')]
candidate_pairs[,UID:=paste(EIS,File,Par,Gram,sep = '_')]
doc_pairs = candidate_pairs[,{cb = combn(EIS_File_Par,2); list(cb[1,],cb[2,])},by=.(gram_hash)]
#candidate_pairs = candidate_pairs[!duplicated(EIS_File_Par),]

#count up frequency of pairs
freq_pairs = doc_pairs[,.N,by=gram_hash][order(-N),]
print(table(freq_pairs$N<=7))
print(table(freq_pairs$N>=5000))
#drop pairs that aren't frequent enough
freq_pairs = freq_pairs[N>5&N<5000,]
#drop pairs that are too frequent
doc_pairs = doc_pairs[gram_hash %in% freq_pairs$gram_hash,]

#only compare 2 pages one time
combos_to_search = doc_pairs[,.(V1,V2)]
combos_to_search = combos_to_search[!duplicated(combos_to_search)]
combos_to_search <-  combos_to_search[str_extract(combos_to_search$V1,'^[0-9]{8}')==str_extract(combos_to_search$V2,'^[0-9]{8}'),]

doc_list_dt$EIS_File_Par = paste(doc_list_dt$EIS,doc_list_dt$File,doc_list_dt$Paragraph,sep = '_')

e1_vector = match(combos_to_search$V1,doc_list_dt$EIS_File_Par)
e2_vector = match(combos_to_search$V2,doc_list_dt$EIS_File_Par)


library(doParallel)
mcoptions <- list(preschedule=FALSE, set.seed=T,cleanup = T)
cl <- makeCluster(8)
registerDoParallel(cl)
local_align_list = foreach(i=1:nrow(combos_to_search),.options.multicore=mcoptions) %dopar% {
   textreuse::align_local(a = doc_list_dt$text[e1_vector[i]],b = doc_list_dt$text[e2_vector[i]],progress = F)
}
stopCluster(cl)

saveRDS(doc_list_dt,'scratch/reuse/within_eis/text_metadata.RDS')
saveRDS(local_align_list,'scratch/reuse/within_eis/local_aling_list.RDS')
saveRDS(combos_to_search,'scratch/reuse/within_eis/combos_searched.RDS')


