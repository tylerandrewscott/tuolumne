# library(tm)
# library(quanteda)
# library(data.table)
# library(textclean)
# library(stringr)
# library(textmineR)
# library(stm)
# library(textmineR)
# library(textreuse)
# library(text2vec)
# library(pbapply)
#setwd('bucket_mount/tuolumne/')

rerun_existing = T
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(tm)){install.packages('tm');require(tm)}
if(!require(quanteda)){install.packages('quanteda');require(quanteda)}
if(!require(textclean)){install.packages('textclean');require(textclean)}
if(!require(stringr)){install.packages('stringr');require(stringr)}
if(!require(pbapply)){install.packages('pbapply');require(pbapply)}
if(!require(parallel)){install.packages('parallel');require(parallel)}
if(!require(doParallel)){install.packages('doParallel');require(doParallel)}
if(!require(textreuse)){install.packages('textreuse');require(textreuse)}
if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
#setwd("google_drive/tuolumne/")
#setDTthreads(3)
#getDTthreads()
mcores = floor(detectCores() - 15 )
options("mc.cores" = floor(detectCores() - 15))

#table(projects$AGENCY,projects$PROJECT_TYPE)
projects = fread('../bucket_mount/tuolumne/scratch/boilerplate/project_candidates_eis_only.csv')
projects = projects[Document=='Final',]
projects = projects[grepl('^201[3-9]|^2020',PROJECT_ID),]


scratch_loc = '../bucket_mount/tuolumne/scratch/boilerplate/hash_candidates/'

minhash <- minhash_generator(n = 240, seed = 40)
progress_bars = T
gc()
cluster = makeCluster(mcores)
registerDoParallel(cl = cluster,cores = mcores)
parallel::clusterEvalQ(cluster,'require(data.table)')
parallel::clusterEvalQ(cluster,'require(textreuse)')
#hash_file = paste0('../bucket_mount/big_eis_text.rds')
full_tlist <- readRDS('../bucket_mount/tuolumne/scratch/boilerplate/big_text_files/big_eis_text.rds')

chars = nchar(full_tlist$text)
periods = stringr::str_count(full_tlist$text,"\\.")
numbers = stringr::str_count(full_tlist$text,"[0-9]")
caps = stringr::str_count(full_tlist$text,'[A-Z]')
tildes = stringr::str_count(full_tlist$text,'~')
quotes = stringr::str_count(full_tlist$text,'\\"')
spaces = stringr::str_count(full_tlist$text,'\\s')

cut = 0.1
full_tlist  = full_tlist[chars>400&{periods/chars}<cut&{quotes/chars}<cut&{tildes/chars}<cut&{numbers/chars}<cut&{caps/chars}<cut&{spaces/chars}<{cut*2},]

#####

hash_file = paste0(scratch_loc,'eis_page_hashes.rds')
flist = as.character(full_tlist$text)
names(flist) <- paste0(full_tlist$File,'_',full_tlist$Page)

 eis_corpus =  TextReuseCorpus(text = flist,
                               meta = list(File = full_tlist$File,Page = full_tlist$Page),
                               tokenizer = tokenize_ngrams, n = 10,
                               minhash_func = minhash, keep_tokens = TRUE,
                               progress = progress_bars,skip_short = T)
 gc()
saveRDS(eis_corpus,'../bucket_mount/tuolumne/scratch/eis_page_corp_scratch.rds')

#eis_corpus = readRDS('../bucket_mount/tuolumne/scratch/eis_page_corp_scratch.rds')
#file.exists('../bucket_mount/tuolumne/scratch/eis_page_corp_scratch.rds')
split_corpus_ntiles = dplyr::ntile(x = seq(eis_corpus),n = mcores*10)
split_corpus = split(eis_corpus,split_corpus_ntiles)
 
split_buckets = foreach(x = split_corpus) %dopar% {textreuse::lsh(x,bands = 60)}
gc()
# 
while(any(sapply(split_buckets,is.null))){
   null_fails = which(sapply(split_buckets,is.null))
   split_buckets[null_fails] <- pblapply(null_fails,function(x) lsh(split_corpus[[x]],bands = 40,progress = F),cl = 5)
 }

 eis_buckets = do.call(rbind,split_buckets)
 saveRDS(eis_buckets,'../bucket_mount/tuolumne/scratch/eis_page_buckets_scratch.rds')

#eis_buckets = readRDS('../bucket_mount/tuolumne/scratch/eis_page_buckets_scratch.rds')
eis_candidates <- lsh_candidates(buckets = eis_buckets)
require(dplyr)
candidate_splits = split(eis_candidates,ntile(1:nrow(eis_candidates),n = nrow(eis_candidates) %/% 10000))
gc()

#corpus_meta = eis_corpus$meta
#corpus_meta$names <- names(eis_corpus)[!names(eis_corpus)%in%skipped(eis_corpus)]
require(doParallel)

score_list = foreach(i = candidate_splits) %dopar% {
  send_names = unique(c(i$a,i$b))
  send_text = flist[send_names]
  score_list = mapply(function(aa,bb) textreuse::align_local(a = aa,b=bb)$score,aa = send_text[i$a],bb = send_text[i$b])
  data.table::data.table(a = i$a, b= i$b,score = score_list)
}


score_dt = rbindlist(score_list)

saveRDS(score_dt,'../bucket_mount/tuolumne/scratch/eis_page_scores_scratch.rds')

