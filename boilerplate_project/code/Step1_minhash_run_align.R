
##### A FEW STEPS HERE, PARTICULARLY THE CORPUS GENERATION, ARE EXTREMELY COMPUTATIONALLY INTENSIVE
##### THIS WAS RUN ON A GOOGLE VIRTUAL MACHINE WITH 16 CORES AND 360 GB OF RAM (MEMORY IS THE LIMITING FACTOR, ADDING MORE CORES DOES NOT HELP)
##### IN ADDITION, IF MEMORY GETS TOO TIGHT, THE CORPUS GENERATION WILL APPEAR TO WORK, BUT A MESSAGE WILL OCCUR THAT SAYS A BUNCH (E.G. 1M OR 100K) OF THE TEXTS
# WERE TOO SHORT AND THUS DROPPED. IF THIS OCCURS, IT MEANS THE CODE DID NOT WORK RIGHT AND SHOULD BE RUN AGAIN.
where = 'remote'# or "remote"
rerun_existing = F

pack = c('data.table','data.table','quanteda','tm','textclean','stringr','pbapply',
         'stringr','parallel','doParallel','benchmarkme','tidyverse','textreuse','dplyr')
need = pack[!pack %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(pack,require,character.only=T)


#table(projects$AGENCY,projects$PROJECT_TYPE)
projects = fread('boilerplate_project/data_products/project_candidates_eis_only.csv')

#dir.create('boilerplate_project/scratch/hash_candidates/')
#if(where=='local'){
scratch_loc = 'boilerplate_project/scratch/hash_candidates/'#}
#if(where == 'remote'){scratch_loc = '../bucket_mount/tuolumne/scratch/boilerplate/hash_candidates/'}

minhash <- minhash_generator(n = 240, seed = 40)
progress_bars = T
gc()

#hash_file = paste0('../bucket_mount/big_eis_text.rds')

full_tlist <- readRDS('boilerplate_project/input/eis_corpus_2013-2020.rds')


full_tlist[,text:=gsub('\"\"','',text,fixed = T)]

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

summary(nchar(full_tlist$text))
gc()


##### setting cores high requires a big ass computer, and still takes awhile, and sometimes a thread chokes
mcores = 8
#options("mc.cores" = floor(detectCores()/2))
options("mc.cores" = mcores)

eis_corpus =  TextReuseCorpus(text = flist,
                              meta = list(File = full_tlist$File,Page = full_tlist$Page),
                              tokenizer = tokenize_ngrams, n = 10,
                              minhash_func = minhash, keep_tokens = TRUE,
                              progress = progress_bars,skip_short = T)
gc()
#saveRDS(eis_corpus,'../bucket_mount/tuolumne/scratch/eis_page_corp_scratch.rds')

mcores = 50
options("mc.cores" = mcores)
#eis_corpus = readRDS('../bucket_mount/tuolumne/scratch/eis_page_corp_scratch.rds')
#file.exists('../bucket_mount/tuolumne/scratch/eis_page_corp_scratch.rds')
split_corpus_ntiles = dplyr::ntile(x = seq(eis_corpus),n = mcores*10)
split_corpus = split(eis_corpus,split_corpus_ntiles)
rm(eis_corpus)
gc()

cluster = makeCluster(mcores)
registerDoParallel(cl = cluster)
parallel::clusterEvalQ(cluster,'require(data.table)')
parallel::clusterEvalQ(cluster,'require(textreuse)')
split_buckets = foreach(x = split_corpus) %dopar% {textreuse::lsh(x,bands = 60)}

while(any(sapply(split_buckets,is.null))){
  null_fails = which(sapply(split_buckets,is.null))
  split_buckets[null_fails] <- pblapply(null_fails,function(x) lsh(split_corpus[[x]],bands = 60,progress = F),cl = 5)
}

eis_buckets = do.call(rbind,split_buckets)
#saveRDS(eis_buckets,'../bucket_mount/tuolumne/scratch/eis_page_buckets_scratch.rds')

#eis_buckets = readRDS('../bucket_mount/tuolumne/scratch/eis_page_buckets_scratch.rds')
eis_candidates <- lsh_candidates(buckets = eis_buckets)
require(dplyr)
candidate_splits = split(eis_candidates,ntile(1:nrow(eis_candidates),n = nrow(eis_candidates) %/% 10000))
gc()

#corpus_meta = eis_corpus$meta
#corpus_meta$names <- names(eis_corpus)[!names(eis_corpus)%in%skipped(eis_corpus)]

parallel::clusterEvalQ(cluster,'require(data.table)')
parallel::clusterEvalQ(cluster,'require(textreuse)')
score_list = foreach(i = candidate_splits) %dopar% {
  send_names = unique(c(i$a,i$b))
  send_text = flist[send_names]
  score_list = mapply(function(aa,bb) textreuse::align_local(a = aa,b=bb)$score,aa = send_text[i$a],bb = send_text[i$b])
  data.table::data.table(a = i$a, b= i$b,score = score_list)
}
stopCluster(cluster)
gc()

score_dt = rbindlist(score_list)
score_dt <- score_dt[score>=300,]

dir.create('boilerplate_project/data_products/score_results/')
####### this isn' awesome solution, but allows the files to be small enough to sync in normal github
cuts = ceiling(nrow(score_dt) / 5e6)
cut_index = dplyr::ntile(1:nrow(score_dt),n = cuts)

lapply(1:cuts,function(cut) {
  saveRDS(score_dt[cut_index==cut,],paste0("boilerplate_project/data_products/score_results/eis_page_scores_scratch_file_",cut,".rds"),compress = TRUE)
})

