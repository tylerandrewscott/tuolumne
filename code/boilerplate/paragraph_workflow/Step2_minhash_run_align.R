

require(devtools)
#install_github('ropensci/textreuse')
require(textreuse)
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(tm)){install.packages('tm');require(tm)}
if(!require(quanteda)){install.packages('quanteda');require(quanteda)}
if(!require(textclean)){install.packages('textclean');require(textclean)}
if(!require(stringr)){install.packages('stringr');require(stringr)}
if(!require(pbapply)){install.packages('pbapply');require(pbapply)}
if(!require(parallel)){install.packages('parallel');require(parallel)}
if(!require(doParallel)){install.packages('doParallel');require(doParallel)}

#setwd("google_drive/tuolumne/")
#setDTthreads(3)
#getDTthreads()
mcores = detectCores() / 2
options("mc.cores" = detectCores() /2)

#table(projects$AGENCY,projects$PROJECT_TYPE)
projects = fread('../bucket_mount/tuolumne/scratch/boilerplate/project_candidates_eis_only.csv')
projects = projects[Document=='Final',]
projects = projects[grepl('^201[3-9]|^2020',PROJECT_ID),]
documents = fread( '../bucket_mount/tuolumne/scratch/boilerplate/document_candidates_eis_only.csv')
documents = documents[PROJECT_ID %in% projects$PROJECT_ID,]
scratch_loc = '../bucket_mount/tuolumne/scratch/boilerplate/hash_candidates/'

tokpars = '../bucket_mount/tuolumne/scratch/tokenized_paragraphs/'
# flist  = list.files(tokpars)
# flist <- flist[grepl('^201[3-9]|^2020',flist)]
# flist <- flist[flist %in% gsub('pdf$|PDF$','txt',documents$File_Name)]
# flist = flist[!grepl('^([0-9]{8})_(CEQ|\\1)',flist)]

minhash <- minhash_generator(n = 240, seed = 24)
progress_bars = T
cluster = makeCluster(mcores)
registerDoParallel(cl = cluster,cores = mcores)
parallel::clusterEvalQ(cluster,'require(data.table)')
parallel::clusterEvalQ(cluster,'require(tidyverse)')
parallel::clusterEvalQ(cluster,'require(textreuse)')
#####
hash_file = paste0(scratch_loc,'eis_page_hash_paragraph_candidates.RDS')
full_tlist <- readRDS('../bucket_mount/tuolumne/scratch/paragraph_list.rds')
chars = nchar(full_tlist)
full_tlist = full_tlist[chars<(20*1000)]
chars2 = nchar(full_tlist)

#eis_corpus =  TextReuseCorpus(text = full_tlist,
#                                meta = list(ID = names(full_tlist)),
#                                tokenizer = tokenize_ngrams, n = 10,
#                                minhash_func = minhash, keep_tokens = TRUE,
#                                progress = progress_bars,skip_short = T)
#saveRDS(eis_corpus,'../bucket_mount/tuolumne/scratch/eis_corp_scratch.rds')
eis_corpus = readRDS('../bucket_mount/tuolumne/scratch/eis_corp_scratch.rds')
# 
# split_corpus_ntiles = dplyr::ntile(x = seq(eis_corpus),n = mcores*10)
# split_corpus = split(eis_corpus,split_corpus_ntiles)
# 
# split_buckets = pblapply(split_corpus,function(x) lsh(x,bands = 40,progress = F),cl = 5)
# 
# while(any(sapply(split_buckets,is.null))){
#   null_fails = which(sapply(split_buckets,is.null))
#   split_buckets[null_fails] <- pblapply(null_fails,function(x) lsh(split_corpus[[x]],bands = 40,progress = F),cl = 5)
# }
# 
# eis_buckets = do.call(rbind,split_buckets)
# saveRDS(eis_buckets,'../bucket_mount/tuolumne/scratch/eis_buckets_scratch.rds')

eis_buckets = readRDS('../bucket_mount/tuolumne/scratch/eis_buckets_scratch.rds')
eis_candidates <- lsh_candidates(buckets = eis_buckets)
saveRDS(eis_candidates,'../bucket_mount/tuolumne/scratch/eis_candidates_scratch.rds')

eis_candidates <- readRDS('../bucket_mount/tuolumne/scratch/eis_candidates_scratch.rds')
require(dplyr)
candidate_splits = split(eis_candidates,ntile(1:nrow(eis_candidates),n = nrow(eis_candidates) %/% 1000))

score_list = foreach(i = candidate_splits) %dopar% {
  send_names = unique(c(i$a,i$b))
  send_text = full_tlist[send_names]
  parallel::clusterExport(cluster,varlist = list())
  score_list = mapply(function(aa,bb) textreuse::align_local(a = aa,b=bb)$score,aa = send_text[i$a],bb = send_text[i$b])
  data.table::data.table(a = i$a, b= i$b,score = score_list)}

score_dt = rbindlist(score_list)
saveRDS(score_dt,'../bucket_mount/tuolumne/scratch/eis_scores_scratch.rds')


# 
# t1 = data.table::fread(fls[i],sep ='\t');
# t2 = t1$text;names(t2) <- t1$Project_File_Par
# t2}
# text_set = unlist(tlist)
# keep = which(nchar(text_set)>=750)
# 
# eis_corpus =  TextReuseCorpus(text = text_set[keep],
#                               meta = list(Project_File_Par = names(text_set)[keep],PROJECT_ID = str_remove(names(text_set)[keep],'--.*')),
#                               tokenizer = tokenize_ngrams, n = 10,
#                               minhash_func = minhash, keep_tokens = TRUE,
#                               progress = progress_bars,skip_short = T)
# eis_buckets <- lsh(eis_corpus, bands = 40, progress = progress_bars)
# eis_candidates <- lsh_candidates(eis_buckets)
# #eis_candidates$a = text_names[keep][as.numeric(str_extract(eis_candidates$a,'[0-9]{1,}'))]
# #eis_candidates$b = text_names[keep][as.numeric(str_extract(eis_candidates$b,'[0-9]{1,}'))]
# saveRDS(eis_candidates,hash_file)
# }



