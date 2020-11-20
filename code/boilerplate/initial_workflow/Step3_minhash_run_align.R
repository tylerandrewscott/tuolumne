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
#setwd("google_drive/tuolumne/")
#setDTthreads(3)
#getDTthreads()
mcores = detectCores() / 2
options("mc.cores" = detectCores() /2)
flist  = list.files('input/filtered_text_files/')

ids = gsub('--','',str_extract(flist,'.+--'))

#table(projects$AGENCY,projects$PROJECT_TYPE)
projects = fread('scratch/boilerplate/projects_used.csv')
documents = fread('scratch/boilerplate/documents_used.csv')
scratch_loc = 'scratch/boilerplate/hash_candidates/'

minhash <- minhash_generator(n = 240, seed = 40)
progress_bars = T
gc()

cluster = makeCluster(mcores)
registerDoParallel(cl = cluster,cores = mcores)
parallel::clusterEvalQ(cluster,'require(data.table)')

#######
hash_file = paste0(scratch_loc,'blm_page_hash_candidates.RDS')
if(rerun_existing|!file.exists(hash_file)){
blm_flist = flist[gsub('--','',str_extract(flist,'--.+\\.(PDF|pdf)')) %in% documents$FILE_NAME[documents$AGENCY=='Bureau of Land Management']]
floc = 'input/filtered_text_files/'
fls = paste0(floc,blm_flist)

tlist2 = foreach(i = seq_along(fls)[2001:length(fls)],.combine = 'c') %dopar% {
  t1 = data.table::fread(fls[i],sep ='\t')
  t2 = t1$text
  names(t2) <- t1$Project_File_Par
  t2}

options('mc.cores' = mcores)
blm_corpus =   TextReuseCorpus(text = tlist,
                               meta = list(Project_File_Par = names(tlist),PROJECT_ID = str_remove(names(tlist),'--.*')),
                               tokenizer = tokenize_ngrams, n = 10,
                               minhash_func = minhash, keep_tokens = TRUE,
                               progress = progress_bars,skip_short = T)

wcount = sapply(tlist,wordcount)
summary(wcount)

wordcount(x)

names(t2)

i = grep('DOI-BLM-ID-T030-2013-0025-EA--Bald_Mountain_Bike_Trails_EA.pdf',blm_flist)
t1 = data.table::fread(fls[i],sep ='\t');
t2 = t1$text
names(t2) <- t1$Project_File_Par
text_set = unlist(tlist)
keep = as.vector(nchar(text_set)>=750)





test_text = data.table::fread(paste0(floc,test_file),sep = '\t')
nchar(test_text$text[test_text$Paragraph==157])


fls = paste0(floc,blm_flist)
tlist_test = foreach(i = 600 + -10:10) %dopar% {
  t1 = data.table::fread(fls[i],sep ='\t');
  t2 = t1$text;names(t2) <- t1$Project_File_Par
  t2}

text_set_test = unlist(tlist_test)
keep_test = as.vector(nchar(text_set_test)>=750)
names(text_set_test)[!keep_test]
blm_corpus_test2 =   TextReuseCorpus(text = text_set_test[keep_test],
                               meta = list(Project_File_Par = names(text_set_test)[keep_test],PROJECT_ID = str_remove(names(text_set_test)[keep_test],'--.*')),
                               tokenizer = tokenize_ngrams, n = 10,
                               minhash_func = minhash, keep_tokens = TRUE,
                               progress = progress_bars,skip_short = T)

test = c(blm_corpus_test,blm_corpus_test2)

is.TextReuseCorpus(test)
names(blm_corpus_test)[names(blm_corpus_test) %in% skipped(blm_corpus)]



str(tlist_test)
str(unlist(tlist_test))

fls = paste0(floc,blm_flist)
tlist = foreach(i = seq_along(fls)) %dopar% {
t1 = data.table::fread(fls[i],sep ='\t');
t2 = t1$text;names(t2) <- t1$Project_File_Par
t2}

summary(sapply(text_vector,nchar))
text_vector = unlist(tlist)
keep = as.vector(nchar(text_vector)>=750)

options('mc.cores' = 1)
blm_corpus =   TextReuseCorpus(text = text_vector[keep],
                               meta = list(Project_File_Par = names(text_vector[keep]),PROJECT_ID = str_remove(names(text_vector[keep]),'--.*')),
                               tokenizer = tokenize_ngrams, n = 10,
                               minhash_func = minhash, keep_tokens = TRUE,
                               progress = progress_bars,skip_short = T)

testvec = text_vector[keep]


sessionInfo()
seed = 24
options('mc.cores' = 12)
corpu25kA =   TextReuseCorpus(text = testvec[1:25000],
                               tokenizer = tokenize_ngrams, n = 10,
                               minhash_func = minhash, keep_tokens = TRUE,
                               progress = progress_bars,skip_short = T)
corpus25kB =   TextReuseCorpus(text = testvec[25001:50000],
                              tokenizer = tokenize_ngrams, n = 10,
                              minhash_func = minhash, keep_tokens = TRUE,
                              progress = progress_bars,skip_short = T)
corpus50k =   TextReuseCorpus(text = testvec[1:50000],
                               tokenizer = tokenize_ngrams, n = 10,
                               minhash_func = minhash, keep_tokens = TRUE,
                               progress = F,skip_short = T)
head(skipped(corpus50k))



corpus50k_V2 =   TextReuseCorpus(text = testvec[1:50000],
                              tokenizer = tokenize_ngrams, n = 10,
                              minhash_func = minhash, keep_tokens = TRUE,
                              progress = F,skip_short = T)

test = skipped(corpus50k)


corpus30k =   TextReuseCorpus(text = testvec[1:30000],
                              tokenizer = tokenize_ngrams, n = 10,
                              minhash_func = minhash, keep_tokens = TRUE,
                              progress = progress_bars,skip_short = T)
corpus40k =   TextReuseCorpus(text = testvec[1:40000],
                              tokenizer = tokenize_ngrams, n = 10,
                              minhash_func = minhash, keep_tokens = TRUE,
                              progress = progress_bars,skip_short = T)
corpus50k =   TextReuseCorpus(text = testvec[1:50000],
                              tokenizer = tokenize_ngrams, n = 10,
                              minhash_func = minhash, keep_tokens = TRUE,
                              progress = progress_bars,skip_short = T)
corpustest =   TextReuseCorpus(text = testvec[40001:50000],
                              tokenizer = tokenize_ngrams, n = 10,
                              minhash_func = minhash, keep_tokens = TRUE,
                              progress = progress_bars,skip_short = T)

mem_used()

#options('mc.cores' = 12)
blm_corpus3 =   TextReuseCorpus(text = testvec,
                                tokenizer = tokenize_ngrams, n = 10,
                                minhash_func = minhash, keep_tokens = TRUE,
                                progress = progress_bars,skip_short = T)
head(skipped(blm_corpus2))
head(skipped(blm_corpus3))

gc()



htestvecs = text_vector[match(sk2,names(text_vector))]
test_corpus  =   TextReuseCorpus(text = testvecs,
                               meta = list(Project_File_Par = names(testvecs),PROJECT_ID = str_remove(names(testvecs),'--.*')),
                               tokenizer = tokenize_ngrams, n = 10,
                               minhash_func = minhash, keep_tokens = TRUE,
                               progress = progress_bars,skip_short = T)



tokenizers::tokenize_ngrams(testvecs[1],n = 10)
skipped(test_corpus)
all(sk2 %in% sk1)
length(c(unique(sk1,sk2)))

table(sk1 %in% sk2)
table(sk2 %in% sk1)


length(union(sk1,sk2))
length(intersect(sk1,sk2))


test = grep('20160171--20160171_Chapter-4 - Environmental Consequences.pdf',blm_flist,value=T)
fread(test,)
skipped(blm_corpus)
nchar(test[6983])

which(names(test)== "DOI-BLM-ID-T030-2013-0025-EA--Bald_Mountain_Bike_Trails_EA.pdf--157" )

skipped(blm_corpus)



nchar(text_set[1:100])

keep[1:100]
which(!keep[1:100])

names(text_set[1:100])
skipped(blm_corpus)
blm_buckets <- lsh(blm_corpus, bands = 40, progress = T)
blm_candidates <- lsh_candidates(blm_buckets)
#blm_candidates$a = names(text_set)[keep][as.numeric(str_extract(blm_candidates$a,'[0-9]{1,}'))]
#blm_candidates$b = names(text_set)[as.numeric(str_extract(blm_candidates$b,'[0-9]{1,}'))]
saveRDS(blm_candidates,hash_file)
}


#######
hash_file = paste0(scratch_loc,'usfs_page_hash_candidates.RDS')
if(rerun_existing|!file.exists(hash_file)){
usfs_flist = flist[gsub('--','',str_extract(flist,'--.+\\.(PDF|pdf)')) %in% documents$FILE_NAME[documents$AGENCY=='Forest Service']]
floc = 'input/filtered_text_files/'
fls = paste0(floc,usfs_flist)
tlist = foreach(i = seq_along(fls)) %dopar% {
  t1 = data.table::fread(fls[i],sep ='\t');
  t2 = t1$text;names(t2) <- t1$Project_File_Par
  t2}
text_set = unlist(tlist)
keep = nchar(text_set)>=750

usfs_corpus =  TextReuseCorpus(text = text_set[keep],
                               meta = list(Project_File_Par = names(text_set)[keep],PROJECT_ID = str_remove(names(text_set)[keep],'--.*')),
                               tokenizer = tokenize_ngrams, n = 10,
                               minhash_func = minhash, keep_tokens = TRUE,
                               progress = progress_bars,skip_short = T)

#skp = skipped(usfs_corpus)
#names(usfs_corpus)[!names(usfs_corpus) %in% skp] <- id_vec[!names(usfs_corpus) %in% skp]
usfs_buckets <- lsh(usfs_corpus, bands = 40, progress = progress_bars)
usfs_candidates <- lsh_candidates(usfs_buckets)
#usfs_candidates$a = text_names[keep][as.numeric(str_extract(usfs_candidates$a,'[0-9]{1,}'))]
#usfs_candidates$b = text_names[keep][as.numeric(str_extract(usfs_candidates$b,'[0-9]{1,}'))]

#print(dim(usfse_candidates))
#aid = str_extract(usfse_candidates$a,'^(.*?)--')
#bid = str_extract(usfse_candidates$b,'^(.*?)--')
saveRDS(usfs_candidates,hash_file)
}


#######
hash_file = paste0(scratch_loc,'doe_page_hash_candidates.RDS')
if(rerun_existing|!file.exists(hash_file)){
doe_flist = flist[gsub('--','',str_extract(flist,'--.+\\.(PDF|pdf)')) %in% documents$FILE_NAME[documents$AGENCY=='Department of Energy']]
floc = 'input/filtered_text_files/'
fls = paste0(floc,doe_flist)

tlist = foreach(i = seq_along(fls)) %dopar% {
  t1 = data.table::fread(fls[i],sep ='\t');
  t2 = t1$text;names(t2) <- t1$Project_File_Par
  t2}
text_set = unlist(tlist)
keep = nchar(text_set)>=750

doe_corpus = TextReuseCorpus(text = text_set[keep],
                             meta = list(Project_File_Par = names(text_set)[keep],PROJECT_ID = str_remove(names(text_set)[keep],'--.*')),
                             tokenizer = tokenize_ngrams, n = 10,
                             minhash_func = minhash, keep_tokens = TRUE,
                             progress = progress_bars,skip_short = T)
doe_buckets <- lsh(doe_corpus, bands = 40, progress = progress_bars)
doe_candidates <- lsh_candidates(doe_buckets)
#doe_candidates$a = names(text_set)[keep][as.numeric(str_extract(doe_candidates$a,'[0-9]{1,}$'))]
#doe_candidates$b = names(text_set)[as.numeric(str_extract(doe_candidates$b,'[0-9]{1,}$'))]

#print(dim(doe_candidates))
#aid = str_extract(doe_candidates$a,'^(.*?)--')
#bid = str_extract(doe_candidates$b,'^(.*?)--')
saveRDS(doe_candidates,hash_file)
}


#####
hash_file = paste0(scratch_loc,'eis_page_hash_candidates.RDS')
if(rerun_existing|!file.exists(hash_file)){
  eis_flist = flist[gsub('--','',str_extract(flist,'--.+\\.(PDF|pdf)')) %in% documents$FILE_NAME[documents$PROJECT_TYPE=='EIS']]
  floc = 'input/filtered_text_files/'
  fls = paste0(floc,eis_flist)
  tlist = foreach(i = seq_along(fls)) %dopar% {
    t1 = data.table::fread(fls[i],sep ='\t');
    t2 = t1$text;names(t2) <- t1$Project_File_Par
    t2}
  text_set = unlist(tlist)
  keep = which(nchar(text_set)>=750)
  
  eis_corpus =  TextReuseCorpus(text = text_set[keep],
                                meta = list(Project_File_Par = names(text_set)[keep],PROJECT_ID = str_remove(names(text_set)[keep],'--.*')),
                                tokenizer = tokenize_ngrams, n = 10,
                                minhash_func = minhash, keep_tokens = TRUE,
                                progress = progress_bars,skip_short = T)
  eis_buckets <- lsh(eis_corpus, bands = 40, progress = progress_bars)
  eis_candidates <- lsh_candidates(eis_buckets)
  #eis_candidates$a = text_names[keep][as.numeric(str_extract(eis_candidates$a,'[0-9]{1,}'))]
  #eis_candidates$b = text_names[keep][as.numeric(str_extract(eis_candidates$b,'[0-9]{1,}'))]
  saveRDS(eis_candidates,hash_file)
}



