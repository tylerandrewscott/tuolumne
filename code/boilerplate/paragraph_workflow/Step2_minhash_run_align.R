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

#table(projects$AGENCY,projects$PROJECT_TYPE)
projects = fread('scratch/boilerplate/project_candidates_eis_only.csv')
projects = projects[Document=='Final',]
projects = projects[grepl('^201[3-9]|^2020',PROJECT_ID),]
documents = fread( 'scratch/boilerplate/document_candidates_eis_only.csv')
documents = documents[PROJECT_ID %in% projects$PROJECT_ID,]
scratch_loc = 'scratch/boilerplate/hash_candidates/'

tokpars = 'scratch/tokenized_paragraphs/'
flist  = list.files(tokpars)
flist <- flist[grepl('^201[3-9]|^2020',flist)]
flist <- flist[flist %in% gsub('pdf$|PDF$','txt',documents$File_Name)]
flist = flist[!grepl('^([0-9]{8})_(CEQ|\\1)',flist)]

str(flist)
minhash <- minhash_generator(n = 240, seed = 24)
progress_bars = T
gc()
cluster = makeCluster(mcores)
registerDoParallel(cl = cluster,cores = mcores)
parallel::clusterEvalQ(cluster,'require(data.table)')


#####
hash_file = paste0(scratch_loc,'eis_page_hash_paragraph_candidates.RDS')
if(rerun_existing|!file.exists(hash_file)){

  tlist = foreach(i = seq_along(flist)) %dopar% {
    t1 = readLines(paste0(tokpars,flist[i]))
    t1}
  names(tlist)<- flist
 
  full_tlist = unlist(tlist)
  rm(tlist)
  gc()
  #saveRDS(full_tlist,'scratch/paragraph_list.rds')
  full_tlist <- readRDS('../bucket_mount/paragraph_list.rds')
  eis_corpus =  TextReuseCorpus(text = full_tlist,
                                meta = list(ID = names(full_tlist)),
                                tokenizer = tokenize_ngrams, n = 10,
                                minhash_func = minhash, keep_tokens = TRUE,
                                progress = progress_bars,skip_short = T)
  saveRDS(eis_corpus,'../bucket_mount/scratch/eis_corp_scratch.rds')
  eis_buckets <- lsh(eis_corpus, bands = 20, progress = progress_bars)
  eis_candidates <- lsh_candidates(eis_buckets)
}
  
  
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



