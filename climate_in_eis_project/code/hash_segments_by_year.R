

packs = c('data.table','tm','quanteda','textclean','stringr','pbapply','parallel','doParallel','textreuse','spacyr')
need = packs[!packs %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(packs,require,character.only = T)


minhash <- minhash_generator(n = 240, seed = 40)
progress_bars = T
gc()

rerun_existing = T

#mcores = detectCores() / 2
#options("mc.cores" = detectCores() /2)

#cluster = makeCluster(mcores)
#registerDoParallel(cl = cluster,cores = mcores)
#parallel::clusterEvalQ(cluster,'require(data.table)')

dir.create('scratch/climate_in_nepa/eis_climate_hash/')
scratch_loc = 'scratch/climate_in_nepa/eis_climate_hash/'

segment_lists = lapply(2013:2020,function(year) readRDS(paste0('scratch/climate_in_nepa/climate_segments/temp_segments_',year,'.rds')))
segment_vector = unlist(segment_lists)

segment_vector = segment_vector[!grepl('\\.{10,}',segment_vector)]

#######
#for(year in 2013:2020){
hash_file = paste0(scratch_loc,paste0('eis_climate_hashes.RDS'))
#if(rerun_existing|!file.exists(hash_file)){
tlist = segment_vector
ids = str_extract(names(tlist),'^[0-9]{8}')
  #text_set = unlist(tlist)
keep = which(nchar(text_set)>=750)
start_line = str_extract(str_extract(names(tlist),'txt_.*'),'txt_l[0-9]{1,}')
start_line = str_extract(start_line,'[0-9]{1,}')
end_line = str_extract(names(tlist),'[0-9]{1,}$')
  
corpus =   TextReuseCorpus(text = tlist,
             meta = list(Project_File = str_extract(names(tlist),'^.*txt'),
             PROJECT_ID = str_remove(names(tlist),'_.*'),start_line = start_line,end_line = end_line),
                                 tokenizer = tokenize_ngrams, n = 10,
                                 minhash_func = minhash, keep_tokens = TRUE,
                                 progress = progress_bars,skip_short = T)


  #eis_flist = flist[gsub('--','',str_extract(flist,'--.+\\.(PDF|pdf)')) %in% documents$FILE_NAME[documents$PROJECT_TYPE=='EIS']]
  #floc = 'input/filtered_text_files/'
  buckets <- lsh(corpus, bands = 40, progress = progress_bars)
  segment_candidates <- lsh_candidates(buckets)
  #eis_candidates$a = text_names[keep][as.numeric(str_extract(eis_candidates$a,'[0-9]{1,}'))]
  #eis_candidates$b = text_names[keep][as.numeric(str_extract(eis_candidates$b,'[0-9]{1,}'))]
  saveRDS(segment_candidates,hash_file)





