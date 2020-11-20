
#setwd('bucket_mount/tuolumne/')
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(tm)){install.packages('tm');require(tm)}
if(!require(quanteda)){install.packages('quanteda');require(quanteda)}
if(!require(textclean)){install.packages('textclean');require(textclean)}
if(!require(stringr)){install.packages('stringr');require(stringr)}
if(!require(pbapply)){install.packages('pbapply');require(pbapply)}
if(!require(doParallel)){install.packages('doParallel');require(doParallel)}
if(!require(textreuse)){install.packages('textreuse');require(textreuse)}
if(!require(slam)){install.packages('slam');require(slam)}
if(!require(textmineR)){install.packages('textmineR');require(textmineR)}

if(!dir.exists('input/clean_text_files')){dir.create('input/clean_text_files')}

parcores = detectCores() / 2 
cores = 1
flist = list.files('input/filtered_text_files',full.names = T)
clean_names = paste0('input/clean_text_files/',basename(flist))
clean_files = list.files('input/clean_text_files',full.names = T)
bad_files = clean_files[!basename(clean_files) %in% basename(flist)]
file.remove(bad_files)
go_get = which(!basename(flist) %in% basename(clean_files))

pblapply(go_get,function(i) {
  dt = fread(flist[i],sep = '\t')
  dt$text = unlist(mclapply(dt$text,removePunctuation,mc.cores = cores,mc.cleanup = T,mc.preschedule = T))
  dt$text = unlist(mclapply(dt$text,stripWhitespace,mc.cores = cores,mc.cleanup = T,mc.preschedule = T))
  dt$text = unlist(mclapply(dt$text,function(x) removeWords(x,stopwords('english')),mc.cores = cores,mc.cleanup = T,mc.preschedule = T))
  dt$text = unlist(mclapply(dt$text,function(x) removeWords(x,state.name),mc.cores = cores,mc.cleanup = T,mc.preschedule = T))
  dt$text = unlist(mclapply(dt$text,function(x) removeWords(x,c('yes','no')),mc.cores = cores,mc.cleanup = T,mc.preschedule = T))
  dt$text = unlist(mclapply(dt$text,function(x) gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", x),mc.cores = cores,mc.cleanup = T,mc.preschedule = T))
  dt$text = unlist(mclapply(dt$text,function(x) removeNumbers(x),mc.cores = cores,mc.cleanup = T,mc.preschedule = T))
  dt$text = unlist(mclapply(dt$text,tolower,mc.cores = cores,mc.cleanup = T,mc.preschedule = T))
  dt$text = unlist(mclapply(dt$text,stripWhitespace,mc.cores = cores,mc.cleanup = T,mc.preschedule = T))
  dt$text = unlist(mclapply(dt$text,function(x) gsub("^ +| +$|( ) +", "\\1",x),mc.cores = cores,mc.cleanup = T,mc.preschedule = T))
  dt = dt[(!grepl('iodd',dt$text))&(!grepl('icdd',dt$text)),]
  if(!file.exists(paste0('input/clean_text_files/',basename(flist[i])))){
    fwrite(x = dt,file = paste0('input/clean_text_files/',basename(flist[i])))
  }
},cl = parcores)


