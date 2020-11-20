
pack = c('data.table','textreuse','tokenizers','parallel','stringr','doParallel')
need = pack[!pack %in% installed.packages()[,'Package']]
if(length(need)>0){lapply(need,install.packages)}
lapply(pack,require,character.only=T)

mcores = detectCores()-4
cluster = makeCluster(mcores)
registerDoParallel(cl = cluster,cores = mcores)
hashfloc = 'scratch/boilerplate/hash_candidates/'
textfloc = 'input/filtered_text_files/'
gc()
lda_result_file = 'scratch/boilerplate/lda_combo_results_V4.RDS'
lda_result_addition = 'scratch/boilerplate/lda_combo_results_V4_a2.RDS'
if(file.exists(lda_result_file)){lda_result = readRDS(lda_result_file)}else{lda_result = data.table(a = character(),b=character(),score = character())}

hash_sets = list.files(hashfloc,pattern = 'V2')

flist  = list.files('scratch/boilerplate/big_text_files/',full.names = T,pattern = 'text.rds')
flist_list = lapply(flist,readRDS)
flist_dt = rbindlist(flist_list)
flist_dt$PROJECT_ID = str_remove(flist_dt$File,'(--|_(?!States)).*')
flist_dt$File = gsub('\\s{1,}','_',flist_dt$File,perl = T)
flist_dt$File = gsub('_{1,}','_',flist_dt$File,perl = T)
flist_dt$File = gsub('¿{1,}','_',flist_dt$File,perl = T)
flist_dt$File = gsub('_{1,}','_',flist_dt$File,perl = T)
#flist_dt$File <- str_remove(flist_dt$File,'^.*--')

projects = fread('scratch/boilerplate/project_candidates.csv')
projects$MASTER_ID[grepl('Eastern',projects$PROJECT_ID)] <- projects$PROJECT_ID[grepl('Eastern',projects$PROJECT_ID)]
projects_used = unique(projects$PROJECT_ID)
flist_dt = flist_dt[PROJECT_ID %in% projects$PROJECT_ID,]
flist_dt$PFID = paste(flist_dt$PROJECT_ID,flist_dt$File,flist_dt$Page,sep = '--')

flist_dt$PID<-NA 
file_dash = grepl('--',flist_dt$File)
flist_dt$PID[file_dash] = paste(flist_dt$File[file_dash],flist_dt$Page[file_dash],sep = '--')
flist_dt$PID[!file_dash] = paste(flist_dt$PROJECT_ID[!file_dash],flist_dt$File[!file_dash],flist_dt$Page[!file_dash],sep = '--')



#PRJPAGES = flist_dt[,.N,by = .(PROJECT_ID)]
#fwrite(PRJPAGES,'scratch/pages_by_project.csv')

combos_dt = rbindlist(lapply(seq_along(hash_sets),function(i) {
  combos = data.table(readRDS(paste0(hashfloc,hash_sets[i])))
  combos = combos[a!=b,]
  combos}))
combos_dt = combos_dt[!duplicated(combos_dt),]
combos_dt = combos_dt[order(a,b),]
combos_dt$a = str_replace(combos_dt$a,'^([0-9A-Z|-]+--)\\1','\\1')
combos_dt$b = str_replace(combos_dt$b,'^([0-9A-Z|-]+--)\\1','\\1')
combos_dt$a = gsub('\\.pdf\\.txt','.txt',combos_dt$a,perl = T)
combos_dt$b = gsub('\\.pdf\\.txt','.txt',combos_dt$b,perl = T)
combos_dt = combos_dt[!paste(combos_dt$a,combos_dt$b) %in% paste(lda_result$a,lda_result$b),]

matchA = match(combos_dt$a,flist_dt$PID)
matchB = match(combos_dt$b,flist_dt$PID)
bad = is.na(matchA)|is.na(matchB)

combos_dt = combos_dt[!bad,]
quant = floor(nrow(combos_dt)/250000)

qts = dplyr::ntile(1:nrow(combos_dt),quant)

if(nrow(combos_dt)>0){
for(q in 101:200){
print(q)
subcombos = combos_dt[qts==q,]
temp_flist = flist_dt[PID %in% subcombos$a | PID %in% subcombos$b,]
clusterExport(cluster,varlist = list('temp_flist'))
clusterExport(cluster,varlist = list('bad'))
scores = foreach(l = 1:nrow(subcombos)) %dopar% {
  matchA = match(subcombos$a,temp_flist$PID)
  matchB = match(subcombos$b,temp_flist$PID)
  a_text = temp_flist$text[matchA[l]]
  b_text = temp_flist$text[matchB[l]]
  align_result = tryCatch(textreuse::align_local(a=a_text, b=b_text),error = function(e) NULL)
  if(is.null(align_result)){align_result = data.table::data.table(score = NA)}
  data.table::data.table(a = subcombos$a[l],b = subcombos$b[l],score = align_result$score)
}
score_dt = rbindlist(scores,use.names=T,fill = T)
lda_result = rbind(lda_result,score_dt)
saveRDS(object = lda_result,file = lda_result_addition)
}
}

#saveRDS(object = lda_result,file = lda_result_file)

stopImplicitCluster()
stopCluster(cluster)



