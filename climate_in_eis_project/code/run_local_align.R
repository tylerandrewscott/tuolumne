if(!require(textreuse)){install.packages('textreuse');require(textreuse)}
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(tokenizers)){install.packages('tokenizers');require(tokenizers)}
#if(!require(readtext)){install.packages('readtext');require(readtext)}
if(!require(parallel)){install.packages('parallel');require(parallel)}

require(data.table)
require(tokenizers)
require(rvest)

require(pdftools)
require(stringr)
require(Matrix)
require(parallel)
require(doParallel)
require(pbapply)

mcores = detectCores()-2
cluster = makeCluster(mcores)
registerDoParallel(cl = cluster,cores = mcores)
hashfloc = 'scratch/climate_in_nepa/eis_year_tests/'

projs = fread('../eis_documents/enepa_repository/meta_data/eis_record_detail.csv')
projs = projs[grepl('Draft',Document.Type),]
projs = projs[str_extract(projs$EIS.Number,'^20(1[3-9]|20)') %in% 2013:2020,]

hash_file = paste0(scratch_loc,paste0('eis_climate_hashes.RDS'))
hash = readRDS(hash_file)
hash_pages_a = as.data.table(do.call(rbind,str_split(str_remove(str_extract(hash$a,'txt_.*'),'txt_l'),'_')))
hash_pages_b = as.data.table(do.call(rbind,str_split(str_remove(str_extract(hash$b,'txt_.*'),'txt_l'),'_')))
file_a = str_extract(hash$a,'^.*txt')
file_b = str_extract(hash$b,'^.*txt')

same_file = file_a==file_b
b_in_a_seq = hash_pages_b$V1 > hash_pages_a$V1 & hash_pages_b$V1 < hash_pages_a$V2

hash_keep = hash[!b_in_a_seq&!same_file,]

# 
# combos_dt = rbindlist(lapply(seq_along(hash_sets),function(i) {
#   combos = data.table(readRDS(paste0(hashfloc,hash_sets[i])))
#   combos = combos[a!=b,]
#   combos}))
# combos_dt = combos_dt[!duplicated(combos_dt),]
# combos_dt = combos_dt[order(a,b),]
# combos_dt = combos_dt[!paste(combos_dt$a,combos_dt$b) %in% paste(lda_result$a,lda_result$b),]
# combos_dt = combos_dt[str_extract(a,'^[0-9]{8}')!=str_extract(b,'^[0-9]{8}'),]

segment_lists = lapply(2013:2020,function(year) readRDS(paste0('scratch/climate_in_nepa/climate_segments/temp_segments_',year,'.rds')))
segment_vector = unlist(segment_lists)


quant = 50
qts = dplyr::ntile(1:nrow(hash_keep),quant)
for(q in 1:quant){
print(q)
subcombos = hash_keep[qts==q,]
fl1 = gsub('(\\.txt).*','\\1',subcombos$a,perl = T)
fl2 = gsub('(\\.txt).*','\\1',subcombos$b,perl = T)
l1 = gsub('--','',str_extract(subcombos$a,'txt_l[0-9]{1,}_[0-9]{1,}$'))
l2 = gsub('--','',str_extract(subcombos$a,'txt_l[0-9]{1,}_[0-9]{1,}$'))
l1a = as.numeric(gsub('txt_','',str_extract(l1,'txt_[0-9]{1,}')))
l1b = as.numeric(str_extract(l1,'[0-9]{1,}$'))
l2a = as.numeric(gsub('txt_','',str_extract(l2,'txt_[0-9]{1,}')))
l2b = as.numeric(str_extract(l2,'[0-9]{1,}$'))
segments_needed = unique(c(unique(subcombos$a),unique(subcombos$b)))
#pages_needed = pblapply(files_needed,function(x) {unique(c(p1[fl1==x],p2[fl2==x]))},cl = mcores)
text_entries = segment_vector[names(segment_vector) %in% segments_needed]

#subcombos = subcombos[a %in% text_entries$Project_File_Par & b %in% text_entries$Project_File_Par,]
scores = foreach(l = 1:nrow(subcombos)) %dopar% {
  a_text = text_entries[[subcombos$a[l]]]
  b_text = text_entries[[subcombos$b[l]]]
  align_result = textreuse::align_local(a=a_text, b=b_text)
  data.table::data.table(a = subcombos$a[l],b = subcombos$b[l],score = align_result$score)
}

score_dt = rbindlist(scores,use.names=T,fill = T)
lda_result = rbind(lda_result,score_dt)
saveRDS(lda_result,lda_result_file)
}
stopImplicitCluster()
stopCluster(cluster)


segment_vector['20160051_Chapter_7_Northern_Mariana_Islands.txt_l1218_1418']
segment_vector['20160051_Chapter_5_American_Samoa.txt_l1220_1420']






