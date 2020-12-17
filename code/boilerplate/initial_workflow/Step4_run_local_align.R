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
hashfloc = 'scratch/boilerplate/hash_candidates/'
textfloc = 'input/filtered_text_files/'

lda_result_file = 'scratch/boilerplate/lda_combo_results.RDS'
if(file.exists(lda_result_file)){lda_result = readRDS(lda_result_file)}else{lda_result = data.table(a = character(),b=character(),score = character())}

documents_used = fread('scratch/boilerplate/documents_used.csv')
hash_sets = list.files(hashfloc)

combos_dt = rbindlist(lapply(seq_along(hash_sets),function(i) {
  combos = data.table(readRDS(paste0(hashfloc,hash_sets[i])))
  combos = combos[a!=b,]
  combos}))
combos_dt = combos_dt[!duplicated(combos_dt),]
combos_dt = combos_dt[order(a,b),]
combos_dt = combos_dt[!paste(combos_dt$a,combos_dt$b) %in% paste(lda_result$a,lda_result$b),]
quant = 50
qts = dplyr::ntile(1:nrow(combos_dt),quant)

for(q in 1:quant){
print(q)
subcombos = combos_dt[qts==q,]
fl1 = paste0(str_remove(subcombos$a,'--[0-9]{1,}$'),'.txt')
fl2 = paste0(str_remove(subcombos$b,'--[0-9]{1,}$'),'.txt')
p1 = as.numeric(gsub('--','',str_extract(subcombos$a,'--[0-9]{1,}$')))
p2 = as.numeric(gsub('--','',str_extract(subcombos$b,'--[0-9]{1,}$')))
files_needed = unique(c(fl1,fl2))
segments_needed = unique(c(unique(subcombos$a),unique(subcombos$b)))
#pages_needed = pblapply(files_needed,function(x) {unique(c(p1[fl1==x],p2[fl2==x]))},cl = mcores)

text_entries = rbindlist(pblapply(seq_along(files_needed),function(i) fread(paste0(textfloc,files_needed[i]),sep = '\t')[Project_File_Par %in% segments_needed,],cl = mcores))

#subcombos = subcombos[a %in% text_entries$Project_File_Par & b %in% text_entries$Project_File_Par,]
scores = foreach(l = 1:nrow(subcombos)) %dopar% {
  match(text_entries$Project_File_Par,subcombos$a[l])
  a_text = text_entries$text[text_entries$Project_File_Par %in% subcombos$a[l]]
  b_text = text_entries$text[text_entries$Project_File_Par %in% subcombos$b[l]]
  align_result = textreuse::align_local(a=a_text, b=b_text)
  data.table::data.table(a = subcombos$a[l],b = subcombos$b[l],score = align_result$score)
}
score_dt = rbindlist(scores,use.names=T,fill = T)
lda_result = rbind(lda_result,score_dt)
saveRDS(lda_result,lda_result_file)
}
stopImplicitCluster()
stopCluster(cluster)

