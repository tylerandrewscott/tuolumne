
library(readtext)
library(hunspell)
library(spelling)

tt = list.files('input/epa_comment_txt',full.names = T)
savedir = 'input/epa_comment_txt_spellchecked/'
have = list.files(savedir)
for(f in tt){
  if(!basename(f) %in% have){
    print(f)
file = readLines(f)
temp = hunspell(unlist(file))
uq_temp = sort(unique(temp[[1]]))
uq_temp = uq_temp[!grepl('[A-Z]{2,}',uq_temp)]
sugg_temp = hunspell_suggest(uq_temp)
for(i in seq_along(uq_temp)){
if(length(sugg_temp[[i]]>0)){
  file <- gsub(uq_temp[i],sugg_temp[[i]][1],file)}}
checked <- file
write.table(checked,file = paste0('input/epa_comment_txt_spellchecked/',basename(f)))
}}









