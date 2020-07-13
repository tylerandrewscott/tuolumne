library(tidyverse)
library(stringr)

text_dt = readRDS('scratch/descriptive_eis/eis_execsum_text.RDS')

doc_sentences = pblapply(seq_along(text_dt$text),function(tl){
sents = tokenizers::tokenize_sentences(text_dt$text[tl])
sents = unlist(sents)
regex_line_number_string = "([0-9]{1,2}\\s){3,}"
regex_table_dump = "(Alternative\\s[A-Z0-9]\\s){2,}"
regex_largenumber_dump = "[0-9]{5,}|[0-9]{3,}-[0-9]{3,}"
regex_internet_dump = "@|\\.com|cc:"
regex_funkychar_dump = "\\*|_"
sents = sents[!grepl(regex_line_number_string,sents)]
sents = sents[!grepl(regex_table_dump,sents)]
sents = sents[!grepl(regex_largenumber_dump,sents)]
sents = sents[!grepl(regex_funkychar_dump,sents)]
sents = sents[nchar(sents)>50 & nchar(sents) < 400]
sents = gsub("[^[:print:]]", "", sents)
#drop weird spacing
sents = sents[!grepl('\\s[a-z]\\s[a-z]\\s|\\s[A-Za-z]\\s[A-Za-z]\\s[A-Za-z]\\s',sents)]
sents = sents[!grepl('\\s{2,}',sents)]
#end in a period
sents = sents[grepl('\\.$',sents)]
sents = sents[!duplicated(sents)]
sents = sents[!grepl("\\([^\\)]+$",sents)]
sents = sents[!grepl("[A-Z]{8,}",sents)]
if(length(sents)>0)
{data.table(text = sents,file = text_dt$File[tl])}
},cl = 4)

sent_dt = rbindlist(doc_sentences)
saveRDS(sent_dt,'scratch/descriptive_eis/exec_summary_sentences.RDS')
