library(tidyverse)
library(stringr)

text_locs  = 'input/eis_es_text/'
#text_locs2 = 'input/eis_intro_text/'
tlist = list.files('input/eis_es_text',"*.txt$")
#tlist2 = list.files('input/eis_intro_text',"*.txt$")
#tlist= c(tlist,tlist2)
es_df = readtext::readtext(list.files('input/eis_es_text/',full.names = T))
es_df$EIS_Number = str_extract(es_df$doc_id,'[0-9]{8}')

doc_sentences = do.call(rbind,lapply(seq_along(es_df$EIS_Number),function(tl){
sents = tokenizers::tokenize_sentences(es_df$text[tl])
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
#drop weird spacing
sents = sents[!grepl('\\s[a-z]\\s[a-z]\\s|\\s[A-Za-z]\\s[A-Za-z]\\s[A-Za-z]\\s',sents)]
sents = sents[!grepl('\\s{2,}',sents)]
#end in a period
sents = sents[grepl('\\.$',sents)]
sents = sents[!duplicated(sents)]
sents = sents[!grepl("\\([^\\)]+$",sents)]
sents = sents[!grepl("[A-Z]{8,}",sents)]
if(length(sents)>0)
{data.frame(text = sents,file = tlist[tl])}
}))

data.table::fwrite(doc_sentences,'input/scratch/exec_summary_sentences.csv')
