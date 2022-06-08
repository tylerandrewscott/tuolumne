library(tidyverse)
library(stringr)
library(lubridate)
library(data.table)
library(pbapply)

ca_ref = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vSW1BL6LAW6zGiL2Ms-QC4n_H7ASJfr-pBxjvCnGB8rtaP4x08MQnmK5RHoRoYPO3syl6LubHbxCOeG/pub?output=csv')
#eis$In_Subsample = (eis$EIS.Number %in% ca_ref$`EIS Number`)+0

library(readtext)
flist = list.files('scratch/leverage_points/ca_text_files/')
flist_EIS = ca_ref$`EIS Number`[match(flist,paste0(ca_ref$`Standard Name`,'.txt'))]

text_locs = 'scratch/leverage_points/ca_text_files/'

#text_locs  = 'scratch/leverage_points/all_eis_text_files/'
#text_locs2 = 'input/eis_intro_text/'
#tlist = list.files(text_locs,"*.txt$")
tlist = flist
#ca_df$`EIS Number` %in% str_extract(tlist,'^[0-9]{8}')
#tlist2 = list.files('input/eis_intro_text',"*.txt$")
#tlist= c(tlist,tlist2)
es_df = readtext::readtext(paste0(text_locs,tlist),ignore_missing_files=T)

library(data.table)
es_df$EIS_Number = ca_ref$`EIS Number`[match(es_df$doc_id,paste0(ca_ref$`Standard Name`,'.txt'))]
es_df$FR_Year = str_extract(es_df$EIS_Number,'^[0-9]{4}')

doc_sentences = rbindlist(pblapply(seq_along(es_df$EIS_Number),function(tl){
sents = tokenizers::tokenize_sentences(es_df$text[tl])
sents = unlist(sents)
regex_line_number_string = "([0-9]{1,2}\\s){3,}"
regex_table_dump = "(Alternative\\s[A-Z0-9]\\s){2,}"
regex_largenumber_dump = "[0-9]{5,}|[0-9]{3,}-[0-9]{3,}"
regex_internet_dump = "@|\\.com|cc:"
regex_funkychar_dump = "\\*|_"
regex_too_many_periods_dump = "(\\.){4,}"
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
{data.table(text = sents,file = tlist[tl])}
},cl = 8))
gc()

doc_sentences$EIS_Number = es_df$EIS_Number[match(doc_sentences$file,es_df$doc_id)]

data.table::fwrite(doc_sentences,'scratch/leverage_points/ca_focal_eis_water_energy_tokenized_sentences.csv')
