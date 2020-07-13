library(tidyverse)
library(stringr)
library(lubridate)
library(data.table)
library(pbapply)
eis = fread('input/epa_master_repository/eis_record_detail.csv')
eis$CA = grepl('CA',eis$State.or.Territory)+0
ca_ref = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vSW1BL6LAW6zGiL2Ms-QC4n_H7ASJfr-pBxjvCnGB8rtaP4x08MQnmK5RHoRoYPO3syl6LubHbxCOeG/pub?output=csv')
eis$In_Subsample = (eis$EIS.Number %in% ca_ref$`EIS Number`)+0
eis$Federal.Register.Date = mdy(eis$Federal.Register.Date)
eis$FR_Year = year(eis$Federal.Register.Date)
set = c('solar','wind','water','hydroelectric','hydro','riparian','plants','floodway','levee','river bank','watershed',
        'flood','flood control','geothermal','pipeline','gas','oil','drilling','energy','storage',
        'transmission','reservoir','dam','canal','wetlands','renwable','transfer','water resource')
eis$water_energy = grepl(paste(paste0('\\b',set,'\\b'),collapse='|'),tolower(eis$Title)) + 0
eis = eis[eis$FR_Year>=2010,]
eis = eis[eis$water_energy==1,]

library(readtext)

docs = read_csv('input/epa_master_repository/eis_document_record.csv')
docs = docs[docs$EIS.Number %in% eis$EIS.Number,]
docs = docs[docs$File_Name!=paste0(paste(docs$EIS.Number,docs$EIS.Number,sep='_'),".pdf"),]
docs$Text_File_Name = gsub('pdf$','txt',docs$File_Name)
#flist = list.files('scratch/eis_documents_plaintext/')
#flist = flist[flist %in% docs$Text_File_Name & !grepl('[0-9]{8}_[0-9]{8}\\.txt',flist)]

flist = list.files('scratch/eis_documents_plaintext/')
flist = flist[str_extract(flist,'^[0-9]{8}') %in% eis$EIS.Number]
flist = flist[!grepl('[0-9]{8}_(CEQ|CEQ_|CEQ-|)[0-9]{8}\\.txt',flist)]
flist_num = str_extract(flist,'^[0-9]{8}')
flist_year = str_extract(flist,'^[0-9]{4}')
flist = flist[flist_year>=2012]

text_locs = 'scratch/eis_documents_plaintext/'

#text_locs  = 'scratch/leverage_points/all_eis_text_files/'
#text_locs2 = 'input/eis_intro_text/'
#tlist = list.files(text_locs,"*.txt$")
tlist = flist
#ca_df$`EIS Number` %in% str_extract(tlist,'^[0-9]{8}')
#tlist2 = list.files('input/eis_intro_text',"*.txt$")
#tlist= c(tlist,tlist2)
es_df = readtext::readtext(paste0(text_locs,tlist),ignore_missing_files=T)

library(data.table)
es_df$EIS_Number = str_extract(es_df$doc_id,'^[0-9]{8}')
es_df$FR_Year = str_extract(es_df$doc_id,'^[0-9]{4}')

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

data.table::fwrite(doc_sentences,'scratch/leverage_points/all_eis_water_energy_tokenized_sentences.csv')
