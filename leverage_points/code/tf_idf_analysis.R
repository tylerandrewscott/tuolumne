

library(tidyverse)
library(stringr)
library(lubridate)
library(ggthemes)
library(data.table)
library(pbapply)
library(tidytext)
#eis$In_Subsample = (eis$EIS.Number %in% ca_ref$`EIS Number`)+0
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
eis = eis[eis$FR_Year>=2012,]
eis = eis[eis$water_energy==1,]
eis = eis[eis$In_Subsample==0,]

library(readtext)
flist = list.files('../../../../net/tmp/tscott1/tuolumne_scratch/scratch/leverage_points/ca_text_files/')
flist_EIS = ca_ref$`EIS Number`[match(flist,paste0(ca_ref$`Standard Name`,'.txt'))]
text_locs = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/leverage_points/ca_text_files/'
tlist = flist
es_df = readtext::readtext(paste0(text_locs,tlist),ignore_missing_files=T)
es_df$EIS_Number = ca_ref$`EIS Number`[match(es_df$doc_id,paste0(ca_ref$`Standard Name`,'.txt'))]

epa_locs = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/eis_documents_plaintext/'
epa_flist = list.files('../../../../net/tmp/tscott1/tuolumne_scratch/scratch/eis_documents_plaintext/')
epa_flist = epa_flist[str_extract(epa_flist,'^[0-9]{8}') %in% eis$EIS.Number]
epa_flist = epa_flist[!grepl('[0-9]{8}_(CEQ|CEQ_|CEQ-|)[0-9]{8}\\.txt',epa_flist)]
epa_flist_num = str_extract(epa_flist,'^[0-9]{8}')
epa_flist_year = str_extract(epa_flist,'^[0-9]{4}')
epa_tlist = epa_flist

es_df2 = readtext::readtext(paste0(epa_locs,epa_tlist),ignore_missing_files=T)
es_df2$EIS_Number = str_extract(es_df2$doc_id,'^[0-9]{8}')
es_df2 = es_df2 %>% ungroup() %>% group_by(EIS_Number) %>% summarise(text = paste(text,collapse=' '))
all_es_df = rbind(es_df[,c(3,2)],es_df2)
#all_es_df = all_es_df[sample(1:nrow(all_es_df),20),]
gc()
#text_locs  = 'scratch/leverage_points/all_eis_text_files/'
#text_locs2 = 'input/eis_intro_text/'
#tlist = list.files(text_locs,"*.txt$")
#ca_df$`EIS Number` %in% str_extract(tlist,'^[0-9]{8}')
#tlist2 = list.files('input/eis_intro_text',"*.txt$")
#tlist= c(tlist,tlist2)

eis_ngrams <-all_es_df %>%
  unnest_tokens(word, text,token = "ngrams", n = c(2)) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word) %>%
  count(EIS_Number, word, sort = TRUE) %>%
  ungroup()

eis_words<- all_es_df %>%
  unnest_tokens(word, text,token = "words") %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word) %>%
  count(EIS_Number, word, sort = TRUE) %>%
  ungroup()

eis_ngrams_tfidf <- eis_ngrams %>%
  bind_tf_idf(word, EIS_Number, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))
eis_words_tfidf <- eis_words %>%
  bind_tf_idf(word, EIS_Number, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

eis_terms_tfidf = rbind(eis_ngrams_tfidf,eis_words_tfidf)


key_terms = c('drought','climate change','resilience','climate risk','renewable','environmental justice','economic development','extreme weather')

focal_terms_tfif = eis_terms_tfidf[eis_terms_tfidf$word %in% key_terms,]


library(lubridate)
library(ggthemes)

focal_terms_tfif$CA = (focal_terms_tfif$EIS_Number %in% ca_ref$`EIS Number`)+0
focal_terms_tfif$Date <- NA
focal_terms_tfif$Date[focal_terms_tfif$CA==1] <- decimal_date(mdy(ca_ref$`Federal Register Date`[match(focal_terms_tfif$EIS_Number[focal_terms_tfif$CA==1] ,ca_ref$`EIS Number`)]))
focal_terms_tfif$Date[focal_terms_tfif$CA==0] <- decimal_date(eis$Federal.Register.Date[match(str_extract(focal_terms_tfif$EIS_Number[focal_terms_tfif$CA==0],'^[0-9]{8}'),eis$EIS.Number)])

saveRDS(focal_terms_tfif,'scratch/leverage_points/focal_tfidf.RDS')

