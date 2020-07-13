library(tensorflow)
library(keras)
library(readtext)
library(data.table)
library(tidyverse)
library(lubridate)

library(tidyverse)
library(stringr)
library(lubridate)
library(data.table)
library(pbapply)
eis = fread('input/epa_master_repository/eis_record_detail.csv')
doc = fread('input/epa_master_repository/eis_document_record.csv')
eis$CA = grepl('CA',eis$State.or.Territory)+0
ca_ref = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vSW1BL6LAW6zGiL2Ms-QC4n_H7ASJfr-pBxjvCnGB8rtaP4x08MQnmK5RHoRoYPO3syl6LubHbxCOeG/pub?output=csv')
eis$In_Subsample = (eis$EIS.Number %in% ca_ref$`EIS Number`)+0
eis$Federal.Register.Date = mdy(eis$Federal.Register.Date)
eis$FR_Year = year(eis$Federal.Register.Date)
set = c('solar','wind','water','hydroelectric','hydro','riparian','plants','floodway','levee','river bank','watershed',
        'flood','flood control','geothermal','pipeline','gas','oil','drilling','energy','storage','stormwater','aquifer',
        'wastewater','desalination',
        'transmission','reservoir','dam','canal','wetlands','renwable','transfer','water resource')
eis$water_energy = grepl(paste(paste0('\\b',set,'\\b'),collapse='|'),tolower(eis$Title)) + 0
eis = eis[eis$FR_Year>=2010,]
eis = eis[eis$water_energy==1,]

tokenizer = keras::load_text_tokenizer('model_objects/leverage_points/tokenizer')
class_model = keras::load_model_hdf5('model_objects/leverage_points/topic_classifier_fixed_waterclimatedrought.h5')

focal_dt = data.table::fread('scratch/leverage_points/ca_focal_eis_water_energy_tokenized_sentences.csv')
focal_dt$CA_Sample = 1
focal_dt$EIS_Number <- ca_ref$`EIS Number`[match(focal_dt$file,paste0(ca_ref$`Standard Name`,'.txt'))]
focal_dt = focal_dt %>% group_by(EIS_Number) %>% mutate(s = 1) %>% mutate(S = cumsum(s)) %>% dplyr::select(-s)%>% ungroup()
focal_dt$SID = paste(focal_dt$EIS_Number,focal_dt$S,sep='_')
focal_dt = focal_dt %>% ungroup()
other_dt = data.table::fread('scratch/leverage_points/all_eis_water_energy_tokenized_sentences.csv')
other_dt$CA_Sample = 0
other_dt = other_dt %>% group_by(EIS_Number) %>% mutate(s = 1) %>% mutate(S = cumsum(s)) %>% dplyr::select(-s)%>% ungroup()
other_dt$SID = paste(other_dt$EIS_Number,other_dt$S,sep='_')
other_dt = other_dt[!other_dt$EIS_Number %in% focal_dt$EIS_Number,]
all_dt = rbind(other_dt,focal_dt)

all_dt = all_dt[!grepl('\\.{4,}',all_dt$text),]
all_dt = all_dt[!grepl('^[0-9]',all_dt$text),]

# table(str_extract(unique(other_dt$EIS_Number),'^[0-9]{4}'))
# sum(table(str_extract(unique(focal_dt$EIS_Number),'^[0-9]{4}')))
# table(str_extract(unique(all_dt$EIS_Number),'^[0-9]{4}'))
# unique(focal_dt$EIS_Number)[str_extract(unique(focal_dt$EIS_Number),'^[0-9]{4}') == '2017']

text_sequences = keras::texts_to_sequences(tokenizer,texts = all_dt$text)
text_sequences = keras::pad_sequences(sequences = text_sequences,maxlen = class_model$input_shape[[2]])
pred_by_sentence = class_model$predict(text_sequences)

pred_dt = data.table(all_dt ,pred_by_sentence)
pred_dt = pred_dt %>% rename(P_Water = V1,P_Drought = V2, P_Climate = V3)
pred_dt$EIS_Number = doc$EIS.Number[match(pred_dt$file,gsub('pdf$','txt',doc$File_Name))]
pred_dt$EIS_Number[pred_dt$CA_Sample==1] <- ca_ref$`EIS Number`[match(pred_dt$file[pred_dt$CA_Sample==1],paste0(ca_ref$`Standard Name`,'.txt'))]

pred_dt = pred_dt[pred_dt$CA_Sample==1 | {pred_dt$CA_Sample==0 & !pred_dt$EIS_Number %in% ca_ref$`EIS Number`},]
pred_dt$California = eis$CA[match(pred_dt$EIS_Number,eis$EIS.Number)]
pred_dt$California[pred_dt$CA_Sample==1] <- 1
pred_dt$FR_Date = ymd(eis$Federal.Register.Date[match(pred_dt$EIS_Number,eis$EIS.Number)])
pred_dt$FR_Date[is.na(pred_dt$FR_Date)] <- mdy(ca_ref$`Federal Register Date`[match(pred_dt$EIS_Number[is.na(pred_dt$FR_Date)],ca_ref$`EIS Number`)])

data.table::fwrite(pred_dt,file = 'scratch/leverage_points/classified_ca_sentences_waterclimatedrought.csv')


