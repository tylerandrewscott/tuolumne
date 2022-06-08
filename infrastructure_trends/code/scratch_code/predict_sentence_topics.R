library(tensorflow)
library(keras)
library(readtext)
library(data.table)
library(tidyverse)
library(lubridate)
tokenizer = keras::load_text_tokenizer('model_objects/leverage_points/tokenizer')
class_model = keras::load_model_hdf5('model_objects/leverage_points/topic_classifier_fixed_2topic.h5')


ca_url = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSW1BL6LAW6zGiL2Ms-QC4n_H7ASJfr-pBxjvCnGB8rtaP4x08MQnmK5RHoRoYPO3syl6LubHbxCOeG/pub?output=csv'
ca_df = read_csv(ca_url)
text_dt = data.table::fread('scratch/leverage_points/ca_set_tokenized_sentences.csv')
text_dt$Standard_Nqme = gsub('\\.txt$','',text_dt$file)

eis_dt = fread('input/epa_master_repository/eis_record_detail.csv')
doc_dt = fread('input/epa_master_repository/eis_document_record.csv')

text_dt$EIS_Number = ca_df$`EIS Number`[match(text_dt$Standard_Nqme,ca_df$`Standard Name`)]


unique(text_dt$Standard_Nqme[!text_dt$Standard_Nqme %in% ca_df$`Standard Name`])


eis_dt = eis_dt[eis_dt$EIS.Number %in% ca_df$`EIS Number`,]
doc_dt = doc_dt[doc_dt$EIS.Number %in% ca_df$`EIS Number`,]
doc_dt$File_Name = gsub('pdf$','txt',doc_dt$File_Name)


text_sequences = keras::texts_to_sequences(tokenizer,texts = text_dt$text)
text_sequences = keras::pad_sequences(sequences = text_sequences,maxlen = class_model$input_shape[[2]])

pred_by_sentence = class_model$predict(text_sequences)
pred_dt = data.table(text_dt,pred_by_sentence)
pred_dt = pred_dt %>% rename(P_Water = V1,P_Climate = V2)

unique(gsub('\\.txt','',pred_dt$file)[gsub('\\.txt','',pred_dt$file) %in% ca_df$`Standard Name`])

data.table::fwrite(pred_dt,file = 'scratch/leverage_points/classified_ca_sentences_2topic.csv')


eis_dt$CA = grepl('CA',eis_dt$State.or.Territory) + 0
eis_dt$FR_Year = year(mdy(eis_dt$Federal.Register.Date))
eis_dt = eis_dt[eis_dt$FR_Year>=2008 & eis_dt$FR_Year<2019,]
doc_dt = doc_dt[doc_dt$EIS.Number %in% eis_dt$EIS.Number,]

text_dt = text_dt[text_dt$file %in% doc_dt$File_Name,]
