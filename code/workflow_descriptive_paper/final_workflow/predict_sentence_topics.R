
#setwd('Box/tuolumne/')

#require(reticulate)

require(tensorflow)
require(keras)
tokenizer = load_text_tokenizer('model_objects/tokenizer')


library(readtext)
library(data.table)
library(tidyverse)
library(lubridate)
class_model = load_model_hdf5('model_objects/topic_classifier_fixed_9topic.h5')

text_dt = readRDS('scratch/descriptive_eis/exec_summary_sentences.RDS')

text_sequences = keras::texts_to_sequences(tokenizer,texts = text_dt$text)
text_sequences = keras::pad_sequences(sequences = text_sequences,maxlen = class_model$input_shape[[2]])

pred_by_sentence = class_model$predict(text_sequences)
pred_dt = data.table(text_dt,pred_by_sentence)

trainers = fread('scratch/descriptive_eis/auto_trainers.csv')
names(pred_dt)[grep('V[1-9]',names(pred_dt))] <- gsub('X_','P_',grep('^X_',names(trainers),value = T))
saveRDS(pred_dt,'scratch/descriptive_eis/predicted_es_sentence_topics.RDS')

