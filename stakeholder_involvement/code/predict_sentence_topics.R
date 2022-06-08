library(tensorflow)
library(keras)
library(readtext)
library(data.table)
library(tidyverse)
tokenizer = keras::load_text_tokenizer('model_objects/tokenizer')
class_model = keras::load_model_hdf5('model_objects/topic_classifier_fixed.h5')

text_dt = data.table::fread('input/scratch/all_eis_clean_sentences.csv')

text_sequences = keras::texts_to_sequences(tokenizer,texts = text_dt$text)
text_sequences = keras::pad_sequences(sequences = text_sequences,maxlen = class_model$input_shape[[2]])

pred_by_sentence = class_model$predict(text_sequences)
pred_dt = data.table(pred_by_sentence)
colnames(pred_dt) <-gsub('X_','P_',colnames(fread('input/scratch/auto_trainers.csv') %>% select(contains('X_'))))
pred_dt = cbind(text_dt,pred_dt)
data.table::fwrite(pred_dt,file = 'input/scratch/classified_eis_sentences.csv')

comment_dt = data.table::fread('input/scratch/all_eis_clean_sentences_comments.csv')
comment_sequences = keras::texts_to_sequences(tokenizer,texts = comment_dt$text)
comment_sequences = keras::pad_sequences(sequences = comment_sequences,maxlen = class_model$input_shape[[2]])

pred_by_sentence = class_model$predict(comment_sequences)
pred_dt = data.table(pred_by_sentence)
colnames(pred_dt) <- gsub('X_','P_',colnames(fread('input/scratch/auto_trainers.csv') %>% select(contains('X_'))))
comment_dt = cbind(comment_dt,pred_dt)
data.table::fwrite(comment_dt,file = 'input/scratch/classified_eis_comment_sentences.csv')


