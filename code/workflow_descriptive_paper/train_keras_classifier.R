library(keras)
library(readtext)
library(data.table)
library(tidyverse)
#install_keras(tensorflow = "1.5")
auto_coded_df = read.csv('input/scratch/auto_trainers.csv',encoding = 'UTF-8',stringsAsFactors = F,row.names = 1)
auto_y_matrix = auto_coded_df[,grepl('X_',colnames(auto_coded_df))] %>% as.matrix()
auto_texts = auto_coded_df$text
seed = 24
VALIDATION_SPLIT = 0.2
MAX_NUM_WORDS = 50000
EMBEDDING_DIM = 300
GLOVE_DIR = 'pretrained_wordvecs/'
MAX_SEQUENCE_LENGTH = 150
#texts = rbind(auto_texts,hand_texts)
texts = auto_texts
labels = auto_y_matrix

# split the data into a training set and a validation set
# finally, vectorize the text samples into a 2D integer tensor
tokenizer <- text_tokenizer(num_words=MAX_NUM_WORDS,lower=FALSE)
tokenizer %>% fit_text_tokenizer(texts)

# save the tokenizer in case we want to use it again
# for prediction within another R session, see:
# https://keras.rstudio.com/reference/save_text_tokenizer.html
save_text_tokenizer(tokenizer, "model_objects/tokenizer")
sequences <- texts_to_sequences(tokenizer, texts)
word_index <- tokenizer$word_index
cat(sprintf('Found %s unique tokens.\n', length(word_index)))

data <- pad_sequences(sequences, maxlen=MAX_SEQUENCE_LENGTH)

embeddings_index <- new.env(parent = emptyenv())
lines = fread(paste0(GLOVE_DIR, 'glove.840B.300d.txt'))
word = lines$V1
sub_lines = lines[lines$V1 %in% names(word_index)]
coefs = split(sub_lines,by = 'V1',keep.by = F)
coefs = lapply(coefs,as.numeric)
names(coefs) <- sub_lines$V1
for(w in 1:length(coefs))
{
embeddings_index[[word[w]]] <- coefs[[w]]  
}

num_words <- min(MAX_NUM_WORDS, length(word_index) + 1)
prepare_embedding_matrix <- function() {
  embedding_matrix <- matrix(0L, nrow = num_words, ncol = EMBEDDING_DIM)
  for (word in names(word_index)) {
    index <- word_index[[word]]
    if (index >= MAX_NUM_WORDS)
      next
    embedding_vector <- embeddings_index[[word]]
    if (!is.null(embedding_vector)) {
      # words not found in embedding index will be all-zeros.
      embedding_matrix[index,] <- embedding_vector
    }
  }
  embedding_matrix
}

embedding_matrix <- prepare_embedding_matrix()

cat('Shape of data tensor: ', dim(data), '\n')
cat('Shape of label tensor: ', dim(labels), '\n')

# split the data into a training set and a validation set
indices <- 1:nrow(data)
indices <- sample(indices)
data <- data[indices,]
labels <- labels[indices,]

num_validation_samples <- as.integer(VALIDATION_SPLIT * nrow(data))

x_train <- data[-(1:num_validation_samples),]
y_train <- labels[-(1:num_validation_samples),]
x_val <- data[1:num_validation_samples,]
y_val <- labels[1:num_validation_samples,]

#comment_input = Input((maxlen,))
seq_input = layer_input(shape = list(MAX_SEQUENCE_LENGTH),dtype = 'int32')
# we start off with an efficient embedding layer which maps
# our vocab indices into embedding_dims dimensions
#comment_emb = Embedding(max_features, embedding_dims, input_length=maxlen, 
#                        embeddings_initializer="uniform")(comment_input)
emb_layer = layer_embedding(object = seq_input,embeddings_initializer = 'uniform',output_dim = EMBEDDING_DIM,
                              input_length = MAX_SEQUENCE_LENGTH,input_dim = min(num_words,length(word_index)+1),weights = list(embedding_matrix),
                            trainable = TRUE)
# we add a GlobalMaxPooling1D, which will extract features from the embeddings
# of all words in the comment
#h = GlobalMaxPooling1D()(comment_emb)
h = layer_global_max_pooling_1d(emb_layer)

# We project onto a six-unit output layer, and squash it with a sigmoid:
#output = Dense(6, activation='sigmoid')(h)
output =  layer_dense(object = h,units = ncol(y_val),activation = 'sigmoid')

#model = Model(inputs=comment_input, outputs=output)
model = keras_model(inputs = seq_input,outputs = output)
model %>% compile(loss = 'binary_crossentropy',optimizer = 'adam',metrics = 'acc')
early_stopping <- callback_early_stopping(monitor = 'val_loss', patience = 0,min_delta = 0.0001)
hist = model %>% fit(x_train, y_train, batch_size=1000, epochs=20, validation_data = list(x_val,y_val),callbacks = c(early_stopping))

keras::save_model_hdf5(model,filepath = 'model_objects/topic_classifier_fixed_9topic.h5',include_optimizer = TRUE)

