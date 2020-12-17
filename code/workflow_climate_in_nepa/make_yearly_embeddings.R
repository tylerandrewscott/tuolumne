reticulate::py_discover_config()

Sys.setenv(RETICULATE_PYTHON="/usr/bin/python3")
library(reticulate)
library(purrr)
library(text2vec) 
library(dplyr)
library(Rtsne)
library(ggplot2)
library(plotly)
library(stringr)
library(tensorflow)
library(keras)

reticulate::use_python("/usr/bin/python3")
reticulate::
tf$constant("Hellow Tensorflow")


keras::use_condaenv()
mnist <- dataset_mnist()
mnist$train$x <- mnist$train$x/255
mnist$test$x <- mnist$test$x/255



flist = list.files('scratch/full_text_documents/',full.names = T,recursive = T,pattern = 'txt$')
flist <- sample(flist,1000,replace = F)
require(readtext)
tx_files = readtext(file = flist[104])

readLines(flist[104])

readtext(flist[104],text_field = 1)
any(flist=='')
text_list = pbsapply(flist[1:100],extract_text,cl = 8)
names(text_list) <- basename(names(text_list))
text_collapse = pblapply(text_list,paste,collapse=' ')

all_text = unlist(text_collapse)
all_text = substr(all_text,start = 0,stop = 1e3)
all_text = as.vector(all_text)






tokenizer <- text_tokenizer(num_words = 20000)
tokenizer %>% fit_text_tokenizer(elected_no_retweets$text)


skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  function() {
    skip <- generator_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    list(x, y)
  }
}


# Number of Dimensions in the embedding vector.
embedding_size <- 128  
# Size of context window
skip_window <- 5       
# Number of negative examples to sample for each word.
num_sampled <- 1       
input_target <- layer_input(shape = 1)
input_context <- layer_input(shape = 1)


embedding <- layer_embedding(
  input_dim = tokenizer$num_words + 1, 
  output_dim = embedding_size, 
  input_length = 1, 
  name = "embedding"
)

target_vector <- input_target %>% 
  embedding() %>% 
  layer_flatten()

context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()

dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)
output <- layer_dense(dot_product, units = 1, activation = "sigmoid")

model <- keras_model(list(input_target, input_context), output)
model %>% compile(loss = "binary_crossentropy", optimizer = "adam")
summary(model)

model %>%
  fit_generator(
    skipgrams_generator(elected_no_retweets$text, tokenizer, skip_window, negative_samples), 
    steps_per_epoch = 100, epochs = 2
  )















install.packages('plotly')


flist = list.files('../eis_documents/enepa_repository/text_as_datatable/2018/',full.names = T,recursive = T)
project_record = fread('../eis_documents/enepa_repository/meta_data/eis_record_detail.csv')
project_record = project_record[grepl('Draft',project_record$Document.Type),]

flist = flist[str_extract(basename(flist),'^[0-9]+') %in% project_record$EIS.Number]



tokenizer <- text_tokenizer(num_words = 4000)
tokenizer %>% fit_text_tokenizer(all_text)

#from https://blogs.rstudio.com/ai/posts/2017-12-22-word-embeddings-with-keras/
skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  function() {
    skip <- generator_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    list(x, y)
  }
}


embedding_size <- 50  # Dimension of the embedding vector.
skip_window <- 5       # How many words to consider left and right.
num_sampled <- 1       # Number of negative examples to sample for each word.

input_target <- layer_input(shape = 1)
input_context <- layer_input(shape = 1)

embedding <- layer_embedding(
  input_dim = tokenizer$num_words + 1, 
  output_dim = embedding_size, 
  input_length = 1, 
  name = "embedding"
)

target_vector <- input_target %>% 
  embedding() %>% 
  layer_flatten()

context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()

dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)
output <- layer_dense(dot_product, units = 1, activation = "sigmoid")

model <- keras_model(list(input_target, input_context), output)
model %>% compile(loss = "binary_crossentropy", optimizer = "adam")

summary(model)

length(all_text)
model %>%
  fit_generator(verbose = T,
    skipgrams_generator(as.vector(all_text), tokenizer, skip_window, negative_samples), 
    steps_per_epoch = 100, epochs = 5
  )


library(dplyr)

embedding_matrix <- get_weights(model)[[1]]

words <- data.table(
  word = names(tokenizer$word_index), 
  id = as.integer(unlist(tokenizer$word_index))
)

words <- words %>%
  filter(id <= tokenizer$num_words) %>%
  arrange(id)

row.names(embedding_matrix) <- c("UNK", words$word)




