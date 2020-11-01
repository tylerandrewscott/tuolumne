require(reticulate)

use_virtualenv('scottConda')

library(keras)
library(tensorflow)

library(readr)

sents <- read_lines('scratch/sentences_by_year/drafts2015.txt') 
lt = sapply(sents,nchar)
sents = sents[lt>10&lt<1000][1:1000]
sents <- iconv(sents, to = "UTF-8")

tokenizer <- text_tokenizer(num_words = 1e3)
tokenizer %>% fit_text_tokenizer(sents)

library(purrr)
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


model %>%
  fit_generator(
    skipgrams_generator(sents, tokenizer, skip_window, negative_samples), 
    steps_per_epoch = 1000, epochs = 5,verbose = 2
  )
os <- import("os")
os$listdir(".")




use_python("/usr/bin/python3")

reticulate::conda_list()

reticulate::conda_version()
?reticulate::conda_python()
tokenizer <- text_tokenizer(num_words = 20000)

reticulate::miniconda_path()
