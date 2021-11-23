
packs = c('foreach','doParallel','parallel','stringr')
sapply(packs[!sapply(packs,require,character.only = T)],install.packages)
sapply(packs,require,character.only = T)


batchStopwords <- function(custom_stopwords = NULL,front_append = '\\b',back_append = '\\b',
                           max_words = 100){
  stopword_breaks = paste0(front_append,custom_stopwords,back_append)
  stopword_splits = split(stopword_breaks,dplyr::ntile(stopword_breaks,n = length(stopword_breaks)%/%max_words))
  stopword_splits <- lapply(stopword_splits,paste,collapse = '|')
  return(stopword_splits)}



sequentialStrRemoveAll <- function(text = NULL,patterns = NULL){
  t = text
  patterns = stopws
  for(p in patterns){
    t <- str_remove_all(string = t,pattern = p)[[1]]
  }
  return(t)
  }


