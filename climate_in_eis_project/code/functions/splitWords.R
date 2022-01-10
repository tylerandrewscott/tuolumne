splitWords <- function(text_string){
  #split doc by word
  words <- stringr::str_split(text_string,'\\s{1,}',simplify = F)[[1]]
  #check for bad spells
  bad = !hunspell::hunspell_check(words)
  #get replacement suggestions
  sugg = hunspell::hunspell_suggest(words[bad])
  #quick fix to ignore cases with 0 suggestions
  sugg[sapply(sugg,length)==0]<-'noguess'
  #id replacement suggestions that are actually 2 wordss
  sugg_space = grepl('\\s',sapply(sugg,function(y) y[[1]]))
  #replace bad spells that have first-choice suggestion = 2 words
  words[bad][sugg_space] <- sapply(sugg[sugg_space],function(y) y[[1]])
  #put document back together again
  words_combo <- paste(words,collapse = ' ')
  #return document
  return(words_combo)
}
  
