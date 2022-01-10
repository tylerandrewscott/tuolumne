splitCompounds <- function(w){
  check <- hunspell::hunspell_check(w)
  if(check){w}
  if(!check & grepl('\\s',hunspell::hunspell_suggest(w)[[1]][1])){w}
  else{hunspell::hunspell_suggest(w)[[1]][1]}}
