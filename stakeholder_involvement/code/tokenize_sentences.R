text_locs  = 'input/eis_text/'
tlist = list.files('input/eis_text',"*.txt$")
comment_locs = 'input/eis_comment_feis_text/'
doc_sentences = lapply(seq_along(tlist),function(tl){
x = readLines(paste0(text_locs,tlist[tl]))
sents = tokenizers::tokenize_sentences(x)[[1]]
regex_line_number_string = "([0-9]{1,2}\\s){3,}"
regex_table_dump = "(Alternative\\s[A-Z0-9]\\s){2,}"
regex_largenumber_dump = "[0-9]{5,}|[0-9]{3,}-[0-9]{3,}"
regex_internet_dump = "@|\\.com|cc:"
regex_funkychar_dump = "\\*|_"
sents = sents[!grepl(regex_line_number_string,sents)]
sents = sents[!grepl(regex_table_dump,sents)]
sents = sents[!grepl(regex_largenumber_dump,sents)]
sents = sents[!grepl(regex_funkychar_dump,sents)]
sents = sents[nchar(sents)>50 & nchar(sents) < 400]
#drop weird spacing
sents = sents[!grepl('\\s[a-z]\\s[a-z]\\s|\\s[A-Za-z]\\s[A-Za-z]\\s[A-Za-z]\\s',sents)]
sents = sents[!grepl('\\s{2,}',sents)]
#end in a period
sents = sents[grepl('\\.$',sents)]
sents = sents[!duplicated(sents)]
sents = sents[!grepl("\\([^\\)]+$",sents)]
sents = sents[!grepl("[A-Z]{8,}",sents)]
data.frame(text = sents,file = tlist[tl])})

all_sents_df = do.call(rbind,doc_sentences)
all_sents_df$text = as.character(all_sents_df$text)

data.table::fwrite(all_sents_df,'input/scratch/all_eis_clean_sentences.csv')



tlist_comments = list.files('input/eis_comment_feis_text',"*.txt")


doc_sentences = lapply(seq_along(tlist_comments),function(tl){
  x = readLines(paste0(comment_locs,tlist_comments[tl]))
  sents = tokenizers::tokenize_sentences(x)[[1]]
  regex_line_number_string = "([0-9]{1,2}\\s){3,}"
  regex_table_dump = "(Alternative\\s[A-Z0-9]\\s){2,}"
  regex_largenumber_dump = "[0-9]{5,}|[0-9]{3,}-[0-9]{3,}"
  regex_internet_dump = "@|\\.com|cc:"
  regex_funkychar_dump = "\\*|_"
  sents = sents[!grepl(regex_line_number_string,sents)]
  sents = sents[!grepl(regex_table_dump,sents)]
  sents = sents[!grepl(regex_largenumber_dump,sents)]
  sents = sents[!grepl(regex_funkychar_dump,sents)]
  sents = sents[nchar(sents)>50 & nchar(sents) < 400]
  #drop weird spacing
  sents = sents[!grepl('\\s[a-z]\\s[a-z]\\s|\\s[A-Za-z]\\s[A-Za-z]\\s[A-Za-z]\\s',sents)]
  sents = sents[!grepl('\\s{2,}',sents)]
  #end in a period
  sents = sents[grepl('\\.$',sents)]
  sents = sents[!duplicated(sents)]
  sents = sents[!grepl("\\([^\\)]+$",sents)]
  sents = sents[!grepl("[A-Z]{8,}",sents)]
  if(length(sents)>0){
    df = data.frame(text = sents,file = tlist_comments[tl])}
  if(length(sents)==0) {
    df = data.frame(text = NA,file = tlist_comments[tl])}
df})
all_sents_df = do.call(rbind,doc_sentences)
all_sents_df$text = as.character(all_sents_df$text)

data.table::fwrite(all_sents_df,'input/scratch/all_eis_clean_sentences_comments.csv')
