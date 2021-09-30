require(here)
require(data.table)
require(stringr)
flist = list.files('scratch/full_text_documents/',full.names=T)

flist = flist[!grepl('([0-9]{8})_\\1\\.txt',flist)]

eis_record = fread('../eis_documents/enepa_repository/meta_data/eis_record_detail.csv')
draft_eis = eis_record[grepl('Draft',Document),]
flist = flist[str_extract(basename(flist),'^[0-9]{8}') %in% draft_eis$EIS.Number]


flist = flist[str_extract(basename(flist),'^[0-9]{4}') %in% 2012:2020]

year_sets = split(flist,str_extract(basename(flist),'^[0-9]{4}'))
clean_text = function(x){str_remove_all(x,'[^a-zA-z0-9\\s]')}


for(i in seq_along(year_sets)){
  year = names(year_sets)[i]
  print(year)
  fname = paste0('scratch/paragraphs_by_year/drafts',year,'.txt')
  if(file.exists(fname)){next}
  year_set_dt = readtext::readtext(year_sets[[i]],ignore_missing_files = T)
  require(tokenizers)
  sentences = tokenizers::tokenize_paragraphs(year_set_dt$text,'.\n')
  sentences = sentences[sapply(sentences,length)>1]
  sentences_all=unlist(sentences)
  chars = nchar(sentences_all)
  sentences_all = sentences_all[chars>10&chars<1e5]
  sentences_all <- gsub('\\s{2,}','  ',sentences_all)
  sentences_all <- textclean::replace_non_ascii(sentences_all)
  sentences_all = tolower(sentences_all)
  sentences_all = clean_text(sentences_all)
  dir.create('scratch/paragraphs_by_year/')
  write.table(x = sentences_all,file =fname ,row.names = F,col.names = F,quote = F)
  }

