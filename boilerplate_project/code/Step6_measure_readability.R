pack = c("tidyverse","Matrix","statnet","data.table","stringr","lolog","textreuse","gridExtra","sf","lubridate","doParallel","htmlTable","ggplot2","viridis","forcats",'ggthemes','tm','pbapply','sylcount')
need = !pack %in% installed.packages()[,"Package"]
#sudo apt-get install libfontconfig1-dev
if(any(need)){sapply(pack[need],install.packages)}
sapply(pack,require,character.only=T)

projects = fread('boilerplate_project/data_products/project_candidates_eis_only.csv')


docs = fread('boilerplate_project/data_products/document_candidates_eis_only.csv')

flist_dt <- readRDS('boilerplate_project/input/feis_corpus_2013-2020.rds')
flist_dt = flist_dt[str_replace(flist_dt$File,'txt$','pdf') %in% docs$FILE_NAME,]
flist_dt$EIS.Number <- str_remove(flist_dt$File,'_.*')

source('boilerplate_project/code/functions/cleanText.R')
flist_dt <- cleanText(flist_dt)

uq_id = sort(unique(flist_dt$EIS.Number))
doc_texts = pblapply(uq_id,function(x) paste0(flist_dt$text[flist_dt$EIS.Number==x],collapse = ' '),cl = detectCores()*0.5)
names(doc_texts) <- uq_id
full_set = unlist(doc_texts)
rd = readability(full_set)
rd$EIS.Number = names(full_set)

saveRDS(object = rd,'boilerplate_project/data_products/readability_scores_by_project.rds')
