if(!require(readtext)){install.packages('readtext');require(readtext)}
if(!require(tokenizers)){install.packages('tokenizers');require(tokenizers)}
if(!require(pbapply)){install.packages('pbapply');require(pbapply)}
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(doParallel)){install.packages('doParallel');require(doParallel)}
if(!require(stringr)){install.packages('stringr');require(stringr)}
if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(pdftools)){install.packages('pdftools');require(pdftools)}
#setwd('bucket_mount/tuolumne/')

project = 'scott-davis-remote'
zone = 'us-west1-a'
account_key = 'scott-davis-remote-56e7dae929b7.json'
Sys.setenv(GCE_AUTH_FILE = account_key,
           GCE_DEFAULT_PROJECT_ID = project,
           GCE_DEFAULT_ZONE = zone)
library(googleLanguageR)

googleLanguageR::gl_auth(json_file = account_key)

text_storage = 'input/filtered_text_files/'
flist = list.files(text_storage)
projects_used = fread('scratch/boilerplate/projects_used.csv')
files_used = fread('scratch/boilerplate/documents_used.csv')
files_used$text_name = paste0(paste(files_used$PROJECT_ID,files_used$FILE_NAME,sep = '--'),'.txt')
ent_loc = 'scratch/boilerplate/project_name_org_entiities/'
overwrite = T

#table(file.exists(paste0(ent_loc,projects_used$PROJECT_ID,'.txt')))
#for(i in 1:nrow(projects_used)){
for(i in  i + -2:2){
  id = projects_used$PROJECT_ID[i]
  if(!file.exists(paste0(ent_loc,id,'.txt'))|overwrite){
  print(i)
  pfiles = files_used[PROJECT_ID == id,]
  if(nrow(pfiles)==0){next}
  if(nrow(pfiles)>0){
  temp_txt = rbindlist(lapply(1:nrow(pfiles),function(x) fread(paste0(text_storage,pfiles$text_name[x]),sep = '\t')))
  temp_txt$keep = 0
prep_papers = grepl('PREPARER\\W|Preparer\\W|Preparers\\W|Contributors|PREPARERS|CONTRIBUTORS|Team Members',temp_txt$text)&!grepl('\\.{7,}|TABLE OF CONTENTS|Table of Contents',temp_txt$text)
temp_txt$keep[prep_papers] <- 1
temp_txt = temp_txt[!duplicated(text),]
temp_txt = temp_txt[!grepl('\\.{7,}',text),]

if(all(temp_txt$keep==0)){next}
add = which(temp_txt$keep==1) + 0:2
add = add[add<=nrow(temp_txt)]
temp_txt$keep[add]<-1
temp_txt = temp_txt[keep==1,]
temp_txt = temp_txt[!grepl('\\.{8,}',text),]

if(nrow(temp_txt)==0){print('skip');next}
tx_string = paste(temp_txt$text,collapse=' ')
nlp_result <- gl_nlp(string = tx_string,nlp_type = 'analyzeEntities',type = 'PLAIN_TEXT',language = 'en')
ents = data.table(nlp_result$entities[[1]])
ents$PROJECT_ID = id
ents = ents[!is.na(type),]
if(nrow(ents)==0){next}
people = ents[mention_type ==  'PROPER' & type == 'PERSON',]
if(nrow(people)==0){people = data.table()}
if(nrow(people)>0){
people = people[grepl('\\s',people$name)&!grepl('\\/',people$name)&nchar(people$name)<35]
people = people[!grepl('[0-9]',people$name),]
people = people[,.(PROJECT_ID,name,type)]
}
orgs = ents[type == "ORGANIZATION" & mention_type == 'PROPER',]
if(nrow(orgs)==0){orgs = data.table()}
if(nrow(orgs)>0){
orgs = orgs[,.(PROJECT_ID,name,type)]
}
et = rbind(people,orgs,use.names=T)
fwrite(et,paste0(ent_loc,id,'.txt'),sep = '\t')
rm(list = c('temp_text','nlp_result','tx_string','people','orgs','et','keep','pfiles','id'))
  }}}

#testcase 
#DOI-BLM-AK-A020-2016-0032-EA

