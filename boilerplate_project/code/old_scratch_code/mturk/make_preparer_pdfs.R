if(!require(readtext)){install.packages('readtext');require(readtext)}
if(!require(tokenizers)){install.packages('tokenizers');require(tokenizers)}
if(!require(pbapply)){install.packages('pbapply');require(pbapply)}
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(doParallel)){install.packages('doParallel');require(doParallel)}
if(!require(stringr)){install.packages('stringr');require(stringr)}
if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(pdftools)){install.packages('pdftools');require(pdftools)}
if(!require(googleLanguageR)){install.packages('googleLanguageR')}
if(!require(rvest)){install.packages('rvest')}
#setwd('bucket_mount/tuolumne/')

project = 'scott-davis-remote'
zone = 'us-west1-a'
account_key = '../../../Box/tuolumne/scratch/scott-davis-remote-56e7dae929b7.json'

Sys.setenv(GCE_AUTH_FILE = account_key,
           GCE_DEFAULT_PROJECT_ID = project,
           GCE_DEFAULT_ZONE = zone)
library(googleLanguageR)
googleLanguageR::gl_auth(json_file = account_key)


pymat = fread('scratch/boilerplate/python_spacy_matcher_person_names.csv')
text_storage = 'input/filtered_text_files/'
flist = list.files(text_storage)
projects_used = fread('scratch/boilerplate/project_candidates_eis_only.csv')
files_used = fread('scratch/boilerplate/document_candidates_eis_only.csv')
tlist = list.files('../eis_documents/enepa_repository/text_as_datatable/',full.names = T, recursive = T)
#entity_file = 'scratch/boilerplate/entity_extraction_results.rds'
#if(!file.exists(entity_file)){ents =list()}else{ents = readRDS(entity_file)}
files_used$FILE_NAME <- str_replace(files_used$FILE_NAME,'\\.pdf$','\\.txt')
#files_used = files_used[!FILE_NAME %in% ents$FILE_NAME,]
keep_tlist = tlist[basename(tlist) %in% files_used$FILE_NAME]


#20200253

library(tm)
read <- readPDF(control = list(text = "-layout"))
The control argument enables you to set up parameters as you would write them in the command line. Think of the above function as writing xpdf -layout in the shell.

Then, you’re ready to import the PDF document:
  
  document <- Corpus(URISource("./71_PV.62.pdf"), readerControl = list(reader = read))
doc <- content(document[[1]])
head(doc)

require(tm)

good_string = 'List of Preparers|LIST OF PREPARERS|List Of Preparers|Preparers and Contributors|PREPARERS AND CONTRIBUTORS'
pages_list = pblapply(keep_tlist,function(i) {

  document <- Corpus(URISource(i), readerControl = list(reader = read))
  doc = content(document[[1]])
  
  base_file = basename(i)
  id = str_remove(basename(base_file),'_.*')
  #start_dt = data.table(FILE_NAME = base_file,PROJECT_ID = id,people = list(),orgs = list())
  temp_txt =  fread(i,sep = '\t')
  temp_txt$FILE = base_file
  temp_txt$keep = temp_txt$keep2 = 0
  prep_papers = grepl(good_string,str_sub(temp_txt$text,1,100))&!grepl('TABLE OF CONTENTS|Table of Contents|^CONTENTS|How to Use This Document|Date Received',str_sub(temp_txt$text,1,100))
  temp_txt$keep[prep_papers] <- 1
  prep_papers2 = grepl(good_string,  str_sub(temp_txt$text,nchar(temp_txt$text)-100,nchar(temp_txt$text)))&!grepl('TABLE OF CONTENTS|Table of Contents|^CONTENTS|How to Use This Document|Date Received',  str_sub(temp_txt$text,1,100))
  temp_txt$keep2[prep_papers2] <- 1
  temp_txt = temp_txt[!duplicated(text),]
  
  temp_txt$count = str_count(temp_txt$text,good_string)
  
#  temp_txt = temp_txt[!grepl('\\.{7,}',text),]
  if(any(temp_txt$keep!=0)){
    add = sort(unique(c(sapply(which(temp_txt$keep==1), function(x) x + 0:3))))
    add = add[add<=nrow(temp_txt)]
    temp_txt$keep[add]<-1
  }
  if(any(temp_txt$keep2!=0)){
    add = sort(unique(c(sapply(which(temp_txt$keep2==1), function(x) x + 0:3))))
    add = add[add<=nrow(temp_txt)]
    temp_txt$keep2[add]<-1
  }
  temp_txt = temp_txt[keep==1|keep2==1,]
  temp_txt = temp_txt[count == max(count),]
  temp_txt},cl = 4)


pages_dt = rbindlist(pages_list,use.names = T, fill = T)
pages_dt$PROJECT_ID <- str_remove(pages_dt$FILE,'_.*')
goodfinds = pages_dt[,.N,by=.(FILE)][N<10,]

pdfList = list.files('../eis_documents/enepa_repository/documents/',recursive = T,full.names = T)
dir.create('scratch/preparer_sections/')

ufiles  =unique(goodfinds$FILE)
ufiles = ufiles[!file.exists(paste0('scratch/preparer_sections/PREPARER_',str_replace(basename(ufiles),'txt$','pdf')))]

require(pdftools)
pblapply(ufiles,function(u){
  pdfLoc = grep(str_replace(u,'\\.txt$','.pdf'),pdfList,value = T,fixed = T)
  if(length(pdfLoc)==1){
  pgs = pages_dt$Page[pages_dt$FILE == u]
  pdf_subset(input = pdfLoc,pages = pgs,output = paste0('scratch/preparer_sections/PREPARER_',basename(pdfLoc)))
  }
  cl = 4
})




