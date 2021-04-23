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

OVERWRITE = TRUE
project = 'scott-davis-remote'
zone = 'us-west1-a'
account_key = 'scratch/scott-davis-remote-56e7dae929b7.json'

Sys.setenv(GCE_AUTH_FILE = account_key,
           GCE_DEFAULT_PROJECT_ID = project,
           GCE_DEFAULT_ZONE = zone)
library(googleLanguageR)
googleLanguageR::gl_auth(json_file = account_key)


text_storage = 'input/filtered_text_files/'
flist = list.files(text_storage)
projects_used = fread('boilerplate_project/data_products/project_candidates_eis_only.csv')
files_used = fread('boilerplate_project/data_products/document_candidates_eis_only.csv')
tlist = list.files('../eis_documents/enepa_repository/text_as_datatable/',full.names = T, recursive = T)
#entity_file = 'scratch/boilerplate/entity_extraction_results.rds'
#if(!file.exists(entity_file)){ents =list()}else{ents = readRDS(entity_file)}
files_used$FILE_NAME <- str_replace(files_used$FILE_NAME,'\\.pdf$','\\.txt')
#files_used = files_used[!FILE_NAME %in% ents$FILE_NAME,]
keep_tlist = tlist[basename(tlist) %in% files_used$FILE_NAME]




good_string = 'PREPARERS|CONTRIBUTORS|Preparers|Contributors|Consultants|Interdisciplinary Team'#LIST OF PREPARERS|List of Preparers|List Of Preparers|PREPARERS AND CONTRIBUTORS|Preparers and Contributors|Preparers \\& Contributors|Preparers And Contributors'
bad_string = 'TABLE OF CONTENTS|Table of Contents|^CONTENTS|Contents|How to Use This Document|Date Received'

pages_list = pblapply(keep_tlist,function(i) {
  print(i) 
  #i = grep('20170165',keep_tlist,value = T)
  base_file = basename(i)
  id = str_remove(basename(base_file),'_.*')
  #start_dt = data.table(FILE_NAME = base_file,PROJECT_ID = id,people = list(),orgs = list())
  temp_txt =  fread(i,sep = '\t')
  temp_txt$FILE = base_file
  temp_txt$keep = temp_txt$keep2 = 0
  temp_txt$looks_like_frontmatter = !grepl('[A-Z0-9]',str_remove(temp_txt$text,'.*\\s'))
#  temp_txt = temp_txt[grepl('[A-Z0-9]',str_remove(temp_txt$text,'.*\\s')),]
  prep_papers = grepl(good_string,str_sub(temp_txt$text,1,200))&!grepl(bad_string,str_sub(temp_txt$text,1,200))
  temp_txt$keep[prep_papers] <- 1
  prep_papers2 = grepl(good_string,  str_sub(temp_txt$text,nchar(temp_txt$text)-200,nchar(temp_txt$text)))&!grepl(bad_string,  str_sub(temp_txt$text,1,200))
  temp_txt$keep2[prep_papers2] <- 1
  temp_txt = temp_txt[!duplicated(text),]
  temp_txt$count = str_count(temp_txt$text,good_string) - str_count(temp_txt$text,bad_string)


  
  if(any(temp_txt$count[!temp_txt$looks_like_frontmatter>0])){
    temp_txt = temp_txt[!temp_txt$looks_like_frontmatter,]
  }
  if(all(temp_txt$count<=0)&any(temp_txt$keep)){
  temp_txt = temp_txt[keep==1,]  
  }
  temp_txt[,.(Page,count,keep,keep2)]
  temp_txt = temp_txt[Page>=min(temp_txt$Page[which(temp_txt$count==max(temp_txt$count))]),]

  if(any(temp_txt$keep!=0)){
    add = sort(unique(c(sapply(which(temp_txt$keep==1), function(x) x + 0:2))))
    add = add[add<=nrow(temp_txt)]
    temp_txt$keep[add]<-1
  }
  if(any(temp_txt$keep2!=0)){
    add = sort(unique(c(sapply(which(temp_txt$keep2==1), function(x) x + 0:2))))
    add = add[add<=nrow(temp_txt)]
    temp_txt$keep2[add]<-1
  }
  if(any(temp_txt$keep>0|temp_txt$keep2>0)){
    temp_txt = temp_txt[keep==1|keep2==1,]}
  if(all(temp_txt$keep==0&temp_txt$keep2==0)){
    temp_txt = temp_txt[count>0,]
  }
  
    temp_txt = temp_txt[keep!=0|keep2!=0|temp_txt$count>0,]
  #  temp_txt = temp_txt[!grepl('\\.{8,}',text),]
    temp_txt = temp_txt[!{grepl('References Cited|Works Cited|REFERENCES|Index|INDEX',temp_txt$text) & !grepl('Preparers|Contributors|PREPARERS|CONTRIBUTORS',temp_txt$text)},]
     temp_txt
     },cl = 7)
  

pages_dt = rbindlist(pages_list,use.names = T)


#table(projects_used$EIS.Number %in% str_remove(pages_dt$FILE,'_.*'))
#projects_used$EIS.Number[!projects_used$EIS.Number %in% str_remove(pages_dt$FILE,'_.*')]


pages_dt$USE_KEEP2 = pages_dt$FILE %in% pages_dt[,list(sum(keep),sum(keep2)),by=.(FILE)][V1>10&V2>0]$FILE
pages_dt = pages_dt[{!USE_KEEP2}|keep2==1,]
#pages_dt[,.(FILE,Page,count,keep,keep2)][grepl('20130008',FILE),]
saveRDS(pages_dt,'boilerplate_project/data_products/detected_preparer_pages.RDS')

pages_by_file = split(pages_dt$Page,pages_dt$FILE)
pdf_files = list.files('../eis_documents/enepa_repository/documents/',full.names =T,recursive = T)
index = match(gsub('\\.txt$','.pdf',names(pages_by_file)),basename(pdf_files))

cluster = makeCluster(7)
registerDoParallel(cluster)
clusterEvalQ(cluster,require(data.table))
clusterEvalQ(cluster,require(pdftools))
clusterEvalQ(cluster,require(stringr))
clusterEvalQ(cluster,require(googleLanguageR))
clusterExport(cluster,varlist = list('pdf_files','account_key'))
clusterEvalQ(cluster,googleLanguageR::gl_auth(json_file = account_key))


# clusterEvalQ(cluster,{
# model = 'en_core_web_sm'
# require(spacyr)
# spacy_initialize(model)
# })

#preparer_ids = list.files('scratch/preparer_sections/',full.names = T,recursive = T)

#projects_used$EIS.Number[!projects_used$EIS.Number %in% str_remove(names(pages_by_file),'_.*')]

dir.create('scratch/entity_results/')

file_text = foreach(p =pages_by_file,f = names(pages_by_file),i = stringr::str_remove(names(pages_by_file),'_.*')) %dopar% {
  fl = basename(gsub('txt$','pdf',f))
  fname = paste0('scratch/entity_results/',fl,'.RDS')
  if(!file.exists(fname)){
  tryCatch({
    #p =pages_by_file[1];f = names(pages_by_file)[1];i = str_remove(names(pages_by_file),'_.*')[1]
#fl = basename(gsub('txt$','pdf',f))
fileLocation = grep(basename(gsub('txt$','pdf',f)),pdf_files,value = T,fixed = T)
full_text = pdftools::pdf_text(fileLocation)
tx_string = full_text[p]
print(length(tx_string))
if(length(tx_string)>0){
#tx_split_string = unlist(str_split(tx_string,'\\n'))
#tx_split_string = gsub('(^[A-Z][a-z]+)\\,(\\s[A-Z][a-z]+)','\\1\\2',tx_split_string)
nlp_result <- googleLanguageR::gl_nlp(string = tx_string,nlp_type = 'analyzeEntities',type = 'PLAIN_TEXT',language = 'en')
if(length(nlp_result$entities)==1){ent_temp = data.table(nlp_result$entities)}
if(length(nlp_result$entities)>1){ent_temp = rbindlist(nlp_result$entities,use.names = T,fill = T)}
ent_temp$FILE = f
ent_temp$PROJECT_ID = i
saveRDS(ent_temp,fname)
rm(ent_temp);rm(fname)
#nlp_result$entities
}},error = function(e) NULL)
}
}



