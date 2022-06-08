#setwd('../manitou/')
require(boxr)
require(data.table)
require(utf8)
library(pbapply)
require(stringr)
library(doParallel)
stringcombos = c('List of Preparers','Title and Contribution','Preparers and Contributors',
                 'Contributors to the Environmental Impact Statement', 
                 'Project Delivery Team','Report Contributors','List of Reviewers and Preparers',
                 'Persons or Entities Involved','Document Preparers','Interdisciplinary Team','EIS Preparers',
                 'List of Report Preparers','Project Review Team','EIS Preparation','Interdisciplinary Team Members',
                 'Consultation and Coordination','Preparers and Persons Consulted') 
stc = paste(c(stringcombos,toupper(stringcombos)),collapse='|')
stc = paste0(stc,'|PREPARERS|CONSULTANT TEAM')

flist = list.files('../eis_documents/enepa_repository/text_as_datatable/',full.names = T,recursive = T)
flist_ids = str_remove(basename(flist),'(_|--).*')
projs = fread('scratch/boilerplate/project_candidates_eis_only.csv')

prep_file = 'scratch/boilerplate/preparer_pages.rds'
if(file.exists(prep_file)){prep_dt = readRDS(prep_file)}else{prep_dt = data.table()}

projs = projs[!PROJECT_ID %in% prep_dt$PROJECT_ID,]

flist = flist[flist_ids %in% projs$PROJECT_ID]
flist = flist[!grepl('^[0-9]{8}_[0-9]{8}.txt',basename(flist))]


#projs[,.N,by=.(AGENCY)][order(-N)]
#projs = projs[AGENCY %in% c('Forest Service')&PROJECT_TYPE=='EIS',]

projs$AGENCY <- as.factor(projs$AGENCY)
#projs <- projs[AGENCY %in% c('Forest Service','Bureau of Land Management'),]
#projs = projs[AGENCY%in% c('Forest Service','Bureau of Land Management'),]
proj_agencies = split(projs,by = 'AGENCY')

list_of_preparers = lapply(proj_agencies,function(f) {
  print(f)
  rbindlist(pblapply(f$PROJECT_ID,function(g){
  temp_flist = grep(g,flist,value = T)

  if(!identical(character(0),temp_flist)){
  temp_dt = rbindlist(lapply(temp_flist,function(j) {
    temp = tryCatch(fread(j),error = function(e) NULL)
    temp$File = j
    temp}))
  temp_prep = temp_dt[grepl(stc,text)&!grepl('Table of Contents|TABLE OF CONTENTS|DOCUMENT ORGANIZATION|\\.{8,}|PERSONS CONTACTED|Persons Contacted|Other Organizations|OTHER ORGANIZATIONS|COMMENTED|PARTICIPATED|Commented|Participated',text),]
  temp_prep}},cl = 6))
})


prep_new_dt = rbindlist(list_of_preparers)
prep_new_dt$PROJECT_ID <- str_remove(basename(prep_new_dt$File),'(--|_).*')

prep_dt = rbind(prep_dt,prep_new_dt,use.names = T)
saveRDS(prep_dt,'scratch/boilerplate/preparer_pages.rds')

#projs = projs[AGENCY == 'Bureau of Indian Affairs',]

#bia_ids = projs$PROJECT_ID[projs$AGENCY=='Bureau of Indian Affairs']
#bia_flist = grep(paste(bia_ids,collapse='|'),flist,value = T)






