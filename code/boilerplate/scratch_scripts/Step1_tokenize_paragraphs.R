
if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(stringr)){install.packages('stringr');require(stringr)}
if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(doParallel)){install.packages('doParallel');require(doParallel)}
if(!require(pdftools)){install.packages('pdftools');require(pdftools)}


if(file.exists('scratch/boilerplate/project_candidates.csv')){projects  = fread('scratch/boilerplate/project_candidates.csv')}else{projects = data.table()}
if(file.exists('scratch/boilerplate/document_candidates.csv')){documents = fread('scratch/boilerplate/document_candidates.csv')}else{documents = data.table()}
#if(file.exists('scratch/boilerplate/document_index.csv')){all_page_dt = fread('scratch/boilerplate/document_index.csv')}else{all_page_dt = data.table()}
documents = documents[order(PROJECT_ID,FILE_NAME),]

# 

# is_local = dir.exists('~/Box/eis_documents/')
# if(!is_local){
#  require(boxr)
#  box_auth_service('~/.boxr-auth/token.json')
#  find_doc_repo = as.data.frame(boxr::box_search_folders('eis_documents/enepa_repository/documents/'))
#  find_doc_repo
#  docs = box_ls(find_doc_repo$id[find_doc_repo$name=='eis_documents'],max = Inf,fields = c('name','id','path'))
#  ?box_ls
# }


#library(readtext)
#library(officer)
## Read in Word data (.docx)
#storage = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/eis_documents/'
#flist = list.files(storage,recursive = T)
#flist = list.files(storage)
#epa_docs$file_loc = flist[match(epa_docs$File_Name,basename(flist))]

#mcoptions <- list(preschedule=FALSE, set.seed=T,cleanup = T)
#cl <- makeCluster(2)
#registerDoParallel(cl)

fnames = paste(documents$FILE_LOC,paste(documents$PROJECT_ID,documents$FILE_NAME,sep = '_'),sep = '/')
fnames2 = paste(documents$FILE_LOC,documents$FILE_NAME,sep = '/')
fnames3 = paste(documents$FILE_LOC,paste(documents$PROJECT_ID,documents$FILE_NAME,sep = '--'),sep = '/')

ex = file.exists(fnames)
ex2 = file.exists(fnames2)
ex3 = file.exists(fnames3)
documents$fname_locs = NA
documents$fname_locs[ex] <- fnames[ex]
documents$fname_locs[ex2] <- fnames2[ex2]
documents$fname_locs[ex3] <- fnames3[ex3]
#test = projects[AGENCY == 'Department of Energy'&PROJECT_TYPE == 'EA']

fn = paste0('input/filtered_text_files/',paste(documents$PROJECT_ID,documents$FILE_NAME,sep = '--'),'.txt')
#ex = file.exists(fn)

flt = list.files('input/filtered_text_files',full.names = T)
#table(flt %in% fn)
#file.remove(flt[!flt %in% fn])

library(textreuse)
library(doParallel)
test_names = paste0('input/filtered_text_files/',paste(documents$PROJECT_ID,documents$FILE_NAME,sep = '--'),'.txt')
cores = detectCores() / 2
have_already = file.exists(test_names)
current = list.files('input/filtered_text_files',full.names = T)


# ttfile = "../eis_documents/agency_nepa_libraries/blm/text_as_datatable/2018/DOI-BLM-CA-C060-2018-0124-EA--DR_-_C060-2018-0124-EA-BERRY_14_APD's_in_Main_Camp_Oil_Field.pdf.txt"
# tt = fread(ttfile)
# dim(tt)
# grep('DOI-BLM-CA-C060-2018-0124-EA',tfiles,value=T)

#mclapply(which(!test_names %in% flt),function(i) {
mclapply(seq_along(test_names),function(i) {
  #i = which(documents$FILE_NAME=='41909_95853_FSPLT3_1658799.pdf')
  tname = paste0('input/filter_text_files/',paste(documents$PROJECT_ID[i],documents$FILE_NAME[i],sep = '--'),'.txt')
  #if(file.exists(tname)){next}#temp = fread(tname);temp[,text:=NULL];all_page_dt = rbind(all_page_dt,temp,use.names=T,fill=T);next}
  rm(id);rm(temp_text);rm(temp_page)
  print(i)
  id = documents$PROJECT_ID[i]
  fname = documents$FILE_NAME[i]
  loc = documents$FILE_LOC[i]
  full_fname = documents$fname_locs[i]#paste(loc,paste(id,fname,sep = '_'),sep = '/')
  temp_text = tryCatch({pdftools::pdf_text(full_fname)},error = function(e) NULL)
  if(length(temp_text)>0){
    temp_page = unlist(sapply(temp_text,function(x) x))
    temp_page = gsub('\\s{1,}',' ',temp_page)
    temp = data.table::data.table(PROJECT_ID = id,FILE_NAME = fname,Paragraph = seq_along(temp_page),text = temp_page,stringsAsFactors = F)
    temp = temp[nchar(temp$text)<10000,]
    temp = temp[!grepl('\\.{10,}',temp$text),]
    temp = temp[!grepl('^The U\\.S\\. Department of Agriculture \\(USDA\\) prohibits discrimination',temp$text),]
    temp[,Project_File_Par:=paste(PROJECT_ID,FILE_NAME,Paragraph,sep = '--')]
    temp$text = textclean::replace_non_ascii(temp$text)
    temp = temp[!grepl('^Figure [0-9]',temp$text),]
    temp$text = gsub('\\s{1,}$','',temp$text)
    temp = temp[nchar(temp$text)>750,]
    temp = temp[!duplicated(text),]
    if(file.exists(tname)&nrow(temp)==0){file.remove(tname)}
    if(!file.exists(tname)&nrow(temp)>0){fwrite(x = temp,file = tname,sep = '\t')}
    #temp[,text:=NULL]
    #all_page_dt = rbind(all_page_dt,temp,use.names=T,fill=T)}
  }
}
,mc.cores = cores,mc.cleanup = T,mc.preschedule = T)



#all_page_dt = all_page_dt[!duplicated(all_page_dt),]
#fwrite(all_page_dt,'scratch/boilerplate/document_index.csv')
flist  = list.files('input/filtered_text_files/')
ids = gsub('--','',str_extract(flist,'.+--'))

projects = projects[PROJECT_ID %in% ids,]
#table(projects$AGENCY,projects$PROJECT_TYPE)
fwrite(projects,'scratch/boilerplate/projects_used.csv')
documents = documents[paste0(paste(documents$PROJECT_ID,documents$FILE_NAME,sep = '--'),'.txt') %in% flist,]
fwrite(documents,'scratch/boilerplate/documents_used.csv')

#parallel::stopCluster(cl)
#stopImplicitCluster()




