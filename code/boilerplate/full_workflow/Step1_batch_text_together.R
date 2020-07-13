
library(data.table)
library(tidyverse)
#setwd('Box/tuolumne/')
            
docgroups = fread('scratch/boilerplate/document_candidates.csv')
projects = fread('scratch/boilerplate/project_candidates.csv')
docgroups = docgroups[!duplicated(docgroups)]


#text_files = list.files('../eis_documents/enepa_repository/text_as_datatable/',full.names = T,recursive = T)

#docgroups$FILE_NAME = gsub('\\s','_',docgroups$FILE_NAME)

doe_docs = docgroups[AGENCY=='Department of Energy',]

have_pdf = doe_docs[file.exists(paste(doe_docs$FILE_LOC,doe_docs$FILE_NAME,sep = '/')),]

nms = colnames(fread(paste(have_pdf[1]$FILE_LOC,have_pdf[1]$FILE_NAME,sep = '/')))
big_doe_text = data.table(numeric(),character(),character())
colnames(big_doe_text) <- c(nms,'File')
dir.create('scratch/boilerplate/big_text_files')
fname = 'scratch/boilerplate/big_text_files/big_doe_text.txt'
if(!file.exists(fname)){fwrite(x = big_doe_text ,file = fname,sep = '\t')}
if(file.exists(fname)){already = fread(fname,sep = '\t');already = already[File %in% docgroups$FILE_NAME,]}

already = already[File %in% docgroups$FILE_NAME,]

still_need = have_pdf[!have_pdf$FILE_NAME %in% already$File]
tiles = dplyr::ntile(1:nrow(still_need),max(floor(nrow(still_need)/25),1))
uq_tiles = unique(tiles)
tfiles = paste(still_need$FILE_LOC,still_need$FILE_NAME,sep = '/')
library(pbapply)

#temp_fname_list = tfiles[uq_tiles==u]
#fls = pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx})

for(f in rev(tfiles)){
  #temp_fname_list = tfiles[uq_tiles==u]
  #fls = pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx})
  fls = fread(f);fls$File = basename(f)
  fwrite(x = fls,file = fname,append = T,verbose = F,sep = '\t')
}

wholefile = fread(fname,sep ='\t')
wholefile = wholefile[!duplicated(wholefile)]
wholefile[,PID:= str_remove(File,'(--|_).*')]
wholefile = wholefile[PID %in% projects$PROJECT_ID,]
wholefile = wholefile[!grepl('pdf.txt$',File),]
wholefile[,PID:=NULL]
fwrite(wholefile,fname,sep = '\t')
fwrite(wholefile[,.(text)],'scratch/boilerplate/big_text_files/big_doe_text_only.txt',sep = '\t')
fwrite(wholefile[,.(File,Page)],'scratch/boilerplate/big_text_files/big_doe_metadata.txt',sep = '\t')
saveRDS(wholefile,gsub('txt','rds',fname))


usfs_docs = docgroups[AGENCY=='Forest Service',]
test_name = paste(usfs_docs$FILE_LOC,usfs_docs$FILE_NAME,sep = '/')
exists = file.exists(test_name)
have_pdf = usfs_docs[exists,]
nohave = usfs_docs[!exists,]
nms = colnames(fread(paste(have_pdf[1]$FILE_LOC,have_pdf[1]$FILE_NAME,sep = '/')))
big_usfs_text = data.table(numeric(),character(),character())
colnames(big_usfs_text) <- c(nms,'File')

dir.create('scratch/boilerplate/big_text_files')
fname = 'scratch/boilerplate/big_text_files/big_usfs_text.txt'
if(!file.exists(fname)){fwrite(x = big_usfs_text ,file = fname,sep = '\t')}
if(file.exists(fname)){already = fread(fname,sep = '\t')}

already = already[File %in% docgroups$FILE_NAME,]
already$File = gsub('\\.pdf\\.txt$','.txt',already$File)

still_need = have_pdf[!have_pdf$FILE_NAME %in% already$File]
tiles = dplyr::ntile(1:nrow(still_need),max(floor(nrow(still_need)/25),1))
uq_tiles = unique(tiles)
tfiles = paste(still_need$FILE_LOC,still_need$FILE_NAME,sep = '/')


for(u in uq_tiles){
  print(u)
  temp_fname_list = tfiles[uq_tiles==u]
  combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
  fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
}

wholefile = fread(fname,sep = '\t')
wholefile = wholefile[!duplicated(wholefile)]

wholefile[,PID:= str_remove(File,'(--|_).*')]
wholefile = wholefile[PID %in% projects$PROJECT_ID,]
wholefile = wholefile[!grepl('pdf.txt$',File),]
wholefile[,PID:=NULL]
fwrite(wholefile,fname,sep = '\t')
fwrite(wholefile[,.(text)],'scratch/boilerplate/big_text_files/big_usfs_text_only.txt',sep = '\t')
fwrite(wholefile[,.(File,Page)],'scratch/boilerplate/big_text_files/big_usfs_metadata.txt',sep = '\t')
saveRDS(wholefile,gsub('txt','rds',fname))


blm_docs = docgroups[AGENCY=='Bureau of Land Management',]
test_name = paste(blm_docs$FILE_LOC,blm_docs$FILE_NAME,sep = '/')
exists = file.exists(test_name)
have_pdf = blm_docs[exists,]
nohave = blm_docs[!exists,]

nms = colnames(fread(paste(have_pdf[1]$FILE_LOC,have_pdf[1]$FILE_NAME,sep = '/')))
big_blm_text = data.table(Page = numeric(),text = character(),File = character())
colnames(big_blm_text) <- c(nms,'File')
dir.create('scratch/boilerplate/big_text_files')
fname = 'scratch/boilerplate/big_text_files/big_blm_text.txt'


if(!file.exists(fname)){fwrite(x = big_blm_text ,file = fname,sep = '\t')}
if(file.exists(fname)){already = fread(fname,sep = '\t');already = already[File %in% docgroups$FILE_NAME,]}


bia_ids = already[grepl('prepared by the Bureau of Indian Affairs',text,perl = T)&Page==1]$File

docgroups = docgroups[!PROJECT_ID %in% bia_ids,]
already = already[File %in% docgroups$FILE_NAME,]
still_need = have_pdf[!have_pdf$FILE_NAME %in% already$File]

finfo = file.info(paste(still_need$FILE_LOC,still_need$FILE_NAME,sep = '/'))
still_need = still_need[finfo$size>10,]

tiles = dplyr::ntile(1:nrow(still_need),max(floor(nrow(still_need)/100),1))
uq_tiles = unique(tiles)
tfiles = paste(still_need$FILE_LOC,still_need$FILE_NAME,sep = '/')

for(u in uq_tiles){
  print(u)
  temp_fname_list = tfiles[uq_tiles==u]
  combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
  fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
}


wholefile = fread(fname,header = T,sep = '\t')
wholefile = wholefile[!duplicated(wholefile)]

wholefile[,PID:= str_remove(File,'(--|_).*')]
wholefile = wholefile[PID %in% projects$PROJECT_ID,]
wholefile = wholefile[!grepl('pdf.txt$',File),]
wholefile[,PID:=NULL]
fwrite(wholefile,fname,sep = '\t')
fwrite(wholefile[,.(text)],'scratch/boilerplate/big_text_files/big_blm_text_only.txt',sep = '\t')
fwrite(wholefile[,.(File,Page)],'scratch/boilerplate/big_text_files/big_blm_metadata.txt',sep = '\t')
saveRDS(wholefile,gsub('txt','rds',fname))


eis_docs = docgroups[!AGENCY%in%c('Forest Service','Department of Energy','Bureau of Land Management'),]
#eis_docs[!file.exists(paste('../eis_documents/',eis_docs$FILE_LOC,eis_docs$FILE_NAME,sep = '/')),]

have_pdf = eis_docs[file.exists(paste(eis_docs$FILE_LOC,eis_docs$FILE_NAME,sep = '/')),]
nms = colnames(fread(paste(have_pdf[1]$FILE_LOC,have_pdf[1]$FILE_NAME,sep = '/')))

big_eis_text = data.table(numeric(),character(),character())
colnames(big_eis_text) <- c(nms,'File')
dir.create('scratch/boilerplate/big_text_files')
fname = 'scratch/boilerplate/big_text_files/big_eis_text.txt'
if(!file.exists(fname)){fwrite(x = big_eis_text ,file = fname,sep = '\t')}
if(file.exists(fname)){already = fread(fname,sep ='\t');already = already[File %in% docgroups$FILE_NAME,]}
already = already[File %in% docgroups$FILE_NAME,]
still_need = have_pdf[!have_pdf$FILE_NAME %in% already$File]

finfo = file.info(paste(still_need$FILE_LOC,still_need$FILE_NAME,sep = '/'))
still_need = still_need[finfo$size>10,]

tiles = dplyr::ntile(1:nrow(still_need),max(floor(nrow(still_need)/25),1))
uq_tiles = unique(tiles)
tfiles = paste(still_need$FILE_LOC,still_need$FILE_NAME,sep = '/')
if(length(tfiles)!=0){
for(u in uq_tiles){
  print(u)
  temp_fname_list = tfiles[uq_tiles==u]
  combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
  fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
}
}

wholefile = fread(fname,sep = '\t')
wholefile = wholefile[!duplicated(wholefile)]

wholefile[,PID:= str_remove(File,'(--|_).*')]
wholefile = wholefile[PID %in% projects$PROJECT_ID,]
wholefile = wholefile[!grepl('pdf.txt$',File),]
wholefile[,PID:=NULL]
fwrite(wholefile,fname,sep = '\t')
fwrite(wholefile[,.(text)],'scratch/boilerplate/big_text_files/big_eis_text_only.txt',sep = '\t')
fwrite(wholefile[,.(File,Page)],'scratch/boilerplate/big_text_files/big_eis_metadata.txt',sep = '\t')
saveRDS(wholefile,gsub('txt','rds',fname))

# 


fwrite(docgroups ,file = 'scratch/boilerplate/document_candidates.csv')

projects = projects[!PROJECT_ID %in% bia_ids]
fwrite(projects ,file = 'scratch/boilerplate/project_candidates.csv')


