
library(data.table)
library(tidyverse)
#setwd('Box/tuolumne/')

projects = fread('boilerplate_project/data_products/project_candidates_eis_only.csv')
documents = fread('boilerplate_project/data_products/document_candidates_eis_only.csv')

flist = list.files('../eis_documents/enepa_repository/text_as_datatable/',recursive = T,full.names = T)


#flist = list.files('../../../Desktop/text_as_datatable/',recursive = T,full.names = T)
base.file = basename(flist)

eis_docs = documents
eis_docs$FILE_LOC <- dirname(flist)[match(gsub('pdf$','txt',eis_docs$FILE_NAME),base.file)]


#eis_docs[!file.exists(paste('../eis_documents/',eis_docs$FILE_LOC,eis_docs$FILE_NAME,sep = '/')),]
have_pdf = eis_docs[file.exists(paste(eis_docs$FILE_LOC,gsub('pdf$','txt',eis_docs$FILE_NAME),sep = '/')),] 

nms = colnames(fread(paste(have_pdf[1]$FILE_LOC,gsub('pdf$','txt',have_pdf$FILE_NAME[1]),sep = '/')))

big_eis_text = data.table(numeric(),character(),character())
colnames(big_eis_text) <- c(nms,'File')
dir.create('scratch/boilerplate/big_text_files')
fname = 'scratch/boilerplate/big_text_files/big_eis_text.txt'
start_over<-FALSE

if(!file.exists(fname)|start_over){fwrite(x = big_eis_text ,file = fname,sep = '\t');already = big_eis_text}
if(file.exists(fname)&!start_over){ already = fread(fname,sep ='\t');
already = already[File %in% gsub('pdf$','txt',documents$FILE_NAME),]}


still_need = have_pdf[!gsub('pdf$','txt',have_pdf$FILE_NAME) %in% already$File]

finfo = file.info(paste(still_need$FILE_LOC,gsub('pdf$','txt',still_need$FILE_NAME),sep = '/'))
still_need = still_need[finfo$size>10,]

tiles = dplyr::ntile(1:nrow(still_need),max(floor(nrow(still_need)/25),1))
uq_tiles = unique(tiles)
tfiles = paste(still_need$FILE_LOC,gsub('pdf$','txt',still_need$FILE_NAME),sep = '/')

print(length(tfiles))
if(length(tfiles)!=0){

  for(u in uq_tiles){
    print(u)
    temp_fname_list = tfiles[uq_tiles==u]
    combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x,sep = '\t',);xx$File = basename(x);xx}))
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
