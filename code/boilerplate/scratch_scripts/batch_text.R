
library(data.table)
#setwd('Box/tuolumne/')

docgroups = fread('scratch/boilerplate/document_candidates.csv')
docgroups$FILE_NAME = gsub('\\s','_',docgroups$FILE_NAME)
docgroups = docgroups[!duplicated(docgroups)]


doe_docs = docgroups[AGENCY=='Department of Energy',]
have_pdf = doe_docs[file.exists(paste(doe_docs$FILE_LOC,doe_docs$FILE_NAME,sep = '/')),]


nms = colnames(fread(paste(have_pdf[1]$FILE_LOC,have_pdf[1]$FILE_NAME,sep = '/')))
big_doe_text = data.table(numeric(),character(),character())
colnames(big_doe_text) <- c(nms,'File')
dir.create('scratch/boilerplate/big_text_files')
fname = 'scratch/boilerplate/big_text_files/big_doe_text.txt'
if(!file.exists(fname)){fwrite(x = big_doe_text ,file = fname,sep = '\t')}
if(file.exists(fname)){already = fread(fname)}


still_need = have_pdf[!have_pdf$FILE_NAME %in% already$File]

tiles = dplyr::ntile(1:nrow(still_need),floor(nrow(still_need)/25))
uq_tiles = unique(tiles)
tfiles = paste(still_need$FILE_LOC,still_need$FILE_NAME,sep = '/')


for(u in uq_tiles){
  print(u)
  temp_fname_list = tfiles[uq_tiles==u]
  combined_files <- rbindlist(lapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
  fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
}

wholefile = fread(fname)
wholefile = wholefile[!duplicated(wholefile)]
fwrite(wholefile[,.(text)],'scratch/boilerplate/big_text_files/big_doe_text_only.txt')
fwrite(wholefile[,.(File,Page)],'scratch/boilerplate/big_text_files/big_doe_metadata.txt')


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
if(file.exists(fname)){already = fread(fname)}

still_need = have_pdf[!have_pdf$FILE_NAME %in% already$File]

tiles = dplyr::ntile(1:nrow(still_need),floor(nrow(still_need)/25))
uq_tiles = unique(tiles)
tfiles = paste(still_need$FILE_LOC,still_need$FILE_NAME,sep = '/')


tt = list.files('../eis_documents/enepa_repository/text_as_datatable/2014/',recursive = T)
usfs_docs[grep('20140252',PROJECT_ID),]
already[grepl('20140252',File),]
still_need[grepl('20140252',FILE_NAME),]
usfs_docs[grep('20140252',PROJECT_ID),]

for(u in uq_tiles){
  print(u)
  temp_fname_list = tfiles[uq_tiles==u]
  combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
  fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
}

wholefile = fread(fname)
wholefile = wholefile[!duplicated(wholefile)]
fwrite(wholefile[,.(text)],'scratch/boilerplate/big_text_files/big_usfs_text_only.txt')
fwrite(wholefile[,.(File,Page)],'scratch/boilerplate/big_text_files/big_usfs_metadata.txt')


blm_docs = docgroups[AGENCY=='Bureau of Land Management',]
test_name = paste(blm_docs$FILE_LOC,blm_docs$FILE_NAME,sep = '/')
exists = file.exists(test_name)
have_pdf = blm_docs[exists,]
nohave = blm_docs[!exists,]

nms = colnames(fread(paste(have_pdf[1]$FILE_LOC,have_pdf[1]$FILE_NAME,sep = '/')))
big_blm_text = data.table(numeric(),character(),character())
colnames(big_blm_text) <- c(nms,'File')
dir.create('scratch/boilerplate/big_text_files')
fname = 'scratch/boilerplate/big_text_files/big_blm_text.txt'
if(!file.exists(fname)){fwrite(x = big_blm_text ,file = fname,sep = '\t')}
if(file.exists(fname)){already = fread(fname)}

still_need = have_pdf[!have_pdf$FILE_NAME %in% already$File]

tiles = dplyr::ntile(1:nrow(still_need),floor(nrow(still_need)/25))
uq_tiles = unique(tiles)
tfiles = paste(still_need$FILE_LOC,still_need$FILE_NAME,sep = '/')

for(u in uq_tiles){
  print(u)
  temp_fname_list = tfiles[uq_tiles==u]
  combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
  fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
}


wholefile = fread(fname)
wholefile = wholefile[!duplicated(wholefile)]
fwrite(wholefile[,.(text)],'scratch/boilerplate/big_text_files/big_blm_text_only.txt')
fwrite(wholefile[,.(File,Page)],'scratch/boilerplate/big_text_files/big_blm_metadata.txt')


eis_docs = docgroups[!AGENCY%in%c('Forest Service','Department of Energy','Bureau of Land Management'),]
eis_docs[!file.exists(paste('../eis_documents/',eis_docs$FILE_LOC,eis_docs$FILE_NAME,sep = '/')),]

have_pdf = eis_docs[file.exists(paste(eis_docs$FILE_LOC,eis_docs$FILE_NAME,sep = '/')),]
nms = colnames(fread(paste(have_pdf[1]$FILE_LOC,have_pdf[1]$FILE_NAME,sep = '/')))

big_eis_text = data.table(numeric(),character(),character())
colnames(big_eis_text) <- c(nms,'File')
dir.create('scratch/boilerplate/big_text_files')
fname = 'scratch/boilerplate/big_text_files/big_eis_text.txt'
if(!file.exists(fname)){fwrite(x = big_eis_text ,file = fname,sep = '\t')}
if(file.exists(fname)){already = fread(fname)}

still_need = have_pdf[!have_pdf$FILE_NAME %in% already$File]

tiles = dplyr::ntile(1:nrow(still_need),floor(nrow(still_need)/25))
uq_tiles = unique(tiles)
tfiles = paste(still_need$FILE_LOC,still_need$FILE_NAME,sep = '/')


for(u in uq_tiles){
  print(u)
  temp_fname_list = tfiles[uq_tiles==u]
  combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
  fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
}

wholefile = fread(fname)
wholefile = wholefile[!duplicated(wholefile)]
fwrite(wholefile[,.(text)],'scratch/boilerplate/big_text_files/big_eis_text_only.txt')
fwrite(wholefile[,.(File,Page)],'scratch/boilerplate/big_text_files/big_eis_metadata.txt')



# 
# vec1 = paste(blm_docs$FILE_LOC,blm_docs$FILE_NAME,sep = '/')
# test = file.exists(vec1)
# library(stringr)
# library(pbapply)
# alt_loc = pbsapply(seq_along(blm_docs$PROJECT_ID),function(x) str_replace(blm_docs$FILE_NAME[x],paste0('^',blm_docs$PROJECT_ID[x],'--'),paste0(blm_docs$PROJECT_ID[x],'_')))
# vec2 = paste(blm_docs$FILE_LOC,alt_loc,sep = '/')
# test2 =  file.exists(vec2)
# for(i in 1:nrow(blm_docs)){
#   if(!test[i]&test2[i])
#   {
#     print(i)
#     file.rename(from = vec2[i],to = vec1[i])
#   }
# 
#   blm_docs[!test,]
# 
#   
#   table(!test&test2)
# 
# }
# if(!test & test2){
#   rename()
# }
# blmset = list.files('../eis_documents/agency_nepa_libraries/blm/text_as_datatable/2019',recursive = T,full.names = T)

# 
# 
# usfs_docs = docgroups[AGENCY=='Forest Service',]
# usfs_docs[!file.exists(paste('../eis_documents/',usfs_docs$FILE_LOC,usfs_docs$FILE_NAME,sep = '/')),]
# # table(docgroups$AGENCY,docgroups$PROJECT_TYPE)
# # 
# have = usfs_docs[file.exists(paste(usfs_docs$FILE_LOC,usfs_docs$FILE_NAME,sep = '/')),]
# 
# vec1 = paste(usfs_docs$FILE_LOC,usfs_docs$FILE_NAME,sep = '/')
# test = file.exists(vec1)

# library(stringr)
# library(pbapply)
# alt_loc = pbsapply(seq_along(usfs_docs$PROJECT_ID),function(x) str_replace(usfs_docs$FILE_NAME[x],paste0('^',usfs_docs$PROJECT_ID[x],'--'),paste0(usfs_docs$PROJECT_ID[x],'_')))
# vec2 = paste(usfs_docs$FILE_LOC,alt_loc,sep = '/')




