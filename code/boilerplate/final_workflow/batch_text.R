
library(data.table)
#setwd('Box/tuolumne/')
docgroups = fread('scratch/boilerplate/document_candidates.csv')
docgroups$FILE_NAME = gsub('\\s','_',docgroups$FILE_NAME)
docgroups = docgroups[!duplicated(docgroups)]

# 
doe_docs = docgroups[AGENCY=='Department of Energy',]
doe_docs[!file.exists(paste('../eis_documents/',doe_docs$FILE_LOC,doe_docs$FILE_NAME,sep = '/')),]
have = doe_docs[file.exists(paste(doe_docs$FILE_LOC,doe_docs$FILE_NAME,sep = '/')),]
nms = colnames(fread(paste(have[1]$FILE_LOC,have[1]$FILE_NAME,sep = '/')))
big_doe_text = data.table(numeric(),character(),character())
colnames(big_doe_text) <- c(nms,'File')
dir.create('scratch/boilerplate/big_text_files')
fwrite(x = big_doe_text ,file = 'scratch/boilerplate/big_text_files/big_doe_text.txt',sep = '\t')
fname = 'scratch/boilerplate/big_text_files/big_doe_text.txt'

tiles = dplyr::ntile(1:nrow(have),floor(nrow(have)/100))
uq_tiles = unique(tiles)
tfiles = paste(have$FILE_LOC,have$FILE_NAME,sep = '/')

for(u in uq_tiles){
print(u)
temp_fname_list = tfiles[uq_tiles==u]
combined_files <- rbindlist(lapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
}



usfs_docs = docgroups[AGENCY=='Forest Service',]
test_name = paste(usfs_docs$FILE_LOC,usfs_docs$FILE_NAME,sep = '/')
exists = file.exists(test_name)
have = usfs_docs[exists,]
nohave = usfs_docs[!exists,]
nms = colnames(fread(paste(have[1]$FILE_LOC,have[1]$FILE_NAME,sep = '/')))
big_usfs_text = data.table(numeric(),character(),character())
colnames(big_usfs_text) <- c(nms,'File')
dir.create('scratch/boilerplate/big_text_files')
fwrite(x = big_usfs_text ,file = 'scratch/boilerplate/big_text_files/big_usfs_text.txt',sep = '\t')
fname = 'scratch/boilerplate/big_text_files/big_usfs_text.txt'
tiles = dplyr::ntile(1:nrow(have),floor(nrow(have)/100))
uq_tiles = unique(tiles)
tfiles = paste(have$FILE_LOC,have$FILE_NAME,sep = '/')

for(u in uq_tiles){
  print(u)
  temp_fname_list = tfiles[uq_tiles==u]
  combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
  fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
}



blm_docs = docgroups[AGENCY=='Bureau of Land Management',]
test_name = paste(blm_docs$FILE_LOC,blm_docs$FILE_NAME,sep = '/')
exists = file.exists(test_name)
have = blm_docs[exists,]
nohave = blm_docs[!exists,]

nms = colnames(fread(paste(have[1]$FILE_LOC,have[1]$FILE_NAME,sep = '/')))
big_blm_text = data.table(numeric(),character(),character())
colnames(big_blm_text) <- c(nms,'File')
dir.create('scratch/boilerplate/big_text_files')
fwrite(x = big_blm_text ,file = 'scratch/boilerplate/big_text_files/big_blm_text.txt',sep = '\t')
fname = 'scratch/boilerplate/big_text_files/big_blm_text.txt'

tiles = dplyr::ntile(1:nrow(have),floor(nrow(have)/100))
uq_tiles = unique(tiles)
tfiles = paste(have$FILE_LOC,have$FILE_NAME,sep = '/')


for(u in uq_tiles){
  print(u)
  temp_fname_list = tfiles[uq_tiles==u]
  combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
  fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
}





 eis_docs = docgroups[!AGENCY%in%c('Forest Service','Department of Energy','Bureau of Land Management'),]
 eis_docs[!file.exists(paste('../eis_documents/',eis_docs$FILE_LOC,eis_docs$FILE_NAME,sep = '/')),]
 
 have = eis_docs[file.exists(paste(eis_docs$FILE_LOC,eis_docs$FILE_NAME,sep = '/')),]
 nms = colnames(fread(paste(have[1]$FILE_LOC,have[1]$FILE_NAME,sep = '/')))
 
 big_eis_text = data.table(numeric(),character(),character())
 colnames(big_eis_text) <- c(nms,'File')
 dir.create('scratch/boilerplate/big_text_files')
 fwrite(x = big_eis_text ,file = 'scratch/boilerplate/big_text_files/big_eis_text.txt',sep = '\t')
 fname = 'scratch/boilerplate/big_text_files/big_eis_text.txt'
 tiles = dplyr::ntile(1:nrow(have),floor(nrow(have)/100))
 uq_tiles = unique(tiles)
 tfiles = paste(have$FILE_LOC,have$FILE_NAME,sep = '/')

 for(u in uq_tiles){
   print(u)
   temp_fname_list = tfiles[uq_tiles==u]
   combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
   fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
 }



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




