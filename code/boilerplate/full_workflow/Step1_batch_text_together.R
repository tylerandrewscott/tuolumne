
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
if(length(tfiles)!=0){
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
}

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

uq_tiles
if(length(tfiles)!=0){
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
}

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


docs = fread('scratch/boilerplate/document_candidates.csv')
docs$FILE_NAME <- gsub('_{1,}','_',docs$FILE_NAME)
docs = docs[!duplicated(docs)]
docs = docs[PROJECT_ID!='29099']
docs = docs[PROJECT_ID!='34565']
docs = docs[PROJECT_ID!='DOI-BLM-CA-C060-2017-0067-EA',]
docs = docs[PROJECT_ID!='DOI-BLM-WY-P000-2016-0001-EA',]
docs = docs[PROJECT_ID!='DOI-BLM-CA-C060-2018-0064-EA',]
docs = docs[FILE_NAME!='DOI-BLM-UT-G010-2016-0023-EA--2016-0023-EApdf.txt',]
docs = docs[FILE_NAME!='48884--48884_103699_FSPLT3_4318610.txt',]
docs = docs[FILE_NAME!='EA-1849-S-1--EA-1849-S-1-2014.txt',]
docs = docs[FILE_NAME!='42542--42542_96785_FSPLT3_1653493.txt',]
docs = docs[PROJECT_ID!='52446'|FILE_NAME=='52446--52446_107604_FSPLT3_4396800.txt',]
docs = docs[PROJECT_ID!='51876'|FILE_NAME=='51876--51876_106953_FSPLT3_4510414.txt',]
docs = docs[PROJECT_ID!='48918'|FILE_NAME=='48918--48918_103741_FSPLT3_4107141.txt',]
docs = docs[PROJECT_ID!='47978'|FILE_NAME=='47978--47978_102754_FSPLT3_3105953.txt',]
docs = docs[PROJECT_ID!='DOI-BLM-CO-N010-2016-0015-EA'|FILE_NAME=='DOI-BLM-CO-N010-2016-0015-EA--TMA1_FEA_112718_Final.txt',]
docs = docs[PROJECT_ID!='EA-2070'|FILE_NAME=='EA-2070--EA-2070_FORGE%20EA_20180329_508-2.txt',]
docs = docs[!FILE_NAME%in% c('44189--44189_98564_FSPLT3_2984355.txt',
                             '45650--45650_100166_FSPLT3_2579923.txt',
                             '46498--46498_101151_FSPLT3_3906847.txt',
                             '48644--48644_103449_FSPLT3_4301226.txt',
                             '39348--39348_88686_FSPLT3_1451085.txt',
                             '43080--43080_97340_FSPLT3_2937817.txt',
                             '46624--46624_101291_FSPLT3_2562471.txt',
                             '50643--50643_105558_FSPLT3_4661502.txt',
                             '28443--28443_57831_FSPLT3_2285893.txt',
                             'DOI-BLM-NV-S010-2014-0066-EA--Lone_Mountain_Community_Pit_Sand_&_Gravel_Sales_PDF.txt',
                             '53048--53048_108254_FSPLT3_4634360.txt',
                             '53048--53048_108254_FSPLT3_4634355.txt',
                             '53048--53048_108254_FSPLT3_4630484.txt',
                             '45123--45123_99614_FSPLT3_2982914.txt',
                             '40841--40841_92933_FSPLT3_2354859.txt',
                             '40207--40207_91215_FSPLT3_1662936.txt',
                             'DOI-BLM-CO-N050-2019-0040-EA--NWD_EA_Comment_Sept2019.txt',
                             'EA-1956--EA-1956-DEA-2014.txt',
                             'DOI-BLM-UT-W010-2019-0001-EA--SLFO_OG_DOI-BLM-UT-W010-2019-0001-EA_(Box_Elder)_01-25-2019.txt',
                             'EA-1329-S1--ea-1329-s1-wildfire-hazard-reduction-forest-health-improvement-program-04-2019.txt')]
docs = docs[FILE_NAME!='EA-1982--EA-1982%20Parker-Davis%202014-12.txt',]
docs = docs[FILE_NAME!='34320--34320_74093_FSPLT2_291931.txt',]

weirds = grep('_-_',docs$FILE_NAME,value = T)
docs = docs[!FILE_NAME %in% weirds[gsub('_-_','_',weirds) %in% docs$FILE_NAME],]


bfiles = list.files('scratch/boilerplate/big_text_files/',pattern = 'rds',full.names = T)
rds_list = lapply(bfiles,readRDS)
x = 4
lapply(seq_along(rds_list),function(x){
flist_dt <- rds_list[[x]]
flist_dt$File = gsub('\\.pdf\\.txt$','.txt',flist_dt$File)
flist_dt$File <- gsub('_{2,}','_',flist_dt$File,perl = T)
flist_dt$PROJECT_ID = str_remove(flist_dt$File,'(--|_).*')
flist_dt$PID<-NA
file_dash = grepl('--',flist_dt$File)
flist_dt$PID[file_dash] = paste(flist_dt$File[file_dash],flist_dt$Page[file_dash],sep = '--')
flist_dt$PID[!file_dash] = paste(flist_dt$PROJECT_ID[!file_dash],flist_dt$File[!file_dash],flist_dt$Page[!file_dash],sep = '--')
flist_dt = flist_dt[!grepl('\\.{10,}',text,perl = T),]
test = flist_dt[,list(sum(duplicated(text)),.N),by=.(PROJECT_ID)]
high_duplication = test[order(-V1/N),][V1/N>=0.1,]
for(i in 1:nrow(high_duplication)){
  print(high_duplication$PROJECT_ID[i])
  dup_pages = flist_dt[PROJECT_ID==high_duplication$PROJECT_ID[i],][order(-File,Page),][duplicated(text),][,.N,by=.(File)]
  pages = flist_dt[PROJECT_ID==high_duplication$PROJECT_ID[i],][order(-File,Page),][,.N,by=.(File)]
  setnames(dup_pages,'N','dups')
  pages = merge(pages,dup_pages,all = T)  
  docs = docs[!FILE_NAME%in% pages[dups/N>=0.30,]$File,]
}
flist_dt = flist_dt[File %in% docs$FILE_NAME,]
test = flist_dt[,list(sum(duplicated(text)),.N),by=.(PROJECT_ID)]
high_duplication = test[order(-V1/N),][V1/N>=0.1,]
if(nrow(high_duplication)>0){
for(i in 1:nrow(high_duplication)){
  print(high_duplication$PROJECT_ID[i])
  dup_pages = flist_dt[PROJECT_ID==high_duplication$PROJECT_ID[i],][order(File,Page),][duplicated(text),][,.N,by=.(File)]
  pages = flist_dt[PROJECT_ID==high_duplication$PROJECT_ID[i],][order(-File,Page),][,.N,by=.(File)]
  setnames(dup_pages,'N','dups')
  pages = merge(pages,dup_pages,all = T)  
  docs = docs[!FILE_NAME%in% pages[dups/N>=0.20,]$File,]
}}
flist_dt = flist_dt[File %in% docs$FILE_NAME,]
saveRDS(flist_dt,file = bfiles[[x]])
})


fwrite(docs,'scratch/boilerplate/document_candidates.csv')










