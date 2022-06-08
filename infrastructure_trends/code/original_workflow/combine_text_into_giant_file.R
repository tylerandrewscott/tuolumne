setwd('Box/tuolumne/')
library(data.table)
library(tidyverse)
eis20132019 = list.files('../eis_documents/enepa_repository/text_as_datatable/',pattern = '^201[3-9]',recursive = T,full.names = T)
finfo = file.info(eis20132019)
file.remove(eis20132019[which(finfo$size==10)])
eis20132019 = list.files('../eis_documents/enepa_repository/text_as_datatable/',pattern = '^201[3-9]',recursive = T,full.names = T)
big_eis_text = data.table(Page = numeric(),text = character(),File = character())

yrs = str_extract(basename(eis20132019),'^[0-9]{4}')
flists = rev(list.files('scratch/',pattern = 'eis_text_[0-9]{4}.txt',full.names = T))


for(f in flists){
  print(f)
  if(file.exists(f)){dt = fread(f,sep = '\t')}else{dt = big_eis_text}
  subyear = eis20132019[yrs==str_extract(f,'[0-9]{4}')]
  still_need = basename(subyear)[!basename(subyear) %in% unique(dt$File)]
  length(still_need)
  tiles = dplyr::ntile(1:length(still_need),floor(length(still_need)/25))
  uq_tiles = unique(tiles)
  #tfiles = paste(still_need$FILE_LOC,still_need$FILE_NAME,sep = '/')
  dt = dt[!duplicated(dt),]
  for(u in uq_tiles){
    print(u)
    temp_fname_list = subyear[uq_tiles==u]
    combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x);xx$File = basename(x);xx}))
    if(!file.exists(f)){fwrite(x=combined_files,file = f,verbose = T,sep = '\t')}
    else{fwrite(x = combined_files,file = f,append = T,verbose = F,sep = '\t')}
  }
}


