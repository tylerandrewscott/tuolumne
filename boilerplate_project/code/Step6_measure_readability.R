pack = c("tidyverse","Matrix","statnet","data.table","stringr","lolog","textreuse","gridExtra","sf","lubridate","doParallel","htmlTable",'treemapify',"ggridges","ggplot2","viridis","hrbrthemes","forcats",'ggthemes','tm','pbapply')
need = !pack %in% installed.packages()[,"Package"]
#sudo apt-get install libfontconfig1-dev
if(any(need)){sapply(pack[need],install.packages)}
sapply(pack,require,character.only=T)

require(forcats)

projects = fread('boilerplate_project/data_products/project_candidates_eis_only.csv')
full = c('Forest Service','Bureau of Land Management','Department of Commerce','National Park Service','Federal Energy Regulatory Commission','Department of Health and Human Services','National Oceanic and Atmospheric Administration','Bureau of Reclamation',"U.S. Army Corps of Engineers" ,'Department of Defense',"Department of Transportation (other)",'Department of Energy',"Bureau of Indian Affairs","Department of Homeland Security","Department of Interior (other)","Department of Housing and Urban Development","California Department of Transportation","Nuclear Regulatory Commission","Federal Highway Administration" ,"Fish and Wildlife Service","General Services Administration" ,"Tennessee Valley Authority" ,"USDA (non-FS)"  )
brev =c('FS','BLM','DOC','NPS','FERC','DHHS','NOAA','BR','ACOE','DOD','DOT (other)','DOE','BIA','DHS','DOI (other)','DHUD','CalDOT','NRC','FHWA','FWS','GSA','TVA','USDA (non-FS)')
nms = data.table(full,brev)
projects$AGENCY_SHORT <- nms$brev[match(projects$AGENCY,nms$full)]
projects$EIS.Number <- as.character(projects$EIS.Number)

docs = fread('boilerplate_project/data_products/document_candidates_eis_only.csv')

flist_dt <- readRDS('../bucket_mount/tuolumne/scratch/boilerplate/big_text_files/big_eis_text.rds')
flist_dt = flist_dt[str_replace(flist_dt$File,'txt$','pdf') %in% docs$FILE_NAME,]
flist_dt$EIS.Number <- str_remove(flist_dt$File,'_.*')
flist_dt$text = gsub('\"\"','',flist_dt$text,fixed = T)
chars = nchar(flist_dt$text)
periods = stringr::str_count(flist_dt$text,"\\.")
numbers = stringr::str_count(flist_dt$text,"[0-9]")
caps = stringr::str_count(flist_dt$text,'[A-Z]')
tildes = stringr::str_count(flist_dt$text,'~')
quotes = stringr::str_count(flist_dt$text,'\\"')
spaces = stringr::str_count(flist_dt$text,'\\s')
cut = 0.1
flist_dt  = flist_dt[chars>400&{periods/chars}<cut&{quotes/chars}<cut&{tildes/chars}<cut&{numbers/chars}<cut&{caps/chars}<cut&{spaces/chars}<{cut*2},]



require(pbapply)
require(sylcount)
uq_id = sort(unique(flist_dt$EIS.Number))
doc_texts = pblapply(uq_id,function(x) paste0(flist_dt$text[flist_dt$EIS.Number==x],collapse = ' '),cl = mcores)
names(doc_texts) <- uq_id
full_set = unlist(doc_texts)
rd = readability(full_set)
rd$EIS.Number = names(full_set)

saveRDS(object = rd,'boilerplate_project/data_products/readability_scores_by_project.rds')
