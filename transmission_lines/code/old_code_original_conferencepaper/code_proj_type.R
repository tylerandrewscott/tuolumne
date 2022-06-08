require(data.table)
require(stringr)
transline_url = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQck262IIZxf_o5q2Ci9gRlY2qTPzw0WqCsEch3RXNAqYVd-JKZgZLpSPGc3GxB3TsW0Dek2hd3_H6k/pub?gid=811733457&single=true&output=csv'

projs = fread(transline_url)
projs = projs[Agency%in%c('DOE','BLM')]
flist = list.files('../eis_documents/agency_nepa_libraries/blm/text_as_datatable/',pattern = 'txt$',recursive = T,full.names = T)
flist2 = list.files('../eis_documents/agency_nepa_libraries/doe/text_as_datatable/',pattern = 'txt$',recursive = T,full.names = T)
flist = c(flist,flist2)
ids = str_remove(basename(flist),'(--|_).*')

flist = flist[ids %in% projs$ID]

strings = c('replace','realign','reconduct','upgrade the existing')

ids = str_remove(basename(flist),'(--|_).*')

file_splits = split(flist,ids)

require(pbapply)
match_list = pblapply(seq_along(file_splits),function(x) {
 # print(x)
  i = names(file_splits)[x]
  files = file_splits[[x]]
  text = rbindlist(lapply(files,fread,nrows = 10))
  matches = data.table(ID = i,colSums(as.matrix(sapply(strings,grepl,text$text))),strings)
  matches
},cl = 4)

labels = dcast(rbindlist(match_list,use.names = T,fill = T),ID ~ strings,value.var = 'V2')
labels[,sum_total:=realign+reconduct+replace+`upgrade the existing`]

saveRDS(labels,'scratch/transmission_lines/text_projtype_matches.rds')
