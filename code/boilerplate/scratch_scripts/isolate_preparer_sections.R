require(data.table)
require(pbapply)
require(stringr)


pl = list.files('scratch/boilerplate/',pattern = 'eis.*preparer.*rds$',full.names =T)
lst = lapply(pl,readRDS)
lst <- unlist(lst)
base = basename(lst)
ids = str_remove(base,'(--|_).*')

projs = fread('scratch/boilerplate/project_candidates_eis_only.csv')
lst= lst[(ids %in% projs$PROJECT_ID)]

found = dcast(as.data.table(table(projs$AGENCY,projs$PROJECT_ID %in% ids)),V1~V2,value.var = 'N')
setnames(found,c('TRUE','FALSE'),c('NO','YES'))

flist = list.files('../eis_documents/enepa_repository/documents/',full.names = T,recursive = T)
stringcombos = c('List of Preparers','Team Members','Authors','Preparers','Consulted','Other Contributors','Technical Support')
stc = paste(c(stringcombos,toupper(stringcombos)),collapse='|')


require(pdftools)
slist = lst
dubs = pblapply(slist,function(f){
temp = fread(f)
tt  = temp[grepl(stc,temp$text),]
tt$File = f;tt},cl = 6)

 
  sapply(slist,class)=='try-error'
#skipped
dubs2 = pblapply(slist[which(sapply(dubs,function(x) class(x)[1])=='try-error')],function(f){
  temp = fread(f)
  tt  = temp[grepl(stc,temp$text),]
  tt$File = f;tt},cl = 4)

sapply(dubs2 ,class)
fread(slist[1064])


f = slist[100]
temp = fread(f)
temp[grepl(stc,temp$text),]


dubs
grep(stc,temp$text)
temp$text[115]
temp = temp[grepl(stc,temp$text),]
if(nrow(temp)>1){f}})

dubs
slist
