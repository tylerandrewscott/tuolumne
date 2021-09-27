require(data.table)
require(pbapply)
require(stringr)

pl = list.files('scratch/climate_in_nepa/',pattern = 'climate_sections.*rds$',full.names =T)
lst = lapply(pl,readRDS)

lst <- unlist(lst)
base = basename(lst)
ids = str_remove(base,'(--|_).*')

projs = fread('scratch/boilerplate/project_candidates.csv')
lst= lst[(ids %in% projs$PROJECT_ID)]

projs[AGENCY=='Bureau of Indian Affairs',]
found = dcast(as.data.table(table(projs$AGENCY,projs$PROJECT_ID %in% ids)),V1~V2,value.var = 'N')
setnames(found,c('TRUE','FALSE'),c('NO','YES'))

found


flist = list.files('../eis_documents/enepa_repository/documents/',full.names = T,recursive = T)

grep('20140042',flist,value = T)

found[order(-NO/(NO + YES)),]
3303 /(3280+3303)
1398/ (1772+1398)
lst[!file.exists(lst)]
file.exists("../eis_documents/agency_nepa_libraries/blm/text_as_datatable//2006/DOI-BLM-ID-T010-2006-0001-RMP-EIS--06_Approved_RMP.txt" )
DOI-BLM-ID-T010-2006-0001-RMP-EIS--06_Approved_RMP
DOI-BLM-ID-T010-2006-0001-RMP-EIS--06_Approved_RMP.txt"  
DOI-BLM-CO-N020-2008-0001-RMP-EIS--KFO_CH6
DOI-BLM-CO-N020-2008-0001-RMP-EIS--KFO_CH6.txt"
stringcombos = c('List of Preparers','Team Members','Authors','Preparers','Consulted','Other Contributors','Technical Support')

stc = paste(c(stringcombos,toupper(stringcombos)),collapse='|')


lst[!file.exists(lst)]

require(pdftools)
slist = lst
dubs = pblapply(slist,function(f){
temp = fread(f)
tt  = temp[grepl(stc,temp$text),]
tt$File = f;tt},cl = 4)


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
