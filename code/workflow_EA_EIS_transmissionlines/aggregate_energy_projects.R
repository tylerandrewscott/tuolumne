
library(INLA)
library(data.table)
library(lubridate)
library(tidyverse)

doe_current_ea = fread('https://www.energy.gov/sites/prod/files/datatables/EA%20Project%20Pivot%20Table%207-31-20%20-%20Sheet1%20%281%29.csv')
doe_current_ea$ID = gsub(':$','',doe_current_ea$`EA#`)
doe_current_ea[,`EA#`:=NULL]
doe_current_ea$Doc_Type = 'EA'
doe_current_eis = fread('https://www.energy.gov/sites/prod/files/datatables/EIS%20Project%20Pivot%20Table%208-5-20%20-%20Sheet1.csv')
doe_current_eis$ID = gsub(':$','',doe_current_eis$`EIS-#`)
doe_current_eis[,`EIS-#`:=NULL]
doe_current_eis$Doc_Type = 'EIS'
doe_current = rbind(doe_current_ea,doe_current_eis)
#doe_current = doe_current[doe_current$Office%in%c('WAPA','BPA'),]
setnames(doe_current ,'Date','YEAR')
docs = readRDS('../eis_documents/agency_nepa_libraries/doe/metadata/doe_nepa_document_record.RDS')
docs = docs[order(NEPA_ID,YEAR),][!duplicated(NEPA_ID),]

doe_current$YEAR= docs$YEAR[match(doe_current$ID,docs$NEPA_ID)]
doe_current$YEAR[is.na(doe_current$YEAR)] <- doe_current$YEAR[match(doe_current$ID[is.na(doe_current$YEAR)],doe_current$DOE_ID)]

doe_current$YEAR[doe_current$ID=='EA-1987'] <- '2017'
doe_current$YEAR[doe_current$ID=='EA-2008'] <- '2016'
doe_current$YEAR[doe_current$ID=='EA-2078'] <- '2017'
doe_current$YEAR[doe_current$ID=='EA-2104'] <- '2019'
doe_current$YEAR[doe_current$ID=='EIS-0116-S1'] <- '1982'
doe_current$YEAR[doe_current$ID=='EIS-0169'] <- '1996'
doe_current$YEAR[doe_current$ID=='EIS-0170'] <- '1997'
doe_current$YEAR[doe_current$ID=='EIS-0232'] <- '1997'
doe_current$YEAR[doe_current$ID=='EIS-0384'] <- '2010'
doe_current$YEAR[doe_current$ID=='EIS-0516'] <- '2015'
doe_current$YEAR[doe_current$ID=='EIS-0516'] <- '2015'
doe_current$YEAR[doe_current$ID=='EIS-0540'] <- '2019'
doe_current$YEAR[doe_current$ID=='EIS-0434'] <- '2009'
doe_current$YEAR[doe_current$ID=='EIS-0358'] <- '2003'
doe_current$YEAR[doe_current$ID=='EIS-0376'] <- '2006'
doe_current$YEAR[doe_current$ID=='EIS-0367'] <- '2006'
doe_current$YEAR[doe_current$ID=='EIS-0323-S1'] <- '2006'
doe_current$YEAR[doe_current$ID=='EIS-0315-S1'] <- '2001'
doe_current= doe_current[!is.na(YEAR),]
doe_current <- doe_current[doe_current$Topic%in%c('Electric Transmission'),]

blm = fread('../eis_documents/agency_nepa_libraries/blm/metadata/project_record.csv')
blm = blm[`Doc Type`%in% c('EA','EIS'),]
blm = blm[`Program(s)`=='Lands and Realty',]
blm = blm[grepl('electric|mwh|kv|overhead|powerline|electrical|transmission|power supply|intertie|substation',tolower(blm$`Project Name`)),]

#fs = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vTPHD71fQvTZpNljqRNVI6lD4b26wW86ptP_dy3R_qWaxkXyOE2QCuyTzroL_38mw/pub?output=csv')
fs = fread('../manitou/input/usfs_internal_data/Copy of MYTR_National Level(1100).xlsx - Decision Data Set.csv')
fs$ongoing = 0
fs2 = data.table(readRDS('../manitou/input/usfs_internal_data/FS_ongoing_projects_11-2019.rds'),stringsAsFactors = F)
numcols = colnames(fs2)[as.vector(apply(fs2,2,function(x) !any(grepl('[^0-9]',x))))]
fs2[,(numcols):=lapply(.SD,as.numeric),.SDcols = numcols]
fs = merge(fs,fs2,all = T)
fs = fs[!duplicated(paste(`PROJECT NUMBER`,`LMU (ACTUAL)`)),] 
fs = fs[fs$`DECISION TYPE`!='PAD',]
#fs = fs2
fs = fs[`ET Electric transmission – activity`==1,]
fs = fs[,!grepl('activity$|purpose$',colnames(fs)),with = F]
fs = fs[,.(`PROJECT NUMBER`,`PROJECT NAME`,`LMU (ACTUAL)`,`PROJECT STATUS`,`INITIATION DATE`,`DECISION TYPE`,`DECISION SIGNED`)]

keywords = c('power supply','powerline','transmission line','electric','kv\\b','mwh','intertie','power system','substation')

tva = readRDS('../eis_documents/agency_nepa_libraries/tva/metadata/tva_project_records.RDS')
tva = tva[grepl(paste(keywords,collapse='|'),tolower(Title)),]

keywords = c('power supply','powerline','transmission line','electric','kv\\b','mwh','intertie','power system','substation','tap','line relocation')
rus = readRDS('../eis_documents/agency_nepa_libraries/rus/metadata/rus_nepa_projects.RDS')
rus = rus[grepl(paste(keywords,collapse='|'),tolower(Project_Name)),]

transmission_lines = rbindlist(list(rus,tva,doe_current,fs,blm),use.names = T,fill = T)


transmission_lines[,Document_URLs:=NULL]
transmission_lines[,Documents:=NULL]
transmission_lines$Content = sapply(transmission_lines$Content,paste,collapse = ' ')

fwrite(transmission_lines,'output/raw_tarnsmission_line_sheet.csv')

