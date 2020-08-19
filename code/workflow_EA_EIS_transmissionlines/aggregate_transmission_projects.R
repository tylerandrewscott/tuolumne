
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

doe_current <- doe_current[doe_current$Topic%in%c('Electric Transmission','Solar','Wind','Hydropower','Geothermal'),]


library(rvest)
library(pbapply)

doe_current$page_description <- pbsapply(seq_along(doe_current$URL),function(x) {read_html(doe_current$URL[x]) %>% 
    html_nodes('#main_content p') %>% html_text(trim=T) %>% paste(unlist(.),collapse=' ')},cl = 4)
#doe_current$page_description = pbsapply(doe_current$URL,function(x) {read_html(x) %>% html_nodes('#main_content p') %>% html_text(trim=T)},cl = 4)

blm = fread('../eis_documents/agency_nepa_libraries/blm/metadata/project_record.csv')
blm = blm[`Doc Type`%in% c('EA','EIS'),]
blm[,.N,by=.(`Program(s)`,`Doc Type`)][order(-N)]
blm[`Program(s)`=='Renewable Energy',.N,by=.(`Doc Type`)]

unique(blm$`Program(s)`)



test = blm[grep('power line|transmission line|intertie',tolower(`Project Name`),value=F),]

test[`Program(s)`=='Lands and Realty']


table(blm[grep('power line|transmission line|intertie',tolower(`Project Name`),value=F),]$`Program(s)`)

blm <- blm[grep('power line|transmission line|intertie',tolower(`Project Name`),value=F),]
blm <- blm[`Doc Type`%in% c('EA','EIS'),]
fs = fread('../manitou/input/Copy of MYTR_National Level(1100).xlsx - Decision Data Set.csv')
fs = fs[`ET Electric transmission – activity`==1,]
fs = fs[`DECISION TYPE` %in% c('DN','ROD'),]







table(blm$`Doc Type`)


test = fread('input/fr_text/notice_of_availability_records.csv')
grep('BPA|WAPA',test$raw_result,value=T)
test = fread('input/agency_nepa/doe/bpa_nepa_record.csv')
test <- test[test$NEPA!='CATEX',]
table(grepl('EIS',test$Title))
test$Title
table(doe_current$Doc_Type)

grep('transmission',tolower(test$raw_result),value=T)

doe_current$Doc_Type[]
library(readtext)
doe_fls = readtext(fls)
grep('0128',tolower(fls),value=T)
grep('map',tolower(fls),value=T)

grep('mitigation action plan',doe_fls$text)

flist

doe_current[doe_current$ID=='EIS-0128',]
doe_fls[grep('mitigation action',doe_fls$text),]
str_extract(tolower(doe_fls$text),'.{10}mitigation.{10}')





