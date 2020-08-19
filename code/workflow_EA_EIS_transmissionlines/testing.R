test= readRDS('input/agency_nepa/doe/doe_ea_eis_record.RDS')
test$CAT = 'Other'
test$CAT[grepl('WAPA|Western Area Power Administration',test$proj_description)] <- 'WAPA'
test$CAT[grepl('BPA|Bonneville Power Administration',test$proj_description)] <- 'BPA' 

table(test$CAT,test$NEPA)
test = 'https://www.energy.gov/sites/prod/files/datatables/EIS%20Project%20Pivot%20Table%205-2-19%20-%20Sheet1.csv'
temp = fread(test)
temp = temp[temp$Office %in% c('WAPA','BPA')]


test = fread('https://gis.blm.gov/arcgis/rest/services/ePlanning/BLM_Natl_Epl_NepaLup_PRODUCTION/MapServer/0/query?f=json&returnIdsOnly=true&where=PROJECTSTATUS%20IN%20(%27Preparation%20and%20Planning%27%2C%20%27Public%20Scoping%27%2C%20%27Analysis%20%26%20Document%20Preparation%27%2C%20%27Comment%20and%20Review%20Period%27%2C%20%27PRMP%20-%20Protest%20and%20Resolution%27%2C%20%27Final%20EIS%20(non%20LUP)%27%2C%20%27Decision%20and%20Appeal%27%2C%20%27Decision%20and%20Protest%27)&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100')

library(stringr)
test = readxl::read_excel('input/agency_nepa/blm/BLM_NEPA_CX_Search_Results_2019-02-15.xlsx')
table(gsub('-','',gsub('-[0-9]{4}$','',str_extract(test$`NEPA #`,'-[0-9]{4}-[0-9]{4}'))))

temp[temp$Date==1996]
table(temp$Office)
fread('https://www.energy.gov/sites/prod/files/datatables/Temp%205-2-19%20-%20Sheet1.csv')


fread('https://www.energy.gov/sites/prod/files/datatables/')
temp$`EIS-#`[!temp$`EIS-#` %in% test$DOE_ID]

temp[!gsub('-(S|FS|S[0-4])$','',temp$`EIS-#`) %in% test$DOE_ID,]

grep('Walla Walla',test$proj_description,value=T)
table(temp$Office)



library(rvest)
sess = html_session('https://www.energy.gov/nepa/doe-environmental-impact-statements')
form = html_form(sess)

rvest::
form[[2]]

rvest::for
sess
head(test)
which(!grepl('BPA|Bonneville Power Administration',test$proj_description)&
        grepl('Bonneville',test$proj_description))
test$proj_description[52]

table(test$CAT,test$NEPA)

library(data.table)
tt = fread('input/fr_text/notice_of_availability_records.csv')

       


test = readRDS('input/agency_nepa/doe/cx_dt.RDS')


grep('FERC|Regulatory Commission',test$temp_info)
test$temp_info

tt
table(tt$CAT,tt$)


,'WAPA',
          ifelse(grepl('BPA|Bonneville Power Administration',test$proj_description),'BPA','Other'))


ifelse(grepl('BPA|Bonneville Power Administration',test$proj_description),'BPA','Other')

grepl('WAPA|Western Area Power Administration',test$proj_description)

grep('BPA',tt$raw_result,value=T)
grep('WAPA',tt$raw_result,value=T)


length(grep('WAPA|Western Area Power Administration',test$proj_description))
length(grep('BPA|Bonneville Power Administration',test$proj_description))
