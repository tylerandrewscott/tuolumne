empty = data.table(Type = NA,ID = NA)

cxdt = readRDS('input/agency_nepa/doe/cx_dt.RDS')
cxdt = cxdt[!duplicated(cxdt$temp_url),]
cxdt$ID = str_extract(cxdt$temp_title,'CX-[0-9]{1,}')
cx_file = data.table()
cxdt$Download = NA
library(lubridate)
cxdt$CATEX_Date <- mdy(cxdt$temp_dates)
cxdt$Year <- year(cxdt$Date)
cxdt$Type = 'CX'

uqdt = rbindlist(list(empty,cxdt[,.(ID,Type,CATEX_Date)]),fill=T)
uqdt = uqdt[year(uqdt$CATEX_Date)>=2009,]

doe_df = fread('input/agency_nepa/doe/doe_nepa_records_2019-01-26.csv')
doe_df = doe_df[doe_df$TYPE_SET!='CX',]
doe_df$ID = str_extract(doe_df$Number,'EIS(-|:)[0-9]{1,}|EA(-|:)[0-9]{1,}')
doe_df$Number = gsub('EA- 1255','EA-1255',doe_df$Number)
doe_df$Number = gsub('‐','-',doe_df$Number)
doe_df$ID[is.na(doe_df$ID)] <- str_extract(doe_df$Number[is.na(doe_df$ID)],'EIS(-|:|\\s)[0-9]{1,}|EA(-|:|\\s)[0-9]{1,}')
doe_df = doe_df[!is.na(doe_df$ID),]
doe_df$Type =  str_extract(doe_df$ID,'EIS|EA')

eadt = doe_df[doe_df$Type == 'EA',]

ea_dates = rbindlist(lapply(unique(eadt$ID),function(i){
 td = eadt[eadt$ID==i,] 
 data.table(ID = i,
  Draft_Date = ifelse(any(grepl('DRAFT',toupper(td$Number))),td$Date[grepl('DRAFT',toupper(td$Number))][1],NA),
  Final_Date = ifelse(any(grepl('FINAL',toupper(td$Number))),td$Date[grepl('FINAL',toupper(td$Number))][1],NA),
  FONSI_Date = ifelse(any(grepl('FINDING|FONSI',toupper(td$Number))),td$Date[grepl('FINDING|FONSI',toupper(td$Number))][1],NA))
 }))
ea_dates$Draft_Date[ea_dates$ID=='EA-2043'] <- 'April 22, 2016'
ea_dates$FONSI_Date[ea_dates$ID=='EA-2043'] <- ea_dates$Final_Date[ea_dates$ID=='EA-2043']


ea_dates[ !is.na(ea_dates$Final_Date) & is.na(ea_dates$FONSI_Date),]

table(year(mdy(ea_dates$Final_Date)))


eadt[eadt$ID=='EA-2043',]
ea_dates[ea_dates$ID=='EA-2043',]
td$Date[grepl('DRAFT',toupper(td$Number))]


doe_df$Stage = str_extract(tolower(doe_df$url),'draft|final')

doe_df[is.na(doe_df$Stage),]
doe_df$Doc = NA

tail(doe_df$Number[doe_df$Type=='EA'])


for(i in unique(doe_df$ID))
{
i = doe_df$ID[10]  
doe_df[doe_df$ID==i,]

}


doe_df$Doc[grepl('DRAFT',toupper(doe_df$Number))|grepl('DRAFT',toupper(doe_df$url))] <- 'Draft'
doe_df$Doc[grepl('FINAL',toupper(doe_df$Number))|grepl('FINAL',toupper(doe_df$url))] <- 'Final'
doe_df$Doc[grepl('Finding Of No Significant Impact',doe_df$Number)|grepl('FINAL',toupper(doe_df$url))] <- 'Final'





doe_df[is.na(doe_df$Doc),]
table(is.na(doe_df$Doc))
doe_df

doe_df$FILE = grepl('pdf$',doe_df$url)+0





setnames(cxdt,'Date','CATEX_Date')

uqdt = cxdt[,.(ID,CATEX_Date)]
uqdt$Type = 'CX'






rec = fread('input/agency_nepa/doe/doe_current_records_2019-02-16.csv')
rec$Title <- gsub("EIs-0285","EIS-0285",rec$Title)
rec$ID = str_extract(rec$Title,'EIS(-|:)[0-9]{1,}|EA(-|:)[0-9]{1,}')
rec$Type = str_extract(rec$ID,'EIS|EA')
rec = rec[!is.na(rec$ID),]
rec$Date = mdy(rec$Date)


rec[order(ID)]

setkey(rec,ID,Date)
rec = rec[order(ID,-Date),][!duplicated(ID),]
rec$Year = year(rec$Date)
uqdt = rbind(uqdt,rec[,.(ID,Year,Type)])


uqdt = rbind(uqdt,doe_df[,.(ID,Type,Year)])





fwrite(uqdt,'input/agency_nepa/doe/master_project_ids.csv')



uqdt[uqdt$ID=='EIS-0433',]
uqdt[duplicated(uqdt$ID)&is.na(uqdt$Year),]


table(duplicated(uqdt$ID),is.na(uqdt$Year))


