cxdt = readRDS('input/agency_nepa/doe/cx_dt.RDS')
cxdt = cxdt[!duplicated(cxdt$temp_title),]
library(lubridate)
cxdt$Date <- mdy(cxdt$temp_dates)
cxdt$Year <- year(cxdt$Date)
cxdt$ID = str_extract(cxdt$temp_title,'CX-[0-9]{1,}')

uqdt = cxdt[,.(ID)]
uqdt$Type = 'CX'

rec = fread('input/agency_nepa/doe/doe_current_records_2019-02-16.csv')
rec$Title <- gsub("EIs-0285","EIS-0285",rec$Title)
rec$ID = str_extract(rec$Title,'EIS(-|:)[0-9]{1,}|EA(-|:)[0-9]{1,}')
rec$Type = str_extract(rec$ID,'EIS|EA')
rec = rec[!is.na(rec$ID),]
rec$Date = mdy(rec$Date)
setkey(rec,ID,Date)
rec = rec[order(ID,-Date),][!duplicated(ID),]
rec$Year = year(rec$Date)
uqdt = rbind(uqdt,rec[,.(ID,Type)])

doe_df = fread('input/agency_nepa/doe/doe_nepa_records_2019-01-26.csv')
doe_df = doe_df[doe_df$TYPE_SET!='CX',]
doe_df$ID = str_extract(doe_df$Number,'EIS(-|:)[0-9]{1,}|EA(-|:)[0-9]{1,}')
doe_df$Number = gsub('EA- 1255','EA-1255',doe_df$Number)
doe_df$Number = gsub('‐','-',doe_df$Number)
doe_df$ID[is.na(doe_df$ID)] <- str_extract(doe_df$Number[is.na(doe_df$ID)],'EIS(-|:|\\s)[0-9]{1,}|EA(-|:|\\s)[0-9]{1,}')
doe_df = doe_df[!is.na(doe_df$ID),]
doe_df$FILE = grepl('pdf$',doe_df$url)+0
doe_df$Type =  str_extract(doe_df$ID,'EIS|EA')
uqdt = rbind(uqdt,doe_df[,.(ID,Type)])
fwrite(uqdt,'input/agency_nepa/doe/master_project_ids.csv')






for(i in 1:nrow(doe_df))
{if(doe_df$FILE[i]==0)}
{
  read_html(paste0('https://www.energy.gov/',doe_df$url[10])
}




doe_df$Date = mdy(doe_df$Date)
doe_df = doe_df[!grepl('EIS-0[0-1]',doe_df$ID),]



rec[rec$ID=='EIS-0538']
doe_df[doe_df$ID=='EIS-0538']



doe_df[grepl('EIS-02',doe_df$ID),]


doe_df[is.na(doe_df$Date),]

doe_df[doe_df$ID=='EIS-0109',]

doe_df[is.na(doe_df$Date),]
table(is.na(doe_df$ID))

doe_df <- doe_df[!doe_df$Status%in%c('Active','Canceled','Inactive'),]

doe_df$Status
table(doe_df$Status)
doe_df = doe_df[order(ID,-Date),][!duplicated(ID),]
doe_df <- doe_df[!doe_df$ID %in% uqdt$ID,]
doe_df$Year = year(doe_df$Date)






table(is.na(doe_df$Date))

doe_df[is.na(doe_df$Year)][260:276,]

rbind(uqdt,doe_df[,.(Type,ID,Year)])
table(is.na(doe_df$Date))
fread('input/agency_nepa/doe/')


doe_df[is.na(doe_df$NEPA_NUMBER),]



rec$TYPE_SET = NA
rec$TYPE_SET[grepl('EIS',rec$Title)&grepl('Draft',rec$Title)] <- 'EIS'


doe_df$TYPE_SET[is.na(doe_df$TYPE_SET)&grepl('Suppl',doe_df$Number)] <- 'SUPPLEMENT'
doe_df$TYPE_SET[doe_df$TYPE_SET=='EIS'&grepl('FINAL',toupper(doe_df$Number))&!grepl('SUPPL',toupper(doe_df$Number))] <- 'FINAL'
doe_df$TYPE_SET[doe_df$TYPE_SET=='EIS'&grepl('FINAL',toupper(doe_df$Number))&grepl('SUPPL',toupper(doe_df$Number))] <- 'SUPPLEMENT'
doe_df$TYPE_SET[doe_df$TYPE_SET=='EIS'&grepl('DRAFT',toupper(doe_df$Number))&!grepl('SUPPL',toupper(doe_df$Number))] <- 'DRAFT'
doe_df$TYPE_SET[doe_df$TYPE_SET=='EIS'&grepl('DRAFT',toupper(doe_df$Number))&grepl('SUPPL',toupper(doe_df$Number))] <- 'SUPPLEMENT'
doe_df$TYPE_SET[doe_df$TYPE_SET=='EIS'] <- 'FINAL'
doe_df$TYPE_SET[doe_df$TYPE_SET%in%c('DRAFT_SUPPLEMENT','FINAL_SUPPLEMENT')] <- 'SUPPLEMENT'







doe_counts = doe_df %>% group_by(YEAR,TYPE_SET) %>% summarize(count = n())

ggplot(doe_counts %>% filter(YEAR<2018),aes(x= YEAR,y = count,colour = TYPE_SET))  +
  geom_line() + 
  geom_point() + 
  theme_bw() + scale_color_tableau() + theme(legend.position = c(0.25,0.7)) + 
  scale_x_continuous(name = 'YEAR') + scale_y_continuous(name = '# published') + 
  ggtitle('Yearly NEPA actions recorded by DOE') + #facet_wrap(~TYPE_SET,scales = 'free_y') +
  NULL

ggplot(doe_counts %>% filter(YEAR<2018),aes(x= YEAR,y = count,colour = TYPE_SET))  +
  geom_line() + 
  geom_point() + 
  theme_bw() + scale_color_tableau() + theme(legend.position = c(0.8,0.2)) + 
  scale_x_continuous(name = 'YEAR') + scale_y_continuous(name = '# published') + 
  ggtitle('Yearly NEPA actions recorded by DOE') + facet_wrap(~TYPE_SET,scales = 'free_y') +
  guides(colour = FALSE) + NULL


doe_df$EA_FONSI = NA
doe_df$EA_FONSI[doe_df$TYPE_SET=='EA'] <-   doe_df$NEPA_NUMBER[doe_df$TYPE_SET=='EA'] %in% doe_df$NEPA_NUMBER[doe_df$TYPE_SET=='FONSI']  + 0
doe_df$NEPA_NUMBER[doe_df$TYPE_SET=='EA'] %in% doe_df$NEPA_NUMBER[doe_df$TYPE_SET=='FONSI']  + 0

fonsi_by_year = doe_df %>% filter(TYPE_SET=='EA') %>% group_by(YEAR,EA_FONSI) %>% summarise(count = n()) %>% spread(EA_FONSI,count) %>%
  filter(YEAR>1995) %>% mutate(total_EAs = `0`+`1`) %>% mutate(Prop_FONSI = `1`/total_EAs)


ggplot() + 
  geom_path(data = fonsi_by_year %>% filter(YEAR<2018),aes(x = YEAR,y = total_EAs,colour = 'TOTAL EAs')) + 
  geom_path(data = fonsi_by_year %>% filter(YEAR<2018),aes(x = YEAR,y = `1`,colour = 'FONSI')) +
  geom_path(data = doe_counts %>% filter(TYPE_SET == 'DRAFT',YEAR>1995,YEAR<2018),aes(x=YEAR,y = count,colour = 'Draft EISs')) +
  theme_bw() + 
  scale_color_tableau(name = '') + theme(legend.position = c(0.2,0.6)) + 
  scale_y_continuous(name = '# published') +
  ggtitle('EA outcomes by year, DOE')

table(doe_df$YEAR,doe_df$TYPE_SET)
doe_cmat = spread(doe_counts,TYPE_SET,count) 
doe_cmat[is.na(doe_cmat)] <- 0
doe_cmat %>% filter(YEAR>1995)





table(full$Title[full$TYPE_SET=='CX'] %in% cxdt$temp_title)

head(rec)

table9rec