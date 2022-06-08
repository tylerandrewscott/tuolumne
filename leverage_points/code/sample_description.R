library(tidyverse)
library(lubridate)
epa = read_csv('input/epa_master_repository/eis_record_detail.csv')
epa = epa[grepl('CA',epa$State.or.Territory),]
epa$FR_Year = year(mdy(epa$Federal.Register.Date))
epa = epa[epa$FR_Year>=2008 & epa$FR_Year<2019,]
doc = read_csv('input/epa_master_repository/eis_document_record.csv')
doc = doc[doc$EIS.Number %in% epa$EIS.Number,]





