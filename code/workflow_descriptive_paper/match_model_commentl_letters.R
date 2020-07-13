library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(RCurl)
library(pbapply)

txt_storage = 'input/epa_comment_txt/'
record_df = fread('input/epa_master_repository/eis_record_detail.csv',stringsAsFactors = F,na.strings = 'NA',encoding = "Latin-1")
#record_df = record_df %>% mutate_if(is.logical,as.character)
record_df$Have_Text_Letter = file.exists(paste0(txt_storage,'Comment_',record_df$EIS.Number,'.txt'))

record_df$Document.Type[is.na(record_df$Document.Type)] <- record_df$Document[is.na(record_df$Document.Type)] 
record_df$Title = tolower(record_df$Title)
record_df = record_df[!grepl('^adoption',record_df$Title),]

final_df = record_df[record_df$Document.Type == 'Final',]
library(lubridate)
final_df$Year = year(mdy(final_df$Federal.Register.Date))
#final_df = final_df[final_df$Year>=2001,]
#final_df$Title = stringi::stri_enc_toutf8(final_df$Title)
library(stringdist)
choices_df = record_df[!record_df$EIS_ID%in%final_df$EIS_ID,]
choices_df$Sub = gsub('^ | $','',gsub('environmental impact statement|final|draft|programmatic|project','',choices_df$Title))
choices_df$Sub = str_sub(choices_df$Sub,1,100)

mt = lapply(1:nrow(final_df),function(x){
  print(x)
  #print(x)
    temp = choices_df[mdy(choices_df$Federal.Register.Date)<mdy(final_df$Federal.Register.Date[x]) & choices_df$Agency==final_df$Agency[x],]
  if(nrow(temp)!=0){
    simdf = stringsim(
      gsub('^ | $','',gsub('environmental impact statement|final|draft|programmatic|project','',final_df$Title[x])),temp$Sub,,q = 3,method = 'jaccard',nthread = 4)
    data.frame(EIS.Number= final_df$EIS.Number[x],title = final_df$Title[x],match_title = temp$Title[which.max(simdf)],match_eis_number =temp$EIS.Number[which.max(simdf)],sim_score = simdf[which.max(simdf)])
  }
  else{data.frame(EIS.Number= final_df$EIS.Number[x],title = final_df$Title[x],match_title = NA,match_eis_number= NA,sim_score = NA)}
})


mt_dt = data.table(do.call(rbind,mt))
mt_dt = mt_dt[!is.na(mt_dt$sim),]
mt_dt = mt_dt[order(sim_score),][sim_score>=0.3,]
final_df$Alt_EIS_Number = mt_dt$match_eis_number[match(final_df$EIS.Number,mt_dt$EIS.Number)]

fwrite(x = final_df,'input/epa_master_repository/final_eis_2001_record.csv')





