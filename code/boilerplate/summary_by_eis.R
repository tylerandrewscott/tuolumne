library(data.table)
library(stringr)

scratch_folder = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/'

aligns = readRDS(paste0(scratch_folder,'cross_eis/local_aling_list.RDS'))
aligns_dt = rbindlist(aligns)
combos = readRDS(paste0(scratch_folder,'cross_eis/combos_searched.RDS'))
combos$EIS1 <- str_extract(combos$V1,'[0-9]{8}')
combos$EIS2 <- str_extract(combos$V2,'[0-9]{8}')
aligns_dt$EIS1 <- str_extract(combos$V1,'[0-9]{8}')
aligns_dt$EIS2 <- str_extract(combos$V2,'[0-9]{8}')

drop = combos$EIS1%in%c(20170006,20170008)|combos$EIS2%in%c(20170006,20170008)
aligns_dt = aligns_dt[!drop,]
combos_dt = combos[!drop,]

aligns_dt = merge(aligns_dt,combos_dt)
aligns_dt$index = 1:nrow(aligns_dt)
combos_dt$index = 1:nrow(combos_dt)
setkey(aligns_dt,'EIS1','EIS2','index')
setkey(combos_dt,'EIS1','EIS2','index')
aligns_dt = aligns_dt[combos_dt,]

aligns_dt[,max(score)==score,by = .()]


data_list = fread(paste0(scratch_folder,'tokenized_pars_epa_repo.csv'))
data_list$ID = paste(data_list$EIS,data_list$File,data_list$Paragraph,sep = '_')
aligns_dt = aligns_dt[order(-score),]
max_dt = aligns_dt[!duplicated(V2),]

data_list$max_score = max_dt$score[match(data_list$ID,max_dt$V2)]
data_list$max_score[is.na(data_list$max_score)] <- 0
library(data.table)

data_list[,mean(max_score),by=.(EIS)]


head(data_list)
names(data_list)
summary(nchar(data_list$text))

test = data_list[nchar(data_list$text)>1000000,]
dim(test)
test[1:4,]

aligns_dt[V2=='20130027_20130027_06 Chapter 3b Biological Environment.pdf_17',]

aligns_dt[V2=='20130017_20130017_Rim Paunina Project Final Environmental Impact Statement.pdf_450',]


aligns_dt[,which.max(score),by=.()]

eis_meta[EIS.Number %in% c(20150148,20150149)]


combos = combos[!drop,]






eis_meta[EIS.Number %in% c(20160104 ,20170008),]

aligns_dt$a_edits[aligns_dt$score==2448]
aligns_dt$b_edits[aligns_dt$score==2448]

eis_meta[EIS.Number%in%c(20150121, 20170006),]
aligns_dt[score==2448,]
aligns_dt[order(-score),][1:5,]
aligns_dt[1,]
aligns_dt[]



combos = readRDS(paste0(scratch_folder,'cross_eis/combos_searched.RDS'))
doc_meta = readRDS(paste0(scratch_folder,'cross_eis/text_metadata.RDS'))
doc_meta = doc_meta[,.(EIS,File,Paragraph,EIS_File_Par)]
eis_meta = fread('input/epa_master_repository/eis_record_detail.csv')


