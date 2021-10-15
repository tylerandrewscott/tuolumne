pack = c("tidyverse","Matrix","statnet","data.table","stringr","lolog","textreuse","gridExtra","sf","lubridate","doParallel","htmlTable",'treemapify',"ggridges","ggplot2","viridis","hrbrthemes","forcats",'ggthemes','tm','pbapply')
need = !pack %in% installed.packages()[,"Package"]
#sudo apt-get install libfontconfig1-dev
if(any(need)){sapply(pack[need],install.packages)}
sapply(pack,require,character.only=T)


projects = fread('boilerplate_project/data_products/project_candidates_eis_only.csv')

docs = fread('boilerplate_project/data_products/document_candidates_eis_only.csv')

flist_dt = readRDS('boilerplate_project/input/feis_corpus_2013-2020.rds')

flist_dt = flist_dt[str_replace(flist_dt$File,'txt$','pdf') %in% docs$FILE_NAME,]
source('boilerplate_project/code/functions/cleanText.R')
flist_dt <- cleanText(flist_dt)

flist_dt$EIS.Number <- str_extract(flist_dt$File,'^[0-9]{8}')
meta_dt = flist_dt[,.(Page,File,EIS.Number)]
page_counts = meta_dt[,.N,by=.(EIS.Number)]
setnames(page_counts,'N','total_pages')

projects[,.N,by=.(AGENCY)][order(-N)]
projects[,USE_DOCS:=EIS.Number %in% meta_dt$EIS.Number]


lda = readRDS('boilerplate_project/data_products/score_results/eis_page_scores_scratch_file.rds')
lda$score = as.numeric(lda$score)
lda = lda[!duplicated(lda),]
lda = lda[score>=300,]
lda$a = gsub('^(((?!--).)+-{2})\\1','\\1',lda$a,perl = T)
lda$b = gsub('^(((?!--).)+-{2})\\1','\\1',lda$b,perl = T)
lda = lda[a!=b,]
lda = lda[!duplicated(lda),]
gc()

lda$a_page = as.numeric(str_remove(lda$a,'.*_'))
lda$b_page = as.numeric(str_remove(lda$b,'.*_'))
lda$a_id = (str_remove(lda$a,'_.*'))
lda$b_id = (str_remove(lda$b,'_.*'))

lda$a_file = (str_remove(lda$a,'_[0-9]{1,}$'))
lda$b_file = (str_remove(lda$b,'_[0-9]{1,}$'))

lda <- lda[a_file %in% str_replace(docs$FILE_NAME,'pdf$','txt'),]
lda <- lda[b_file %in% str_replace(docs$FILE_NAME,'pdf$','txt'),]



lda_within = lda[a_id == b_id,]
lda_solo = rbind(lda_within[,.(a_id,score,a,a_file,a_page)],lda_within[,.(b_id,score,b,b_file,b_page)],use.names = F)
lda_solo$AGENCY = projects$AGENCY[match(lda_solo$a_id,projects$EIS.Number)]
#lda_solo = lda_solo[!a_file %in% badfiles,]
lda_solo$a_file = gsub('\\s','_',lda_solo$a_file)
lda_solo$a_file = gsub('\\.pdf\\.txt$','.txt',lda_solo$a_file)
lda_solo$a = gsub('\\.pdf\\.txt','.txt',lda_solo$a)

over300 = lda_solo[order(-score),][!duplicated(a),]
countover300 = over300[,.N,by = .(a_id,AGENCY)]
setnames(countover300,'a_id','EIS.Number')
setnames(countover300,'N','over300')
page_counts = page_counts[page_counts$EIS.Number %in% projects$EIS.Number,]
page_counts$AGENCY = projects$AGENCY[match(page_counts$EIS.Number,projects$EIS.Number)]
countover300 = merge(countover300,page_counts[EIS.Number %in% projects$EIS.Number,],all = T)
countover300$AGENCY_SHORT = projects$AGENCY_SHORT[match(countover300$AGENCY,projects$AGENCY)]
countover300$over300[is.na(countover300$over300)] <- 0
projects$within_prop = {countover300$over300/countover300$total_pages}[match(projects$EIS.Number,countover300$EIS.Number)]



lda_between = lda[a_id != b_id,]
lda_solo = rbind(lda_between[,.(a_id,score,a,a_file,a_page)],lda_between[,.(b_id,score,b,b_file,b_page)],use.names = F)
lda_solo$AGENCY = projects$AGENCY[match(lda_solo$a_id,projects$EIS.Number)]
#lda_solo = lda_solo[!a_file %in% badfiles,]
lda_solo$a_file = gsub('\\s','_',lda_solo$a_file)
lda_solo$a_file = gsub('\\.pdf\\.txt$','.txt',lda_solo$a_file)
lda_solo$a = gsub('\\.pdf\\.txt','.txt',lda_solo$a)

over300 = lda_solo[order(-score),][!duplicated(a),]
countover300 = over300[,.N,by = .(a_id,AGENCY)]
setnames(countover300,'a_id','EIS.Number')
setnames(countover300,'N','over300')
page_counts = page_counts[page_counts$EIS.Number %in% projects$EIS.Number,]
page_counts$AGENCY = projects$AGENCY[match(page_counts$EIS.Number,projects$EIS.Number)]
countover300 = merge(countover300,page_counts[EIS.Number %in% projects$EIS.Number,],all = T)
countover300$AGENCY_SHORT = projects$AGENCY_SHORT[match(countover300$AGENCY,projects$AGENCY)]
countover300$over300[is.na(countover300$over300)] <- 0
projects$between_prop = {countover300$over300/countover300$total_pages}[match(projects$EIS.Number,countover300$EIS.Number)]

projects = projects[USE_DOCS==T&!duplicated(EIS.Number),]

projects$total_pages = page_counts$total_pages[match(projects$EIS.Number,page_counts$EIS.Number)]

(compare_between_within = ggplot(projects,aes(x = between_prop,y = within_prop,size = total_pages)) + 
  geom_point(alpha = 0.5,pch = 21) + 
  scale_x_continuous('Proportion of pages w/ between-EIS LDA score > 300') +
  scale_y_continuous('Proportion of pages w/ within-EIS LDA score > 300') +
  theme_bw() + 
  theme(legend.position = c(0.8,0.7),legend.background = element_rect(fill = alpha('white',0))) + 
  scale_size_binned_area(breaks=c(100,500,1000,10000),name = '# EIS pages') + 
  ggtitle('Between-project vs. within-project text reuse'))
ggsave(compare_between_within,filename = 'boilerplate_project/output/figures/Figure6_compare_within_between.tiff',dpi = 400,width = 7,height = 6,units = 'in')


