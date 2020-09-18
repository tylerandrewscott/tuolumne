library(tidyverse)
library(Matrix)
library(statnet)
library(data.table)
library(stringr)
library(lolog)
library(textreuse)
library(sf)
library(lubridate)
library(doParallel)


mcores = detectCores() * 0.75
samples = 1000
iters = 100
stepsize = 10

epa_record = fread('scratch/eis_record_detail.csv')
epa_record = epa_record[grepl('^201[3-9]',epa_record$EIS.Number),]
epa_record = epa_record[epa_record$Document.Type=='Final',]
#epa_record[,.N,by = .(Agency)][order(-N)]
epa_record$Agency_Short = epa_record$Agency
epa_record$Agency_Short[epa_record$Agency=='Federal Energy Regulatory Commission'] <- 'FERC'
epa_record$Agency_Short[epa_record$Agency=="National Oceanic and Atmospheric Administration"] <- 'NOAA'
epa_record$Agency_Short[epa_record$Agency=="National Park Service"] <- 'NPS'
epa_record$Agency_Short[epa_record$Agency=="Bureau of Land Management"] <- 'BLM'
epa_record$Agency_Short[epa_record$Agency=="Forest Service"] <- 'FS'
epa_record$Agency_Short[epa_record$Agency=="Bureau of Indian Affairs"] <- 'BIA'
epa_record$Agency_Short[epa_record$Agency=="United States Air Force"] <- 'USAF'
epa_record$Agency_Short[epa_record$Agency=="United States Navy"] <- 'USN'
epa_record$Agency_Short[epa_record$Agency=="U.S. Army Corps of Engineers"] <- 'ACOE'
epa_record$Agency_Short[epa_record$Agency=="Nuclear Regulatory Commission"] <- 'NRC'
epa_record$Agency_Short[epa_record$Agency=="Fish and Wildlife Service"] <- 'FWS'
epa_record$Agency_Short[epa_record$Agency=="Federal Highway Administration"] <- 'FHA'
epa_record$Agency_Short[epa_record$Agency=="Federal Railroad Administration" ] <- 'FRA'
epa_record$Agency_Short[epa_record$Agency=="National Marine Fisheries Service"  ] <- 'NMFS'
epa_record$Agency_Short[epa_record$Agency== "Department of Energy"   ] <- 'DOE'
epa_record$Agency_Short[epa_record$Agency== "United States Army"   ] <-  'US Army'
epa_record$Agency_Short[epa_record$Agency== "Federal Transit Administration"   ] <-  'FTA'
epa_record$Agency_Short[epa_record$Agency== "Bureau of Reclamation"   ] <-  'BR'

#levels(fct_infreq(epa_record$Agency_Short))[1:18] 
epa_record$Agency_Short = fct_infreq(epa_record$Agency_Short) 

metafiles = list.files('scratch/boilerplate/big_text_files/',pattern = 'metadata',full.names = T)
meta_dt = rbindlist(lapply(metafiles,fread))
colnames(meta_dt) <- c('File','Page')
meta_dt = meta_dt[!meta_dt$File %in% dropfiles,]
meta_dt$File <- gsub('\\s','_',meta_dt$File)
meta_dt$DUPE = duplicated(meta_dt)
meta_dt$PROJECT_ID = str_remove(meta_dt$File,'(--|_).*')
meta_dt = meta_dt[!meta_dt$DUPE,]
page_counts = meta_dt[,.N,by = .(PROJECT_ID)]
setnames(page_counts,'N','total_pages')

#projpages = projpages[!duplicated(projpages),]

projects = fread('scratch/boilerplate/project_candidates.csv')

#projects[AGENCY%in%c('Bureau of Land Management','Forest Service','Department of Energy'),.N,by = .(PROJECT_TYPE,AGENCY)]
#project_candidates[AGENCY%in%c('Bureau of Land Management','Forest Service','Department of Energy'),.N,by = .(PROJECT_TYPE,AGENCY)]

documents = fread('scratch/boilerplate/document_candidates.csv')
dropfiles = c( "20130252_HSTT EIS CH 3.5-7 VOL II 08-13.pdf.txt","20130252_HSTT EIS Exec Sum-CH 3.4 VOL I 08-13.pdf.txt")
documents = documents[!FILE_NAME %in% dropfiles ]
documents$FILE_NAME = gsub('\\s','_',documents$FILE_NAME)

text_storage = 'input/filtered_text_files/'
flist = list.files(text_storage)
lda_result_file = 'scratch/boilerplate/lda_combo_results_V2.RDS'
lda = readRDS(lda_result_file)

lda$a = gsub('\\s','_',lda$a)
lda$b = gsub('\\s','_',lda$b)
lda = lda[lda$a != lda$b,]
lda$score = as.numeric(lda$score)
lda = lda[!duplicated(lda)]
lda$a_id = str_remove(lda$a,'--.*')
lda$b_id = str_remove(lda$b,'--.*')

lda$a = str_replace(lda$a,"(.+-{2})\\1","\\1")
lda$b = str_replace(lda$b,"(.+-{2})\\1","\\1")
lda$p1 = as.numeric(gsub('--','',str_extract(lda$a,'--[0-9]{1,}$')))
lda$p2 = as.numeric(gsub('--','',str_extract(lda$b,'--[0-9]{1,}$')))

lda$f1 = paste0(str_remove(lda$a,'--[0-9]{1,}$'))
lda$f2 = paste0(str_remove(lda$b,'--[0-9]{1,}$'))
lda$f1 = ifelse(lda$f1 %in% documents$FILE_NAME,lda$f1,str_remove(lda$f1,'.+--'))
lda$f2 = ifelse(lda$f2 %in% documents$FILE_NAME,lda$f2,str_remove(lda$f2,'.+--'))

lda = lda[a_id %in% projects$PROJECT_ID & b_id %in% projects$PROJECT_ID,]
lda = lda[f1 %in% documents$FILE_NAME & f2 %in% documents$FILE_NAME]
lda$agency1 = projects$AGENCY[match(lda$a_id,projects$PROJECT_ID)]
lda$agency2 = projects$AGENCY[match(lda$b_id,projects$PROJECT_ID)]
lda = lda[a_id == b_id,]
lda = lda[score>400,]

lda_solo = rbind(lda[,.(a_id,score,a)],lda[,.(b_id,score,b)],use.names = F)
lda_solo = lda_solo[!duplicated(a),]
lda_solo$AGENCY = projects$AGENCY[match(lda_solo$a_id,projects$PROJECT_ID)]
lda_solo = lda_solo[lda_solo$a_id %in% epa_record$EIS.Number,]
#lda_solo$AGENCY_SHORT = epa_record$Agency_Short[match(lda_solo$AGENCY,epa_record$Agency)]

countover400 = lda_solo[,.N,by = .(a_id)]
countover400$total_pages = page_counts$total_pages[match(countover400$a_id,page_counts$PROJECT_ID)]
setnames(countover400,'a_id','PROJECT_ID')
eis_page_counts = page_counts[PROJECT_ID %in% projects$PROJECT_ID[projects$PROJECT_TYPE=='EIS']]
eis_page_counts = left_join(eis_page_counts ,countover400)
eis_page_counts$AGENCY_SHORT = epa_record$Agency_Short[match(eis_page_counts$PROJECT_ID,epa_record$EIS.Number)]
eis_page_counts$N[is.na(eis_page_counts$N)]<-0

eis_page_counts$AGENCY_SHORT[grepl("Power",eis_page_counts$AGENCY_SHORT)] <- 'DOE'

eis_page_counts = data.table(eis_page_counts)
table(eis_page_counts$N<=eis_page_counts$total_pages)
eis_page_counts[N>total_pages,]


lda[a_id=='20130027',]

documents[PROJECT_ID==20170040,]
projects[PROJECT_ID==20170040,]
epa_record[EIS.Number==20170040,]
page_counts[PROJECT_ID==20170040,]


(ggplot(eis_page_counts,aes(x = fct_rev(AGENCY_SHORT),y = N/total_pages)) + geom_boxplot() + coord_flip() +
    theme_bw() + scale_y_continuous(name = '# pages w/ intr-doc LDA>400 / total pages by project') + 
    theme(axis.title.y = element_blank(),axis.text = element_text(size = 12)) + 
    ggtitle("Intraproject text reuse by EIS project and agency"))


lda_ea_solo = rbind(lda[,.(a_id,score,a)],lda[,.(b_id,score,b)],use.names = F)
lda_ea_solo = lda_ea_solo[!duplicated(a),]

subcounts = page_counts[PROJECT_ID %in% projects$PROJECT_ID[projects$AGENCY %in%c('Forest Service','Bureau of Land Management','Department of Energy')],]
subcounts$AGENCY = projects$AGENCY[match(subcounts$PROJECT_ID,projects$PROJECT_ID)]
subcounts$PROJECT_TYPE = projects$PROJECT_TYPE[match(subcounts$PROJECT_ID,projects$PROJECT_ID)]
lda_ea_solo = lda_ea_solo[a_id %in% subcounts$PROJECT_ID,]

ea_countover400 = lda_ea_solo[,.N,by = .(a_id)]
setnames(ea_countover400,'a_id','PROJECT_ID')
ea_countover400 = left_join(subcounts,ea_countover400)
ea_countover400$N[is.na(ea_countover400$N)] <- 0

ea_countover400$AGENCY[ea_countover400$AGENCY=='Department of Energy'] <- 'DOE'
ea_countover400$AGENCY[ea_countover400$AGENCY=='Forest Service'] <- 'FS'
ea_countover400$AGENCY[ea_countover400$AGENCY=='Bureau of Land Management'] <- 'BLM'

(ea_vs_eis_pagecount = ggplot(ea_countover400) + geom_boxplot(aes(x = AGENCY,y = total_pages,col = PROJECT_TYPE)) + coord_flip() +
    ggtitle("Pages analyzed") +
    theme_bw() + scale_y_continuous(name = '# pages by project') + 
    theme(axis.title.y = element_blank(),legend.position = c(0.8,0.4),legend.title = element_blank(),text = element_text(size = 12),legend.text = element_text(size = 14),axis.text = element_text(size = 12)) + 
    scale_color_brewer(labels = c('EA','EIS'),type = 'qual',palette = 2))

(ea_vs_eis_plot = ggplot(ea_countover400) + 
    geom_boxplot(aes(x = AGENCY,y = N/total_pages,col = PROJECT_TYPE)) + coord_flip() +
    ggtitle("Intradocument reuse") +
    theme_bw() + scale_y_continuous(name = '# pages w/ LDA>400 / total pages by project') + 
    theme(axis.text.y = element_blank(),axis.title.y = element_blank(),legend.position = c(0.8,0.4),legend.title = element_blank(),text = element_text(size = 12),legend.text = element_text(size = 14),axis.text = element_text(size = 12)) + 
    scale_color_brewer(labels = c('EA','EIS'),type = 'qual',palette = 2))

library(gridExtra)
lay <- rbind(c(1,1,1,1,1,2,2,2,2))
grid.arrange(ea_vs_eis_pagecount,ea_vs_eis_plot,ncol = 10,layout_matrix = lay)






