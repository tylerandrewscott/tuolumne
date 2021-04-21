pack = c("tidyverse","Matrix","statnet","data.table","stringr","lolog","textreuse","gridExtra","sf","lubridate","doParallel","htmlTable",'treemapify',"ggridges","ggplot2","viridis","hrbrthemes","forcats",'ggthemes','tm','pbapply')
need = !pack %in% installed.packages()[,"Package"]
#sudo apt-get install libfontconfig1-dev
if(any(need)){sapply(pack[need],install.packages)}
sapply(pack,require,character.only=T)

#update.packages(ask = FALSE, checkBuilt = TRUE)
runEISnet = TRUE
mcores = detectCores() - 2
samples = 10000
iters = 40
stepsize = 50


projects = fread('boilerplate_project/input/project_candidates_eis_only.csv')
projects$Title <- gsub('\"',"",projects$Title,fixed = T)
projects$clean.title = str_replace(projects$Title,'\\s{1,}',' ')
projects$DECISION = NA
projects$DECISION[grepl('\\WAMP\\W|RMP|(CONSERVATION|MANAGEMENT|LAND USE|FOREST|GRASSLAND|NOURISHMENT|ECOSYSTEMS|DEVELOPMENT|MASTER|TRAVEL) PLAN|TRAVEL MANAGEMENT|GENERIC|PLAN REVISION|PLAN AMENDMENT|CONTROL MANUAL',toupper(projects$clean.title))] <- 'Plan'
projects$DECISION[grepl('LEGISLATIVE|ACTIVITIES|DETERMINATION|REGULATORY|REGULATIONS|EXCHANGE|(READINESS|OIL AND GAS|GEOPHYSICAL) ACTIVITIES|RECAPITALIZATION|INVASIVE PLANT TREATMENT|PROGRAMMATIC|(\\W|^)PROGRAM(\\W|$)|PERMIT RENEWAL|\\WUSE OF\\W|POLICY|CONTRACT|RULE|MEASURES|PERMIT TO RELEASE|RISK MANAGEMENT|FORESTWIDE|REZONING|MANAGEMENT RESPONSE|TRAINING AND TESTING|RESIDUAL MANAGEMENT|LEASING|NATIONWIDE|WESTERN STATES|SYSTEM OPERATIONS|AIRFIELD OPERATIONS|DAMAGE REDUCTION|TRAINING AND OPERATIONS',toupper(projects$clean.title))] <- 'Program/policy'
#note: after coding the other instances of word "management", remainder are actually forest service _projects_ that sound like plans
#NOTE: 'plan of operation" connotes a case where new thing is built, eis studies project+intended operations
projects$DECISION[grepl('(US|I|SR)(-|\\s)[0-9]{1,3}|\\WPROJECT(S|)(\\W|$)|DRAWDOWN|COMMUNICATION SITE|SPECIFIC PLAN|CHANNEL|BRIDGE|ECOSYSTEM RESTORATION|TAMARISK REMOVAL|PRODUCTION|TRANSMISSION|\\WLOCKS\\W|MODERNIZATION|PLAN OF OPERATION|OPERATIONAL PLAN|OPERATIONS PLAN|STATION|EXPERIMENTAL REMOVAL|\\WMINE$|FREEWAY|DREDGING|DEEPWATER PORT|REFORESTATION|EXTENSION|REBUILD|REHABILITATION|LIGHT RAIL|DISPOSAL|CROSSING|RELOCATION|REMEDIATION|LEVEES|RESTORATION$|RECONSTRUCTION|CONVERSION|CLOSURE|IMPROVEMENT|PARKWAY|HIGHWAY|\\WTRAIN\\W|CORRIDOR|EXPANSION|MAINTENANCE|UPGRADE|SALE|(FUELS|VEGETATION|RESTORATION) MANAGEMENT|DISPOSITION|FEE(-|\\s)TO(-|\\s)TRUST|BEDDOWN|EVALUATION|FACILITY|\\WMINE\\W|PASSAGE',toupper(projects$clean.title))&is.na(projects$DECISION)] <- 'Project'
table(is.na(projects$DECISION)
sample(projects$clean.title[is.na(projects$DECISION)],10)

grep('ACTIVITIES',toupper(projects$clean.title),value = T)
grep("(US|I)(-|\\s)[0-9]{1,3}",projects$clean.title,value = T)


projects$clean.title[grepl('MANAGEMENT',toupper(projects$clean.title))&!grepl('PLAN',toupper(projects$clean.title))&is.na(projects$DECISION)]

grep('Feasibility',projects$clean.title,value = T)

projects$DECISION[grepl('\\WPERMIT(\\W|$)|\\WLICENSE(\\W|$)|\\WRENEWAL(\\W|$)|\\WLEASE(\\W|$)',toupper(projects$Title))] <- 'Permit/license/lease'

projects[grepl('MANAGEMENT$',toupper(clean.title))&is.na(DECISION),]

projects[grepl('MANAGEMENT$',toupper(clean.title))&AGENCY=='Forest Service',]



require(lineprof)
require(pryr)
mem_used()
rm(pages_list)
gc()
table(is.na(projects$DECISION))
projects[is.na(DECISION)]  %>%
  unnest_tokens(word, Title,token = 'ngrams',n=1) %>% count(word,sort = T) %>% head(10)

library(tm)
library(tidytext)
tidytext::unnest_tokens(projects$Title)

  unnest_tokens(bigram, text, token = "ngrams", n = 2)



rm(already)


projects[is.na(DECISION),.N,by=.(Agency)][order(-N)]

projects$Title[is.na(projects$DECISION)]
rm(wholefile)
gc()
projects$Title[grepl('\\WPROGRAM\\W|\\WPOLICY\\W',toupper(projects$Title))]
projects$Title[is.na(projects$DECISION)]



table(is.na(projects$DECISION))
projects$DECISION[grepl('PERMIT',toupper(projects$Title))]
grep('PERMIT',toupper(projects$Title),value = T)
table(grepl("PLAN",toupper(projects$Title)),grepl('PROGRAMMATIC',toupper(projects$Title)))
table(projects$DECISION,is.na(projects$DECISION))

table(projects$Agency)

epa_record = fread('scratch/eis_record_detail.csv')
epa_record = epa_record[epa_record$Document.Type=='Final',]

#epa_record[,.N,by = .(Agency)][order(-N)]
epa_record$Agency_Short = epa_record$Agency
epa_record$Agency_Short[epa_record$Agency=='Federal Energy Regulatory Commission'] <- 'FERC'
epa_record$Agency_Short[epa_record$Agency=="National Oceanic and Atmospheric Administration"] <- 'NOAA'
epa_record$Agency_Short[epa_record$Agency=="National Marine Fisheries Service"  ] <- 'NOAA'
epa_record$Agency_Short[epa_record$Agency=="National Park Service"] <- 'NPS'
epa_record$Agency_Short[epa_record$Agency=="Bureau of Land Management"] <- 'BLM'
epa_record$Agency_Short[epa_record$Agency=="Forest Service"] <- 'FS'
epa_record$Agency_Short[epa_record$Agency=="Bureau of Indian Affairs"] <- 'BIA'
epa_record$Agency_Short[epa_record$Agency=="United States Air Force"] <- 'USAF'
epa_record$Agency_Short[epa_record$Agency=="United States Navy"] <- 'USN'
epa_record$Agency_Short[epa_record$Agency=="U.S. Army Corps of Engineers"] <- 'ACOE'
epa_record$Agency_Short[epa_record$Agency=="Nuclear Regulatory Commission"] <- 'NRC'
epa_record$Agency_Short[epa_record$Agency=="Fish and Wildlife Service"] <- 'FWS'
epa_record$Agency_Short[epa_record$Agency=="Federal Highway Administration"] <- 'FHWA'
epa_record$Agency_Short[epa_record$Agency=="Federal Railroad Administration" ] <- 'FRA'

epa_record$Agency_Short[epa_record$Agency== "Department of Energy"   ] <- 'DOE'
epa_record$Agency_Short[epa_record$Agency== "United States Army"   ] <-  'US Army'
epa_record$Agency_Short[epa_record$Agency== "Federal Transit Administration"   ] <-  'FTA'
epa_record$Agency_Short[epa_record$Agency== "Bureau of Reclamation"   ] <-  'BR'
epa_record$Agency_Short[epa_record$Agency%in%c("Bonneville Power Administration","Western Area Power Administration")] <-  'DOE'


epa_record[,.N,by=.(Agency)][order(-N)]
#levels(fct_infreq(epa_record$Agency_Short))[1:18] 

old_epa_record = epa_record[grepl('^2001|^201[1-2]',epa_record$EIS.Number),]
epa_record = epa_record[grepl('^201[3-9]',epa_record$EIS.Number),]
epa_record$Agency_Short = fct_infreq(epa_record$Agency_Short) 

feis_2001_to_2012 <- old_epa_record[,.N,by=.(Agency_Short)]
projects = fread('scratch/boilerplate/project_candidates.csv')
projects$AGENCY[projects$AGENCY=='National Marine Fisheries Service']<-"National Oceanic and Atmospheric Administration"
projects = projects[!PROJECT_ID%in% c('20170008','20170006','DOI-BLM-MT-C030-2017-0013'),]

projects_used = unique(projects$PROJECT_ID)

epa_record[Agency_Short=='FS'&!(EIS.Number %in% projects$PROJECT_ID|EIS.Number %in% projects$MASTER_ID),]

eis_used = unique(projects$PROJECT_ID[projects$PROJECT_TYPE=='EIS'])


rds_list = lapply(list.files('scratch/boilerplate/big_text_files/',pattern = 'rds',full.names = T),readRDS)
flist_dt = rbindlist(rds_list)
rm(rds_list)
flist_dt$File = gsub('\\.pdf\\.txt$','.txt',flist_dt$File)
flist_dt$File <- gsub('_{2,}','_',flist_dt$File,perl = T)
flist_dt$PROJECT_ID = str_remove(flist_dt$File,'(--|_).*')
flist_dt$PID<-NA
file_dash = grepl('--',flist_dt$File)
flist_dt$PID[file_dash] = paste(flist_dt$File[file_dash],flist_dt$Page[file_dash],sep = '--')
flist_dt$PID[!file_dash] = paste(flist_dt$PROJECT_ID[!file_dash],flist_dt$File[!file_dash],flist_dt$Page[!file_dash],sep = '--')
flist_dt = flist_dt[!grepl('\\.{10,}',text,perl = T),]


docs = fread('scratch/boilerplate/document_candidates.csv')
docs$FILE_NAME = gsub('_{2,}','_',docs$FILE_NAME,perl = T)
docs = docs[PROJECT_ID!='DOI-BLM-NM-F010-2016-0001-EA'|FILE_NAME=='DOI-BLM-NM-F010-2016-0001-EA--2017-10-05_FINAL_January_2017_Lease_Sale_DOI-BLM-NM-F010-2016-0001_EA_FONSI.txt',]

flist_dt = flist_dt[File %in% docs$FILE_NAME,]


#docs = docs[PROJECT_ID %in% projects$PROJECT_ID[projects$AGENCY=='Bureau of Land Management'&projects$PROJECT_TYPE=='EA']]
#docs = docs[!grepl('Decision_Record|Proposed_Decision|DECISION_RECORD|(--|_)DR(\\W|_)',FILE_NAME),]

page_counts = flist_dt[,.N,by=.(PROJECT_ID)]
setnames(page_counts,'N','total_pages')
page_counts$MASTER_ID = projects$MASTER_ID[match(page_counts$PROJECT_ID,projects$PROJECT_ID)]
page_counts = page_counts[total_pages>3,]

flist_dt = flist_dt[PROJECT_ID %in% page_counts$PROJECT_ID,]
projects_used = unique(flist_dt$PROJECT_ID)
meta_dt = flist_dt[,.(Page,File,PROJECT_ID)]
#meta_dt = meta_dt[!meta_dt$File %in% dropfiles,]


projects[,.N,by=.(AGENCY)][order(-N)]
projects[,HAVE:=PROJECT_ID %in% meta_dt$PROJECT_ID]
count = projects[!duplicated(MASTER_ID),.N,by=.(AGENCY,PROJECT_TYPE,HAVE)]
eis_counts = dcast(count[PROJECT_TYPE=='EIS',],AGENCY~ PROJECT_TYPE + HAVE,value.var = 'N')
eis_counts$AGENCY = fct_reorder(eis_counts$AGENCY,.x = eis_counts$EIS_TRUE,.fun = max)
eis_counts[order(-AGENCY),]
setnames(eis_counts,'AGENCY','Agency')
epa_record = epa_record[!grepl('^ADOPT|^WITHD',Title),]
epa_record$Agency[grepl('Power Administration',epa_record$Agency)]<-'Department of Energy'

eis_counts  = merge(eis_counts,epa_record[,.N,by=.(Agency)],all.x=T)
eis_counts$Agency = as.factor(eis_counts$Agency)
eis_counts$Agency <- fct_reorder(eis_counts$Agency,.x = eis_counts$EIS_TRUE,.fun = min)
eis_counts$EIS_FALSE[is.na(eis_counts$EIS_FALSE)]<-0

eis_counts = eis_counts[order(-N),.(Agency,EIS_TRUE,N)]

tableout <-htmlTable(eis_counts)
outdir.tables = "output/boilerplate/tables/" 
sink(paste0(outdir.tables,"eis_count_table.html"))
print(tableout,type="html",useViewer=F)
sink()



projects[,HAVE:=PROJECT_ID %in% meta_dt$PROJECT_ID]
count = projects[!duplicated(MASTER_ID),.N,by=.(AGENCY,PROJECT_TYPE,HAVE)]
ea_counts = dcast(count[PROJECT_TYPE=='EA',],AGENCY~ PROJECT_TYPE + HAVE,value.var = 'N')
ea_counts$AGENCY = fct_reorder(ea_counts$AGENCY,.x = ea_counts$EA_TRUE,.fun = max)
ea_counts[order(-AGENCY),]
setnames(ea_counts,'AGENCY','Agency')
ea_counts$N = ea_counts$EA_FALSE + ea_counts$EA_TRUE
ea_counts = ea_counts[order(-N),.(Agency,EA_TRUE,N)]

tableout <-htmlTable(ea_counts)
outdir.tables = "output/boilerplate/tables/" 
sink(paste0(outdir.tables,"ea_count_table.html"))
print(tableout,type="html",useViewer=F)
sink()


#meta_dt = flist_dt[,.(File,Page)]
#meta_dt$PROJECT_ID = str_remove(meta_dt$File,'(_|--).*')


#projects[AGENCY%in%c('Bureau of Land Management','Forest Service','Department of Energy'),.N,by = .(PROJECT_TYPE,AGENCY)]
#project_candidates[AGENCY%in%c('Bureau of Land Management','Forest Service','Department of Energy'),.N,by = .(PROJECT_TYPE,AGENCY)]
lda_result_files = list.files(pattern  = 'result_V2\\.RDS')
lda_list = lapply(lda_result_files,readRDS)
lda = rbindlist(lda_list)
lda$score = as.numeric(lda$score)
lda = lda[!duplicated(lda),]
lda = lda[score>=300,]
lda$a = gsub('^(((?!--).)+-{2})\\1','\\1',lda$a,perl = T)
lda$b = gsub('^(((?!--).)+-{2})\\1','\\1',lda$b,perl = T)
lda = lda[a!=b,]
lda = lda[!duplicated(lda),]
rm(lda_list);gc()

lda$a_page = as.numeric(gsub('.*--','',lda$a,perl = T))
lda$b_page = as.numeric(gsub('.*--','',lda$b,perl = T))
lda$a_id = gsub('(--|_(?!States)]).*','',lda$a,perl = T)
lda$b_id = gsub('(--|_(?!States)]).*','',lda$b,perl = T)
lda = lda[b %in% flist_dt$PID|a %in% flist_dt$PID,]

lda = lda[a_id %in% projects_used & b_id %in% projects_used,]
lda$agency1 = projects$AGENCY[match(lda$a_id,projects$PROJECT_ID)]
lda$agency2 = projects$AGENCY[match(lda$b_id,projects$PROJECT_ID)]

lda = lda[a %in% flist_dt$PID & b %in% flist_dt$PID,]

lda$a_file = flist_dt$File[match(lda$a,flist_dt$PID)]
lda$b_file = flist_dt$File[match(lda$b,flist_dt$PID)]



# test = lda[score>300&score<350,]
# test[agency1=='Federal Energy Regulatory Commission'&agency2 == 'Federal Energy Regulatory Commission']
aa = flist_dt$text[flist_dt$PID=='20130046--20130046_Mid_Fork_American_FEIS_FERC_2079.txt--308']
bb = flist_dt$text[flist_dt$PID=='20180175--20180175_Final_Environmental_Impact_Statement_for_the_Lassen_Lodge_Hydroelectric_Project-P-12496-002.txt--177']
align_local(aa,bb)
# 
# test = lda[score>600&score<750,]
# test[agency1=='Federal Energy Regulatory Commission'&agency2 == 'Federal Energy Regulatory Commission']
aa = flist_dt$text[flist_dt$PID=='20170148--20170148_VolumeI_FEIS_MXPGXP_20170713.txt--551']
bb = flist_dt$text[flist_dt$PID=='20180144--20180144_MIDSHIP_Project_Final_EIS_Vol_I.txt--295']
align_local(aa,bb)


lda = lda[a_id != b_id,]
lda_eis = lda[a_id %in% eis_used & b_id %in% eis_used,]
lda_solo = rbind(lda_eis[,.(a_id,score,a,a_file)],lda_eis[,.(b_id,score,b,b_file)],use.names = F)
lda_solo$AGENCY = projects$AGENCY[match(lda_solo$a_id,projects$PROJECT_ID)]
#lda_solo = lda_solo[!a_file %in% badfiles,]
lda_solo$a_file = gsub('\\s','_',lda_solo$a_file)
lda_solo$a_file = gsub('\\.pdf\\.txt$','.txt',lda_solo$a_file)
lda_solo$a = gsub('\\.pdf\\.txt','.txt',lda_solo$a)

over300 = lda_solo[order(-score),][!duplicated(a),]
countover300 = over300[,.N,by = .(a_id,AGENCY)]
setnames(countover300,'a_id','PROJECT_ID')
setnames(countover300,'N','over300')
page_counts$AGENCY = projects$AGENCY[match(page_counts$PROJECT_ID,projects$PROJECT_ID)]
countover300 = merge(countover300,page_counts[PROJECT_ID %in% projects$PROJECT_ID[projects$PROJECT_TYPE=='EIS']],all = T)
countover300$AGENCY_SHORT = epa_record$Agency_Short[match(countover300$AGENCY,epa_record$Agency)]
countover300$over300[is.na(countover300$over300)] <- 0

agency_summary_stats = countover300[,list(mean(over300/total_pages),median(over300/total_pages)),by = .(AGENCY_SHORT)]
names(agency_summary_stats) <- c('AGENCY_SHORT','mean','median')
agency_summary_stats$mean = round(agency_summary_stats$mean,3)
agency_summary_stats$median = round(agency_summary_stats$median,3)

(gg_pages_over_300 = ggplot() + 
    #  geom_jitter(data = countover300,aes(x = fct_rev(AGENCY_SHORT),y = over300/total_pages),pch = 21,colour = 'grey50') + 
    geom_boxplot(data = countover300,aes(x = fct_rev(AGENCY_SHORT),y = over300/total_pages),fill = NA) + coord_flip() +
    #    ggplot2::annotate("label",x = fct_rev(agency_summary_stats$AGENCY_SHORT),y = agency_summary_stats$median*1.5,label = agency_summary_stats$median) + 
    theme_bw() + scale_y_continuous(name = '# pages w/ LDA>300 / total pages by project',limits=c(0,1)) + 
    theme(axis.title.y = element_blank(),axis.text = element_text(size = 12)) + ggtitle("Text reuse between EISs by project and agency"))

ggsave(gg_pages_over_300,filename = 'output/boilerplate/figures/agencies_lda_over_300.png',dpi = 300,units = 'in',height = 4.5,width = 6)
saveRDS(countover300,'scratch/boilerplate/eis_between_over300_pagecount.RDS')


lda_ea_solo = rbind(lda[,.(a_id,score,a,a_file)],lda[,.(b_id,score,b,b_file)],use.names = F)
lda_ea_solo$AGENCY = projects$AGENCY[match(lda_ea_solo$a_id,projects$PROJECT_ID)]
lda_ea_solo = lda_ea_solo[AGENCY %in% c('Forest Service','Bureau of Land Management','Department of Energy'),]

ea_over300 = lda_ea_solo
ea_over300 = ea_over300[order(-score),][!duplicated(a),]
#ea_over300 = ea_over300[!a_file%in%str_remove(badfiles,'.*--'),]
ea_countover300 = ea_over300[,.N,by = .(a_id,AGENCY)]
setnames(ea_countover300,c('a_id','N'),c('PROJECT_ID','over300'))
ea_countover300 = data.table(ea_countover300)

ea_countover300 = merge(ea_countover300,page_counts[PROJECT_ID %in% projects$PROJECT_ID[projects$AGENCY %in% c('Department of Energy','Forest Service','Bureau of Land Management')],],all = T)
ea_countover300$over300[is.na(ea_countover300$over300)] <- 0


ea_countover300$AGENCY[ea_countover300$AGENCY=='Department of Energy'] <- 'DOE'
ea_countover300$AGENCY[ea_countover300$AGENCY=='Forest Service'] <- 'FS'
ea_countover300$AGENCY[ea_countover300$AGENCY=='Bureau of Land Management'] <- 'BLM'
ea_countover300$PROJECT_TYPE = projects$PROJECT_TYPE[match(ea_countover300$PROJECT_ID,projects$PROJECT_ID)]


ea_countover300[,median(total_pages),by=.(AGENCY,PROJECT_TYPE)]
ea_countover300[,median(over300/total_pages),by=.(AGENCY,PROJECT_TYPE)]

ea_countover300[PROJECT_TYPE=='EA'&total_pages>3000,]

ea_countover300[,list(median(total_pages)),by=.(AGENCY,PROJECT_TYPE)]
ea_countover300[,list(median(over300/total_pages)),by=.(AGENCY,PROJECT_TYPE)]


(ea_vs_eis_pagecount = ggplot(ea_countover300) + geom_boxplot(aes(x = AGENCY,y = total_pages,col = PROJECT_TYPE)) + coord_flip() +
    ggtitle("Pages analyzed") +
    theme_bw() + scale_y_continuous(name = '# pages by project') + 
    theme(axis.title.y = element_blank(),legend.position = c(0.8,0.4),
          legend.title = element_blank(),text = element_text(size = 10)) + 
    scale_color_brewer(labels = c('EA','EIS'),type = 'qual',palette = 2))


(ea_vs_eis_plot = ggplot(ea_countover300) + 
    geom_boxplot(aes(x = AGENCY,y = over300/total_pages,col = PROJECT_TYPE)) + coord_flip() +
    ggtitle("High reuse between projects") +
    theme_bw() + scale_y_continuous(name = 'LDA>300 / total pages') + 
    theme(axis.title.y = element_blank(),legend.position = c(0.8,0.4),
          legend.title = element_blank(),text = element_text(size = 10))+ 
    scale_color_brewer(labels = c('EA','EIS'),type = 'qual',palette = 2))

lay <- rbind(c(1,1,1,1,1,2,2,2,2))
ggsave(grid.arrange(ea_vs_eis_pagecount,ea_vs_eis_plot,ncol = 2),units = 'in',width = 6,height = 4,dpi = 300,filename = 'output/boilerplate/figures/case_ea_vs_eis_over300.png')

saveRDS(ea_countover300,'scratch/boilerplate/ea_between_over300_pagecount.RDS')


# sample_score = 300
# show_example = lda[score>sample_score&a_id!=b_id,][a_id %in% projects$PROJECT_ID[projects$PROJECT_TYPE=='EIS'] & b_id %in% projects$PROJECT_ID[projects$PROJECT_TYPE=='EIS'],]
# library(htmlTable)
# head(show_example)
# #sm = sample(1:nrow(show_exm))
# sm = sample(which(show_example$score>550&show_example$score<600 & show_example$agency1!='Bureau of Land Management'),1)

#> show_example$a[sm]
#[1] "20130046--20130046_Mid Fork American FEIS FERC 2079.pdf--335"
#> show_example$b[sm]
#[1] "20130378--20130378_Toledo Bend Hydro Project FEIS.pdf--199"
# file1 = fread(paste0(text_storage,show_example[sm,]$f1),sep = '\t')
# file2 = fread(paste0(text_storage,show_example[sm,]$f2),sep = '\t')
# id1 = file1$PROJECT_ID[1]
# id2 = file2$PROJECT_ID[1]
# page1 = file1[Project_File_Par==show_example[sm,]$a,]
# page2 = file2[Project_File_Par==show_example[sm,]$b,]

#> show_example$a[9869]
#[1] "20160104--20160104_08a_Chapter_3_Affected_Environment_and_Environmental_Consequences_Part 1.pdf--75"
#> show_example$b[9869]
#[1] "20170008--20170008_008a_Ch3_AffectEnv-EnvConseq.pdf--73"

# > page1$Project_File_Par
# [1] "20130252--20130252_Hawaii-Southern_California_Training_and_Testing_Final_EIS-OEIS_Vol_I.pdf--132"
# > page2$Project_File_Par
# [1] "20150281--20150281_Northwest Training and Testing Final EIS-OEIS Volume 1.pdf--126"
# 
# sample_local = align_local(page1$text,page2$text)
# side_by_side = cbind(sample_local$a_edits,sample_local$b_edits)
# colnames(side_by_side) <- c(paste(epa_record$Agency[epa_record$EIS.Number==id1],epa_record$Title[epa_record$EIS.Number==id1],sep = ': '),
#                             paste(epa_record$Agency[epa_record$EIS.Number==id2],epa_record$Title[epa_record$EIS.Number==id2],sep = ': '))
# htmlTable(side_by_side,caption = paste0('LDA score = ',sample_local$score))

# countn = lda[,.N,by = .(a_id,b_id)]
# score90q = lda[,quantile(score,0.90),by = .(a_id,b_id)]
# score95q = lda[,quantile(score,0.95),by = .(a_id,b_id)]
# scoreMax = lda[,max(score),by = .(a_id,b_id)]
# align800 = lda[score>800,][!duplicated(paste(a_id,b_id)),]
text_ids = unique(flist_dt$PROJECT_ID)
projects = projects[!duplicated(projects)&PROJECT_ID %in% text_ids,]
eis_used = projects[PROJECT_TYPE == 'EIS',]
doe_used = projects[AGENCY == 'Department of Energy',]
blm_used = projects[AGENCY == 'Bureau of Land Management',]
usfs_used = projects[AGENCY == 'Forest Service',]

lda_eis = lda[a_id %in% eis_used$PROJECT_ID&b_id %in% eis_used$PROJECT_ID,]
lda_eis = lda_eis[a_id != b_id,]
lda_eis$agencyA = projects$AGENCY[match(lda_eis$a_id,projects$PROJECT_ID)]
lda_eis$agencyB = projects$AGENCY[match(lda_eis$b_id,projects$PROJECT_ID)]
# 


# agency_avgs = rbindlist(lapply(unique(c(lda_eis$agencyA,lda_eis$agencyB)),function(x)
#   data.table(Agency = x,
#              Comps = sum(lda_eis$agencyA==x|lda_eis$agencyB==x),
#              q25 = quantile(lda_eis$score[lda_eis$agencyA==x|lda_eis$agencyB==x],0.25),
#              q75 = quantile(lda_eis$score[lda_eis$agencyA==x|lda_eis$agencyB==x],0.75),
#              Avg_Score = mean(lda_eis$score[lda_eis$agencyA==x|lda_eis$agencyB==x]),
#              Median_Score = median(lda_eis$score[lda_eis$agencyA==x|lda_eis$agencyB==x]),
#              Min_Score = min(lda_eis$score[lda_eis$agencyA==x|lda_eis$agencyB==x]),
#              Max_Score = max(lda_eis$score[lda_eis$agencyA==x|lda_eis$agencyB==x]))))
# 
# agency_avgs$Agency_Short = epa_record$Agency_Short[match(agency_avgs$Agency,epa_record$Agency)]
# pages = fread('scratch/pages_by_project.csv')
# pages$Agency = epa_record$Agency[match(pages$pid,epa_record$EIS.Number)]
# pages_by_agency = pages[,sum(N),by = .(Agency)]
# agency_avgs$Total_Pages = pages_by_agency$V1[match(agency_avgs$Agency,pages_by_agency$Agency)]

# ggplot(agency_avgs,aes(x = Comps)) + 
#   geom_segment(aes(x = Comps,xend=Comps,y = q25,yend= q75)) + 
#   geom_point(aes(y = Median_Score)) 
# require(htmlTable)
# require(ggrepel)
# htmlTable(agency_avgs[,.(Agency,Total_Pages,Comps,Min_Score,round(Avg_Score),floor(Median_Score),Max_Score)])
# 
# ggplot(data = agency_avgs,aes(x = Avg_Score,y = Comps,label = Agency_Short)) + 
#   geom_label_repel()

lda_doe = lda[a_id %in% doe_used$PROJECT_ID&b_id %in% doe_used$PROJECT_ID,]
lda_doe = lda_doe[a_id != b_id,]
lda_blm = lda[a_id %in% blm_used$PROJECT_ID&b_id %in% blm_used$PROJECT_ID,]
lda_blm = lda_blm[a_id != b_id,]
lda_usfs = lda[a_id %in% usfs_used$PROJECT_ID&b_id %in% usfs_used$PROJECT_ID,]
lda_usfs = lda_usfs[a_id != b_id,]

n_eis = nrow(eis_used)
n_blm = nrow(blm_used)
n_usfs = nrow(usfs_used)
n_doe = nrow(doe_used)

eis_ids = sort(eis_used$PROJECT_ID)
blm_ids = sort(blm_used$PROJECT_ID)
doe_ids = sort(doe_used$PROJECT_ID)
usfs_ids = sort(usfs_used$PROJECT_ID)

eis_blank_mat = matrix(0,ncol = n_eis,nrow = n_eis,dimnames = list(eis_ids,eis_ids))
doe_blank_mat = matrix(0,ncol = n_doe,nrow = n_doe,dimnames = list(doe_ids,doe_ids))
blm_blank_mat = matrix(0,ncol = n_blm,nrow = n_blm,dimnames = list(blm_ids,blm_ids))
usfs_blank_mat = matrix(0,ncol = n_usfs,nrow = n_usfs,dimnames = list(usfs_ids,usfs_ids))


geo = readRDS(paste0('scratch/','ngaz_matches_V3.RDS'))
geo = geo[!is.na(PRIM_LONG_DEC)&!is.na(PRIM_LAT_DEC),]
geo = geo[PRIM_LAT_DEC>16,]
geo$PRIM_LONG_DEC[geo$PRIM_LONG_DEC>170] <- geo$PRIM_LONG_DEC[geo$PRIM_LONG_DEC>170] * -1

project_centroids = geo[,list(mean(PRIM_LONG_DEC),mean(PRIM_LAT_DEC)),by = .(PROJECT_ID)]

blm_ea_centroids = project_centroids[PROJECT_ID %in% projects$PROJECT_ID[projects$AGENCY=='Bureau of Land Management'&projects$PROJECT_TYPE=='EA'],]

blm_ea_centroids = left_join(blm_ea_centroids,ea_countover300)

us_states  = tigris::states(class = 'sf',year = 2015)
us_states = us_states[!us_states$STUSPS %in%  c('VI','MP','GU','AS'),]
# 
# ggplot() + geom_sf(data = us_states,fill = NA,lwd = 0.2) + 
#   scale_y_continuous(expand = c(0,0)) + 
#   scale_x_continuous(limits=c(-180,-50),expand = c(0,0)) + 
#   geom_point(data = blm_ea_centroids,aes(x = V1,y = V2,color = 100 * N/total_pages),alpha = 0.5) +
#   scale_color_viridis_c(name = '% high reuse',direction = -1) + theme_map()
# 

# 
gg_centroid = ggplot() + 
  geom_sf(data = us_states,fill = NA,lwd = 0.2) + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(limits=c(-180,-50),expand = c(0,0)) + 
  geom_point(pch = 19,alpha=0.2,size = 0.5,data = project_centroids,aes(x = V1,y = V2)) + theme_map() +
  theme_map() +
  ggtitle("Estimated project centroids from geotagging") + theme(text = element_text(size = 12)) +  
  NULL
gg_centroid

project_dist = dist(as.matrix(project_centroids[order(PROJECT_ID),.(V1,V2)]),upper = T,diag = F)
dist_mat = log(as.matrix(project_dist)+0.001)
project_centroids$PROJECT_ID -> rownames(dist_mat) -> colnames(dist_mat)


# 
# ent_loc = 'scratch/boilerplate/project_name_entities/'
# fl = list.files(ent_loc,full.names = T)
#  library(data.table)
#  library(pbapply)
#  library(rvest)
# ents = pblapply(fl,fread)
# cls = lapply(ents,class)
# redo = which(sapply(cls,length)!=2)
# for (r in redo){
# ents[[r]]<-fread(paste0(ent_loc,fl[r]))
# }
# ent_dt = rbindlist(ents,use.names=T,fill=T)
# fwrite(ent_dt,'scratch/all_entity_datatable.csv')


ent_dt = fread('scratch/all_entity_datatable.csv')
ent_dt$name[grepl('STRATIFIED ENVIRONMENTAL (&|AND) ARCHAEOLOGICAL',toupper(ent_dt$name))] <- "STRATIFIED ENVIRONMENTAL & ARCHAEOLOGICAL SERVICES, LLC"
ent_dt$name[grepl('WALSH ENVIRON',toupper(ent_dt$name))] <- "WALSH ENVIRONMENTAL SCIENTISTS AND ENGINEERS, LLC"
ent_dt$name[grepl('KERLINGER',toupper(ent_dt$name))] <- "CURRY & KERLINGER, LLC"
ent_dt$name[grepl('SOLV LLC',toupper(ent_dt$name))|toupper(ent_dt$name)=='SOLV'] <- 'SOLVE, LLC'
ent_dt$name[grepl('GRETTE ASSOC',toupper(ent_dt$name))] <- "GRETTE ASSOCIATES, LLC"
ent_dt$name[grepl('^HELIA ENV',toupper(ent_dt$name))] <- "HELIA ENVIRONMENTAL, LLC"
ent_dt$name[grepl('PEAK ECOLOG',toupper(ent_dt$name))] <- "PEAK ECOLOGICAL SERVICES, LLC"
ent_dt$name[grepl('ALPINE GEOPHYSICS',toupper(ent_dt$name))] <- "ALPINE GEOPHYSICS, LLC"
ent_dt$name[grepl("HAYDEN(\\s|-)WING",toupper(ent_dt$name))]<- "HAYDEN-WING ASSOCIATES, LLC"
ent_dt$name[grepl("WILD TROUT ENTERPRISES",toupper(ent_dt$name))] <- "WILD TROUT ENTERPRISES, LLC"
ent_dt$name[grepl("ZEIGLER GEOLOGIC",toupper(ent_dt$name))] <- "ZEIGLER GEOLOGIC CONSULTING, LLC"
ent_dt$name[grepl("NORTHERN LAND USE",toupper(ent_dt$name))] <- "NORTHERN LAND USE RESEARCH ALASKA, LLC"
ent_dt$name[grepl("LEGACY LAND",toupper(ent_dt$name))] <- "LEGACY LAND & ENVIRONMENTAL SOLUTIONS, LLC"
ent_dt$name[grepl("SOUTHWEST GEOPHY",toupper(ent_dt$name))] <- 'SOUTHWEST GEOPHYSICAL CONSULTING, LLC'
ent_dt$name[grepl("SYRACUSE ENVIRONMENTAL RESEARCH ASSOCIATES",toupper(ent_dt$name))] <- "SYRACUSE ENVIRONMENTAL RESEARCH ASSOCIATES"
ent_dt$name[grepl("ANDERSON PERRY (&|AND) ASSOCIATES",toupper(ent_dt$name))] <- "ANDERSON PERRY & ASSOCIATES, INC." 
ent_dt$name[grepl("VEATCH",toupper(ent_dt$name))] <- "BLACK & VEATCH" 
ent_dt$name[grepl("TETRA TECH",toupper(ent_dt$name))] <- "TETRA TECH INC." 
ent_dt$name[grepl("FLUOR",toupper(ent_dt$name))] <- "FLUOR CORP." 
ent_dt$name[grepl('\\bERM\\b',ent_dt$name)] <- "ERM" 
ent_dt$name[grepl('\\bKiewit\\b',ent_dt$name)] <- "KIEWIT CORP." 
ent_dt$name[grepl('\\bArcadis\\b',ent_dt$name)] <- "ARCADIS NV" 
ent_dt$name[grepl('\\bECC\\b',ent_dt$name)] <- "ECC" 
ent_dt$name[grepl('\\bMWH\b',ent_dt$name)] <- "MWH CONSTRUCTORS, INC." 
ent_dt$name[grepl("MOTT MACDONALD",toupper(ent_dt$name))] <- "MOTT MACDONALD" 
ent_dt$name[grepl("AECOM",toupper(ent_dt$name))] <- "AECOM" 
ent_dt$name[grepl("RAMBOLL",toupper(ent_dt$name))] <- "RAMBOLL"
ent_dt$name[grepl("GOLDER ASSOCIATES",toupper(ent_dt$name))] <-"GOLDER ASSOCIATES CORP."
other_consultants =  c("STRATIFIED ENVIRONMENTAL & ARCHAEOLOGICAL SERVICES, LLC",
                       "WALSH ENVIRONMENTAL SCIENTISTS AND ENGINEERS, LLC",
                       "CURRY & KERLINGER, LLC",
                       'SOLVE, LLC',
                       "GRETTE ASSOCIATES, LLC",
                       "HELIA ENVIRONMENTAL, LLC",
                       "PEAK ECOLOGICAL SERVICES, LLC",
                       "ALPINE GEOPHYSICS, LLC",
                       "HAYDEN-WING ASSOCIATES, LLC",
                       "WILD TROUT ENTERPRISES, LLC",
                       "ZEIGLER GEOLOGIC CONSULTING, LLC",
                       "NORTHERN LAND USE RESEARCH ALASKA, LLC",
                       "LEGACY LAND & ENVIRONMENTAL SOLUTIONS, LLC",
                       'SOUTHWEST GEOPHYSICAL CONSULTING, LLC',
                       "ALPINE ENVIRONMENTAL CONSULTANTS, LLC",
                       "SYRACUSE ENVIRONMENTAL RESEARCH ASSOCIATES",
                       "NUTTER AND ASSOCIATES",
                       "GARCIA AND ASSOCIATES","HORIZON ENVIRONMENTAL SERVICES, INC.",
                       "WESTERN ECOSYSTEMS TECHNOLOGY, INC.",
                       "ANDERSON PERRY & ASSOCIATES, INC." )

ent_dt$type[ent_dt$name %in% other_consultants]<-'ORGANIZATION'
ent_dt$type[grepl("CONSULTING",toupper(ent_dt$name))]<-'ORGANIZATION'

#### make author matrix ####
people = ent_dt[type == 'PERSON',]
people = people[!grepl('Louis Berger|CDM S',name),]
people = people[!grepl('\\W[a-z]',name),]
exclude = c('Coordinator','Wildlife Biologist','Native American','Public Involvement PERSONS','Air Resource Specialist','Wild Horse')
badstrings = c('Applicant','Appendix','Economic','Analysis','Locator','Botanist','Native','Register','FR Doc','Resident','Population','Hydro','Botany','Specialist','Planner','Biologist','Analyst','Recreation','[0-9]','Friends','Geologic','Heritage','Petroleum','Advocates','CD-IV','Scientist','Science','Associates','Biological','Manager',"Director",'Coordinator','Basin','Creek','Habitat')
people = people[!name %in% exclude,]
people = people[!grepl(paste(badstrings,collapse='|'),name),]
people = people[!grepl('[A-Z]{4,}',name),]
people = people[!grepl('\\.$',name),]
people = people[!grepl(paste(paste0('(^|\\W)',state.abb,'\\W'),collapse = '|'),name),]
people = people[!duplicated(people),]
people = people[!name %in% people[,.N,by = .(name)][N==1,]$name,]
author_matrix = as.matrix(table(people$name,people$PROJECT_ID))
notin = projects$PROJECT_ID[!projects$PROJECT_ID%in% colnames(author_matrix)]
addin = matrix(0,nrow = nrow(author_matrix),ncol = length(notin),dimnames = list(rownames(author_matrix),sort(notin)))
author_matrix_full = cbind(author_matrix,addin)

author_matrix_full = author_matrix_full[rowSums(author_matrix_full)>1,]
author_triplet =  slam::as.simple_triplet_matrix(author_matrix_full)
project_to_project_author_matrix = slam::crossprod_simple_triplet_matrix(author_triplet)


#ent_dt[type=='PERSON',.N,by=.(name)][order(-N),][1:200]
#ent_loc = 'scratch/boilerplate/project_name_entities/'
#rds_preps = readRDS('scratch/boilerplate/preparer_pages.rds')
#rds_preps = rds_preps[!duplicated(rds_preps),]

### make consult matrix ####
orgs = ent_dt[type == 'ORGANIZATION'|grepl('Louis Berger|CDM S|Battelle',name),]
orgs$name[grepl('Battelle',orgs$name)]<-'Battelle'

#firm_names_css = 'td:nth-child(5)'
#  enr2019_1 = 'https://www.enr.com/toplists/2019-Top-200-Environmental-Firms-1'
#  enr2019_2 = 'https://www.enr.com/toplists/2019-Top-200-Environmental-Firms-2'
#  enr2018_1 = 'https://www.enr.com/toplists/2018-Top-200-Environmental-Firms-1'
#  enr2018_2 = 'https://www.enr.com/toplists/2018-Top-200-Environmental-Firms-2'
#  enr2017_1 = 'https://www.enr.com/toplists/2017-Top-200-Environmental-Firms-1'
#  enr2017_2 = 'https://www.enr.com/toplists/2017-Top-200-Environmental-Firms-2'
#  des2019_1 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms1'
#  des2019_2 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms2'
#  des2019_3 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms3'
#  des2019_4 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms4'
#  des2019_5 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms5'
# # 
# des2017_1 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms1'
#  des2017_2 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms2'
#  des2017_3 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms3'
#  des2017_4 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms4'
#  des2017_5 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms5'
# # 
# # 
#  firm_names_css_2015 = 'td:nth-child(2)'
#  enr2015_1 = 'https://www.enr.com/toplists/2015_Top_200_Environmental_Firms1'
#  enr2015_2 = 'https://www.enr.com/toplists/2015_Top_200_Environmental_Firms1'
# # 
# # 
#  des2015_1 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms1'
#  des2015_2 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms2'
#  des2015_3 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms3'
#  des2015_4 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms4'
#  des2015_5 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms5'
# # 
#  firm_lists = ls(pattern = 'des201[579]|enr201[579]')
# # 
#  firm_scrapes = pblapply(firm_lists,function(x) {if(grepl('2015',x)){
#    read_html(get(x))%>% html_nodes(firm_names_css_2015) %>% html_text(trim=T)}
#    else{read_html(get(x))%>% html_nodes(firm_names_css) %>% html_text(trim=T)}
#    },cl = 1)
#saveRDS(firm_scrapes,'scratch/enr_firm_lists.RDS')

firm_scrapes = readRDS('scratch/enr_firm_lists.RDS')
nms = unlist(firm_scrapes)
clean_names = unique(str_remove(nms,'\\,.+'))
clean_names = unique(str_remove(clean_names,'\\s\\(.+'))
clean_names = gsub('|','\\|',clean_names,fixed = T)
clean_names = gsub('&','\\&',clean_names,fixed = T)
#length(unique(toupper(clean_names)))
#orgs$name = orgs$name
orgs$name[grep('CDM\\b|CDM SM',orgs$name)] <- 'CDM SMITH'
orgs$NAME = toupper(orgs$name)

# 
#

#  mcores = 1
#  match1 = pblapply(clean_names,function(x) {grep(paste0('\\b',x,'\\b'),orgs$name)})
#  match2 = pblapply(tm::removePunctuation(clean_names),function(x) grep(paste0('\\b',x,'\\b'),orgs$name),cl = mcores)
#  match3 = pblapply(str_remove(clean_names,'Inc\\.$|LLC$|LLC\\.$|Corp\\.$'),function(x) grep(paste0('\\b',x,'\\b'),orgs$name),cl = mcores)
#  match1A = pblapply(toupper(clean_names),function(x) {grep(paste0('\\b',x,'\\b'),orgs$NAME)},cl = mcores)
#  match2B = pblapply(tm::removePunctuation(toupper(clean_names)),function(x) grep(paste0('\\b',x,'\\b'),orgs$NAME),cl = mcores)
#  match3C = pblapply(str_remove(toupper(clean_names),'INC\\.$|LLC$|LLC\\.$|CORP\\.$'),function(x) grep(paste0('\\b',x,'\\b'),orgs$NAME),cl = mcores)
# # 
# # # # # # 
# name_matches = mapply(function(m1,m2,m3,m4,m5,m6) unique(c(m1,m2,m3,m4,m5,m6)),m1 = match1,m2 = match2,m3 = match3,m4 = match1A,m5 = match2B,m6 = match3C)
#      consultant_project_matches = pblapply(seq_along(name_matches),function(x) {data.table(PROJECT_ID = orgs$PROJECT_ID[name_matches[[x]]],FIRM = clean_names[x])})
#      saveRDS(consultant_project_matches,'scratch/consultant_project_matches.RDS')

consultant_project_matches = readRDS('scratch/consultant_project_matches.RDS')
consults = rbindlist(consultant_project_matches)[!is.na(PROJECT_ID)]
consults$FIRM = toupper(consults$FIRM)
consults = consults[!duplicated(consults),]

handcoded_firms = orgs[NAME %in% other_consultants,.(PROJECT_ID,NAME)]
setnames(handcoded_firms,'NAME','FIRM')
autocoded_firms = orgs[grepl('CONSULTING|CONSULTANTS',NAME),.(PROJECT_ID,NAME)]
setnames(autocoded_firms,'NAME','FIRM')

consults = rbindlist(list(consults,handcoded_firms,autocoded_firms))
consults = consults[!duplicated(consults),]



consults = consults[!FIRM%in%c('NELSON','LITTLE','JOHNSON','WOOD','ENGLAND'),]
consults$FIRM[consults$FIRM=='ICF'] <- 'ICF INTERNATIONAL'
consults$FIRM[grepl('LEIDOS',consults$FIRM)] <- 'LEIDOS INC.'
consults$FIRM[grepl('PARSONS',consults$FIRM)] <- 'PARSONS CORP'
consults$FIRM[grepl('CARDNO',consults$FIRM)] <- 'LEIDOS INC.'
consults$FIRM[grepl('STANTEC',consults$FIRM)] <- 'STANTEC INC.'
consults$FIRM[grepl('PARAMETRIX',consults$FIRM)] <- 'PARAMETRIX INC.'
consults$FIRM[grepl('SWCA ENVIRONMENTAL',consults$FIRM)] <- 'SWCA ENVIRONMENTAL CONSULTANTS'
consults$FIRM[grepl('CH2M',consults$FIRM)] <- 'CH2M HILL'
consults = consults[FIRM!='WHITMAN',]
consults = consults[FIRM!='STEWART',]
consults = consults[FIRM!='PAGE',]
consult_Freq = consults[PROJECT_ID %in% epa_record$EIS.Number,][,.N,by = .(FIRM)][order(-N)][N>=5,]

#consults[PROJECT_ID %in% epa_record$EIS.Number,][,.N,by = .(FIRM)][order(-N)][1:25,]

outdir.tables = "output/boilerplate/tables/" 
tableout <-htmlTable(consult_Freq)
outdir.tables = "output/boilerplate/tables/" 
sink(paste0(outdir.tables,"consultant_table.html"))
print(tableout,type="html",useViewer=F)
sink()


consult_matrix = as.matrix(table(consults$FIRM,consults$PROJECT_ID))
notin = projects$PROJECT_ID[!projects$PROJECT_ID%in% colnames(consult_matrix)]
addin = matrix(0,nrow = nrow(consult_matrix),ncol = length(notin),dimnames = list(rownames(consult_matrix),sort(notin)))

consult_matrix_full = cbind(consult_matrix,addin)
consult_triplet =  slam::as.simple_triplet_matrix(consult_matrix_full)
project_to_project_consult_matrix = slam::crossprod_simple_triplet_matrix(consult_triplet)



lit_dt = fread('input/metadata/nepa_case_filings_2001_2012.csv')
lit_dt$AGENCY_SHORT[lit_dt$AGENCY_SHORT=='FHA']<-'FHWA'
#lit = fread('input/metadata/NEPA litigation - Sheet1.csv',na.strings = 'NA')
#lit_dt = melt(lit)
#lit_dt = lit_dt[,sum(value,na.rm=T),by = .(agency,variable)]
#projects$AGENCY_SHORT = epa_record$Agency_Short[match(projects$PROJECT_ID,epa_record$EIS.Number)]
#lit_dt$variable = gsub('y','',lit_dt$variable)
#lit_dt[order(agency,variable),litigation2001topresent:=cumsum(V1),by=.(agency)]
#lit_dt[order(agency,variable),litigation2001ToPriorYear:=lag(litigation2001topresent),by=.(agency)]
#lit_dt = lit_dt[variable==2013,]
#lit_dt = lit_dt[agency %in% epa_record$Agency_Short,]
lit_dt$FEISs_during_Period = feis_2001_to_2012$N[match(lit_dt$AGENCY_SHORT,feis_2001_to_2012$Agency_Short)]
lit_dt$litigation_per_feis = lit_dt$Case_Filings_2001_2012/lit_dt$FEISs_during_Period
projects[PROJECT_TYPE=='EIS',.N,by=.(AGENCY)]


ggplot(lit_dt[order(litigation_per_feis),],aes(x = FEISs_during_Period,y = litigation_per_feis)) + ggtitle('NEPA litigations, 2001 to 2012') + 
  geom_text(aes(label = AGENCY_SHORT)) + ggtitle('FEISs and NEPA case filings from 2001 to 2012') + ylab('filings/FEIS published') + xlab('FEISs published')


epa_record[,.N,by=.(Agency)][order(-N)]


library(lubridate)
#if(runEISnet){ 
#eis_p_list = pblapply(seq(2,20,2),function(p) {
eis_p_list = lapply(seq(2,100,2),function(p){
  print(p)
  # ######### EIS analysis #########
  s = 300
  tcounts = lda_eis[score>s & a_id!=b_id,.N,by = .(a_id,b_id)]
  eis_nodes = sort(eis_ids)
  eis_net = network.initialize(n = length(eis_nodes),directed = F,bipartite = F,hyper = F,multiple = F,loops = F)
  network.vertex.names(eis_net) <- eis_nodes
  edgelist = tcounts
  edgelist = edgelist[N>=p,]
  network::add.edges.network(eis_net,tail = match(edgelist$a_id,network.vertex.names(eis_net)),
                             head = match(edgelist$b_id,network.vertex.names(eis_net)),names.eval = 'Score_Count',vals.eval = edgelist$N)
  
  in_epa = network.vertex.names(eis_net) %in% epa_record$EIS.Number
  
  network.vertex.names(eis_net)[!in_epa] <- projects$MASTER_ID[match(network.vertex.names(eis_net)[!in_epa],  projects$PROJECT_ID)]
  
  eis_net %v% 'Agency' <- as.character(epa_record$Agency[match(network.vertex.names(eis_net),epa_record$EIS.Number)])
  eis_net %v% 'Agency_Short' <- as.character(epa_record$Agency_Short[match(network.vertex.names(eis_net),epa_record$EIS.Number)])
  eis_net %v% 'Fed_Reg_Date' <- decimal_date(mdy(epa_record$Federal.Register.Date[match(network.vertex.names(eis_net),epa_record$EIS.Number)]))
  eis_net %v% 'Year' <- (eis_net %v% 'Fed_Reg_Date')-2013
  eis_net %v% 'EIS' <- network.vertex.names(eis_net)
  eis_net %v% 'ln_Pages_Hashed' <- log(page_counts$total_pages[match(eis_net%v%'vertex.names',page_counts$MASTER_ID)])
  
  eis_net %v% 'NEPA_litigation_per_FEIS_2001_2012' = lit_dt$litigation_per_feis[match(eis_net %v% 'Agency_Short', lit_dt$AGENCY_SHORT)]
  set.vertex.attribute(eis_net,attrname='NEPA_litigation_per_FEIS_2001_2012',v = which(is.na(eis_net %v% 'NEPA_litigation_per_FEIS_2001_2012')),value = 0)
  # eis_net %v% 'sqrt_NEPA_litigation_2001_2012'  <- sqrt(eis_net %v% 'NEPA_litigation_2001_2012' )
  eis_net %v% 'Consultant' <- network.vertex.names(eis_net) %in% consults$PROJECT_ID
  eis_distance_matrix = dist_mat[rownames(dist_mat) %in% {eis_net %v% 'vertex.names'},colnames(dist_mat) %in% {eis_net %v% 'vertex.names'}]
  eis_net_sub = eis_net
  eis_net_sub = delete.vertices(eis_net_sub,which(!{eis_net %v% 'vertex.names'} %in% rownames(eis_distance_matrix)))
  
  eis_dist_index = match(eis_net_sub %v% 'vertex.names',rownames(eis_distance_matrix))
  eis_distance_matrix = eis_distance_matrix[eis_dist_index,eis_dist_index]
  
  eis_consult_matrix = project_to_project_consult_matrix[rownames(project_to_project_consult_matrix) %in%  eis_ids,colnames(project_to_project_consult_matrix) %in%  eis_ids]
  rownames(eis_consult_matrix) = colnames(eis_consult_matrix) = projects$MASTER_ID[match(rownames(eis_consult_matrix),projects$PROJECT_ID)]
  
  eis_consult_index = match(eis_net %v% 'vertex.names',rownames(eis_consult_matrix))
  eis_consult_matrix = eis_consult_matrix[eis_consult_index,eis_consult_index]
  eis_consult_sub_index = match(eis_net_sub %v% 'vertex.names',rownames(eis_consult_matrix))
  eis_consult_sub_matrix = eis_consult_matrix[eis_consult_sub_index,eis_consult_sub_index]
  
  eis_author_matrix = project_to_project_author_matrix[rownames(project_to_project_author_matrix) %in% eis_ids,colnames(project_to_project_author_matrix) %in% eis_ids]
  rownames(eis_author_matrix) = colnames(eis_author_matrix) = projects$MASTER_ID[match(rownames(eis_author_matrix),projects$PROJECT_ID)]
  eis_author_index = match(eis_net %v% 'vertex.names',rownames(eis_author_matrix))
  eis_author_matrix = eis_author_matrix[eis_author_index,eis_author_index]
  eis_author_sub_index = match(eis_net_sub %v% 'vertex.names',rownames(eis_author_matrix))
  eis_author_sub_matrix = eis_author_matrix[eis_author_sub_index,eis_author_sub_index]
  
  # ##### fit eis_net lolog model ####
  # cl = makeCluster(mcores)
  #  registerDoParallel(cl)
  eis_order = rank(eis_net %v% 'EIS')
  temp_eis_net = eis_net
  if(p == 10){eis_master_net <<- eis_net}
  # delete.edges(temp_eis_net,  which(get.edge.value(temp_eis_net,'Score_Count')<p))
  #tapply(degree(temp_eis_net,gmode = 'graph'),temp_eis_net %v% 'Agency_Short',mean)
  mod_temp_eis = lolog(temp_eis_net ~ edges + 
                         nodeCov('ln_Pages_Hashed') + 
                         nodeMatch('Agency_Short') +
                         nodeFactor('Consultant')+
                         nodeCov('NEPA_litigation_per_FEIS_2001_2012') +
                         absDiff('Fed_Reg_Date') +
                         edgeCov(eis_consult_matrix>0,name = 'Same_Consultant') +
                         edgeCov(eis_author_matrix>0,name = 'Preparers')|eis_order,
                       nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)#,cluster = cl)
  #saveRDS(mod_temp_eis,paste0('scratch/boilerplate/lolog_model_objects/eis_lolog_p',p,".RDS"))
  # 
  # 
  temp_eis_net_sub = eis_net_sub
  if(p == 10){eis_master_net_sub <<- eis_net_sub}
  #delete.edges(temp_eis_net_sub,  which(get.edge.value(temp_eis_net_sub,'Score_Count')<p))
  eis_sub_order = rank(temp_eis_net_sub %v% 'EIS')
  mod_temp_eis_dist = lolog(temp_eis_net_sub ~ edges + 
                              nodeCov('ln_Pages_Hashed') + 
                              nodeMatch('Agency_Short') + 
                              nodeFactor('Consultant')+
                              nodeCov('NEPA_litigation_per_FEIS_2001_2012') +
                              absDiff('Fed_Reg_Date') +
                              edgeCov(eis_consult_sub_matrix>0,name = 'Same_Consultant')  + 
                              edgeCov(eis_author_sub_matrix>0,name = 'Preparers') + 
                              edgeCov(eis_distance_matrix,name = 'Distance')|eis_sub_order,
                            nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)#,cluster = cl)
  #saveRDS(mod_temp_eis_dist,paste0('scratch/boilerplate/lolog_model_objects/eis_lolog_dist_p',p,".RDS"))
  # 
  # 
  
  mod_temp_eis_dist_interaction = lolog(temp_eis_net_sub ~ edges + 
                                          nodeCov('ln_Pages_Hashed') + 
                                          nodeMatch('Agency_Short') + 
                                          nodeFactor('Consultant')+
                                          nodeCov('NEPA_litigation_per_FEIS_2001_2012') +
                                          absDiff('Fed_Reg_Date') +
                                          edgeCov(eis_consult_sub_matrix>0,name = 'Same_Consultant')  + 
                                          edgeCov(eis_author_sub_matrix>0,name = 'Preparers') + 
                                          edgeCov(eis_distance_matrix,name = 'Distance') +
                                          edgeCov(eis_distance_matrix * (eis_author_sub_matrix>0),name = 'DistanceXAuthor')|eis_sub_order,
                                        nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)#,cluster = cl)
  # saveRDS(mod_temp_eis_dist_interaction ,paste0('scratch/boilerplate/lolog_model_objects/eis_lolog_dist_interaction_p',p,".RDS"))
  
  m1 = (summary(mod_temp_eis)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 1)
  m2 = (summary(mod_temp_eis_dist)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 2)
  m3 = (summary(mod_temp_eis_dist_interaction)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 3)
  eis_mresults = rbindlist(list(m1,m2,m3))
  eis_mresults$DV = 'EIS'
  
  eis_mresults$coef = gsub('.*\\.','',eis_mresults$coef)
  eis_mresults$coef = fct_inorder(eis_mresults$coef)
  eis_mresults$coef = fct_rev(eis_mresults$coef)
  
  eis_mresults$coef = fct_recode(eis_mresults$coef,'litigations per FEIS, 2001-2012' = 'NEPA_litigation_per_FEIS_2001_2012','Used consultants' = 'Consultant',
                                 '|publication date diff.|' = 'Fed_Reg_Date','ln(pages)' = 'ln_Pages_Hashed','Same consultants' = 'Same_Consultant')
  eis_mresults$p = p
  eis_mresults
})


eis_net_descriptives = data.table(size = network.size(eis_master_net),edgecount = network.edgecount(eis_master_net),density = network.density(eis_master_net))

sapply(list.vertex.attributes(eis_master_net),function(x) summary(eis_master_net %v% x))


313/(475+313)
exp(6.313)

eis_dt = rbindlist(eis_p_list)
eis_dt$coef <- fct_rev(eis_dt$coef)
eis_dt$coef = fct_recode(eis_dt$coef, 'Same agency' = 'Agency_Short','Used consultant'='1')
eis_dt[,sig:=ifelse(theta-1.96 * se < 0 & theta+1.96*se > 0,0,1)]

gg_sensitivity = ggplot(eis_dt[coef!='edges'&mod!=3,][order(-coef)]) + 
  geom_errorbar(aes(x = p,ymin = theta-1.96 * se,ymax = theta+1.96*se)) + 
  geom_point(aes(x = p,y = theta,fill = as.factor(sig),size = as.factor(sig)),pch = 21,size = 0.5) + 
  facet_wrap(~coef,scales = 'fixed') + 
  scale_fill_manual(values = c('white','black'),name = '95% interval spans 0',labels=c('yes','no')) + 
  theme_bw() + geom_vline(aes(xintercept = 10,col = 'thresh'),lty = 2) + geom_hline(yintercept = 0,lty = 3) + 
  theme(text = element_text(family = 'Times',size = 10),legend.position = c(0.85,0.15),
        legend.box = "vertical") + 
  scale_color_manual(labels = 'threshold for main models',name = '',values = 'black') + 
  scale_size_manual(values = c(0.5,1))+
  scale_x_continuous(name = '# pages where LDA > 300',breaks = c(0,10,25,50,75,100)) +
  scale_y_continuous(name = '95% confidence interval (additive log-odds')+
  ggtitle('Parameter estimates by threshold used to assign tie between projects')
gg_sensitivity
ggsave(gg_sensitivity,filename = 'output/boilerplate/figures/threshold_sensitivity.png',dpi = 300,width = 7,height =  6,units = 'in')



eis_dt$coef =  fct_rev(eis_dt$coef)
eis_dt[,sig:=ifelse(theta - 1.96 * se <0 & theta + 1.96 * se>0,0,1)]

eis_dt$mod_sig = paste0(eis_dt$mod,eis_dt$sig)
eis_dt[coef!='edges'&p==10&mod!=3,]

(eis_grob = ggplot(eis_dt[coef!='edges'&p==10&mod!=3,],aes(group = as.factor(mod))) + 
    geom_hline(yintercept = 0,lty = 2,col = 'grey50') + 
    geom_errorbar(aes(ymin = theta - 1.96 * se,ymax = theta + 1.96 * se,x = coef,col = as.factor(mod)),
                  width = 0.2,lwd = .75,position = position_dodge(width = .3)) + 
    geom_point(aes(y = theta,x= coef,
                   col = as.factor(mod)),position = position_dodge2(width = .3),shape = 19,size = .75) + 
    geom_point(data = eis_dt[coef!='edges'&p==10&mod!=3&sig==0,],fill = 'white',
               aes(y = theta,x= coef,
                   col = as.factor(mod)),position = position_dodge2(width = .3),shape = 21,size = .75) + 
    theme(axis.text.y = element_blank()) + ggtitle('Predicting high reuse between EISs') + 
    scale_y_continuous(name = '95% CI (additive log-odds)') + 
    coord_flip() + theme_bw() +
    scale_color_colorblind(name = 'model',labels = c('all EISs','w/ geolocation')) + 
    scale_fill_manual(values = c(NA,'white',NA))+
    theme(legend.position = c(0.8,0.25),axis.title.y = element_blank(),legend.background = element_rect(fill = NA),
          text = element_text(size = 10,family = 'Times')) +
    guides(shape = FALSE,fill = F) + 
    NULL)

ggsave(plot = eis_grob,filename = 'output/boilerplate/figures/eis_lolog.png',width = 5,height = 3.4,units = 'in',dpi = 300)

#modlist = list(mod_temp_eis,mod_temp_eis_dist,mod_temp_eis_dist_interaction )



coef_dt = eis_dt[p==10&mod!=3,]
coef_dt[,CI:=paste0(sprintf("%.3f", round(theta,3)),' (',sprintf("%.3f", round(se,3)),')')]
coef_dt$coef = fct_drop(coef_dt$coef,'DistanceXAuthor')
coef_dtcoef = fct_inorder(coef_dt$coef)

coef_dt$stars = ifelse(coef_dt$pvalue <0.001,"***",ifelse(coef_dt$pvalue<0.01,"**",ifelse(coef_dt$pvalue<0.05,"*",'')))
coef_dt$CI  =  paste0(coef_dt$CI,coef_dt$stars)

coef_cast =  dcast(coef_dt[,.(mod,coef,CI)],coef~mod)
coef_cast = coef_cast[order(fct_rev(coef)),]
outdir.tables = "output/boilerplate/tables/" 
tableout <-htmlTable(coef_cast)
outdir.tables = "output/boilerplate/tables/" 
sink(paste0(outdir.tables,"eis_lolog_coefs.html"))
print(tableout,type="html",useViewer=F)
sink()

if(runUSFSnet){
  ######### USFS analysis #########
  s = 300
  p = 10
  usfs_meta = fread('scratch/forest_service_project_detail.csv',na.strings = "")
  tcounts = lda_usfs[score>s & a_id!=b_id,.N,by = .(a_id,b_id)]
  usfs_nodes = sort(usfs_ids)
  usfs_net = network.initialize(n = length(usfs_nodes),directed = F,bipartite = F,hyper = F,multiple = F,loops = F)
  network.vertex.names(usfs_net) <- usfs_nodes
  edgelist = tcounts
  
  ap = page_counts$total_pages[match(edgelist$a_id,page_counts$PROJECT_ID)]
  bp = page_counts$total_pages[match(edgelist$b_id,page_counts$PROJECT_ID)]
  edgelist = edgelist[N>=p|N==ap|N==bp,]
  network::add.edges.network(usfs_net,tail = match(edgelist$a_id,network.vertex.names(usfs_net)),
                             head = match(edgelist$b_id,network.vertex.names(usfs_net)),names.eval = 'Score_Count',vals.eval = edgelist$N)
  network.density(usfs_net)
  network.size(usfs_net) * {network.size(usfs_net)-1} / 2
  
  #delete.edges(usfs_net,  which(get.edge.value(usfs_net,'Score_Count')<p))
  usfs_net %v% 'EA_Date' <- decimal_date(ymd(usfs_meta$`Decision Signed Date`[match(usfs_net %v% 'vertex.names',usfs_meta$Proj_Num)]))
  usfs_net %v% 'EIS_Date' <- decimal_date(mdy(epa_record$Federal.Register.Date[match(usfs_net %v% 'vertex.names',epa_record$EIS.Number)]))
  usfs_net %v% 'PROJECT_TYPE' <- ifelse({usfs_net %v% 'vertex.names'} %in% epa_record$EIS.Number,'EIS','EA')
  usfs_net %v% 'Date' = ifelse({usfs_net %v% 'PROJECT_TYPE'}=='EIS',usfs_net %v% 'EIS_Date',usfs_net %v% 'EA_Date')
  
  usfs_net %v% 'ln_Pages_Hashed' <- log(page_counts$total_pages[match(usfs_net%v%'vertex.names',page_counts$PROJECT_ID)])
  usfs_net %v% 'Year' <- floor(usfs_net %v% 'Date') - 2013  
  usfs_net %v% 'PROJECT_ID' <- network.vertex.names(usfs_net)
  usfs_net %v% 'Consultant' <- network.vertex.names(usfs_net) %in% consults$PROJECT_ID
  usfs_distance_matrix = dist_mat[rownames(dist_mat) %in% {usfs_net %v% 'vertex.names'},colnames(dist_mat) %in% {usfs_net %v% 'vertex.names'}]
  usfs_net_sub = usfs_net
  usfs_net_sub = delete.vertices(usfs_net_sub,which(!{usfs_net %v% 'vertex.names'} %in% rownames(usfs_distance_matrix)))
  usfs_dist_index = match(usfs_net_sub %v% 'vertex.names',rownames(usfs_distance_matrix))
  usfs_distance_matrix = usfs_distance_matrix[usfs_dist_index,usfs_dist_index]
  
  usfs_consult_matrix = project_to_project_consult_matrix[rownames(project_to_project_consult_matrix) %in% {usfs_net %v% 'vertex.names'},colnames(project_to_project_consult_matrix) %in% {usfs_net %v% 'vertex.names'}]
  usfs_consult_index = match(usfs_net %v% 'vertex.names',rownames(usfs_consult_matrix))
  usfs_consult_matrix = usfs_consult_matrix[usfs_consult_index,usfs_consult_index]
  usfs_consult_sub_index = match(usfs_net_sub %v% 'vertex.names',rownames(usfs_consult_matrix))
  usfs_consult_sub_matrix = usfs_consult_matrix[usfs_consult_sub_index,usfs_consult_sub_index]
  
  usfs_author_matrix = project_to_project_author_matrix[rownames(project_to_project_author_matrix) %in% {usfs_net %v% 'vertex.names'},colnames(project_to_project_author_matrix) %in% {usfs_net %v% 'vertex.names'}]
  usfs_author_index = match(usfs_net %v% 'vertex.names',rownames(usfs_author_matrix))
  usfs_author_matrix = usfs_author_matrix[usfs_author_index,usfs_author_index]
  usfs_author_sub_index = match(usfs_net_sub %v% 'vertex.names',rownames(usfs_author_matrix))
  usfs_author_sub_matrix = usfs_author_matrix[usfs_author_sub_index,usfs_author_sub_index]
  
  ##### fit usfs_net lolog model ####
  
  # cl = makeCluster(mcores)
  #  registerDoParallel(cl)
  usfs_order = rank(usfs_net %v% 'Date')
  
  temp_usfs_net = usfs_net
  require(statnet)
  
  tform = temp_usfs_net ~ edges +  
    nodeFactor('PROJECT_TYPE') + 
    nodeCov('ln_Pages_Hashed') + 
    nodeFactor('Consultant') +
    absDiff('Date') + 
    edgeCov(usfs_consult_matrix>0,name = 'Same_Consultants')  + 
    edgeCov(usfs_author_matrix>0,name = 'Preparers')|usfs_order
  
  mod_usfs_temp = lolog(tform,
                        nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)
  # 
  saveRDS(mod_usfs_temp,'scratch/boilerplate/lolog_model_objects/usfs_lolog_p10.RDS')
  
  temp_usfs_net_sub = usfs_net_sub
  #delete.edges(temp_usfs_net_sub,  which(get.edge.value(temp_usfs_net_sub,'Score_Count')<p))
  usfs_sub_order = rank(temp_usfs_net_sub %v% 'Date')
  
  tform = temp_usfs_net_sub ~ edges + 
    nodeFactor('PROJECT_TYPE') +
    nodeCov('ln_Pages_Hashed') + 
    nodeFactor('Consultant') +
    absDiff('Date') +
    edgeCov(usfs_consult_sub_matrix>0,name = 'Same_Consultants')  +
    edgeCov(usfs_author_sub_matrix>0,name = 'Preparers') +
    edgeCov(usfs_distance_matrix,name = 'Distance')|usfs_sub_order
  mod_usfs_temp_dist = lolog(tform,
                             nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)
  saveRDS(mod_usfs_temp_dist,'scratch/boilerplate/lolog_model_objects/usfs_lolog_dist_p10.RDS')
  # 
  tform=temp_usfs_net_sub ~ edges +
    nodeFactor('PROJECT_TYPE') +
    nodeCov('ln_Pages_Hashed') + 
    nodeFactor('Consultant') +
    absDiff('Date') +
    edgeCov(usfs_consult_sub_matrix>0,name = 'Same_Consultants')  +
    edgeCov(usfs_author_sub_matrix>0,name = 'Preparers') +
    edgeCov(usfs_distance_matrix,name = 'Distance') +
    edgeCov(usfs_distance_matrix * (usfs_author_sub_matrix>0),name = 'DistanceXAuthor')|usfs_sub_order
  mod_usfs_temp_dist_interaction = lolog(tform,
                                         nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)
  saveRDS(mod_usfs_temp_dist_interaction,'scratch/boilerplate/lolog_model_objects/usfs_lolog_dist_interaction_p10.RDS')
  # stopCluster(cl)
  
  m1 = (summary(mod_usfs_temp)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 1)
  m2 = (summary(mod_usfs_temp_dist)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 2)
  m3 = (summary(mod_usfs_temp_dist_interaction)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 3)
  usfs_mresults = rbindlist(list(m1,m2,m3))
  usfs_mresults$DV = 'USFS'
}

usfs_net_descriptives = data.table(size = network.size(usfs_net),edgecount = network.edgecount(usfs_net),density = network.density(usfs_net))

usfs_mresults$coef[grepl('PROJECT_TYPE',usfs_mresults$coef)] <- 'EIS'
usfs_mresults$coef = gsub('.*\\.','',usfs_mresults$coef)
usfs_mresults$coef = fct_inorder(usfs_mresults$coef)
usfs_mresults$coef = fct_rev(usfs_mresults$coef)

(usfs_grob = ggplot(usfs_mresults[coef!='edges',],aes(group = as.factor(mod))) + 
    geom_hline(yintercept = 0,lty = 2,col = 'grey50') + 
    geom_errorbar(aes(ymin = theta - 1.96 * se,ymax = theta + 1.96 * se,x = coef),width = 0.2,position = position_dodge(width = 0.25)) + 
    geom_point(aes(y = theta,x= coef),position = position_dodge2(width = 0.25)) + 
    theme(axis.text.y = element_blank()) + 
    coord_flip() + theme_bw() + #scale_color_colorblind() + 
    NULL)

if(runBLMnet){
  # 
  # ######### BLM analysis #########
  s = 300
  p = 10
  blm_meta = fread('scratch/blm_project_record.csv',na.strings = "")
  blm_doc = fread('scratch/blm_document_record.csv',na.strings = "")
  tcounts = lda_blm[score>s & a_id!=b_id,.N,by = .(a_id,b_id)]
  blm_nodes = sort(blm_ids)
  
  blm_net = network.initialize(n = length(blm_nodes),directed = F,bipartite = F,hyper = F,multiple = F,loops = F)
  network.vertex.names(blm_net) <- blm_nodes
  edgelist = tcounts
  ap = page_counts$total_pages[match(edgelist$a_id,page_counts$PROJECT_ID)]
  bp = page_counts$total_pages[match(edgelist$b_id,page_counts$PROJECT_ID)]
  edgelist = edgelist[N>=p|N==ap|N==bp,]
  tail = match(edgelist$a_id,network.vertex.names(blm_net))
  head = match(edgelist$b_id,network.vertex.names(blm_net))
  network::add.edges.network(blm_net,tail = tail,head = head,names.eval = 'Score_Count',vals.eval = edgelist$N)
  # delete.edges(blm_net,  which(get.edge.value(blm_net,'Score_Count')<p))
  network.density(blm_net)
  network.size(blm_net) * {network.size(blm_net)-1} / 2
  blm_net %v% 'Date' <- decimal_date(mdy(blm_meta$`Decision Date`[match(blm_net %v% 'vertex.names',blm_meta$`NEPA #`)]))
  blm_net %v% 'Year' <- as.numeric(str_extract({blm_net %v% 'vertex.names'},'20[0-9]{2}'))
  blm_net %v% 'Date'  = ifelse(is.na(blm_net %v% 'Date'), blm_net %v% 'Year',blm_net%v% 'Date')
  blm_net %v% 'Year' <- floor(blm_net %v% 'Date') - 2013  
  blm_net %v% 'ln_Pages_Hashed' <- log(page_counts$total_pages[match(blm_net%v%'vertex.names',page_counts$PROJECT_ID)])
  blm_net %v% 'PROJECT_ID' <- network.vertex.names(blm_net)
  blm_net %v% 'PROJECT_TYPE' <- ifelse(grepl('^[0-9]{8}',blm_net %v% 'vertex.names'),'EIS','EA')
  blm_net %v% 'Consultant' <- network.vertex.names(blm_net) %in% consults$PROJECT_ID
  blm_distance_matrix = dist_mat[rownames(dist_mat) %in% {blm_net %v% 'vertex.names'},colnames(dist_mat) %in% {blm_net %v% 'vertex.names'}]
  blm_net_sub = blm_net
  blm_net_sub = delete.vertices(blm_net_sub,which(!{blm_net %v% 'vertex.names'} %in% rownames(blm_distance_matrix)))
  blm_dist_index = match(blm_net_sub %v% 'vertex.names',rownames(blm_distance_matrix))
  blm_distance_matrix = blm_distance_matrix[blm_dist_index,blm_dist_index]
  
  blm_consult_matrix = project_to_project_consult_matrix[rownames(project_to_project_consult_matrix) %in% {blm_net %v% 'vertex.names'},colnames(project_to_project_consult_matrix) %in% {blm_net %v% 'vertex.names'}]
  blm_consult_index = match(blm_net %v% 'vertex.names',rownames(blm_consult_matrix))
  blm_consult_matrix = blm_consult_matrix[blm_consult_index,blm_consult_index]
  blm_consult_sub_index = match(blm_net_sub %v% 'vertex.names',rownames(blm_consult_matrix))
  blm_consult_sub_matrix = blm_consult_matrix[blm_consult_sub_index,blm_consult_sub_index]
  # 
  blm_author_matrix = project_to_project_author_matrix[rownames(project_to_project_author_matrix) %in% {blm_net %v% 'vertex.names'},colnames(project_to_project_author_matrix) %in% {blm_net %v% 'vertex.names'}]
  blm_author_index = match(blm_net %v% 'vertex.names',rownames(blm_author_matrix))
  blm_author_matrix = blm_author_matrix[blm_author_index,blm_author_index]
  blm_author_sub_index = match(blm_net_sub %v% 'vertex.names',rownames(blm_author_matrix))
  blm_author_sub_matrix = blm_author_matrix[blm_author_sub_index,blm_author_sub_index]
  # 
  
  # ##### fit blm_net lolog model ####
  # 
  # cl = makeCluster(mcores)
  #registerDoParallel(cl)
  blm_order = rank(blm_net %v% 'Date')
  
  temp_net_blm = blm_net
  mod_temp_blm = lolog(temp_net_blm ~ edges + 
                         nodeFactor('PROJECT_TYPE') + 
                         nodeCov('ln_Pages_Hashed') + 
                         nodeFactor('Consultant')+
                         absDiff('Date') + 
                         edgeCov(blm_consult_matrix>0,name = 'Same_Consultants')  + edgeCov(blm_author_matrix>0,name = 'Preparers')|blm_order,
                       nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)
  # 
  saveRDS(mod_temp_blm,'scratch/boilerplate/lolog_model_objects/blm_lolog_p10.RDS')
  
  temp_net_blm_sub = blm_net_sub
  #delete.edges(temp_net_blm_sub,  which(get.edge.value(temp_net_blm_sub,'Score_Count')<p))
  blm_sub_order = rank(temp_net_blm_sub %v% 'Date')
  mod_temp_blm_dist = lolog(temp_net_blm_sub ~ edges + 
                              nodeFactor('PROJECT_TYPE') + 
                              nodeCov('ln_Pages_Hashed') + 
                              nodeFactor('Consultant')+
                              absDiff('Date') + 
                              edgeCov(blm_consult_sub_matrix>0,name = 'Same_Consultants')  + edgeCov(blm_author_sub_matrix>0,name = 'Preparers') + edgeCov(blm_distance_matrix,name = 'Distance')|blm_sub_order,
                            nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize,cluster = cl)
  saveRDS(mod_temp_blm_dist,'scratch/boilerplate/lolog_model_objects/blm_lolog_dist_p10.RDS')
  
  mod_temp_blm_dist_interaction = lolog(temp_net_blm_sub ~ edges + 
                                          nodeFactor('PROJECT_TYPE') + 
                                          nodeCov('ln_Pages_Hashed') + 
                                          nodeFactor('Consultant')+
                                          absDiff('Date') + 
                                          edgeCov(blm_consult_sub_matrix>0,name = 'Same_Consultants')  + 
                                          edgeCov(blm_author_sub_matrix>0,name = 'Preparers') + 
                                          edgeCov(blm_distance_matrix,name = 'Distance') +
                                          edgeCov(blm_distance_matrix * (blm_author_sub_matrix>0),name = 'DistanceXAuthor')|blm_sub_order,
                                        nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)
  saveRDS(mod_temp_blm_dist_interaction,'scratch/boilerplate/lolog_model_objects/blm_lolog_dist_interaction_p10.RDS')
  
  # # 
  
  m1 = (summary(mod_temp_blm)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 1)
  m2 = (summary(mod_temp_blm_dist)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 2)
  m3 = (summary(mod_temp_blm_dist_interaction)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 3)
  blm_mresults = rbindlist(list(m1,m2,m3))
  blm_mresults$DV = 'BLM'
  
}

blm_mresults$coef[grepl('PROJECT_TYPE',blm_mresults$coef)] <- 'EIS'
blm_mresults$coef = gsub('.*\\.','',blm_mresults$coef)
blm_mresults$coef = fct_inorder(blm_mresults$coef)
blm_mresults$coef = fct_rev(blm_mresults$coef)

(blm_grob = ggplot(blm_mresults[coef!='edges',],aes(group = as.factor(mod))) + 
    geom_hline(yintercept = 0,lty = 2,col = 'grey50') + 
    geom_errorbar(aes(ymin = theta - 1.96 * se,ymax = theta + 1.96 * se,x = coef),width = 0.2,position = position_dodge(width = 0.25)) + 
    geom_point(aes(y = theta,x= coef),position = position_dodge2(width = 0.25)) + 
    theme(axis.text.y = element_blank()) + 
    coord_flip() + theme_bw() + #scale_color_colorblind() + 
    NULL)


if(runDOEnet){
  # ######### DOE analysis #########
  s = 300
  p = 3
  tcounts = lda_doe[score>s & a_id!=b_id,.N,by = .(a_id,b_id)]
  doe_meta = readRDS('scratch/doe_nepa_record.RDS')
  library(spacyr)
  
  doe_meta = readRDS('scratch/doe_nepa_document_record.RDS')
  doe_nodes = sort(doe_ids)
  doe_net = network.initialize(n = length(doe_nodes),directed = F,bipartite = F,hyper = F,multiple = F,loops = F)
  network.vertex.names(doe_net) <- doe_nodes
  edgelist = tcounts
  ap = page_counts$total_pages[match(edgelist$a_id,page_counts$PROJECT_ID)]
  bp = page_counts$total_pages[match(edgelist$b_id,page_counts$PROJECT_ID)]
  edgelist = edgelist[N>=p|N==ap|N==bp,]
  
  network::add.edges.network(doe_net,tail = match(edgelist$a_id,network.vertex.names(doe_net)),
                             head = match(edgelist$b_id,network.vertex.names(doe_net)),names.eval = 'Score_Count',vals.eval = edgelist$N)
  network.density(doe_net)
  
  network.size(doe_net) * {network.size(doe_net)-1} / 2
  
  doe_pdf_dates = fread('scratch/pdf_created_date.csv')
  doe_pdf_dates$PROJECT_ID = str_remove(doe_pdf_dates$File,'--.*')
  doe_pdf_dates = doe_pdf_dates[ymd_hms(Created_Date)<ymd('2025-01-01'),]
  created_date = doe_pdf_dates[,max(ymd_hms(Created_Date),na.rm=T),by=.(PROJECT_ID)]
  doe_net %v% 'Date' <- decimal_date(mdy(epa_record$Federal.Register.Date[match(network.vertex.names(doe_net),epa_record$EIS.Number)]))
  rid = which(is.na(doe_net %v% 'Date'))
  new_dates =  decimal_date(ymd_hms(created_date$V1[match(network.vertex.names(doe_net)[rid],created_date$PROJECT_ID)]))
  set.vertex.attribute(doe_net,'Date',value = new_dates,v = rid)
  
  doe_net %v% 'Year' <- (doe_net %v% 'Date')-2013
  doe_net %v% 'ln_Pages_Hashed' <- log(page_counts$total_pages[match(doe_net%v%'vertex.names',page_counts$PROJECT_ID)])
  doe_net %v% 'doe' <- network.vertex.names(doe_net)
  doe_net %v% 'PROJECT_TYPE' <- ifelse(grepl('^[0-9]{8}',doe_net %v% 'vertex.names'),'EIS','EA')
  doe_net %v% 'Consultant' <- network.vertex.names(doe_net) %in% consults$PROJECT_ID
  doe_distance_matrix = dist_mat[rownames(dist_mat) %in% {doe_net %v% 'vertex.names'},colnames(dist_mat) %in% {doe_net %v% 'vertex.names'}]
  doe_net_sub = doe_net
  doe_net_sub = delete.vertices(doe_net_sub,which(!{doe_net %v% 'vertex.names'} %in% rownames(doe_distance_matrix)))
  
  doe_dist_index = match(doe_net_sub %v% 'vertex.names',rownames(doe_distance_matrix))
  doe_distance_matrix = doe_distance_matrix[doe_dist_index,doe_dist_index]
  
  doe_consult_matrix = project_to_project_consult_matrix[rownames(project_to_project_consult_matrix) %in% {doe_net %v% 'vertex.names'},colnames(project_to_project_consult_matrix) %in% {doe_net %v% 'vertex.names'}]
  doe_consult_index = match(doe_net %v% 'vertex.names',rownames(doe_consult_matrix))
  doe_consult_matrix = doe_consult_matrix[doe_consult_index,doe_consult_index]
  doe_consult_sub_index = match(doe_net_sub %v% 'vertex.names',rownames(doe_consult_matrix))
  doe_consult_sub_matrix = doe_consult_matrix[doe_consult_sub_index,doe_consult_sub_index]
  
  doe_author_matrix = project_to_project_author_matrix[rownames(project_to_project_author_matrix) %in% {doe_net %v% 'vertex.names'},colnames(project_to_project_author_matrix) %in% {doe_net %v% 'vertex.names'}]
  doe_author_index = match(doe_net %v% 'vertex.names',rownames(doe_author_matrix))
  doe_author_matrix = doe_author_matrix[doe_author_index,doe_author_index]
  doe_author_sub_index = match(doe_net_sub %v% 'vertex.names',rownames(doe_author_matrix))
  doe_author_sub_matrix = doe_author_matrix[doe_author_sub_index,doe_author_sub_index]
  
  # # ##### fit doe_net lolog model ####
  
  doe_order = rank(doe_net %v% 'Date')
  doe_order= seq_along(doe_net %v% 'vertex.names')
  temp_net_doe = doe_net
  
  #delete.edges(temp_net_doe,  which(get.edge.value(temp_net_doe,'Score_Count')<p))
  mod_temp_doe = lolog(temp_net_doe~ edges +  
                         nodeFactor('PROJECT_TYPE')+
                         nodeFactor('Consultant') +
                         nodeCov('ln_Pages_Hashed') + 
                         absDiff('Date') + 
                         edgeCov(doe_consult_matrix>0,name = 'Same_Consultants')  + 
                         edgeCov(doe_author_matrix>0,name = 'Preparers')|doe_order,
                       nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)
  
  saveRDS(mod_temp_doe,'scratch/boilerplate/lolog_model_objects/doe_lolog_p10.RDS')
  
  temp_net_doe_sub = doe_net_sub
  delete.edges(temp_net_doe_sub,  which(get.edge.value(temp_net_doe_sub,'Score_Count')<p))
  doe_sub_order = rank(temp_net_doe_sub %v% 'Date')
  mod_temp_doe_dist = lolog(temp_net_doe_sub ~ edges +  
                              nodeFactor('PROJECT_TYPE') + 
                              nodeFactor('Consultant') +
                              nodeCov('ln_Pages_Hashed') + 
                              absDiff('Date') + 
                              edgeCov(doe_consult_sub_matrix>0,name = 'Same_Consultants')  + 
                              edgeCov(doe_author_sub_matrix>0,name = 'Preparers') + edgeCov(doe_distance_matrix,name = 'Distance')|doe_sub_order,
                            nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)
  
  saveRDS(mod_temp_doe_dist,'scratch/boilerplate/lolog_model_objects/doe_lolog_dist_p10.RDS')
  
  mod_temp_doe_dist_interaction = lolog(temp_net_doe_sub ~ edges +  
                                          nodeFactor('PROJECT_TYPE') + 
                                          nodeFactor('Consultant') +
                                          nodeCov('ln_Pages_Hashed') + 
                                          absDiff('Date') + 
                                          edgeCov(doe_consult_sub_matrix>0,name = 'Same_Consultants')  + 
                                          edgeCov(doe_author_sub_matrix>0,name = 'Preparers') + 
                                          edgeCov(doe_distance_matrix,name = 'Distance') +
                                          edgeCov(doe_distance_matrix * (doe_author_sub_matrix>0),name = 'DistanceXAuthor')|doe_sub_order,
                                        nsamp = samples,verbose=T, maxIter = iters,maxStepSize = stepsize)
  
  saveRDS(mod_temp_doe_dist_interaction,'scratch/boilerplate/lolog_model_objects/doe_lolog_dist_interaction_p10.RDS')
  
  
  m1 = (summary(mod_temp_doe)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 1)
  m2 = (summary(mod_temp_doe_dist)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 2)
  m3 = (summary(mod_temp_doe_dist_interaction)[c('theta','se','pvalue')]) %>% data.frame() %>% mutate(coef = rownames(.),mod = 3)
  doe_mresults = rbindlist(list(m1,m2,m3))
  doe_mresults$DV = 'DOE'
}


doe_mresults$coef[grepl('PROJECT_TYPE',doe_mresults$coef)] <- 'EIS'
doe_mresults$coef = gsub('.*\\.','',doe_mresults$coef)
doe_mresults$coef = fct_inorder(doe_mresults$coef)
doe_mresults$coef = fct_rev(doe_mresults$coef)

(doe_grob = ggplot(doe_mresults[coef!='edges',],aes(group = as.factor(mod))) + 
    geom_hline(yintercept = 0,lty = 2,col = 'grey50') + 
    geom_errorbar(aes(ymin = theta - 1.96 * se,ymax = theta + 1.96 * se,x = coef),width = 0.2,position = position_dodge(width = 0.25)) + 
    geom_point(aes(y = theta,x= coef),position = position_dodge2(width = 0.25)) + 
    theme(axis.text.y = element_blank()) + 
    coord_flip() + theme_bw() + #scale_color_colorblind() + 
    NULL)


modcoefs = rbindlist(list(
  #eis_mresults,
  blm_mresults,
  usfs_mresults,
  doe_mresults))


modcoefs[,sig:=ifelse(theta + 1.96 * se>0&theta - 1.96 * se < 0,0,1),]
modcoefs$coef = fct_recode(modcoefs$coef,'ln(pages)' = 'ln_Pages_Hashed',
                           'Same consultants' = 'Same_Consultants',
                           'Used consultants' = '1',
                           '|publication date diff.|' = 'Date')

(gg_lolog_ea = ggplot(modcoefs[coef!='edges'&mod!=3,],aes(group = as.factor(mod))) + 
    facet_wrap(~DV,ncol = 3) + 
    geom_hline(yintercept = 0,lty = 2,col = 'grey50') + 
    geom_errorbar(aes(ymin = theta - 1.96 * se,ymax = theta + 1.96 * se,x = coef,
                      col = as.factor(mod)),width = 0.2,position = position_dodge(width = 0.5)) + 
    geom_point(aes(y = theta,x= coef,col = as.factor(mod),),position = position_dodge2(width = 0.5),pch = 19) + 
    geom_point(aes(y = theta,x= coef,col = as.factor(mod),fill = as.factor(sig)),position = position_dodge2(width = 0.5),pch = 21) + 
    ggtitle('Predicting text reuse between EA, EIS documents') + 
    scale_color_colorblind(name = 'model',labels = c('all projects','w/ geolocation','author x geo')) + 
    scale_fill_manual(values = c('white',NA)) + 
    coord_flip() + theme_bw() + #scale_color_colorblind() + 
    scale_y_continuous(name = '95% CI (additive log-odds)') +
    theme(axis.title.y = element_blank(),legend.position = c(0.9,0.15),legend.background = element_rect(fill = NA),
          text = element_text(size = 10),legend.title = element_blank()) + 
    guides(fill = F) + 
    NULL)
gg_lolog_ea
ggsave(gg_lolog_ea,filename = 'output/boilerplate/figures/lolog_eis_ea.png',dpi = 300,units = 'in',width = 7,height = 4)


modcoefs[,CI:=paste0(sprintf("%.3f", round(theta,3)),' (',sprintf("%.3f", round(se,3)),')')]
modcoefscoef = fct_inorder(modcoefs$coef)
modcoefs$stars = ifelse(modcoefs$pvalue <0.001,"***",ifelse(modcoefs$pvalue<0.01,"**",ifelse(modcoefs$pvalue<0.05,"*",'')))
modcoefs$CI  =  paste0(modcoefs$CI,modcoefs$stars)
modcoefs = modcoefs[mod!=3,]

coef_cast =  dcast(modcoefs[,.(mod,coef,CI,DV)],coef~DV + mod,value.var = 'CI')
coef_cast = coef_cast[order(fct_rev(coef)),]
coeftab = htmlTable(coef_cast)
outdir.tables = "output/boilerplate/tables/" 
sink(paste0(outdir.tables,"agency_comparison_lolog_coefs.html"))
print(coeftab,type="html",useViewer=FALSE)
sink()



blm_eis_projects = projects[AGENCY == 'Bureau of Land Management'&projects$PROJECT_TYPE=='EIS']
lda_blm = lda[a_id %in% blm_eis_projects$PROJECT_ID & b_id %in% blm_eis_projects$PROJECT_ID,]
lda_blm_highreuse = lda_blm[score>300,.N,by= .(a_id,b_id)]
pgrid = expand.grid(a_id = blm_eis_projects$PROJECT_ID,b_id = blm_eis_projects$PROJECT_ID)

blm_grid = left_join(pgrid,lda_blm_highreuse)
blm_grid$N[is.na(blm_grid$N)] <- 0
blm_grid = data.table(blm_grid)
blm_sagegrouse= c(20180294,20180293,20180295,20180296,20180297,20180298)


pdata = blm_grid[grepl('^2018',a_id) & grepl('^2018',b_id),]
pdata = pdata[as.numeric(a_id)<=as.numeric(b_id),]

(gg_sagegrouse = ggplot() +
    geom_tile(data= pdata[a_id!=b_id,],aes(x = a_id,y = b_id,fill = N)) + 
    geom_tile(data= pdata[a_id==b_id,],aes(x = a_id,y = b_id),fill = 'grey50') + 
    scale_fill_viridis_c(name = '# pages LDA > 300',option = 'B',direction = -1) + 
    ggtitle('# high reuse pages shared between 2018 BLM EISs','red = regional sagegrouse habitat plan') + theme_bw() + 
    theme(axis.title = element_blank(),axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 45,colour = ifelse(sort(unique(pdata$a_id)) %in% blm_sagegrouse,'red','black')),
          axis.text.y = element_text(colour = ifelse(sort(unique(pdata$a_id)) %in% blm_sagegrouse,'red','black')),
          legend.position = c(0.8,0.3)) )

ggsave(gg_sagegrouse,units = 'in',width = 6,height = 6,dpi = 600,filename = 'output/boilerplate/figures/sagegrouse_blm.png')


blm_eas = projects[AGENCY == 'Bureau of Land Management' & PROJECT_TYPE == 'EA',]
blmproj = fread('scratch/blm_project_record.csv')
blm_eas$PROGRAM = blmproj$`Program(s)`[match(blm_eas$PROJECT_ID,blmproj$`NEPA #`)]
blm_eas$REGION = str_extract(blmproj$`Office(s)`[match(blm_eas$PROJECT_ID,blmproj$`NEPA #`)],'^[A-Z]+')
blm_eas$REGION[blm_eas$REGION=='NM']<-c('NM/TX/OK/KS')
blm_eas$REGION[blm_eas$REGION=='ORWA']<-c('OR/WA')
blm_eas$REGION[blm_eas$REGION=='MT']<-c('MT/ND/SD')
blm_eas$total_pages = page_counts$total_pages[match(blm_eas$PROJECT_ID,page_counts$PROJECT_ID)]

blm_ea_lda = lda[a_id %in% blm_eas$PROJECT_ID & b_id %in% blm_eas$PROJECT_ID,]
blm_ea_lda_solo = rbind(blm_ea_lda[,.(a_id,score,a)],blm_ea_lda[,.(b_id,score,b)],use.names = F)
blm_ea_lda_solo = blm_ea_lda_solo[!duplicated(a),]

blm_ea_over_300 = blm_ea_lda_solo[score>300,.N,by = .(a_id)]
setnames(blm_ea_over_300,c('a_id','N'),c('PROJECT_ID','page300'))

blm_eas = data.table(left_join(blm_eas,blm_ea_over_300))
blm_eas$page300[is.na(blm_eas$page300)] <- 0
blm_eas$PROGRAM = gsub('\\s{2,}',' ',gsub('\\n','',blm_eas$PROGRAM))
blm_eas = blm_eas[!is.na(total_pages),]

blm_eas$PROGRAM[blm_eas$PROGRAM %in% c('Interpretation and Environmental Education','Hazard Management and Resource Restoration',
                                       'Conservation & Preservation Areas','Facilities Management','Paleontology','Law Enforcement','Cave and Karst Resources',
                                       'Emergency Stabilization and Rehabilitation','Riparian-Wetlands','Soil Water and Air Quality',
                                       'Cultural-Historical-Native American Resources')] <- 'Other'
blm_eas$PROGRAM[grepl('Fluid Minerals',blm_eas$PROGRAM)] <- 'Fluid Minerals'
blm_eas$PROGRAM[grepl('Grazing',blm_eas$PROGRAM)] <- 'Grazing'
blm_eas$PROGRAM[grepl('Recreation',blm_eas$PROGRAM)] <- 'Recreation'
blm_eas$PROGRAM[grepl('Horse',blm_eas$PROGRAM)] <- 'Wild Horse/Burro'
blm_eas$PROGRAM[grepl('Forest',blm_eas$PROGRAM)] <- 'Forestry'
blm_eas$PROGRAM[grepl('Rangeland',blm_eas$PROGRAM)] <- 'Rangeland'
blm_eas$PROGRAM[grepl('Renew',blm_eas$PROGRAM)] <- 'Renewables'
blm_eas$PROGRAM[grepl('Fire',blm_eas$PROGRAM)] <- 'Wildfire'
blm_eas$PROGRAM[grepl('Emergency Stabilization',blm_eas$PROGRAM)] <- 'Emerg. Stabilization/Rehab.'
blm_eas$PROGRAM <- fct_infreq(blm_eas$PROGRAM)

# ggplot(data = blm_eas,aes(x = PROGRAM,y = page300/total_pages)) + 
#   coord_flip() + geom_jitter(pch = 21,alpha = 0.4) + geom_boxplot(fill = NA,col = 'red',outlier.shape = '') + 
#   theme_bw() + ggtitle('Text reuse in BLM EAs by program') + 
#   scale_y_continuous(name = 'LDA > 300 / total pages') + 
#   theme(text = element_text(size = 12),axis.title.y = element_blank())


# 
# # Plot
# ggplot(blm_eas, aes(x = page300/total_pages, y = REGION, fill = ..x..)) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#   scale_fill_viridis(name = "Temp. [F]", option = "C") +
#   scale_x_continuous(limits=c(0,1),name = '# pages w/ LDA > 300 / total pages') + 
#   labs(title = 'EA text reuse by BLM region') +
#   theme_ipsum() +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8))

# library

library(treemapify)

g1 = ggplot(blm_eas) + geom_bar(aes(x = REGION)) + theme_bw() +
  ggtitle('# of EAs by region') + coord_flip() + scale_y_continuous(name = '# EAs, 2013-2019')+
  theme(text = element_text(size = 12)) + scale_x_discrete(name = 'Administrative region')
g2 = ggplot(blm_eas) + geom_boxplot(aes(y = page300/total_pages,x=REGION)) + theme_bw() +
  ggtitle('Text reuse between EAs by region') + coord_flip() + scale_x_discrete(name ='') + 
  scale_y_continuous('LDA > 300 / total pages') + theme(text = element_text(size = 10))
ggsave(plot = grid.arrange(g1,g2,ncol = 2),filename = 'output/boilerplate/figures/blm_ea_by_region.png',
       dpi = 300,units = 'in',width = 7,height = 3)


g1 = ggplot(blm_eas) + geom_bar(aes(x = PROGRAM)) + theme_bw() +
  ggtitle('# of EAs by program') + coord_flip() + scale_y_continuous(name = '# EAs, 2013-2019')+
  theme(text = element_text(size = 12)) + scale_x_discrete(name = 'BLM program')
g2 = ggplot(blm_eas) + geom_boxplot(aes(y = page300/total_pages,x=PROGRAM)) + theme_bw() +
  ggtitle('Text reuse between EAs by program') + coord_flip() + scale_x_discrete(name ='') + 
  scale_y_continuous('LDA > 300 / total pages') + theme(text = element_text(size = 10))
ggsave(plot = grid.arrange(g1,g2,ncol = 2),filename = 'output/boilerplate/figures/blm_ea_by_program.png',
       dpi = 300,units = 'in',width = 7,height = 3)


#tree_data = blm_eas[,sum(page300),by = .(REGION,PROGRAM)]

tree_data = blm_eas[,list(sum(page300),sum(total_pages)),by = .(PROGRAM)]
setnames(tree_data,c('V1','V2'),c('page300','totalpages'))
tree_data[,lowreusepages:=totalpages-page300]
test = melt(tree_data[,.(PROGRAM,page300,lowreusepages)])
library(treemapify)
test = test[order(value),]
test$PROGRAM = fct_reorder(test$PROGRAM,test$value)
(gg_blm_ea_reuse = ggplot(test, aes(x = PROGRAM,y = value,fill = variable)) + 
    geom_bar(stat = 'identity') + 
    scale_fill_tableau(direction = -1,name = 'between-project',labels=c('# pages LDA>=300','# pages LDA<300')) + 
    coord_flip() + theme_bw() + 
    theme(legend.position = c(0.6,0.2))+
    scale_y_continuous(name = 'Total pages observed') + 
    xlab('Program area')+
    #scale_x_continuous(name = 'Program area') + 
    ggtitle('Total pages of EA text by BLM program 2013-2019'))

ggsave(plot = gg_blm_ea_reuse,filename = 'output/boilerplate/figures/blm_between_ea_barplot.png',
       units = 'in',dpi = 300, width = 7,height = 5)



gg_blm_treemap = ggplot(test, aes(area = value,label = PROGRAM, subgroup = PROGRAM,fill = variable)) +
  ggtitle('Total pages of EA text by BLM program 2013-2019') + 
  geom_treemap()+
  #geom_treemap_subgroup3_border(colour = "blue", size = 1) +
  #geom_treemap_subgroup10_border(colour = "white", size = 3) +
  geom_treemap_subgroup_border(colour = "black", size = 3) +
  geom_treemap_subgroup_text(min.size = 4,
                             place = "topleft",
                             colour = "black",
                             alpha = 1,reflow = T,grow = F) +
  scale_fill_tableau(direction = -1,name = 'between-project',labels=c('# pages LDA>=300','# pages LDA<300')) + 
  theme(legend.position = 'bottom',legend.direction = 'horizontal')

ggsave(plot = gg_blm_treemap,filename = 'output/boilerplate/figures/blm_between_ea_treemap.png',
       units = 'in',dpi = 300, width = 7,height = 5)

net_list = list(eis_master_net,eis_master_net_sub,usfs_net,usfs_net_sub,blm_net,blm_net_sub,doe_net,doe_net_sub)
names(net_list) <- c('eis','eis_dist','usfs','usfs_dist','blm','blm_dist','doe','doe_dist')
ntdt = data.table(nodes = sapply(net_list,network.size),edges = sapply(net_list,network.edgecount),
                  density = sapply(net_list,network.density),net = names(net_list))

tableout <-htmlTable(ntdt)
outdir.tables = "output/boilerplate/tables/" 
sink(paste0(outdir.tables,"network_summary.html"))
print(tableout,type="html",useViewer=F)
sink()


