pack = c("tidyverse","Matrix","statnet","data.table","stringr","lolog","textreuse","stringdist","textclean","gridExtra","sf","lubridate","doParallel","htmlTable","ggridges","ggplot2","viridis","hrbrthemes","forcats",'ggthemes','tm','pbapply')
need = !pack %in% installed.packages()[,"Package"]
#sudo apt-get install libfontconfig1-dev
if(any(need)){sapply(pack[need],install.packages)}
sapply(pack,require,character.only=T)


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
epa_record$Agency_Short[epa_record$Agency=="Federal Highway Administration"] <- 'FHWA'
epa_record$Agency_Short[epa_record$Agency=="Federal Railroad Administration" ] <- 'FRA'
epa_record$Agency_Short[epa_record$Agency=="National Marine Fisheries Service"  ] <- 'NOAA'
epa_record$Agency_Short[epa_record$Agency== "Department of Energy"   ] <- 'DOE'
epa_record$Agency_Short[epa_record$Agency== "United States Army"   ] <-  'US Army'
epa_record$Agency_Short[epa_record$Agency== "Federal Transit Administration"   ] <-  'FTA'
epa_record$Agency_Short[epa_record$Agency== "Bureau of Reclamation"   ] <-  'BR'
epa_record$Agency_Short[epa_record$Agency%in%c("Bonneville Power Administration","Western Area Power Administration")] <-  'DOE'
#levels(fct_infreq(epa_record$Agency_Short))[1:18] 
epa_record$Agency_Short = fct_infreq(epa_record$Agency_Short) 


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

projects = fread('scratch/boilerplate/project_candidates.csv')
projects$MASTER_ID[grepl('Eastern',projects$PROJECT_ID)] <- projects$PROJECT_ID[grepl('Eastern',projects$PROJECT_ID)]
projects = projects[PROJECT_ID!='20190296',]
projects_used = unique(projects$PROJECT_ID)
flist_dt = flist_dt[PROJECT_ID %in% projects$PROJECT_ID,]

eis_used = unique(projects$PROJECT_ID[projects$PROJECT_TYPE=='EIS'])

docs = fread('scratch/boilerplate/document_candidates.csv')
docs = docs[FILE_NAME!='DOI-BLM-WY-D040-2017-0022-EA--2._EA_ATSW&GDB_-_July,_2017.txt',]
docs = docs[FILE_NAME!='DOI-BLM-AZ-G020-2015-0009-EA--Keystone_Peak_EA_021915_Final.txt',]
docs = docs[FILE_NAME!='42886--42886_97139_FSPLT3_2552815.txt',]
docs = docs[FILE_NAME!='36707--36707_81011_FSPLT3_3032131.txt',]


flist_dt = flist_dt[flist_dt$File %in% docs$FILE_NAME,]

dups_and_files = flist_dt[,list(sum(duplicated(text)),length(unique(File))),by=.(PROJECT_ID)]
dups_and_files = dups_and_files[V2>1,]
ea_dups = dups_and_files[V2==2&V1>=10&!PROJECT_ID %in% eis_used,]


docs = docs[order(-FILE_NAME),][!PROJECT_ID%in% ea_dups$PROJECT_ID|!duplicated(PROJECT_ID),]
flist_dt = flist_dt[File %in% docs$FILE_NAME,]


page_counts = flist_dt[,.N,by=.(PROJECT_ID)]
setnames(page_counts,'N','total_pages')
page_counts$MASTER_ID = projects$MASTER_ID[match(page_counts$PROJECT_ID,projects$PROJECT_ID)]
page_counts = page_counts[total_pages>3,]

meta_dt = flist_dt[,.(Page,File,PROJECT_ID)]
#meta_dt = meta_dt[!meta_dt$File %in% dropfiles,]
meta_dt$DUPE = duplicated(meta_dt)
meta_dt = meta_dt[!meta_dt$DUPE,]

projects = projects[PROJECT_ID %in% page_counts$PROJECT_ID,]
flist_dt = flist_dt[PROJECT_ID %in% page_counts$PROJECT_ID,]
projects_used = unique(projects$PROJECT_ID)
gc()


lda_result_files = list.files(pattern  = 'result_V2\\.RDS')
lda_list = lapply(lda_result_files,readRDS)
lda = rbindlist(lda_list)
#rm(lda_list);gc()
lda$score = as.numeric(lda$score)
lda = lda[!duplicated(lda),]
lda = lda[score>=300,]
lda$a = gsub('^(((?!--).)+-{2})\\1','\\1',lda$a,perl = T)
lda$b = gsub('^(((?!--).)+-{2})\\1','\\1',lda$b,perl = T)
lda = lda[a!=b,]
lda = lda[!duplicated(lda),]

lda = lda[b %in% flist_dt$PID|a %in% flist_dt$PID,]

lda$a_page = as.numeric(gsub('.*--','',lda$a,perl = T))
lda$b_page = as.numeric(gsub('.*--','',lda$b,perl = T))

lda$a_id = gsub('(--|_(?!States)]).*','',lda$a,perl = T)
lda$b_id = gsub('(--|_(?!States)]).*','',lda$b,perl = T)
lda = lda[a_id==b_id,]
lda = lda[a_id %in% projects_used & b_id %in% projects_used,]

lda$agency1 = projects$AGENCY[match(lda$a_id,projects$PROJECT_ID)]
lda$agency2 = projects$AGENCY[match(lda$b_id,projects$PROJECT_ID)]

lda$a_file = flist_dt$File[match(lda$a,flist_dt$PID)]
lda$b_file = flist_dt$File[match(lda$b,flist_dt$PID)]
lda = lda[a_file %in% docs$FILE_NAME&b_file %in% docs$FILE_NAME,]




page_counts = flist_dt[,.N,by=.(PROJECT_ID)]
setnames(page_counts,'N','total_pages')
page_counts$MASTER_ID = projects$MASTER_ID[match(page_counts$PROJECT_ID,projects$PROJECT_ID)]
#page_counts = page_counts[total_pages>3,]



lda_solo = rbind(lda[,.(a_id,score,a,a_file)],lda[,.(b_id,score,b,b_file)],use.names = F)
uq = unique(lda_solo$a_id)
lda_solo$AGENCY = projects$AGENCY[match(lda_solo$a_id,projects$PROJECT_ID)]
lda_solo = lda_solo[lda_solo$a_id %in% eis_used,]
lda_solo$AGENCY_SHORT = epa_record$Agency_Short[match(lda_solo$AGENCY,epa_record$Agency)]

fileover300 = lda_solo[,.N,a_file]
setnames(fileover300,c('N','a_file'),c('over300','File'))
filepages = flist_dt[,.N,by=.(File)]
setnames(filepages,'N','pages')
file300 = merge(fileover300,filepages,all = T)
file300$over300[is.na(file300$over300)]<-0


#documents = fread('scratch/boilerplate/document_candidates.csv')
#dropfiles = c( "20130252_HSTT EIS CH 3.5-7 VOL II 08-13.pdf.txt","20130252_HSTT EIS Exec Sum-CH 3.4 VOL I 08-13.pdf.txt")
#documents = documents[!FILE_NAME %in% dropfiles ]
#documents$FILE_NAME = gsub('\\s','_',documents$FILE_NAME)

#lda_solo$a_file = gsub('\\s','_',lda_solo$a_file)
#lda_solo$a_file = gsub('\\.pdf\\.txt$','.txt',lda_solo$a_file)
#lda_solo$a = gsub('\\.pdf\\.txt','.txt',lda_solo$a)

#lda_solo = lda_solo[a_file %in% documents$FILE_NAME,]
#lda_solo = lda_solo[a_id %in% projects$PROJECT_ID,]

over300 = lda_solo[score>=300,]
over300 = over300[!duplicated(a),]

countover300 = over300[,.N,by = .(a_id)]
setnames(countover300,c('a_id','N'),c('PROJECT_ID','over300'))
eis_page_counts = page_counts[PROJECT_ID %in% projects$PROJECT_ID[projects$PROJECT_TYPE=='EIS'],]
#setnames(eis_page_counts,'N','total_pages')
countover300 = merge(countover300,eis_page_counts,all = T)

countover300$over300[is.na(countover300$over300)]<-0
countover300$Agency_Short = epa_record$Agency_Short[match(projects$AGENCY[match(countover300$PROJECT_ID,projects$PROJECT_ID)],epa_record$Agency)]
a = flist_dt[PID=='20130032--20130032_Vol1_Alta_East_PA-FEIS.txt--100']$text
b = flist_dt[PID=='20130032--20130032_Vol1_Alta_East_PA-FEIS.txt--140']$text
align_local(a = a,b=b)


#countover300[Agency_Short=='FS'][order(-over300/total_pages)]
(gg_within_over300 = (ggplot(countover300,aes(x = fct_rev(Agency_Short),y = over300/total_pages)) + geom_boxplot() + coord_flip() +
                       theme_bw() + scale_y_continuous(name = '# pages w/ within-doc. LDA>300 / total pages by project',limits=c(0,1)) + 
                       theme(axis.title.y = element_blank(),axis.text = element_text(size = 12)) + 
                       ggtitle("Within-project text reuse by agency")))
ggsave(gg_within_over300,filename = 'output/boilerplate/figures/within_over300_by_agency.png',dpi = 300,width = 6,height = 5, units = 'in')
saveRDS(countover300,'scratch/boilerplate/eis_within_over300_pagecount.RDS')

lda_solo = rbind(lda[,.(a_id,score,a,a_file)],lda[,.(b_id,score,b,b_file)],use.names = F)
lda_solo = lda_solo[order(-score),][!duplicated(a),][score>=300,]
lda_solo$AGENCY = projects$AGENCY[match(lda_solo$a_id,projects$PROJECT_ID)]
lda_solo$TYPE = projects$PROJECT_TYPE[match(lda_solo$a_id,projects$PROJECT_ID)]
lda_solo = lda_solo[AGENCY %in% c('Forest Service','Bureau of Land Management','Department of Energy'),]

ea_countover300 = lda_solo[,.N,by = .(a_id)]
setnames(ea_countover300,c('a_id','N'),c('PROJECT_ID','over300'))
#ea_countover300$AGENCY = projects$AGENCY[match(ea_countover300$PROJECT_ID,projects$PROJECT_ID)]
#ea_countover300$total_pages = page_counts$total_pages[match(ea_countover300$PROJECT_ID,page_counts$PROJECT_ID)]
subcounts = page_counts[PROJECT_ID %in% projects$PROJECT_ID[projects$AGENCY %in%c('Forest Service','Bureau of Land Management','Department of Energy')],]
subcounts$AGENCY = projects$AGENCY[match(subcounts$PROJECT_ID,projects$PROJECT_ID)]
#setnames(subcounts,'N','total_pages')
ea_countover300 = merge(ea_countover300,subcounts,all=T)
ea_countover300$AGENCY[ea_countover300$AGENCY=='Department of Energy'] <- 'DOE'
ea_countover300$AGENCY[ea_countover300$AGENCY=='Forest Service'] <- 'FS'
ea_countover300$AGENCY[ea_countover300$AGENCY=='Bureau of Land Management'] <- 'BLM'

ea_countover300$TYPE = projects$PROJECT_TYPE[match(ea_countover300$PROJECT_ID,projects$PROJECT_ID)]
ea_countover300$over300[is.na(ea_countover300$over300)] <- 0
ea_countover300 = ea_countover300[PROJECT_ID!='DOI-BLM-NM-F010-2016-0001-EA',]
saveRDS(ea_countover300,'scratch/boilerplate/eas_within_over300_pagecount.RDS')

(ea_vs_eis_pagecount = ggplot(ea_countover300) + geom_boxplot(aes(x = AGENCY,y = total_pages,col = TYPE)) + coord_flip() +
    ggtitle("Pages analyzed") +
    theme_bw() + scale_y_continuous(name = '# pages by project') + 
    theme(axis.title.y = element_blank(),legend.position = c(0.8,0.4),legend.title = element_blank(),text = element_text(size = 11),legend.text = element_text(size = 14),axis.text = element_text(size = 12)) + 
    scale_color_brewer(labels = c('EA','EIS'),type = 'qual',palette = 2))

(ea_vs_eis_plot = ggplot(ea_countover300) + 
    geom_boxplot(aes(x = AGENCY,y = over300/total_pages,col = TYPE)) + coord_flip() +
    ggtitle("Within project reuse") +
    theme_bw() + scale_y_continuous(name = '# pages LDA>300 / total pages',limits=c(0,1)) + 
    theme(axis.text.y = element_blank(),axis.title.y = element_blank(),
          legend.position = c(0.8,0.4),legend.title = element_blank(),text = element_text(size = 11),legend.text = element_text(size = 14),axis.text = element_text(size = 12)) + 
    scale_color_brewer(labels = c('EA','EIS'),type = 'qual',palette = 2))


t1 = flist_dt$text[flist_dt$PID=='DOI-BLM-NM-P020-2017-0030-EA--2016.09.30_Devon_Tomb_Raider_Projects_Preliminary_EA.txt--14']
t2 = flist_dt$text[flist_dt$PID=='DOI-BLM-NM-P020-2017-0030-EA--2016.09.30_Devon_Tomb_Raider_Projects_Preliminary_EA.txt--44']
align_local(t1,t2)

library(gridExtra)
lay <- rbind(c(1,1,1,1,1,2,2,2,2))
gg_within_sub = grid.arrange(ea_vs_eis_pagecount,ea_vs_eis_plot,ncol = 10,layout_matrix = lay)
ggsave(plot = gg_within_sub,filename = 'output/boilerplate/figures/within_over300_ea_eis_subset.png',
       width = 7,height = 4.5,units = 'in',dpi = 300)


lda_solo = rbind(lda[,.(a_id,score,a,a_file)],lda[,.(b_id,score,b,b_file)],use.names = F)
lda_solo = lda_solo[order(-score),][!duplicated(a),][score>=300,]
lda_solo$AGENCY = projects$AGENCY[match(lda_solo$a_id,projects$PROJECT_ID)]
lda_solo$TYPE = projects$PROJECT_TYPE[match(lda_solo$a_id,projects$PROJECT_ID)]
lda_solo = lda_solo[AGENCY %in% c('Bureau of Land Management')&TYPE=='EA',]

ea_countover300 = lda_solo[,.N,by = .(a_id)]
setnames(ea_countover300,c('a_id','N'),c('PROJECT_ID','over300'))
subcounts = page_counts[PROJECT_ID %in% projects$PROJECT_ID[projects$AGENCY %in%c('Bureau of Land Management')&projects$PROJECT_TYPE=='EA'],]
setnames(subcounts,'N','total_pages')
ea_countover300 = merge(ea_countover300,subcounts,all=T)

ea_countover300$TYPE = projects$PROJECT_TYPE[match(ea_countover300$PROJECT_ID,projects$PROJECT_ID)]
ea_countover300$over300[is.na(ea_countover300$over300)] <- 0
ea_countover300$AGENCY = projects$AGENCY[match(ea_countover300$PROJECT_ID,projects$PROJECT_ID)]





ea_countover300$AGENCY[ea_countover300$AGENCY=='Bureau of Land Management'] <- 'BLM'

blmproj = fread('scratch/blm_project_record.csv')
ea_countover300$PROGRAM = blmproj$`Program(s)`[match(ea_countover300$PROJECT_ID,blmproj$`NEPA #`)]
ea_countover300$REGION = str_extract(blmproj$`Office(s)`[match(ea_countover300$PROJECT_ID,blmproj$`NEPA #`)],'^[A-Z]+')
ea_countover300$REGION[ea_countover300$REGION=='NM']<-c('NM/TX/OK/KS')
ea_countover300$REGION[ea_countover300$REGION=='ORWA']<-c('OR/WA')
ea_countover300$REGION[ea_countover300$REGION=='MT']<-c('MT/ND/SD')


ea_countover300$PROGRAM = gsub('\\s{2,}',' ',gsub('\\n','',ea_countover300$PROGRAM))
ea_countover300$PROGRAM[ea_countover300$PROGRAM %in% c('Interpretation and Environmental Education','Hazard Management and Resource Restoration',
                                                       'Conservation & Preservation Areas','Facilities Management','Paleontology','Law Enforcement','Cave and Karst Resources',
                                                       'Emergency Stabilization and Rehabilitation','Riparian-Wetlands','Soil Water and Air Quality',
                                                       'Cultural-Historical-Native American Resources')] <- 'Other'
ea_countover300$PROGRAM[grepl('Fluid Minerals',ea_countover300$PROGRAM)] <- 'Fluid Minerals'
ea_countover300$PROGRAM[grepl('Grazing',ea_countover300$PROGRAM)] <- 'Grazing'
ea_countover300$PROGRAM[grepl('Recreation',ea_countover300$PROGRAM)] <- 'Recreation'
ea_countover300$PROGRAM[grepl('Horse',ea_countover300$PROGRAM)] <- 'Wild Horse/Burro'
ea_countover300$PROGRAM[grepl('Forest',ea_countover300$PROGRAM)] <- 'Forestry'
ea_countover300$PROGRAM[grepl('Rangeland',ea_countover300$PROGRAM)] <- 'Rangeland'
ea_countover300$PROGRAM[grepl('Renew',ea_countover300$PROGRAM)] <- 'Renewables'
ea_countover300$PROGRAM[grepl('Fire',ea_countover300$PROGRAM)] <- 'Wildfire'
ea_countover300$PROGRAM[grepl('Emergency Stabilization',ea_countover300$PROGRAM)] <- 'Emerg. Stabilization/Rehab.'
ea_countover300$PROGRAM <- fct_infreq(ea_countover300$PROGRAM)

g1 = ggplot(ea_countover300) + geom_bar(aes(x = REGION)) + theme_bw() +
  ggtitle('# of EAs by region') + coord_flip() + scale_y_continuous(name = '# EAs, 2013-2019')+
  theme(text = element_text(size = 12)) + scale_x_discrete(name = 'Administrative region')
g2 = ggplot(ea_countover300) + geom_boxplot(aes(y = over300/total_pages,x=REGION),outlier.shape = 21,outlier.fill = NA,outlier.alpha = 0.5) + theme_bw() +
  ggtitle('Text reuse within EAs by region') + coord_flip() + scale_x_discrete(name ='') + 
  scale_y_continuous('LDA > 300 / total pages') + theme(text = element_text(size = 10),
                                                        title = element_text(size = 12))
ggsave(plot = grid.arrange(g1,g2,ncol = 2),filename = 'output/boilerplate/figures/blm_within_ea_by_region.png',
       dpi = 300,units = 'in',width = 7,height = 3)


g1 = ggplot(ea_countover300) + geom_bar(aes(x = PROGRAM)) + theme_bw() +
  ggtitle('# of EAs by program') + coord_flip() + scale_y_continuous(name = '# EAs, 2013-2019')+
  theme(text = element_text(size = 12),
        title = element_text(size = 12)) + scale_x_discrete(name = 'BLM program')
g2 = ggplot(ea_countover300) + geom_boxplot(aes(y = over300/total_pages,x=PROGRAM),outlier.shape = 21,outlier.fill = NA,outlier.alpha = 0.5) + theme_bw() +
  ggtitle('Text reuse within EAs by program') + coord_flip() + scale_x_discrete(name ='') + 
  scale_y_continuous('LDA > 300 / total pages') + theme(text = element_text(size = 10),title = element_text(size = 12))
ggsave(plot = grid.arrange(g1,g2,ncol = 2),filename = 'output/boilerplate/figures/blm_within_ea_by_program.png',
       dpi = 300,units = 'in',width = 7,height = 3)


blm_tree_data = ea_countover300[,list(sum(over300),sum(total_pages)),by = .(PROGRAM)]
setnames(blm_tree_data,c('V1','V2'),c('over300','totalpages'))
blm_tree_data[,lowreusepages:=totalpages-over300]
test = melt(blm_tree_data[,.(PROGRAM,over300,lowreusepages)])
library(treemapify)
test = test[order(value),]
test$PROGRAM = fct_reorder(test$PROGRAM,test$value)
(gg_blm_ea_reuse = ggplot(test, aes(x = PROGRAM,y = value,fill = variable)) + 
    geom_bar(stat = 'identity') + 
    scale_fill_tableau(direction = -1,name = 'within-project',labels=c('# pages LDA>=300','# pages LDA<300')) + 
    coord_flip() + theme_bw() + 
    theme(legend.position = c(0.6,0.2))+
    scale_y_continuous(name = 'Total pages observed') + 
    xlab('Program area')+
    #scale_x_continuous(name = 'Program area') + 
    ggtitle('Total pages of EA text by BLM program 2013-2019'))

ggsave(plot = gg_blm_ea_reuse,filename = 'output/boilerplate/figures/blm_within_ea_barplot.png',
       units = 'in',dpi = 300, width = 7,height = 5)




test = melt(blm_tree_data[,.(PROGRAM,over300,lowreusepages)])
library(treemapify)

gg_blm_treemap = ggplot(test, aes(area = value,label = PROGRAM, subgroup = PROGRAM,fill = variable)) +
  ggtitle('Total pages of EA text by BLM program 2013-2019') + 
  geom_treemap()+
  #geom_treemap_subgroup3_border(colour = "blue", size = 1) +
  #geom_treemap_subgroup2_border(colour = "white", size = 3) +
  geom_treemap_subgroup_border(colour = "black", size = 3) +
  geom_treemap_subgroup_text(min.size = 4,
                             place = "topleft",
                             colour = "black",
                             alpha = 1,reflow = T,grow = F) +
  scale_fill_tableau(direction = -1,name = 'within-project',labels=c('# pages LDA>=300','# pages LDA<300')) + 
  theme(legend.position = 'bottom',legend.direction = 'horizontal')

ggsave(plot = gg_blm_treemap,filename = 'output/boilerplate/figures/blm_within_ea_treemap.png',
       units = 'in',dpi = 300, width = 7,height = 5)

