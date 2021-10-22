pack = c("tidyverse","Matrix","statnet","data.table","stringr","lolog","texreg","textreuse","gridExtra","sf","lubridate","doParallel","htmlTable",'treemapify',"ggridges","ggplot2","viridis","hrbrthemes","forcats","betareg",'ggthemes','tm','pbapply')
need = !pack %in% installed.packages()[,"Package"]
#sudo apt-get install libfontconfig1-dev
if(any(need)){sapply(pack[need],install.packages)}
sapply(pack,require,character.only=T)

mcores = detectCores() - 2
samples = 10000
iters = 40
stepsize = 500
require(forcats)


projects = fread('boilerplate_project/data_products/project_candidates_eis_only.csv')

projects$EIS.Number <- as.character(projects$EIS.Number)

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
  
  lda_eis = lda[a_id == b_id,]
  lda_solo = rbind(lda_eis[,.(a_id,score,a,a_file,a_page)],lda_eis[,.(b_id,score,b,b_file,b_page)],use.names = F)
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
  

  agency_summary_stats = countover300[,list(mean(over300/total_pages),median(over300/total_pages)),by = .(AGENCY_SHORT)]
  names(agency_summary_stats) <- c('AGENCY_SHORT','mean','median')
  agency_summary_stats$mean = round(agency_summary_stats$mean,3)
  agency_summary_stats$median = round(agency_summary_stats$median,3)

countover300$AGENCY_SHORT <- fct_infreq(countover300$AGENCY_SHORT)


    (gg_pages_over_300 = ggplot() + 
       #  geom_jitter(data = countover300,aes(x = fct_rev(AGENCY_SHORT),y = over300/total_pages),pch = 21,colour = 'grey50') + 
       #geom_boxplot(data = countover300,aes(x = fct_rev(AGENCY_SHORT),y = over300/total_pages),fill = NA) + 
       geom_jitter(data = countover300,aes(x = fct_rev(AGENCY_SHORT),y = over300/total_pages),alpha = 0.5,fill = NA,height = 0,pch = 21) + 
       geom_point(data = countover300[,median(over300/total_pages),by=.(AGENCY_SHORT)],
                  aes(x = fct_rev(AGENCY_SHORT),y = V1,col ='red'),pch = '|',size = 5)+
       coord_flip() + scale_color_manual(values = 'red',name = 'Agency median',labels = 'Agency median')+
       #    ggplot2::annotate("label",x = fct_rev(agency_summary_stats$AGENCY_SHORT),y = agency_summary_stats$median*1.5,label = agency_summary_stats$median) + 
       theme_bw() + scale_y_continuous(name = '# pages w/ LDA>300 / total pages by project') + 
       #limits=c(0,round(max(countover300$over300/countover300$total_pages) ,1))) + 
       theme(legend.title = element_blank(),axis.title.y = element_blank(),legend.position = c(0.8,0.25),axis.text = element_text(size = 12)) + 
       ggtitle("Text reuse within EISs by project and agency"))
    
    ggsave(gg_pages_over_300,filename = 'boilerplate_project/output/figures/Figure5_agencies_within_lda_over_300.tiff',dpi = 300,units = 'in',height = 4.5,width = 6)
  
  
  
  
  eis_ids = unique(projects$EIS.Number[projects$USE_DOCS])
  n_eis = length(eis_ids)
  
  projects = projects[USE_DOCS==T&!duplicated(EIS.Number),]
  projects = merge(projects,countover300[,.(EIS.Number,total_pages,over300)],by='EIS.Number')

  readab = readRDS('boilerplate_project/data_products/readability_scores_by_project.rds')
  projects$`Readability score` <- (log(readab$re[match(projects$EIS.Number,readab$EIS.Number)]))
  
  
  lit_dt = fread('boilerplate_project/data_products/nepa_litigation_by_agency_year.csv')
  ldt_long = melt(lit_dt,id.vars = 'recode',measure.vars = patterns('y[0-9]'))
  ldt_long$year <- as.numeric(str_remove(ldt_long$variable,'[a-z]'))
  ltd_total = ldt_long[year<2013,][,sum(value,na.rm = T),by=.(recode)]

  tots = projects[,.N,by=.(AGENCY_SHORT)]
  
  ltd_total$total_eis = tots$N[match(ltd_total$recode,tots$AGENCY_SHORT)]
  ltd_total$litigation_per_eis = ltd_total$V1/ltd_total$total_eis

  projects$litigation_per_eis <- ltd_total$litigation_per_eis[match(projects$AGENCY_SHORT,ltd_total$recode)]
  
  
  ideology_sheet = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSOSX--qpOSyBZcW3OWeWRmiC2-eC74un7fZYXFCzrW8FNB1FOQFMaIq-CW8hMIoBqtZMXYR05UA7Lu/pub?output=csv'
  ideol = fread(ideology_sheet)
  subs = str_split(ideol$epaStandIn,', ')
  projects$index = match(projects$Lead.Agency,ideol$epaName)
  projects = data.table(projects,ideol[unlist(sapply(projects$Lead.Agency,function(x) c(grep(x,ideol$epaName),grep(x,ideol$epaStandIn)))),])
  
  projects$skills_rating[is.na(projects$skills_rating)] <- 0
  
  
  
  consults = readRDS('boilerplate_project/data_products/consultant_project_matches.RDS')
  setnames(consults,'PROJECT_ID','EIS.Number')
  
  projects$USED_CONSULTANT <- (projects$EIS.Number %in% consults$EIS.Number) + 0
  
  projects$beta_dv <- {{projects$over300/projects$total_pages} * (nrow(projects)-1) + mean(projects$over300/projects$total_pages)} / nrow(projects)

  beta_mod = betareg(beta_dv~log(total_pages) + log(`Readability score`) + litigation_per_eis + 
            ideo_rating + skills_rating + DECISION + as.factor(Year) + USED_CONSULTANT,data = projects)
    
  
 summary(lm(over300/total_pages~log(total_pages) + log(`Readability score`) + litigation_per_eis + 
                       ideo_rating + skills_rating + DECISION + as.factor(Year) + USED_CONSULTANT,data = projects))
  

  htmlreg(beta_mod,file = 'boilerplate_project/output/tables/Table7_betaregression_results.html',single.row = T)  



