
packs = c('stm','data.table','stringr','readtext','pbapply','maps','quanteda','tm','ggrepel','DescTools','lubridate')
sapply(packs[!sapply(packs,require,character.only = T)],install.packages)
sapply(packs,require,character.only = T)

redraw_corpus = TRUE
projs = readRDS('climate_in_eis_project/data_products/deis_metadata_with_covariates.RDS')
docs = readRDS('climate_in_eis_project/data_products/deis_doc_metadata.RDS')
risks = readRDS('climate_in_eis_project/data_products/project_risks.RDS')
setnames(risks, 'PROJECT_ID','EIS.Number')
projs$EIS.Number <- as.character(projs$EIS.Number)
projs <- merge(projs,risks,all.x = T)

quanteda_toks = list.files('climate_in_eis_project/yearly_quanteda_tokens/',full.names = T)
tok_list = lapply(quanteda_toks,readRDS)

#do.call is good code, but takes _forever_
#tok_all =do.call("+",tok_list)
#this is bad code, but goes fast
tok_all = tok_list[[1]]
for(t in tok_list[-1]){
  tok_all = tok_all + t
}

qdfm = dfm(tok_all)
qdfm_eis <- dfm_group(qdfm,str_extract(qdfm@Dimnames$docs,'^[0-9]{8}'))
qdfm_eis <- dfm_trim(qdfm_eis,min_docfreq = 3) 
eis3 <- qdfm_eis@Dimnames$features
qdfm <- qdfm[,qdfm@Dimnames$features %in% eis3]


qdfm <- dfm_trim(qdfm, max_docfreq = 0.3,docfreq_type = 'prop')
qdfm_stem = dfm_wordstem(qdfm)
qdfm_stem <- qdfm_stem[ntoken(qdfm_stem)>0,]

meta_eis = projs[match(str_remove(qdfm_stem@Dimnames$docs,'_.*'),projs$EIS.Number),]
meta_eis$EIS.Number <- as.character(meta_eis$EIS.Number)
meta_eis$ID = qdfm_stem@Dimnames$docs


dfm2stm <- convert(qdfm_stem, to = "stm")
meta_eis_sub = meta_eis[ID %in% names(dfm2stm$documents),]
meta_eis_sub$YEAR = str_extract(meta_eis_sub$EIS.Number,'^[0-9]{4}')
dfm2stm$meta = meta_eis_sub


dfm2stm$meta$project_EAL[is.na(dfm2stm$meta$project_EAL)] <- median(dfm2stm$meta$project_EAL,na.rm = T)
dfm2stm$meta$project_CR[is.na(dfm2stm$meta$project_CR)] <- median(dfm2stm$meta$project_CR,na.rm = T)
dfm2stm$meta$project_SVI[is.na(dfm2stm$meta$project_SVI)] <- median(dfm2stm$meta$project_SVI,na.rm = T)

#use k = 0 to automate guess to understand range of k
model.base <- stm(dfm2stm$documents, dfm2stm$vocab, K = 0, data = dfm2stm$meta,
                 prevalence = ~EIS.Number, init.type = "Spectral",verbose = T,#ngroups = 5,
                 seed = 24,max.em.its = 40,emtol = 0.0001) 
saveRDS(model.base,'climate_in_eis_project/scratch/base_stm_searchk.RDS')

K = 80
model.base <- stm(dfm2stm$documents, dfm2stm$vocab, K = K, data = dfm2stm$meta,
                  prevalence = ~EIS.Number, init.type = "Spectral",verbose = T,#ngroups = 5,
                  seed = 24,max.em.its = 40,emtol = 0.0001) 
saveRDS(model.base,'climate_in_eis_project/scratch/base_stm_80k.RDS')


model.cov <- stm(dfm2stm$documents, dfm2stm$vocab, K = K, data = dfm2stm$meta,
                 prevalence = ~EIS.Number + AGENCY + YEAR + PROJECT_TYPE + PROJECT_TOPIC+
                   s(project_EAL) + s(project_SVI) + s(project_CR),
                 init.type = "Spectral",verbose = T,#ngroups = 5,
                  seed = 24,max.em.its = 40,emtol = 0.0001) 
saveRDS(model.cov,'climate_in_eis_project/scratch/base_stm_80k.RDS')


model.stm <- stm(dfm2stm$documents, dfm2stm$vocab, K = 0, data = dfm2stm$meta,
       prevalence = ~EIS.Number + AGENCY + YEAR + PROJECT_TYPE + PROJECT_TOPIC+
          s(project_EAL) + s(project_SVI) + s(project_CR),
                 init.type = "Spectral",verbose = T,
                 seed = 24,max.em.its = 20,emtol = 0.0001) 
saveRDS(model.stm,'climate_in_eis_project/scratch/temp_stm_90k.RDS')




other.stm <- lapply(c(30,45,60,75,105,120),function(k) {
  stm(dfm2stm$documents, dfm2stm$vocab, K = k, data = dfm2stm$meta,
      prevalence = ~EIS.Number + AGENCY + YEAR + PROJECT_TYPE + PROJECT_TOPIC+
        s(project_EAL) + s(project_SVI) + s(project_CR),
      init.type = "Spectral",verbose = T,ngroups = 5,
      seed = 24,max.em.its = 40,emtol = 0.0001) 
})
saveRDS(other.stm ,'climate_in_eis_project/scratch/temp_stm_Ksensitivity.RDS')


model.stm = readRDS(paste0(where_is_scratch,'temp_stm_90k.RDS'))

theta_medians = sapply(1:90,function(t) {median(model.stm$theta[,t])})
mdt = make.dt(model.stm)
require(ggthemes)
frequency <- colMeans(model.stm$theta)
freq_dt = data.table(frequency,rank = rank(-frequency),topic = 1:90)
freq_dt = freq_dt[order(rank)]
ggplot(data = freq_dt) + ggtitle('Topics ordered by expected frequency')+
  geom_bar(aes(x = rank,y = frequency,fill = ifelse(topic == c(11),'Topic 11',ifelse(topic==28,'Topic 28','Other'))),
           stat = 'identity') + 
  geom_label(aes(x = rank,y = frequency,label = topic))+
  coord_flip() + 
  scale_x_continuous(breaks = c(1:90),labels=freq_dt$topic,name = 'topic',expand=c(0,0))+ theme_bw() + 
  theme(axis.ticks = element_blank(),legend.title = element_blank(),
        #axis.text.x = element_text(angle = 45),
        legend.position = c(0.75,0.25)) + scale_fill_colorblind()
freq_dt[topic%in%c(11,28)]

invrank <- order(frequency, decreasing=FALSE)

data.table(frequency,topic = 1:90)[order(-frequency),]
invrank


head(mdt)
ggplot() + geom_histogram(aes(theta_medians)) +
  xlab('Topic proportion')+
  geom_vline(xintercept =theta_medians[c(11,28)] ,lty = 2,col = 'red')+
  geom_label(aes(x = theta_medians[c(11,28)],y = 10,label=c('Topic 11','Topic 28')))+
  
  theme_bw() + ggtitle('Distribution of posterior medians of topic proportions')


hist(theta_medians,breaks = 15)

for(i in 1:length(topics)){
  theta_median <- median(model$theta[,topics[i]])
  
  
  
  stm::plot.STM(model.stm,type = 'hist')
  
  stm::topicQuality(model = model.stm,documents = dfm2stm$documents)
  
  climate_words = c("greenhouse gas*",'climate change*','global warming','carbon emission*','climate impact*','ocean acidification*',
                    'alternative energy','anthropogenic emissions','carbon dioxide','extreme weather','storm surge',
                    'adaptive capacity','adaptation costs','renewable energy','sea level rise',
                    'sealevel rise')
  
  
  stm::labelTopics(model.stm,topics=c(11,28),n = 20)
  
  
  
  nsim = 25
  
  tva_skills_prediction = -0.004452  +  0.205216 * dfm2stm$meta$ideo_rating[dfm2stm$meta$AGENCY=='Tennessee Valley Authority'][1]
  
  dfm2stm$meta$skills_rating[dfm2stm$meta$AGENCY=='Tennessee Valley Authority']<-tva_skills_prediction
  
  
  
  
  # eff_all_un = stm::estimateEffect(stmobj = model.stm,formula = c(11,28)~YEAR + AGENCY + PROJECT_TOPIC +
  #                                    PROJECT_TYPE + skills_rating*ideo_rating,
  #                                  nsims = nsim,metadata = as.data.frame(dfm2stm$meta))
  # saveRDS(eff_all_un,paste0(where_is_scratch,'effect_estimates_withuncertainty.RDS'))
  eff_all_un = readRDS(paste0(where_is_scratch,'effect_estimates_withuncertainty.RDS'))
  
  
  coef_sum = summary(eff_all_un)
  coef11 = data.table(coef_sum$tables[[1]])
  coef11$topic = '11: climate-->project'
  coef11$coef = rownames(coef_sum$tables[[1]])
  coef28 = data.table(coef_sum$tables[[2]])
  coef28$topic = '28: project-->climate'
  coef28$coef = rownames(coef_sum$tables[[2]])
  coefs = rbind(coef11,coef28)
  
  coefs$GROUP[grepl('YEAR',coefs$coef)]<-'Year'
  coefs$GROUP[grepl('AGENCY',coefs$coef)]<-'Agency'
  coefs$GROUP[grepl('TOPIC',coefs$coef)]<-'Focus'
  coefs$GROUP[grepl('TYPE',coefs$coef)]<-'Task'
  coefs$GROUP[grepl('skill|ideo',coefs$coef)]<-'Skill/Ideol.'
  
  coefs$coef <- gsub('ideo','Ideology',coefs$coef)
  coefs$coef <- gsub('skill','Workforce skill',coefs$coef)
  coefs$coef <- gsub('PROJECT_TOPIC','Focus: ',coefs$coef)
  coefs$coef <- gsub('PROJECT_TYPE','Task: ',coefs$coef)
  coefs$coef <- gsub('YEAR','Year: ',coefs$coef)
  coefs$coef <- gsub('AGENCY','',coefs$coef)
  require(forcats)
  require(ggthemes)
  coefs$coef <- fct_inorder(coefs$coef)
  coefs$coef <- fct_rev(coefs$coef)
  
  
  topic_task_coefs = coefs[!grepl('Agency|Year',GROUP)&!grepl('Intercept|rating',coef),]
  topic_task_coefs$topic_or_task = ifelse(grepl('Topic',topic_task_coefs$coef),'Topic','Task')
  
  ggplot(topic_task_coefs) + #facet_wrap(~topic)+
    geom_vline(xintercept = 0,lty = 2,col = 'grey80')+
    geom_errorbarh(aes(xmin = Estimate-1.96*`Std. Error`,xmax = Estimate+1.96*`Std. Error`,
                       y= coef,col = topic),height = 0.2,position = position_dodge(0.3))+
    geom_point(aes(x = Estimate,y = coef,col = topic),position = position_dodge(0.3)) + 
    scale_color_colorblind()+ scale_x_continuous(limits=c(-0.02,0.02))+
    ylab('') + theme_bw() + 
    # facet_wrap(~topic_or_task)+
    theme(axis.title.y = element_blank(),legend.position = c(0.75,0.25),axis.text = element_text(size = 12)) + 
    xlab('Estimate 95% confidence intervals') + ggtitle('Topic focus regressed on focus and task')
  
  
  agency_coefs = coefs[grepl('Agency',GROUP),]
  agency_coefs$coef <- fct_reorder(agency_coefs$coef,agency_coefs$Estimate,.fun = mean)
  
  agency_coefs$ideo_rating = projs$ideo_rating[match(agency_coefs$coef,projs$AGENCY)]
  agency_coefs$skills_rating = projs$skills_rating[match(agency_coefs$coef,projs$AGENCY)]
  agency_coefs$ABBREV = projs$ABBREV[match(agency_coefs$coef,projs$AGENCY)]
  
  
  #facet_wrap(~topic)
  ggplot(agency_coefs) + geom_point(aes(y = Estimate,x = skills_rating,col = topic))
  sort(unique(projs$AGENCY))
  ggplot(agency_coefs) + #facet_wrap(~topic)+
    geom_vline(xintercept = 0,lty = 2,col = 'grey80')+
    geom_errorbarh(aes(xmin = Estimate-1.96*`Std. Error`,xmax = Estimate+1.96*`Std. Error`,
                       y= coef,col = topic),height = 0.2,position = position_dodge(0.3))+
    geom_point(aes(x = Estimate,y = coef,col = topic),position = position_dodge(0.3)) + #scale_x_continuous(limits = c(-0.02,0.03)) +
    scale_color_colorblind()+
    ylab('') + theme_bw() +
    theme(axis.title.y = element_blank(),axis.text = element_text(size = 10),legend.position = c(0.8,0.1)) + 
    xlab('Estimate 95% confidence intervals') + ggtitle('Topic focus regressed on lead agency') + 
    labs(caption = "ordered by agency mean estimate (both topics)")
  
  year_coefs = coefs[grepl('Year',GROUP)|grepl('Intercept',coef),]
  year_coefs$coef <- gsub('Year:','',year_coefs$coef)
  
  year_coefs=full_join(year_coefs,year_coefs[coef=='(Intercept)',list(mean(Estimate),mean(`Std. Error`)),by=.(topic)])
  year_coefs$coef[year_coefs$coef=='(Intercept)']<-'2013'
  year_coefs$coef <- as.numeric(year_coefs$coef)
  year_coefs$Estimate <- ifelse(year_coefs$coef==2013,year_coefs$Estimate,year_coefs$Estimate + year_coefs$V1)
  year_coefs$`Std. Error` <- ifelse(year_coefs$coef==2013,year_coefs$`Std. Error`,year_coefs$`Std. Error` + year_coefs$V2)
  
  sort(unique(projs$PROJECT_TOPIC))
  sort(unique(projs$PROJECT_TYPE))
  agency_coefs_melted = melt(agency_coefs,id.vars = c('topic','coef','GROUP','ABBREV','Estimate','Std. Error','t value','Pr(>|t|)'))
  
  
  agency_coefs_melted$variable<-ifelse(agency_coefs_melted$variable=='ideo_rating','ideology','workforce skill')
  ggplot(agency_coefs_melted) + 
    geom_smooth(aes(y = Estimate,x = value,col = 'blue'),se= F,method = 'glm',lwd =0.5)+
    geom_point(aes(y = Estimate,x = value)) +
    #geom_text_repel(aes(y = Estimate,x = value,label = ABBREV)) +
    facet_grid(topic~~variable) + scale_color_colorblind() + theme_bw() + 
    xlab('Agency attribute') + ylab('Agency fixed effect (topic probability)') + 
    scale_color_colorblind(label = 'linear trend') + 
    theme(legend.title = element_blank(),legend.position = c(0.75,0.6))  +
    ggtitle('Agency fixed effect vs. ideology/skill attributes')
  
  
  geom_point(aes(y = Estimate,x = skills_rating,col = topic,pch = 'Skill')) +
    facet_wrap(~topic) + theme_bw()
  
  
  htmlTable::htmlTable(round(coef_sum$tables[[1]],3))
  htmlTable::htmlTable(round(coef_sum$tables[[2]],3))
  
  ggplot(year_coefs) + #facet_wrap(~topic)+
    #geom_vline(xintercept = 0,lty = 2,col = 'grey80')+ 
    geom_hline(yintercept = 2016.5)+
    geom_errorbarh(aes(xmin = Estimate-1.96*`Std. Error`,xmax = Estimate+1.96*`Std. Error`,
                       y= coef,col = topic),height = 0.2)+
    geom_point(aes(x = Estimate,y = coef,col = topic)) + scale_x_continuous(limits = c(0,0.05)) +
    scale_color_colorblind()+ 
    geom_text(y=2014.5,x = 0.00,label='Obama')+
    geom_text(y=2018.5,x = 0.00,label='Trump')+
    ylab('') + theme_bw() + 
    theme(axis.title.y = element_blank(),legend.position = c(0.8,0.25)) + 
    xlab('Estimate 95% confidence intervals') + ggtitle('Topic focus regressed on year') + 
    coord_flip() +   labs(caption = "2013 = intercept; 2014+ = intercept +/- coef.
                        2014+ SSE = SE(intercept) +/- SE(coef)")
  
  
  
  
  
  eff_noagency = stm::estimateEffect(stmobj = model.stm,formula = c(11,28)~YEAR + PROJECT_TOPIC +
                                       PROJECT_TYPE + skills_rating + ideo_rating,uncertainty = 'None',
                                     nsims = nsim,metadata = as.data.frame(dfm2stm$meta))
  
  
  
  plot(eff_all_un,covariate = 'skills_rating',method = 'continuous',labeltype = 'custom',
       custom.labels = c('11: climate-->project','28 project-->climate'),main='Topic ~ workforce skill')
  plot(eff_all_un,covariate = 'ideo_rating',method = 'continuous',labeltype = 'custom',
       custom.labels = c('11: climate-->project','28 project-->climate'),main='Topic ~ agency ideology')
  
  
  cors = topicCorr(model.stm,method = 'simple',)
  
  
  ggcorrplot::ggcorrplot(cors$cor,type = 'upper',outline.color = NA) + 
    scale_fill_viridis_c() + geom_hline(yintercept = c(10.5,11.5,27.5,28.5),lty = 2,col = 'grey50')+
    geom_vline(xintercept = c(10.5,11.5,27.5,28.5),lty = 2,col = 'grey50')
  
  
  
  frequency <- colMeans(model.stm$theta)
  invrank <- order(frequency, decreasing = FALSE)
  
  
  plot.STM(model.stm,'summary')
  
  
  cbind(K = 1:90,invrank)
  
  
  model.stm$theta
  str(model.stm)
  plot.STM
  plot.STM(model.stm, type = "hist")
  ?plot.STM
  summary(eff_all_un)
  summary(eff_noagency)main = 
    
    
    rbind(data.table(coef_sum$tables[[1]])
          coef_sum$tables[[2]]$topic = '28'
          
          test$topics
          str(eff_all_un)
          eff_all_un$parameters
          plot(eff_all_un,covariate = 'AGENCY')
          summary(eff_all_un)
          
          length(dfm2stm$documents)
          eff_agency = stm::estimateEffect(stmobj = model.stm,formula = ~AGENCY,uncertainty = 'none',
                                           nsims = nsim,metadata = as.data.frame(dfm2stm$meta))
          eff_type = stm::estimateEffect(stmobj = model.stm,formula = ~PROJECT_TOPIC,
                                         nsims = nsim,metadata = as.data.frame(dfm2stm$meta))
          eff_task = stm::estimateEffect(stmobj = model.stm,formula = ~PROJECT_TASK,
                                         nsims = nsim,metadata = as.data.frame(dfm2stm$meta))
          eff_skill = stm::estimateEffect(stmobj = model.stm,formula = ~skill_rating,
                                          nsims = nsim,metadata = as.data.frame(dfm2stm$meta))
          eff_ideol = stm::estimateEffect(stmobj = model.stm,formula = ~ideo_rating,
                                          nsims = nsim,metadata = as.data.frame(dfm2stm$meta))
          
          
          htmlTable::htmlTable(summary(model.stm))
          findThoughts(model = model.stm,topics = c(1),n = 1,)
          
          
          
          
          stm::findTopic(model.stm,list('ocean_acidification'),n=20,verbose = T,type = 'score')
          stm::findTopic(model.stm,list('sealevel_rise'),n=20,verbose = T,type = 'score')
          stm::findTopic(model.stm,list('extreme_weather'),n=20,verbose = T,type = 'score')
          stm::findTopic(model.stm,list('anthropogenic emissions'),n=20,verbose = T,type = 'score')
          stm::findTopic(model.stm,list('climate_impacts'),n=20,verbose = T,type = 'score')
          
          tcor = stm::topicCorr(model.stm,cutoff = 0)
          
          cormelt = data.table(melt(tcor$cor))
          cormelt_uppertri = cormelt[Var1>Var2,]
          
          cormelt_uppertri[,max(value),by=.(Var1)][,mean(V1)]
          
          ### correlation with topic 11
          
          topic11_cor = cormelt_uppertri[Var1==11|Var2==11,][order(value),][value>0.1|value<(-0.1),]
          topic11_cor$Var1[topic11_cor$Var1==11] <- topic11_cor$Var2[topic11_cor$Var1==11]
          topic11_cor$Var2<-11
          topic11_cor_labels = stm::labelTopics(model = model.stm,topics=topic11_cor$Var1,n = 10)
          require(htmlTable)
          
          topic11_cor$value <- round(topic11_cor$value,3)
          htmlTable(topic11_cor)
          
          
          topic28_cor = cormelt_uppertri[Var1==28|Var2==28,][order(value),][value>0.1|value<(-0.1),]
          topic28_cor$Var1[topic28_cor$Var1==28] <- topic28_cor$Var2[topic28_cor$Var1==28]
          topic28_cor$Var2<-28
          topic28_cor_labels = stm::labelTopics(model = model.stm,topics=topic28_cor$Var1,n = 10)
          topic28_cor$value <- round(topic28_cor$value,3)
          htmlTable(topic28_cor)
          
          
          topic28_cor_labels
          
          topic11_cor_labels
          htmlTable(topic11_cor_labels)
          
          stm::labelTopics(model.stm,topics=c(11,28),n = 20)
          
          
          
          
          apply(topic11_cor_labels$prob,1,)
          topic11_cor_labels[[1]]
          
          str(topic11_cor_labels )
          topic11_cor_labels$prob
          
          
          cormelt_uppertri[Var1==28|Var2==28,][order(value),][value>0.075|value<(-0.075),]
          
          cormelt_uppertri[Var1==28|Var2==28,][order(value),]
          stm::plot.topicCorr(tcor)
          
          findTopic(model.stm,list = c('rise'),n = 20)
          findTopic(model.stm,list = c('sealevel'),n = 20)
          
          climate_words = c("greenhouse gas*",'climate change*','global warming','carbon emission*','climate impact*','ocean acidification*',
                            'alternative energy','anthropogenic emissions','carbon dioxide','extreme weather','storm surge',
                            'adaptive capacity','adaptation costs','renewable energy','sea level rise',
                            'sealevel rise')
          
          
          
          
          stm::findTopic(model.stm,list('climate_change'),n=20,verbose = T,type = 'score')
          stm::findTopic(model.stm,list('climate_change'),n=20,verbose = T,type = 'prob')
          stm::findTopic(model.stm,list('climate_change'),n=20,verbose = T,type = 'frex')
          stm::findTopic(model.stm,list('climate_change'),n=20,verbose = T,type = 'lift')
          
          
          
          
          