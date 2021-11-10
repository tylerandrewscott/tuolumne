
packs = c('stm','data.table','stringr','readtext','pbapply','maps','quanteda','tm','lubridate')
sapply(packs[!sapply(packs,require,character.only = T)],install.packages)
sapply(packs,require,character.only = T)

redraw_corpus = TRUE

projs = readRDS('climate_in_eis_project/data_products/eis_metadata_with_covariates.RDS')
docs = readRDS('climate_in_eis_project/data_products/eis_doc_metadata.RDS')

projs$DEC_DATE = decimal_date(mdy(projs$Federal.Register.Date))-2013

projs$STATE <- ifelse(nchar(projs$State.or.Territory)==2,projs$State.or.Territory,'Multi/Programmatic')

quanteda_toks = list.files('climate_in_eis_project/yearly_quanteda_tokens/',full.names = T)
tok_list = lapply(quanteda_toks,readRDS)

#tok_all = tok_all + tok_list[[8]]
tok_all =do.call("+",tok_list)

qdfm = dfm(tok_all)
qdfm_freq <- dfm_trim(qdfm, min_docfreq = 2)
qdfm_freq <- dfm_trim(qdfm_freq, max_docfreq = 0.05,docfreq_type = 'prop')

meta_eis = projs[match(str_remove(qdfm_freq@Dimnames$docs,'_.*'),projs$EIS.Number),]
meta_eis$EIS.Number <- as.character(meta_eis$EIS.Number)
meta_eis$ID = qdfm_freq@Dimnames$docs
K = 40
dfm2stm <- convert(qdfm_freq, to = "stm")
meta_eis_sub = meta_eis[ID %in% names(dfm2stm$documents),]
dfm2stm$meta = meta_eis_sub

model.stm <- stm(dfm2stm$documents, dfm2stm$vocab, K = K, data = dfm2stm$meta,
                 prevalence = ~AGENCY + EIS.Number + TYPE + STATE + s(DEC_DATE),#model = model.stm,
                 init.type = "Spectral",verbose = T,seed = 24,max.em.its = 60,emtol = 0.0001) 


saveRDS(model.stm,'temp_stm_40k.RDS')

summary(model.stm)
?findTopic

doc_length = length(unique(str_remove(dfm2stm$meta$ID,'_p_[0-9]{1,}$')))
proj_length = length(unique(dfm2stm$meta$EIS.Number))
par_length = length(dfm2stm$documents)


prep <- estimateEffect(1:10 ~ AGENCY + EIS.Number + s(DEC_DATE), stmobj = model.stm,metadata = as.data.frame(meta_eis), uncertainty = "Global")


climate_phrases = c('carbon_dioxide')#,'carbon_dioxide','greenhouse_gas')
findTopic(model.stm, list = list(climate_phrases), n = 20, type = c("prob", "frex", "lift", "score"),
          verbose = TRUE)
labelTopics(model.stm)
summary(model.stm)
plot(model.stm)
findTopic(model.stm, list = list('water'), n = 10, type = c("prob", "frex", "lift", "score"),
          verbose = TRUE)


s
class(meta_eis)

R> summary(prep, topics=1)

#29 is climate change
plot(model.stm)
labelTopics(model.stm)
summary(model.stm)
estimateEffect()
summary(model.stm)
meta_eis$EIS.Number
1.335e-04

3.636e-04


grep('Plan',projs$Title,value = T)


table(meta_eis$AGENCY)
meta_eis

load("UNgeneraldebate.stm.RData")
data.frame(t(labelTopics(model.stm, n = 10)$prob))




dfm_keep(qdfm,)
topfeatures(qdfm)

qdfm

quanteda_options("print_dfm_max_nfeat")
dim(qdfm)

qdfm

#processed_text = stm::textProcessor(segment_vector[sample_index],metadata = as.data.frame(meta_eis)[sample_index,],stem = F)
out <- prepDocuments(processed_text$documents, processed_text$vocab, processed_text$meta, lower.thresh = length(segment_vector)*0.025,verbose = T)
k_search = searchK(documents = out$documents,vocab = out$vocab,K = seq(4,20,1),heldout.seed = 24,cores = 6,proportion = 0.2)

