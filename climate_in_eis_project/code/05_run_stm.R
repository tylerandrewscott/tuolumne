
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
qdfm_eis <- dfm_trim(qdfm_eis,min_docfreq = 5) 
eis3 <- qdfm_eis@Dimnames$features
qdfm <- qdfm[,qdfm@Dimnames$features %in% eis3]

qdfm <- dfm_trim(qdfm, max_docfreq = 0.3,docfreq_type = 'prop')
qdfm <- qdfm[ntoken(qdfm)>0,]
qdfm <- qdfm[,!grepl('[x]{3,}',qdfm@Dimnames$features)]

sm <- colSums(qdfm)
data.table(qdfm@Dimnames$features,sm)[order(-sm),][1:60,]
custom_stops <- c('analysis','impact','action','alternatives','table','result','public','study','state','associated','eis','significant',
                  'however','can','chapter','figure')
qdfm <- qdfm[, !qdfm@Dimnames$features %in% custom_stops]
sm <- colSums(qdfm)
tail(data.table(qdfm@Dimnames$features,sm)[order(-sm),],40,)
grep('_',qdfm@Dimnames$features,value =T)
tm::stopwords()

ft = qdfm@Dimnames$features
hspell = hunspell_check(ft)
sug = hunspell_suggest(ft[!hspell])



for(i in seq_along(hspell)){
  if(!hspell[i])
  {
    hun
  }
}
}

hunspell::hunspell_suggest("ofenvironmental")
ft[!hspell]
table(hspell)


dim(qdfm)


tm::stemDocument()

grep('climate',qdfm_stem@Dimnames$features,value = T)
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
model.search <- stm(dfm2stm$documents, dfm2stm$vocab, K = 0, data = dfm2stm$meta,
                 prevalence = ~EIS.Number, init.type = "Spectral",verbose = T,#ngroups = 5,
                 seed = 24,max.em.its = 40,emtol = 0.0001) 
saveRDS(model.search,'climate_in_eis_project/scratch/base_stm_searchk.RDS')

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
saveRDS(model.cov,'climate_in_eis_project/scratch/cov_stm_80k.RDS')

other.stm <- lapply(c(30,45,60,75,105,120),function(k) {
  stm(dfm2stm$documents, dfm2stm$vocab, K = k, data = dfm2stm$meta,
      prevalence = ~EIS.Number + AGENCY + YEAR + PROJECT_TYPE + PROJECT_TOPIC+
        s(project_EAL) + s(project_SVI) + s(project_CR),
      init.type = "Spectral",verbose = T,ngroups = 5,
      seed = 24,max.em.its = 40,emtol = 0.0001) 
})
saveRDS(other.stm ,'climate_in_eis_project/scratch/temp_stm_Ksensitivity.RDS')

where_is_scratch <- 'climate_in_eis_project/scratch/'
model.stm = readRDS(paste0(where_is_scratch,'cov_stm_80k.RDS'))

          
          
          