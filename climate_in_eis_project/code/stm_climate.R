
require(remotes)
remotes::install_github("bstewart/stm",dependencies=TRUE)
require(stm)
require(data.table)
require(stringr)
eis_record = fread('climate_in_eis_project/data_products/eis_metadata_with_covariates.RDS')

hash_dir = 'climate_in_eis_project/data_products/eis_climate_hash/'
dir.create(hash_dir)
scratch_loc = hash_dir

segment_lists = lapply(2013:2020,function(year) readRDS(paste0('scratch/climate_in_nepa/climate_segments/temp_segments_',year,'.rds')))
segment_vector = unlist(segment_lists)
segment_vector = segment_vector[!grepl('\\.{10,}',segment_vector)]

segment_eis_ids = str_extract(names(segment_vector),'^[0-9]{8,}')

meta_eis = eis_record[match(segment_eis_ids,eis_record$EIS.Number),]
sample_index = sample(1:length(segment_vector))
processed_text = stm::textProcessor(segment_vector[sample_index],metadata = as.data.frame(meta_eis)[sample_index,],stem = F)
out <- prepDocuments(processed_text$documents, processed_text$vocab, processed_text$meta, lower.thresh = length(segment_vector)*0.025,verbose = T)
k_search = searchK(documents = out$documents,vocab = out$vocab,K = seq(4,20,1),heldout.seed = 24,cores = 6,proportion = 0.2)


out <- prepDocuments(processed_text$documents, processed_text$vocab, processed_text$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

out$meta$YEAR = str_extract(out$meta$EIS.Number,'^[0-9]{4}')
out$meta$DEC_DATE = lubridate::decimal_date(lubridate::mdy(out$meta$Federal.Register.Date)) - 2012





topicTestK <- manyTopics(out$documents,out$vocab,K=seq(2,20,2),prevalence =~ Lead.Agency + s(DEC_DATE), max.em.its=75, data=out$meta,seed=24)


str(topicTestK)
modFit$
plotModels(modFit)
 summary(modFit)
test = prepDocuments(documents = segment_vector[1:100])

modFit$mu

processed <- textProcessor(data$documents, metadata = data)


> plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
R> 




