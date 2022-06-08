system('export OPENBLAS_NUM_THREADS=4 OMP_NUM_THREADS=4 MKL_NUM_THREADS=4')
library(tm)
library(quanteda)
library(data.table)
library(textclean)
library(stringr)
library(textmineR)
library(stm)

setDTthreads(6)

floc = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/'

kbest = readRDS(paste0(floc,"topic_k_search_sim_results.RDS"))
bestk = kbest[,mean(perplexity),by=.(k)]
topic_k = bestk$k[which.min(bestk$V1)]
#coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)),
#                            coherence = sapply(model_list, function(x) mean(x$coherence)),
#                            stringsAsFactors = FALSE)
#floc = 'file_inputs/'
dt = readRDS(paste0(floc,'tokenized_page_epa_repo_750-10000chars.RDS'))
#gaz_floc = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/gazetteers/NationalFile_20181001.txt'
#gaz = fread(gaz_floc)
#gaznames = unique(gaz$FEATURE_NAME)
#gaznames = gsub(' \\(.*','',gaznames)
#gaznames = grep('^[A-Z]',gaznames,value=T)
#gaznames = textclean::replace_non_ascii(gaznames)
#gaznames = grep('\\\\',gaznames,invert = T,value=T)
#gaznames = grep('[0-9]',gaznames,invert=T,value=T)
#gaznames = gaznames[nchar(gaznames)>4 & nchar(gaznames)<50]
#gaznames = unique(gaznames)
#gaznames = grep('\\)',gaznames,invert=T,value=T)
#gaznames = removePunctuation(gaznames)


#gaznames = gsub("'","",gaznames,fixed = T)

dt$doc_id = dt$index
test = F
test_n = 100000
if(test){dt = dt[sample(nrow(dt),test_n)]}else{dt = dt}
dt$text = removePunctuation(dt$text)
dt$text = stripWhitespace(dt$text)
dt$text = removeWords(dt$text,stopwords('english'))
dt$text = removeNumbers(dt$text)

#gtiles = dplyr::ntile(gaznames,2000)
#for(i in sort(unique(gtiles))){
#  dt$text = removeWords(dt$text,gaznames[gtiles==i])}

dt$text  =tolower(dt$text)
dfsource = tm::DataframeSource(dt[,.(doc_id,text)])
corpus = Corpus(dfsource)
dtm <- tm::DocumentTermMatrix(corpus, control = list(tokenize = 'Boost',stemming = FALSE, tolower = F,
                                                     minWordLength = 4,maxWordLength = 20, removeNumbers = F, removePunctuation =F))
#dtm = dtm[,!colnames(dtm) %in% gaznames]
dtm = dtm[,slam::col_sums(dtm)>1&slam::col_sums(dtm)<{nrow(dtm)/2}]
dtm = dtm[,nchar(colnames(dtm))<25]
epa = fread(paste0(floc,'eis_used.csv'))
epa_docs = fread(paste0(floc,'documents_used.csv'))

MakeSparseDTM <- function(dtm){
  # dtm is a simple triplet matrix
  dtm.sparse <- Matrix::sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, 
                                     dims=c(dtm$nrow, dtm$ncol))
  
  rownames(dtm.sparse) <- tm::Docs(dtm)
  colnames(dtm.sparse) <- tm::Terms(dtm)
  
  return(dtm.sparse)
}

sparse_dtm = MakeSparseDTM(dtm)

doc_meta = data.table(index = rownames(dtm))
doc_meta$EIS = dt$EIS[match(doc_meta$index,dt$index)]
doc_meta$Agency = epa$Agency[match(doc_meta$EIS,epa$EIS.Number)]
require(lubridate)
doc_meta$Date = decimal_date(mdy(epa$Federal.Register.Date[match(doc_meta$EIS,epa$EIS.Number)]))

model = stm(sparse_dtm,K = topic_k,seed = 24, prevalence = ~ s(Date) + Agency,init.type = 'Spectral', data = doc_meta,verbose = T)

saveRDS(model,paste0(floc,paste0('stm_model_k',topic_k,'.RData')))
