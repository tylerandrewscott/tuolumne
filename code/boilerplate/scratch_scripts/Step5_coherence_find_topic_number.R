library(tm)
library(quanteda)
library(data.table)
library(textclean)
library(stringr)
library(textmineR)
library(stm)
library(textmineR)

library(text2vec)


floc = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/'
setDTthreads(4)
eis = fread(paste(floc,'eis_used.csv'))


#floc = 'file_inputs/'
dt = readRDS(paste0(floc,'tokenized_page_epa_repo_750-10000chars.RDS'))

# gaz_floc = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/gazetteers/NationalFile_20181001.txt'
# gaz = fread(gaz_floc)
# gaznames = unique(gaz$FEATURE_NAME)
# gaznames = gsub(' \\(.*','',gaznames)
# gaznames = grep('^[A-Z]',gaznames,value=T)
# gaznames = textclean::replace_non_ascii(gaznames)
# gaznames = grep('\\\\',gaznames,invert = T,value=T)
# gaznames = grep('[0-9]',gaznames,invert=T,value=T)
# gaznames = gaznames[nchar(gaznames)>=4 & nchar(gaznames)<30]
# gaznames = unique(gaznames)
# gaznames = grep('\\)',gaznames,invert=T,value=T)
# gaznames = removePunctuation(gaznames)
# gaznames = gsub("'","",gaznames,fixed = T)
# gaznames = grep('"',gaznames,invert=T,value=T)


dt$doc_id = dt$index
test = F
test_n = 100000
if(test){dt = dt[sample(nrow(dt),test_n)]}else{dt = dt}
dt$text = removePunctuation(dt$text)
dt$text = stripWhitespace(dt$text)
dt$text = removeWords(dt$text,stopwords('english'))
dt$text = removeNumbers(dt$text)

dt$text  =tolower(dt$text)
# group <- 20000
# n <- length(gaznames)
# r <- rep(1:ceiling(n/group),each=group)[1:n]
# d <- split(gaznames,r)
# for (i in 1:length(d)) {dt$text = removeWords(dt$text,c(paste(d[[i]])))}

dfsource = tm::DataframeSource(dt[,.(doc_id,text)])
corpus = Corpus(dfsource)

snum = nrow(dt)*0.1

#k_list <- seq(100, 140, by = 5)
k_list <- seq(10,100, by=15)
simiters = rep(k_list,each = 5)
n_iter = 100
best_k = data.table(sim = as.numeric(),k = as.numeric(),coherence = as.numeric())

for (sim in seq_along(simiters)) {
    print(sim)
    k = simiters[sim]
    dtm <- CreateDtm(doc_vec = dt$text[sample(1:nrow(dt),snum)], # character vector of documents
                 doc_names = dt$index[sample(1:nrow(dt),snum)], # document names, optional
                 ngram_window = c(1,1), # minimum and maximum n-gram length
                 stopword_vec = stopwords::stopwords(source = "smart"), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = F, # punctuation - this is the default
                 remove_numbers = F, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 4)

    m <- FitLdaModel(dtm = dtm, 
                     k = k, 
                     iterations = 1000, 
                     burnin = 200,
                     alpha = 0.1,
                     beta = slam::col_sums(dtm) / sum(dtm) * 100,
                     optimize_alpha = TRUE,
                     calc_likelihood = FALSE,
                     calc_coherence = TRUE,
                     calc_r2 = FALSE,
                     cpus = 4)
    m$k <- k
   best_k =  rbind(best_k,data.table(sim = sim ,k = m$k,coherence = mean(m$coherence),stringsAsFactors = F))
}
saveRDS(best_k,paste0(floc,'topic_k_coherence_search_sim_results.RDS'))

# 
# txt = dt$text
# names(txt) = dt$index
# tokens = word_tokenizer(txt)
# it = itoken(tokens, progressbar = FALSE, ids = ids)
# vocab = create_vocabulary(it)
# vocab = prune_vocabulary(vocab, term_count_min = 5, doc_proportion_min = 0.02)
# dtm = create_dtm(it, vectorizer = vocab_vectorizer(vocab))
# 
# 
# dtm <- tm::DocumentTermMatrix(corpus, control = list(tokenize = 'Boost',stemming = FALSE, tolower = F,
#                                                      minWordLength = 3, removeNumbers = F, removePunctuation =F))
# #dtm = dtm[,!colnames(dtm) %in% gaznames]
# dtm = dtm[,slam::col_sums(dtm)>1&slam::col_sums(dtm)<{nrow(dtm)/2}]
# 
# # fit some LDA models and select the best number of topics
# k_list <- seq(5, 50, by = 5)
# 
# vocabulary = colnames(dtm)
# model_dir <- paste0(floc,"models_", digest::digest(vocabulary, algo = "sha1"))
# 
# MakeSparseDTM <- function(dtm){
#   # dtm is a simple triplet matrix
#   dtm.sparse <- Matrix::sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, 
#                                      dims=c(dtm$nrow, dtm$ncol))
#   
#   rownames(dtm.sparse) <- tm::Docs(dtm)
#   colnames(dtm.sparse) <- tm::Terms(dtm)
#   
#   return(dtm.sparse)
# }
# 
# sparse_dtm = MakeSparseDTM(dtm)
# 
# 
# model_list <- TmParallelApply(X = k_list,cpus = 5, FUN = function(k){
#   filename = file.path(model_dir, paste0(k, "_topics.RDS"))
#   if (!file.exists(filename)) {
#     m <- FitLdaModel(dtm = sparse_dtm , k = k, iterations = 100,burnin = 100,cpus = 1)
#     m$k <- k
#     m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dgc_reduced.dtm , M = 5)
#     save(m, file = filename)
#   } else {
#     load(filename)
#   }
#   m
# })
# 
# saveRDS(model_list,paste0(floc,'lda_model_list.RDS'))

