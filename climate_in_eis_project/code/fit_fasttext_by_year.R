require(fastTextR)
require(parallel)
require(stringr)
require(data.table)

require(spacyr)
spacyr::spacy_initialize()
spacy_install()

2
install.packages('spacyr')


fbase = 'scratch/paragraphs_by_year/'
for(year in 2013:2020){
  year = '2013'
  fl = paste0(fbase,'drafts',year,'.txt')
#sents = readLines(fl,skipNul = T,encoding = 'utf-8',n = 100)
require(readr)
ft = fasttext()
cntl = ft_control(loss = 'softmax',epoch = 10L,min_count = 20L,word_vec_size = 100L,nthreads = 4,min_ngram = 1,max_ngram = 2,max_len_ngram = 2,window_size = 10)
ft$train(file = fl,method = 'supervised',control = cntl)

grep('climate',ft$words(),value = T)
ft$nearest_neighbors('drought')

scratch_loc = 'scratch/climate_in_nepa/fasttext_paragraph_models/'
dir.create(scratch_loc)
ft$save(file = paste0(scratch_loc,'wv_',year,'_model'))
}
