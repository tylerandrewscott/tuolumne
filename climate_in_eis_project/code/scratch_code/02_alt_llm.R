
packs = c('stm','data.table','stringr','readtext','pbapply','maps','quanteda','tm','parallel','devtools','tokenizers','hunspell','rvest','dplyr')
sapply(packs[!sapply(packs,require,character.only = T)],install.packages)
sapply(packs,require,character.only = T)
if(!require(spacyr)){devtools::install_github("quanteda/spacyr", build_vignettes = FALSE);library("spacyr")}

### might need to run this to install miniconda 
### perhaps come back and hard wire this
#spacy_install()
#spacy_initialize()

source('climate_in_eis_project/code/functions/FindCleanPages.R')
redraw_corpus = TRUE
projs = readRDS('climate_in_eis_project/data_products/deis_metadata_with_covariates.RDS')
docs = readRDS('climate_in_eis_project/data_products/deis_doc_metadata.RDS')

text_loc = '../eis_documents/enepa_repository/text_as_datatable/'
flist = list.files(text_loc,recursive = T,full.names = T)
docs$tname <- str_replace(docs$File_Name,'pdf$','txt')
docs$tname = str_remove_all(docs$tname,'.+--')

flist = flist[basename(flist) %in% docs$tname]


tx = fread(flist[1])

library(transforEmotion)

# Example text
text <- neo_ipip_extraversion$friendliness[1:5] # positively worded items only
library(text)

text
# Run transformer function
scrs = transformer_scores(transformer = 'facebook/bart-large-mnli',
  text = tx$text,multiple_classes = F,
  classes = c('climate adaptation','climate mitigation')
)


tx$text[102]
textEm
temp_txt = fread(flist[1])






dir.create('climate_in_eis_project/scratch')
storage = 'climate_in_eis_project/scratch/yearly_quanteda_tokens/'
dir.create(storage)
cities = str_remove(maps::us.cities$name,'\\s[A-Z]{2}$')
counties = str_to_title(str_remove(maps::county.fips$polyname,'^.*\\,'))
wiki_landforms = 'https://en.wikipedia.org/wiki/List_of_landforms'
landforms = read_html(wiki_landforms) %>% html_nodes('.column-width a') %>% html_text() %>% str_to_title(.)

wiki_agencies = 'https://en.wikipedia.org/wiki/List_of_federal_agencies_in_the_United_States'
agencies_css = 'li , li a'
agencies = read_html(wiki_agencies ) %>% html_nodes(agencies_css) %>% html_text() 
agencies = str_remove(unlist(str_split(agencies,'\\s\\(|\\n')),'\\).*')
agencies = agencies[nchar(agencies)<100]
agencies = agencies[!duplicated(agencies)]

agency_pages = read_html('https://www.usa.gov/federal-agencies') %>% html_nodes('.group a') %>% html_attr('href')
letter_pages = paste0('https://www.usa.gov',c(gsub('b#','a#',agency_pages[1],fixed = T),agency_pages))
agency_lists = lapply(letter_pages, function(x) x %>% read_html() %>% html_nodes('.one_column_bullet .url') %>% html_text(trim = T))
agency_set = gsub('\\s\\(.*','',unlist(agency_lists))

special_stopwords = c(cities,counties,state.name,agency_set)
domain_stops <- c('Environmental Impact Assessment','Environmental Impact Statement','NEPA','EIS','DEIS','FEIS','FONSI','ROD',
                  'impact','action','Alternative','Alternatives','Table','result','public','study','state','associated',
                  'significant','Appendix','Executive Summary',
                  'however','can','Chapter','Figure')
extrastops <- c(special_stopwords,domain_stops)

source('climate_in_eis_project/code/functions/stopwordParallel.R')
stopws <- batchStopwords(extrastops)

gloss <- 'https://en.wikipedia.org/wiki/Glossary_of_environmental_science'
gloss_words <- read_html(gloss) %>% html_nodes('ul b a') %>% html_text(trim = T)
ngram2_gloss <- gloss_words[str_count(gloss_words,'\\s')==1]
ngram2_gloss <- tolower(ngram2_gloss[!grepl('[0-9]',ngram2_gloss)])

epa_gloss <- 'https://19january2017snapshot.epa.gov/climatechange/glossary-climate-change-terms_.html'
epa_words <- read_html(epa_gloss) %>% html_nodes('strong') %>% html_text(trim = T)
epa_words <- str_remove(epa_words,'\\s\\(.*')
ngram2_epa <- epa_words[str_count(epa_words,'\\s')==1]
ngram2_epa <- tolower(ngram2_epa)


for(corpus_name in flist[-1]){
  year <- str_extract(corpus_name,'[0-9]{4}')
  corp_file = paste0(storage,'tokens_',year,'.RDS')
  
  if(!file.exists(corp_file)|redraw_corpus){
    print(corpus_name)
    temp = readRDS(paste0(text_loc,corpus_name))
    temp = temp[str_extract(File,'^[0-9]{8}') %in% projs$EIS.Number,]
  
 
    clean_temp = FindCleanPages(temp)
    vecStrRemoveAll <- Vectorize(sequentialStrRemoveAll,vectorize.args = c('text'))
    clean_temp[,text:=pbsapply(text,function(x) {vecStrRemoveAll(x,patterns = stopws)},cl = 30)]
    clean_temp[,text:=tolower(text)]
    clean_temp[,text:=removePunctuation(text)]
    clean_temp[,text:=removeNumbers(text)]
    page_list = vector()
    # ##idea: invert the problem, batch the text, linearize the stopwoerd lists
    # ##### this isn't something that should be parallelized (I think) because editing simultaneously poses some aggregation issues
    # for(i in 1:length(stopword_splits)){
    #   print(i)
    #   pages = str_remove_all(pages,pattern = stopword_splits[[i]])
    # }
    source('climate_in_eis_project/code/functions/splitWords.R')
    #vSplit <- Vectorize(splitWords)
    clean_temp[,text:=pbsapply(text,splitWords,cl = detectCores()-1,USE.NAMES = F)]
    
    corp = corpus(clean_temp$text,docnames = clean_temp$File,docvars = clean_temp[,.(File,Page)],unique_docnames = F)
    toks = tokens(corp,remove_symbols = T,split_hyphens = F,padding = F,remove_url = T)
    toks = tokens_select(toks,pattern = stopwords("en"),selection = 'remove')
    compound_words = c("greenhouse gas*",'climate change*','global warming','carbon emission*','climate impact*','ocean acidification*',
                       'alternative energy','anthropogenic emissions','carbon dioxide','extreme weather','storm surge',
                       'adaptive capacit*','adaptation cost*','renewable energy','sea level rise',
                       'extreme heat','atmospheric river','paris agreement',
                       'renewable energy','solar energy','wind energy','geothermal energy',
                       'fossil fuel*','natural gas','climate mitigat*','climate adapt*',
                       'severe weather',
                       'sealevel rise','air quality','water quality','invasive species','land use','marine heat wave*',
                       'environmental impact statement*','flood control','heat wave*','atmospheric river*')
    compound_words <- unique(do.call(c,list(compound_words,ngram2_gloss,ngram2_epa)))

    toks = tokens_select(toks,min_nchar = 3,max_nchar = max(nchar(compound_words)))
    toks = tokens_compound(toks,pattern = phrase(compound_words))
    saveRDS(toks,paste0(storage,'tokens_',year,'.RDS'))
  }
}






