

#devtools::install_github("quanteda/spacyr", build_vignettes = FALSE)
library("spacyr")
#spacy_install()
spacy_initialize()

require(tokenizers);require(rvest)

source('climate_in_eis_project/code/functions/FindCleanPages.R')
packs = c('stm','data.table','stringr','readtext','pbapply','maps','quanteda','tm')
sapply(packs[!sapply(packs,require,character.only = T)],install.packages)
sapply(packs,require,character.only = T)

redraw_corpus = TRUE

projs = readRDS('scratch/climate_in_nepa/eis_metadata.RDS')
docs = readRDS('scratch/climate_in_nepa/eis_doc_metadata.RDS')


text_loc = 'climate_in_eis_project/input/'
flist = list.files(text_loc,pattern = 'corpus')


dir.create('climate_in_eis_project/scratch')
storage = 'climate_in_eis_project/yearly_quanteda_tokens/'
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
stopword_breaks = paste0('\\b',special_stopwords,'\\b')
stopword_splits = split(stopword_breaks,dplyr::ntile(stopword_breaks,n = 40))
stopword_splits <- lapply(stopword_splits,paste,collapse = '|')
replace_toks = F
for(corpus_name in flist){
  year <- str_extract(corpus_name,'[0-9]{4}')
  corp_file = paste0(storage,'tokens_',year,'.RDS')
  
  if(!file.exists(corp_file)|replace_toks){
    print(corpus_name)
    temp = readRDS(paste0(text_loc,corpus_name))
    clean_temp = FindCleanPages(temp)
    page_list = vector()
    
    dim(temp)
    pages = clean_temp$text
    
    ##### this isn't something that should be parallelized (I think) because editing simultaneously poses some aggregation issues
    for(i in 1:length(stopword_splits)){
      print(i)
      pages = str_remove_all(pages,pattern = stopword_splits[[i]])
    }
    
    
    pages = tolower(pages)
    pages = removePunctuation(pages)
    pages = removeNumbers(pages)
    
    clean_temp$text <- pages
    
    corp = corpus(clean_temp$text,docnames = clean_temp$File,docvars = clean_temp[,.(File,Page)],unique_docnames = F)
    
    toks = tokens(corp,remove_symbols = T,split_hyphens = F,padding = F,remove_url = T)
    toks = tokens_select(toks,pattern = stopwords("en"),selection = 'remove')
    compound_words = c("greenhouse gas*",'climate change*','global warming','carbon emission*','climate impact*','ocean acidification*',
                       'alternative energy','anthropogenic emissions','carbon dioxide','extreme weather','storm surge',
                       'adaptive capacity','adaptation costs','renewable energy','sea level rise',
                       'sealevel rise','air quality','water quality','invasive species','land use',
                       'environmental impact statement*')
    toks = tokens_select(toks,min_nchar = 3,max_nchar = max(nchar(compound_words)))
    toks = tokens_compound(toks,pattern = phrase(compound_words))
    saveRDS(toks,paste0(storage,'tokens_',year,'.RDS'))
  }
}




