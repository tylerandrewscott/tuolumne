

#devtools::install_github("quanteda/spacyr", build_vignettes = FALSE)
library("spacyr")
#spacy_install()
spacy_initialize()


packs = c('stm','data.table','stringr','readtext','pbapply','maps','quanteda','tm')
sapply(packs[!sapply(packs,require,character.only = T)],install.packages)
sapply(packs,require)

redraw_corpus = TRUE

projs = readRDS('scratch/climate_in_nepa/eis_metadata.RDS')
docs = readRDS('scratch/climate_in_nepa/eis_doc_metadata.RDS')

text_loc = 'scratch/tokenized_paragraphs/'
flist = list.files(text_loc)
flist = flist[flist %in% gsub('pdf$','txt',docs$File_Name)]


year_splits = split(projs$EIS.Number,str_extract(projs$EIS.Number,'^[0-9]{4}'))

require(tokenizers);require(rvest)
storage = 'scratch/climate_in_nepa/yearly_quanteda_tokens/'
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
stopword_splits = split(stopword_breaks,dplyr::ntile(stopword_breaks,n = 20))
stopword_splits <- lapply(stopword_splits,paste,collapse = '|')



for(year in names(year_splits)){
  print(year)
  temp_ids = year_splits[[year]]
  temp_flist = flist[str_extract(flist,'^[0-9]{8}') %in% temp_ids]
  par_list = vector()
  
  for(f in temp_flist){
    file = f
    id = str_extract(file,'^[0-9]{8}')
    raw_pars = readLines(paste0(text_loc,file))
    pars = raw_pars#tokenize_paragraphs(raw_pars,paragraph_break = '.\n',simplify = T)
    for(i in 1:length(stopword_splits)){
      pars = str_remove_all(pars,pattern = stopword_splits[[i]])
    }
    pars = tolower(pars)
    pars = removePunctuation(pars)
    pars = removeNumbers(pars)
    names(pars) <- paste(file,'p',1:length(pars),sep = "_")
    par_list = append(x = par_list,pars)
  }
  corp = corpus(par_list)
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




