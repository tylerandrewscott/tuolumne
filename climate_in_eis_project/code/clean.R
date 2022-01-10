
packs = c('stm','data.table','stringr','pbapply','maps','quanteda','tm','parallel','devtools','tokenizers','DBI','hunspell','RSQLite','hunspell',
          'rvest','dplyr','lexicon','parallel','doParallel','qdapRegex')
sapply(packs[!sapply(packs,require,character.only = T)],install.packages)
sapply(packs,require,character.only = T)



rerun_spellcheck <- T


largeUS_dict <- 'climate_in_eis_project/input/hunspell-en_US-custom/en_US-custom.dic'

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
vecStrRemoveAll <- Vectorize(sequentialStrRemoveAll,vectorize.args = c('text'))
projs = readRDS('climate_in_eis_project/data_products/deis_metadata_with_covariates.RDS')
docs = readRDS('climate_in_eis_project/data_products/deis_doc_metadata.RDS')

text_loc = 'climate_in_eis_project/input/raw_corpus/'

flist = list.files(text_loc,pattern = 'corpus',full.names = T)
source('climate_in_eis_project/code/functions/splitWords.R')
#vSplit <- Vectorize(splitWords)
source('climate_in_eis_project/code/functions/FindCleanPages.R')
source('climate_in_eis_project/code/functions/splitCompounds.R')

itis_db <- dbConnect(RSQLite::SQLite(), "climate_in_eis_project/input/ITIS.sqlite")
verns <- dbGetQuery(itis_db, 'SELECT * FROM vernaculars')
verns <- verns[verns$language=='English',]
sci_names <- dbGetQuery(itis_db, 'SELECT * FROM longnames')

split_sci_names <- str_split(sci_names$completename,'\\s')
split_vern_names <- str_split(verns$vernacular_name,'\\s')

add_species_names <- c(tolower(sci_names$completename),unique(tolower(unlist(split_sci_names))),tolower(verns$vernacular_name),unique(tolower(unlist(split_vern_names))))

sciname_dictionary <- dictionary(lang = "en_US", affix = NULL, add_words = add_species_names, cache = TRUE)

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

for(file in flist){
  print(file)
  file <- flist[1]
  corp <- readRDS(file)
  corp <- FindCleanPages(corp)
  corp$text <- rm_email(corp$text)
  corp$text <- vapply(corp$text,FUN = sequentialStrRemoveAll,FUN.VALUE = character(1),patterns = stopws)
  corp$text <- tm::removePunctuation(corp$text,preserve_intra_word_dashes = T)
  corp$text <- tm::removeNumbers(corp$text)
  corp$text <- stringr::str_replace_all(corp$text,'\\s{2,}',' ')
  corp$text <- stringr::str_replace_all(corp$text,'^\\s|\\s$','')
  corp$text_l <- tolower(corp$text)
  check_corp <- pbsapply(corp$text,hunspell_find,dict = largeUS_dict,cl = detectCores()-1)
  unique_flags <- unique(unlist(check_corp))
  spell_dt <- data.table(bads = unique_flags)
  sugg_list <- pbsapply(spell_dt$bads,hunspell_suggest,dict = largeUS_dict,cl = detectCores()-1 )
  
  spell_dt$s1 <- sapply(sugg_list,function(x) x[1])
  spell_dt$s2 <- sapply(sugg_list,function(x) x[2])
  fname<-paste0('climate_in_eis_project/scratch/sugg_spell/',gsub('txt','rds',basename(file)))
  saveRDS(spell_dt,fname)
}
  

spell_list <- lapply(list.files('climate_in_eis_project/scratch/sugg_spell/',full.names = T),readRDS)
spell_dt <- rbindlist(spell_list)

spell_dt[grepl('\\s',s1)&!grepl('^[A-Z]',bads),][,.N,by=.(bads,s1,s2)][order(-N)][61:100,]
spell_dt[bads=='cowcalf']
c('sawgrass','stormwater','cattleguards','ecoregion','ecoregions','powerlines','geospatial','viewshed',
  'waterbody','waterbodies','streambank','streambanks','mainstem',)
spell_dt


corp$File
corp[grepl('cowcalf',corp$text),]$File
