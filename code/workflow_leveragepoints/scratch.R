library(tabulizer)
library(pdftools)
library(tokenizers)
library(data.table)
library(readtext)
flist = list.files('scratch/leverage_points/eis_text_files/')
floc = 'scratch/leverage_points/eis_text_files/'

temp_doc = readtext(paste0(floc,flist[1]))
test = tabulizer::extract_text('scratch/eis_documents/20150171_NOAA Restoration Center Final PEIS.pdf')
pars = tokenizers::tokenize_paragraphs(test)
sents = tokenizers::tokenize_sentences(test)


library(data.table)
lines = fread('glove.6B/glove.6B.300d.txt')

#devtools::install_github('https://github.com/bmschmidt/wordVectors')
library(wordVectors)
library(devtools)
glove6b = wordVectors::as.VectorSpaceModel(matrix = as.matrix(lines))

library(text2vec)


library(keras)
GLOVE_DIR = 'glove.6B/'

vec = read.vectors(filename = paste0(GLOVE_DIR,'glove.6B.100d.txt'),vectors = 100)

embeddings_index <- new.env(parent = emptyenv())
lines <- readLines(file.path(GLOVE_DIR, 'glove.6B.100d.txt'))
for (line in lines) {
  values <- strsplit(line, ' ', fixed = TRUE)[[1]]
  word <- values[[1]]
  coefs <- as.numeric(values[-1])
  embeddings_index[[word]] <- coefs}

word = c('water')
embeddings_index[[word]]


library(text2vec)
find_similar_words <- function(word, embedding_matrix, n = 5) {
  similarities <- embedding_matrix[word, , drop = FALSE] %>%
    sim2(embedding_matrix, y = ., method = "cosine")
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}



library(rvest)
wiki_main = 'p+ ul li , p'
wiki_base = 'https://en.wikipedia.org/wiki/'
wiki_topics = c('Groundwater','Wastewater')

get_wiki_text = function(topic,css) {
  require(rvest);require(tidyverse)
  url = paste0('https://en.wikipedia.org/wiki/',topic)
  text_vector = url %>% read_html() %>% html_nodes(css) %>% html_text(trim=T)
  text = gsub('\\[[0-9]{1,2}\\]','',paste(text_vector,collapse=' '))
  return(text)}

groundwater = get_wiki_text(topic = 'Groundwater',css = wiki_main)

library(tidytext)
text_df <- data_frame(line = 1, text = groundwater)
word_df = text_df %>% unnest_tokens(word, text)
word_df <- word_df %>% anti_join(stop_words)
word_filter = word_df %>% count(word, sort = TRUE) %>% filter(n>1)


library(tokenizers)


word_filter[1:20,]

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)



groundwater = c('aquifer','subsurface','subsidence','plume',"heavy metals",)
water_pollution = c('TMDL','nonpoint source','NPS pollution',"heavy metals","NPDES",'stormwater',
                    "water quality impacts","liquid effluents", 'effluent','effluents',
potable_water = c('')
water_habitat = c('')
irrigation = c('')
water_quantity = c('snowmelt')
flooding_and_storage = c('flood event','flooding','100 year','500 year','100-year','500-year','levee',
                         'reservoir','dam')

water_quality_words = c("TMDL",'nonpoint source','NPS pollution',"heavy metals","NPDES","run-off","water quantity",
                        "water quality impacts","liquid effluents",'stream health',"erosion control",
                        'effluent','effluents','groundwater','water delivery','sedimentation',
                        'hydrologic condition','snowmelt','turbidity','urban runoff','stormwater','surface runoff','pH',
                        'DO concentration','discharge water','flood event','flooding')




