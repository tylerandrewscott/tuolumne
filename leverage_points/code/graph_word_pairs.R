library(tidytext)
library(tidyverse)
library(igraph)
library(data.table)
library(ggraph)
library(tokenizers)

sdf = fread('../../../../net/tmp/tscott1/tuolumne_scratch/scratch/leverage_points/classified_ca_sentences_waterclimatedrought.csv') %>%
  filter(California==1,P_Drought>0.5|P_Climate>0.5)
#sdf = sdf[sdf$P_Climate>0.5,]
sent_words <-sdf %>% #filter(California==1,P_Drought>0.5|P_Climate>0.5) %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)
sw = data.table(sent_words)
sent_ngrams = sent_words %>% unnest_tokens(ngram,word,token = 'ngrams',n=2)
#sent_skip_grams = sent_words %>% unnest_tokens(skip_gram,word,token = 'skip_ngrams',n=3)
sw$V = ifelse(grepl('^201[0-3]',sw$EIS_Number),'2011-2013',ifelse(grepl('^201[4-5]',sw$EIS_Number),'2014-2015','2016-2018'))
total_word_count = sw[,.N,by = .(word,V)]

library(widyr)
sent_word_pairs <- sent_words %>% 
  pairwise_count(word, SID, sort = TRUE, upper=F)

sent_word_pairs <- sent_words %>% 
  pairwise_count(word, SID, sort = TRUE, upper = FALSE)

sent_word_pairs_2011_2013 <- sent_words[grepl('^201[0-3]',sent_words$EIS_Number),]  %>% 
  pairwise_count(word, SID, sort = TRUE, upper = FALSE) %>%
  mutate(sents = sum(grepl('^201[0-3]',sent_words$EIS_Number)))

sent_word_pairs_2014_2015 <- sent_words[grepl('^201[4-5]',sent_words$EIS_Number),] %>% 
  pairwise_count(word, SID, sort = TRUE, upper = FALSE) %>% 
  mutate(sents = sum(grepl('^201[4-5]',sent_words$EIS_Number)))

sent_word_pairs_2016_2018 <- sent_words[grepl('^201[6-8]',sent_words$EIS_Number),] %>% 
  pairwise_count(word, SID, sort = TRUE, upper = FALSE) %>%
  mutate(sents = sum(grepl('^201[6-8]',sent_words$EIS_Number)))

sent_words_all = rbind(
  sent_word_pairs_2011_2013 %>% mutate(V = '2011-2013'),
  sent_word_pairs_2014_2015 %>% mutate(V = '2014-2015'),
  sent_word_pairs_2016_2018 %>% mutate(V = '2016-2018'))

sent_words_all$w  = sent_words_all$n/sent_words_all$sents
swa = data.table(sent_words_all)
swa[,rank := frankv(n,order=c(-1L),ties.method = 'min'),by=.(V)]



total_count = swa[,sum(n),by=.(item1,item2)]
setnames(total_count,'V1','Total_Count')
rel_pars = swa[!grepl('[0-9]',swa$item1)&!grepl('[0-9]',swa$item2),]
total_count[,rank := frankv(Total_Count,order=c(-1L),ties.method = 'min'),by=.(V)]
setkey(total_count,item1,item2)
setkey(rel_pars,item1,item2)
rel_pars = total_count[rel_pars,]


words_in_top_pairs <- unique(c(rel_pars$item1[rel_pars$rank<=20],rel_pars$item2[rel_pars$rank<=20]))
#top_pairs = rel_pars[rank<=40,]
top_pairs = rel_pars[item1%in% words_in_top_pairs&item2%in%words_in_top_pairs,]
top_pairs = top_pairs[Total_Count>=quantile(top_pairs$Total_Count,0.75),]

words_in_top_pairs20 <- unique(c(rel_pars$item1[rel_pars$rank<=20],rel_pars$item2[rel_pars$rank<=20]))
#top_pairs20 = rel_pars[rank<=40,]
top_pairs20 = rel_pars[item1%in% words_in_top_pairs20&item2%in%words_in_top_pairs20,]
top_pairs20 = top_pairs20[Total_Count>=quantile(top_pairs20$Total_Count,0.75),]

wordset = c(unique(t1$item1,t1$item2),unique(t2$item1,t2$item2),unique(t3$item1,t3$item2))
top_pairs = rel_pars[item1 %in% wordset|item2%in%wordset,]
top_pairs_99th = top_pairs[Total_Count>=quantile(top_pairs$Total_Count,0.99),]


t1 = swa[rank %in%1:10,][V=='2011-2013',][order(rank),][,.(item1,item2)]
t2 = swa[rank %in%1:10,][V=='2014-2015',][order(rank),][,.(item1,item2)]
t3 = swa[rank %in%1:10,][V=='2016-2018',][order(rank),][,.(item1,item2)]

cols = c('item1','item2')
t1[, x := Reduce(function(...) paste(..., sep = "+"), .SD), .SDcols = cols]
t2[, x := Reduce(function(...) paste(..., sep = "+"), .SD), .SDcols = cols]
t3[, x := Reduce(function(...) paste(..., sep = "+"), .SD), .SDcols = cols]

library(htmlTable)
htmlTable(cbind(t1[,.(x)],t2[,.(x)],t3[,.(x)]),header=c('2011-2013','2014-2015','2016-2018'),file = 'output/leverage_points/table1.html')



#rel_words = unique(c(swa$item1[swa$item2%in%c('climate','drought')],swa$item2[swa$item1%in%c('climate','drought')]))
#rel_words <- rel_words[!grepl('[0-9]',rel_words)]
#rel_pairs = swa[swa$n>2 & swa$item1 %in% rel_words|swa$item2 %in% rel_words,]
library(ggthemes)

pair_layout = graph_from_data_frame(top_pairs ,directed=F) %>%
  create_layout('linear',circular=T)
pair_layout$text_color = ifelse(pair_layout$name%in%c('drought','climate'),'red','black')
pair_layout$nudge_y = ifelse(pair_layout$y>0,0.1,-0.1)
pair_layout$nudge_x = ifelse(pair_layout$x<0,0.1,-0.1)
pair_layout$Word_Freq <- total_word_count$N[match(pair_layout$name,total_word_count$word)]


figure4 <- ggraph(pair_layout) +
  coord_fixed() +  
  geom_edge_arc(aes(edge_alpha = w,edge_width = w),colour = 'grey50',fold=T) +
  geom_node_point(aes(size = Word_Freq),fill='white',col = 'grey40',pch=21) +
  geom_node_text(aes(label = name,colour = text_color),repel = T,check_overlap = T,size=2.5,family = 'Times') + 
  facet_wrap(~V,ncol=3) + scale_edge_width(range=c(1,3)) +
  theme_void() + scale_color_identity(name = '') + 
  theme(text = element_text(family = 'Times',size=10),strip.text = element_text(face = 'bold')) + 
  scale_size_area() + 
  scale_x_continuous(limits=c(-1.2,1.2))+
  scale_y_continuous(limits=c(-1.2,1.2))+
  #scale_edge_width(name = 'Weighted frequency') + 
  guides(edge_alpha = FALSE,edge_width=F,size=F) +
  ggtitle('Frequency of word pairings in climate or drought sentences')

ggsave(figure4,filename = 'output/leverage_points/figure4.png',dpi = 500,width = 6,height = 2.5)


figure4

sent_ngram_pairs <- sent_ngrams %>% 
  pairwise_count(ngram, SID, sort = TRUE, upper = FALSE)

sent_ngram_pairs_2011_2013 <- sent_ngrams[grepl('^201[0-3]',sent_ngrams$EIS_Number),]  %>% 
  pairwise_count(ngram, SID, sort = TRUE, upper = FALSE) %>%
  mutate(sents = sum(grepl('^201[0-3]',sent_ngrams$EIS_Number)))

sent_ngram_pairs_2014_2015 <- sent_ngrams[grepl('^201[4-5]',sent_ngrams$EIS_Number),] %>% 
  pairwise_count(ngram, SID, sort = TRUE, upper = FALSE) %>% 
  mutate(sents = sum(grepl('^201[4-5]',sent_ngrams$EIS_Number)))

sent_ngram_pairs_2016_2018 <- sent_ngrams[grepl('^201[6-8]',sent_ngrams$EIS_Number),] %>% 
  pairwise_count(ngram, SID, sort = TRUE, upper = FALSE) %>%
  mutate(sents = sum(grepl('^201[6-8]',sent_ngrams$EIS_Number)))

sent_ngrams_all = rbind(
  sent_ngram_pairs_2011_2013[grepl('drought|climate',sent_ngram_pairs_2011_2013$item1)|
                              grepl('drought|climate',sent_ngram_pairs_2011_2013$item2),] %>% mutate(V = '2011-2013'),
  sent_ngram_pairs_2014_2015[grepl('drought|climate',sent_ngram_pairs_2014_2015$item1)|
                              grepl('drought|climate',sent_ngram_pairs_2014_2015$item2),] %>% mutate(V = '2014-2015'),
sent_ngram_pairs_2016_2018[grepl('drought|climate',sent_ngram_pairs_2016_2018$item1)|
                            grepl('drought|climate',sent_ngram_pairs_2016_2018$item2),] %>% mutate(V = '2016-2018'))

sent_ngrams_all$w  = sent_ngrams_all$n/sent_ngrams_all$sents

sent_ngrams_all[sent_ngrams_all$n>20,]
ggbase = sent_ngrams_all %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "circle") +
  geom_edge_link(aes(edge_alpha = w, edge_width = w), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name,col = ifelse(name%in%c('drought','climate'),'red','black')), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void() + guides(colour = FALSE,edge_width=F,edge_alpha=F) + 
  facet_wrap(~V,ncol = 2)

ggbase
#sent_skipgram_pairs <- sent_skip_grams %>% 
#  pairwise_count(skip_gram, SID, sort = TRUE, upper = FALSE)
library(lubridate)



library(ggplot2)
library(igraph)
library(ggraph)

sent_pairs_all = rbind(
sent_word_pairs_2011_2014[grepl('drought|climate',sent_word_pairs_2011_2014$item1)|
                            grepl('drought|climate',sent_word_pairs_2011_2014$item2),] %>% mutate(V = '2011-2014'),
sent_word_pairs_2015_2018[grepl('drought|climate',sent_word_pairs_2015_2018$item1)|
                            grepl('drought|climate',sent_word_pairs_2015_2018$item2),] %>% mutate(V = '2015-2018'))


ggbase = sent_pairs_all %>%
 filter(n >= 40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void() + 
  facet_wrap(~V,ncol = 2)

ggbase
sent_word_pairs_2015_2018[grepl('drought|climate',sent_word_pairs_2015_2018$item1)|
                            grepl('drought|climate',sent_word_pairs_2015_2018$item2),] %>%
  filter(n >= 40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()



sent_word_pairs_2015_2018 %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void() + ggtitle(paste0('Sentence word pairs, 2015-2018 (n = ',
                         sum(year(sent_words$FR_Date[!duplicated(sent_words$EIS_Number)]) %in% c(2015:2018)),
                         ")"))
                         
                         sum(year(sent_words$FR_Date[!duplicated(sent_words$EIS_Number)]) %in% c(2010:2014))
                         
                         
                         
                         



sent_ngram_pairs[grepl('drought',sent_ngram_pairs$item1)|grepl('drought',sent_ngram_pairs$item2),]

sent_ngram_pairs %>%
  filter(n >= 50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()




desc_word_pairs

library(ggplot2)
library(igraph)
library(ggraph)

set.seed(1234)
title_word_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()



head(sent_ngrams)

?unnest_tokens
library(widyr)
title_word_pairs <- sent_words  %>% 
  pairwise_count(word,SID, sort = TRUE, upper = FALSE)



title_word_pairs <- nasa_title %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)


title_word_pairs
sdf$text