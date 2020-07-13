library(wordVectors)
library(magrittr)
library(rvest)
library(lubridate)
library(data.table)

base = fread('input/epa_master_repository/eis_record_detail.csv')
base$Federal.Register.Date = mdy(base$Federal.Register.Date)
base = base[year(base$Federal.Register.Date)>=2013 & year(base$Federal.Register.Date)<2019,]
base$CA = grepl('CA',base$State.or.Territory)+0
base$Doc_Group = paste(ifelse(base$CA==1,'CA','US'),year(base$Federal.Register.Date),sep='_')

flist = list.files('scratch/eis_documents_plaintext/','txt')
floc = 'scratch/eis_documents_plaintext/'
nloc = 'scratch/leverage_points/cookbooks/'

groups = unique(base$Doc_Group)
lapply(seq_along(groups),function(x) {
  sub_flist = flist[grepl(paste(paste0('^',base$EIS.Number[base$Doc_Group==groups[x]]),collapse='|'),flist)]
  file.copy(from = paste0(floc,sub_flist),to = paste0('scratch/leverage_points/cookbooks/',groups[x],'/',sub_flist))
  })

perl -ne 's/[^A-Za-z_0-9 \n]/ /g; print lc $_; scratch/eis_documents_plaintext/"/*.txt > scratch/leverage_points/cookbooks/",groups,".txt")

combo_calls = paste0("perl -ne 's/[^A-Za-z_0-9 \n]/ /g; print lc $_;' scratch/leverage_points/cookbooks/",groups,"/*.txt > scratch/leverage_points/cookbooks/",groups,".txt")
sapply(combo_calls,system)

#perl -ne 's/[^A-Za-z_0-9 \n]/ /g; print lc $_;' cookbooks/*.txt > cookbooks.txt

# Then we *prepare* a single file for word2vec to read in. This does a couple things:
# 
# 1. Creates a single text file with the contents of every file in the original document;
# 2. Uses the `tokenizers` package to clean and lowercase the original text, 
# 3. If `bundle_ngrams` is greater than 1, joins together common bigrams into a single word. For example, "olive oil" may be joined together into "olive_oil" wherever it occurs.
# 
# You can also do this in another language: particularly for large files, that will be **much** faster.
# (For reference: in a console, `perl -ne 's/[^A-Za-z_0-9 \n]/ /g; print lc $_;' cookbooks/*.txt > cookbooks.txt` will do much the same thing on ASCII text in a couple seconds.) If you do this and want to bundle ngrams, you'll then need to call `word2phrase("cookbooks.txt","cookbook_bigrams.txt",...)` to build up the bigrams; call it twice if you want 3-grams, and so forth.

#'scratch/scratch/eis_documents_plaintext/'

"scratch/leverage_points/cookbooks/"
for (i in list.files('scratch/leverage_points/cookbooks','[0-9]{4}.txt',full.names = T)){
prep_word2vec(origin= i,destination=
                gsub('([0-9]{4})','\\1_cookbook',gsub('cookbooks/','cookbooks/',i)),
              lowercase=T,bundle_ngrams=2)}


prep_word2vec_file = function(file){ if(!file.exists(file)){
    prep_word2vec(origin= file,destination=gsub('([0-9]{4})','\\1_cookbook',file),lowercase=T,bundle_ngrams=2)}}

prep_word2vec_file(file = 'scratch/leverage_points/cookbooks/CA_2018.txt')

corp_list = list.files('scratch/leverage_points/cookbooks','txt',full.names = T)

for(i in corp_list){
  print(i)
prep_word2vec(origin= i,destination=gsub('([0-9]{4})','\\1_cookbook',gsub('cookbooks/','cookbooks/prepped_files/',i)),lowercase=T,bundle_ngrams=2)
}


prep_word2vec(origin= i,destination=gsub('([0-9]{4})','\\1_cookbook',gsub('cookbooks/','cookbooks/prepped_files/',i)),lowercase=T,bundle_ngrams=2)

prep_word2vec(origin = 'scratch/leverage_points/cookbooks/CA_2016.txt',
              destination = 'scratch/leverage_points/cookbooks/CA_2016.txt',lowercase = T,bundle_ngrams = 2)

prep_word2vec(origin= 'scratch/leverage_points/cookbooks/all_docs.txt',
              destination='scratch/leverage_points/cookbooks/all_docs_cookbook.txt',
              lowercase=T,bundle_ngrams=2)




if (!file.exists("eis_2013_cookbook.txt")) prep_word2vec(origin="cookbooks",destination="eis_2013_cookbook.txt",lowercase=T,bundle_ngrams=2)
#To train a word2vec model, use the function `train_word2vec`. This actually builds up the model. It uses an on-disk file as an intermediary and then reads that file into memory.
if (!file.exists("eis_2013_cookbook_vector.bin")) {model = train_word2vec("eis_2013_cookbook.txt","eis_2013_cookbook_vector.bin",vectors=100,threads=4,window=12,iter=10,negative_samples=0)} else model = read.vectors("eis_2013_cookbook_vector.bin")

if (!file.exists("eis_2014_cookbook.txt")) prep_word2vec(origin="cookbooks",destination="eis_2014_cookbook.txt",lowercase=T,bundle_ngrams=2)
#To train a word2vec model, use the function `train_word2vec`. This actually builds up the model. It uses an on-disk file as an intermediary and then reads that file into memory.
if (!file.exists("eis_2014_cookbook_vector.bin")) {model = train_word2vec("eis_2014_cookbook.txt","eis_2014_cookbook_vector.bin",vectors=100,threads=4,window=12,iter=10,negative_samples=0)} else model = read.vectors("eis_2014_cookbook_vector.bin")

if (!file.exists("eis_2015_cookbook.txt")) prep_word2vec(origin="cookbooks",destination="eis_2015_cookbook.txt",lowercase=T,bundle_ngrams=2)
#To train a word2vec model, use the function `train_word2vec`. This actually builds up the model. It uses an on-disk file as an intermediary and then reads that file into memory.
if (!file.exists("eis_2015_cookbook_vector.bin")) {model = train_word2vec("eis_2015_cookbook.txt","eis_2015_cookbook_vector.bin",vectors=100,threads=4,window=12,iter=10,negative_samples=0)} else model = read.vectors("eis_2015_cookbook_vector.bin")

if (!file.exists("eis_2016_cookbook.txt")) prep_word2vec(origin="cookbooks",destination="eis_2016_cookbook.txt",lowercase=T,bundle_ngrams=2)
#To train a word2vec model, use the function `train_word2vec`. This actually builds up the model. It uses an on-disk file as an intermediary and then reads that file into memory.
if (!file.exists("eis_2016_cookbook_vector.bin")) {model = train_word2vec("eis_2016_cookbook.txt","eis_2016_cookbook_vector.bin",vectors=100,threads=4,window=12,iter=10,negative_samples=0)} else model = read.vectors("eis_2016_cookbook_vector.bin")

if (!file.exists("eis_2017_cookbook.txt")) prep_word2vec(origin="cookbooks",destination="eis_2017_cookbook.txt",lowercase=T,bundle_ngrams=2)
#To train a word2vec model, use the function `train_word2vec`. This actually builds up the model. It uses an on-disk file as an intermediary and then reads that file into memory.
if (!file.exists("eis_2017_cookbook_vector.bin")) {model = train_word2vec("eis_2017_cookbook.txt","eis_2017_cookbook_vector.bin",vectors=100,threads=4,window=12,iter=10,negative_samples=0)} else model = read.vectors("eis_2017_cookbook_vector.bin")

if (!file.exists("eis_2018_cookbook.txt")) prep_word2vec(origin="cookbooks",destination="eis_2017_cookbook.txt",lowercase=T,bundle_ngrams=2)
#To train a word2vec model, use the function `train_word2vec`. This actually builds up the model. It uses an on-disk file as an intermediary and then reads that file into memory.
if (!file.exists("eis_2017_cookbook_vector.bin")) {model = train_word2vec("eis_2017_cookbook.txt","eis_2017_cookbook_vector.bin",vectors=100,threads=4,window=12,iter=10,negative_samples=0)} else model = read.vectors("eis_2017_cookbook_vector.bin")



# A few notes:
#   1. The `vectors` parameter is the dimensionality of the representation. More vectors usually means more precision, but also more random error and slower operations. Likely choices are probably in the range 100-500.
# 2. The `threads` parameter is the number of processors to use on your computer. On a modern laptop, the fastest results will probably be between 2 and 8 threads, depending on the number of cores.
# 3. `iter` is how many times to read through the corpus. With fewer than 100 books, it can greatly help to increase the number of passes; if you're working with billions of words, it probably matters less. One danger of too low a number of iterations is that words that aren't closely related will seem to be closer than they are.
# 4. Training can take a while. On my laptop, it takes a few minutes to train these cookbooks; larger models take proportionally more time. Because of the importance of more iterations to reducing noise, don't be afraid to set things up to require a lot of training time (as much as a day!)
# 5. One of the best things about the word2vec algorithm is that it *does* work on extremely large corpora in linear time.
# 6. In RStudio I've noticed that this sometimes appears to hang after a while; the percentage bar stops updating. If you check system activity it actually is still running, and will complete.
# 7. If at any point you want to *read in* a previously trained model, you can do so by typing `model =  read.vectors("cookbook_vectors.bin")`.

Now we have a model in memory, trained on about 10 million words from 77 cookbooks. What can it tell us about food?
  
  ## Similarity searches
  
  Well, you can run some basic operations to find the nearest elements:
  


With that list, you can expand out further to search for multiple words:
  
  ```{r}
model %>% 
  closest_to(model[[c("fish","salmon","trout","shad","flounder","carp","roe","eels")]],50)
```

Now we have a pretty expansive list of potential fish-related words from old cookbooks. This can be useful for a few different things:
  
  1. As a list of potential query terms for keyword search.
2. As a batch of words to use as seed to some other text mining operation; for example, you could pull all paragraphs surrounding these to find ways that fish are cooked.
3. As a source for visualization.

Or we can just arrange them somehow. In this case, it doesn't look like much of anything.

```{r}
some_fish = closest_to(model,model[[c("fish","salmon","trout","shad","flounder","carp","roe","eels")]],150)
fishy = model[[some_fish$word,average=F]]
plot(fishy,method="pca")
```

## Clustering

We can use standard clustering algorithms, like kmeans, to find groups of terms that fit together. You can think of this as a sort of topic model, although unlike more sophisticated topic modeling algorithms like Latent Direchlet Allocation, each word must be tied to single particular topic.

```{r}
set.seed(10)
centers = 150
clustering = kmeans(model,centers=centers,iter.max = 40)
```

Here are a ten random "topics" produced through this method. Each of the columns are the ten most frequent words in one random cluster.

```{r}
sapply(sample(1:centers,10),function(n) {
names(clustering$cluster[clustering$cluster==n][1:10])
})
```

These can be useful for figuring out, at a glance, what some of the overall common clusters in your corpus are.

Clusters need not be derived at the level of the full model. We can take, for instance, 
the 20 words closest to each of four different kinds of words.

```{r}
ingredients = c("madeira","beef","saucepan","carrots")
term_set = lapply(ingredients, 
function(ingredient) {
nearest_words = model %>% closest_to(model[[ingredient]],20)
nearest_words$word
}) %>% unlist

subset = model[[term_set,average=F]]

subset %>%
cosineDist(subset) %>% 
as.dist %>%
hclust %>%
plot

```


# Visualization

## Relationship planes.

One of the basic strategies you can take is to try to project the high-dimensional space here into a plane you can look at.

For instance, we can take the words "sweet" and "sour," find the twenty words most similar to either of them, and plot those in a sweet-salty plane.

```{r}
tastes = model[[c("sweet","salty"),average=F]]

# model[1:3000,] here restricts to the 3000 most common words in the set.
sweet_and_saltiness = model[1:3000,] %>% cosineSimilarity(tastes)

# Filter to the top 20 sweet or salty.
sweet_and_saltiness = sweet_and_saltiness[
rank(-sweet_and_saltiness[,1])<20 |
rank(-sweet_and_saltiness[,2])<20,
]

plot(sweet_and_saltiness,type='n')
text(sweet_and_saltiness,labels=rownames(sweet_and_saltiness))

```


There's no limit to how complicated this can get. For instance, there are really *five* tastes: sweet, salty, bitter, sour, and savory. (Savory is usually called 'umami' nowadays, but that word will not appear in historic cookbooks.)

Rather than use a base matrix of the whole set, we can shrink down to just five dimensions: how similar every word in our set is to each of these five. (I'm using cosine similarity here, so the closer a number is to one, the more similar it is.)
                                                                                                                                                         
                                                                                                                                                         ```{r}
                                                                                                                                                         
                                                                                                                                                         tastes = model[[c("sweet","salty","savory","bitter","sour"),average=F]]
                                                                                                                                                         
                                                                                                                                                         # model[1:3000,] here restricts to the 3000 most common words in the set.
                                                                                                                                                         common_similarities_tastes = model[1:3000,] %>% cosineSimilarity(tastes)
                                                                                                                                                         
                                                                                                                                                         common_similarities_tastes[20:30,]
                                                                                                                                                         ```
                                                                                                                                                         
                                                                                                                                                         Now we can filter down to the 50 words that are closest to *any* of these (that's what the apply-max function below does), and
use a PCA biplot to look at just 50 words in a flavor plane.

```{r}
high_similarities_to_tastes = common_similarities_tastes[rank(-apply(common_similarities_tastes,1,max)) < 75,]

high_similarities_to_tastes %>% 
  prcomp %>% 
  biplot(main="Fifty words in a\nprojection of flavor space")
```

This tells us a few things. One is that (in some runnings of the model, at least--there is some random chance built in here.) "sweet" and "sour" are closely aligned. Is this a unique feature of American cooking? A relationship that changes over time? These would require more investigation.

Second is that "savory" really is an acting category in these cookbooks, even without the precision of 'umami' as a word to express it. Anchovy, the flavor most closely associated with savoriness, shows up as fairly characteristic of the flavor, along with a variety of herbs.

Finally, words characteristic of meals seem to show up in the upper realms of the file.

# Catchall reduction: TSNE

Last but not least, there is a catchall method built into the library 
to visualize a single overall decent plane for viewing the library; TSNE dimensionality reduction.

Just calling "plot" will display the equivalent of a word cloud with individual tokens grouped relatively close to each other based on their proximity in the higher dimensional space.

"Perplexity" is the optimal number of neighbors for each word. By default it's 50; smaller numbers may cause clusters to appear more dramatically at the cost of overall coherence.

```{r}
plot(model,perplexity=50)
```

A few notes on this method:

1. If you don't get local clusters, it is not working. You might need to reduce the perplexity so that clusters are smaller; or you might not have good local similarities.
2. If you're plotting only a small set of words, you're better off trying to plot a `VectorSpaceModel` with `method="pca"`, which locates the points using principal components analysis.



# Create iterator over tokens
tokens <- space_tokenizer(all_text)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 10L)
# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
glove$fit(tcm, n_iter = 20)
word_vectors <- glove$get_word_vectors()

climate_change <- word_vectors["climate", , drop = FALSE] + word_vectors["change", , drop = FALSE]

cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)
# berlin     paris    munich    leipzig   germany 
# 0.8015347 0.7623165 0.7013252 0.6616945 0.6540700 

