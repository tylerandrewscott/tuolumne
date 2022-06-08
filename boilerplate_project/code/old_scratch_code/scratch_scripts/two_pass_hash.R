library(textreuse)
library(data.table)
library(tokenizers)
library(rvest)
library(readtext)
library(pdftools)
library(pbapply)
fdir = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/leverage_points/ca_water_energy_pdfs/'
flist = list.files(fdir)
base_text_list = pblapply(flist,function(f) pdf_text(paste0(fdir,f)),cl = 8)
names(base_text_list) <- flist


loc = fdir
toc_list = pblapply(seq_along(base_text_list)[-c(21:24,40:41)],function(i){
  print(i)
fname = names(base_text_list)[i]
temp_tx = base_text_list[[i]]
temp_tx = gsub('^\\s{1,}','',temp_tx)
chapter_nums = sort(as.numeric(str_extract(toupper(str_extract(temp_tx ,'(CHAPTER|Chapter|S E C T I O N)\\s[0-9]{1,2}')),'[0-9]{1,}')))
chapter_nums = chapter_nums[chapter_nums %in% names(table(chapter_nums))[table(chapter_nums)>1]]
chapter_nums <- sort(unique(chapter_nums))
chapter_from_last <- cumsum(c(1, diff(chapter_nums) - 1))
chapter_run_length <- rle(chapter_from_last)
chapter_nums = chapter_nums[which(chapter_from_last == with(chapter_run_length , values[which.max(lengths)]))]

toc = data.table(Chapter = c('TOC','ES',chapter_nums),Start = NA,End = NA,Name = NA)
for(j in 1:nrow(toc)){
  chapter = toc$Chapter[j]
  if(chapter == 'TOC'){first_pass = grep('TABLE OF CONTENTS',toupper(temp))}
  if(chapter == 'ES'){first_pass = grep('EXECUTIVE SUMMARY',toupper(temp))}
  #else if(chapter == 'APPX'){first_pass = grep('APPENDIX',toupper(temp))}
  if(!chapter %in% c('TOC','ES')){
    string_search = paste0('CHAPTER ',chapter,'[^0-9]')
    first_pass = grep(string_search,toupper(temp))}
diff_from_last <- cumsum(c(1, diff(first_pass) - 1))
run_length <- rle(diff_from_last)
chapter_pages = first_pass[which(diff_from_last == with(run_length, values[which.max(lengths)]))]
toc$Start[j] <- min(chapter_pages)
toc$End[j] <- max(chapter_pages)
if(chapter %in% c('TOC','ES')){toc$Name[j]<-chapter}
if(!chapter %in% c('TOC','ES')){
name_candidates = gsub('Chapter[^A-Z]+','',str_extract(temp[chapter_pages],paste0('Chapter ',chapter,'[^\\n]+')))
name = names(table(name_candidates))[table(name_candidates)==max(table(name_candidates))]
toc$Name[j] <- name
if(name == 'Index'){toc = toc[1:j,];break}
}
rm(first_pass);rm(chapter)
}
toc$File = fname
toc},cl = 1)




align_local('text that is not the same cannot be matched ever under any circumstances',
'Lebron James went to play basketball in Cleveland and then in Miami and then back to Cleveland')


toc
paste0('CHAPTER ',chapter,'\\s-\\s\\w')



grep('CHAPTER 1 INTRODUCTION',toupper(temp[chapter_pages]))

rm(first_pass)
}



toc
grep('TABLE OF CONTENTS',toupper(temp))


toc
chapter
paste0('CHAPTER ',chapter,'[^0-9]'),toupper(temp))


15 + 1 %in% chapter_nums | 15 - 1 %in% chapter_nums 

15 + 1 %in% chapter_nums
chapter_nums + 1 %in% chapter_num


temp <- gsub('^\\s{1,}','',temp_tx)
      start_executive_summary = min(grep('^Executive Summary|EXECUTIVE SUMMARY\\n',gsub('^\\s{1,}','',temp)))
      start_introduction = min(grep('^(CHAPTER|Chapter) [0-9](\\.\\s|\\n|\\s{2,})(Introduction|Purpose and Need for Action|INTRODUCTION|PURPOSE AND NEED)|INTRODUCTION\\*\\n',temp))
      start_alternatives = min(grep('^(CHAPTER|Chapter) [0-9](\\.\\s|\\n|\\s{2,})(Alternatives|Proposed Action and Alternatives|Proposed Plan Amendment|PROPOSED ACTION)|ALTERNATIVES\\*\\n',temp))
      start_selected = min(grep('(Chapter|CHAPTER) [0-9]\\s{2,}TENTATIVELY SELECTED PLAN\\n',temp))
      start_recommendations = min(grep('(Chapter|CHAPTER) [0-9]\\s{2,}RECOMMENDATIONS\\n',temp))
      start_recommendations = min(grep('(Chapter|CHAPTER) [0-9]\\s{2,}LIST OF PREPARERS\\n',temp))
      start_glossary= min(grep('(Chapter|CHAPTER) [0-9]\\s{2,}LIST OF PREPARERS\\n',temp))
       start_affected = min(grep('^(CHAPTER|Chapter) [0-9](\\.\\s|\\n|\\s{2,})(Affected Environment|AFFECTED ENVIRONMENT)|AFFECTED ENVIRONMENT\\*\\n',temp))
      start_impacts = min(grep('^(CHAPTER|Chapter) [0-9](\\.\\s|\\n|\\s{2,})(Environmental Consequences|Environmental Effects|ENVIRONMENTAL CONSEQUENCES|ENVIRONMENTAL EFFECTS)|ENVIRONMENTAL CONSEQUENCES\\*\\n',temp))
      start_consultation_coordination_compliance = min(grep('^(CHAPTER|Chapter) [0-9](\\.\\s|\\n|\\s{2,})(Consultation and Coordination|CONSULTATION AND COORDINATION)|CONSULTATION AND COORDINATION\\*\\n|COMPLIANCE\\*\\n',temp))
      start_comments = min(grep('^(CHAPTER|Chapter) [0-9](\\.\\s|\\n|\\s{2,})(PUBLIC COMMENTS|Public Comments)|PUBLIC COMMENTS\\*\\n',temp))
      start_cumulative = min(grep('^(CHAPTER|Chapter) [0-9](\\.\\s|\\n|\\s{2,})(CUMULATIVE EFFECTS|Cumulative Effects)|CUMULATIVE EFFECTS\\*\\n',temp))
      start_refs = min(grep('^(CHAPTER|Chapter) [0-9](\\.\\s|\\n|\\s{2,})(REFERENCES|References)|REFERENCES\\*\\n',temp))
      start_appendix = min(grep('^APPENDIX|APPENDIX\\*\\n',temp))
      grab_pages = sapply(ls(pattern = 'start_[a-z]'),get)
      sections = grab_pages[grab_pages!=Inf]
      data.table(section= names(sections),start = cbind(sections))[order(start.sections),]
      rm(list=ls(pattern = 'start_[a-z]'))
    }
    else{
    start_0_executive_summary = min(grep('^Executive Summary',gsub('^\\s{1,}','',temp)))
    start_1_introduction = min(grep('^(CHAPTER|Chapter) 1(\\.\\s|\\n|\\s{2,})(Introduction|Purpose and Need for Action|INTRODUCTION|PURPOSE AND NEED)',temp))
    start_2_alternatives = min(grep('^(CHAPTER|Chapter) 2(\\.\\s|\\n|\\s{2,})(Alternatives|Proposed Action and Alternatives|Proposed Plan Amendment|PROPOSED ACTION)',temp))
    start_3_affected = min(grep('^(CHAPTER|Chapter) 3(\\.\\s|\\n|\\s{2,})(Affected Environment|AFFECTED ENVIRONMENT)',temp))
    start_4_impacts = min(grep('^(CHAPTER|Chapter) 4(\\.\\s|\\n|\\s{2,})(Environmental Consequences|Environmental Effects|ENVIRONMENTAL CONSEQUENCES|ENVIRONMENTAL EFFECTS)',temp))
    start_5_cumulative = min(grep('^(CHAPTER|Chapter) 5(\\.\\s|\\n|\\s{2,})(CUMULATIVE EFFECTS|Cumulative Effects)',temp))
    start_6_consultation = min(grep('^(CHAPTER|Chapter) 6(\\.\\s|\\n|\\s{2,})(Consultation and Coordination|CONSULTATION AND COORDINATION)',temp))
    start_7_comments = min(grep('^(CHAPTER|Chapter) 7(\\.\\s|\\n|\\s{2,})(PUBLIC COMMENTS|Public Comments)',temp))
    start_8_refs = min(grep('^(CHAPTER|Chapter) 8(\\.\\s|\\n|\\s{2,})(REFERENCES|References)',temp))
    start_9_appendix = min(grep('^APPENDIX',temp))
    }
    empty[[i]] <- sapply(ls(pattern = 'start_[0-9]'),get)
    rm(list=ls(pattern = 'start_[a-z'))
  }


empty

flist[1]

  rm(list=ls(pattern = 'start_[0-9]'))
}

empty



flist = flist[1:10]
base_text_list = pblapply(flist,function(f) pdf_text(paste0(fdir,f)))
names(base_text_list) <- flist

doc_list_by_section = pblapply(seq_along(base_text_list),function(f) {
txt = base_text_list[[f]]
doc = tokenize_paragraphs(txt)
index = seq(1,length(doc),50)
split_factor = c(rep(1:{length(index)-1},each = 50),rep(length(index),length(doc)-max(index)+1))
split_doc = split(doc,split_factor)
split_doc_list = lapply(seq_along(split_doc),function(x) 
  data.table(text = paste(split_doc[[x]],collapse=' '),file = flist[f],section = x))
temp = rbindlist(split_doc_list)
temp
})

doc_list_dt = rbindlist(doc_list_by_section)


library(zoo)
library(pbapply)
ngram = 10

ngram_hash_dt = rbindlist(pblapply(seq_along(doc_list_dt$text),function(x) {
  gram <- tokenize_ngrams(doc_list_dt$text[x],n = ngram)
  gram_hash <- hash_string(unlist(gram))
  td = data.table(gram_hash,ngram = unlist(gram),index = 1:length(gram_hash))
  td$doc = doc_list_dt$file[x]
  td$section = doc_list_dt$section[x]
  rm(gram);rm(gram_hash)
  td}))


mult_grams = ngram_hash_dt[!duplicated(paste(gram_hash,doc,section,sep='_')),][,.N,by = gram_hash][N>1,]
ngram_mult_dt = ngram_hash_dt[gram_hash %in% mult_grams$gram_hash,]
false_positives = ngram_mult_dt[,length(unique(ngram)),by=.(gram_hash)][V1>1,]

ngram_mult_dt <- ngram_mult_dt[!ngram_mult_dt$gram_hash%in%false_positives$gram_hash,]


candidate_pairs <- ngram_mult_dt
candidate_pairs$uid = paste(candidate_pairs$doc,candidate_pairs$section,sep='_')
doc_pairs = candidate_pairs[, {i1 <-  combn(uid, 2)
list(i1[1,], i1[2,]) }, by = gram_hash]
doc_pairs = doc_pairs[V1!=V2,]
freq_pairs = doc_pairs[,.N,by=gram_hash][order(-N),][N>5,]
doc_pairs = doc_pairs[!gram_hash %in% freq_pairs$gram_hash,]

doc_pairs = doc_pairs[!duplicated(doc_pairs),]
combos_to_search = doc_pairs[!duplicated(paste0(V1,V2)),.(V1,V2)]



doc_list_dt$uid = paste(doc_list_dt$file,doc_list_dt$section,sep='_')

d = 1
str(tcorp)
t1 = TextReuseTextDocument(text = doc_list_dt$text[doc_list_dt$uid == combos_to_search$V1[d]],meta = list(id = combos_to_search$V1[d]))
t2 = TextReuseTextDocument(text = doc_list_dt$text[doc_list_dt$uid == combos_to_search$V2[d]],meta = list(id = combos_to_search$V2[d]))

class(t1)
test = align_local(t1,t2)

test$score
names(tcorp)
test = TextReuseTextDocument(doc_list_dt$text[doc_list_dt$uid == combos_to_search$V1[d]],id = combos_to_search$V1[d])

test = align_local(a = doc_list_dt$text[doc_list_dt$uid == combos_to_search$V1[d]],
             b = doc_list_dt$text[doc_list_dt$uid == combos_to_search$V2[d]])


?align_local
str(test)
test$score
summary(test)


pdf_loc = '../../../../net/tmp/tscott1/tuolumne_scratch/eis_main_pdfs/'
pdf_files = list.files(pdf_loc,full.names = T)
flist = list.files(fdir,full.names = T)

test = tokenize_lines(tx)
tx = readLines(flist[1])
str(tx)
tcorp = readtext(flist)



text_from_pdf = pblapply(pdf_files,function(x) pdf_text(x),cl = 8)
pdf_files


str_split()
tcorp$text = sapply(tcorp$text,function(x) substr(x, 1, 100000))
gc()




doc_pairs[1:10,]

test = textreuse::align_local(tcorp$text[13],tcorp$text[14])





pdf_files[2]
test = pdf_text(pdf_files[2])
line_list = pbsapply(test,function(x) tokenize_lines(x))

length(line_list[[16]])
sapply(line_list,length)
line_list[1:10]
chars = as.numeric(pbsapply(test,nchar))

drop_front_matter = 1:min(which(chars>2000))
chars[1:50]


t3 = test[15]
tokenize_lines(t3)



which(chars==0)
which("This page intentionally left blank."==test)
test[4]

pdf_info(pdf_files[1])


test

str(test)
doc_pairs[order(V1,V2),]

tcorp$text[1]

dim(tooinfreq_pairs)
candidate_pairs[gram_hash==-493088565,]

setDT(sample)[, {i1 <-  combn(number, 2)
list(i1[1,], i1[2,]) }, by =  group]

candidate_pairs[,apply(combn(x = doc,m = 2,simplify = F),by=.(gram_hash)]


as.matrix(table(ngram_mult_dt$gram_hash,ngram_mult_dt$doc)))



ngram_mult_dt[,.N,by=.(gram_hash)]


dups = ngram_mult_dt[!duplicated(paste0(ngram,doc)),][duplicated(ngram),]

ngram_mult_checked = ngram_mult_dt[ngram%in%dups$ngram,]


ngram_mult_checked[gram_hash==-1212290705,]
ngram_mult_checked[duplicated(gram_hash)&!duplicated(ngram),]

test = ngram_mult_dt[ngram %in% ngram[duplicated(ngram)],]



?table
table(test$ngram)
test[duplicated(gram_hash)&!duplicated(ngram),]
?tabulate

test[ngram=='2 stat secretary of the',]
ngram_mult_dt[]
head(ngram_mult_dt)
ngram_hash_dt[ngram=='the colorado river state water',]

ngram_hash_dt[!duplicated(gram_hash)&duplicated(ngram),]
ngram_hash_dt[gram_hash==-737455855,]


ngram_hash_dt[duplicated(gram_hash,doc),]

ngram_hash_dt[gram_hash==1241214575,]

edt = data.table()
lapply(seq_along(tcorp$text),function(x){
w = tokenize_words(tcorp$text[x])
h = sapply(w,hash_string)
dict_dt = data.table(word = unlist(w), hash = as.vector(h),doc = tcorp$doc_id[x])
edt <<- rbind(edt,dict_dt,use.names = T)
})

full_dictionary  = edt

freq_hash = ngram_hash_dt[,.N,by = gram_hash]
freq_hash = freq_hash[N>1,]

ngram_hash_dtmatch(freq_hash$gram_hash,ngram_hash_dt$gram_hash)


dim(freq_hash)

dim(ngram_hash_dt)



full_dictionary[,ngram_hash = rollapplyr(hash_string,function(x) hash_string(x:(x + n - 1)))]



hash_string(c('123','332'))
table(duplicated(full_dictionary))


test = tokenize_ngrams(tcorp$text[1],n=5)
length(test[[1]])

tt = full_dictionary[doc==tcorp$doc_id[1],]
dim(tt)

10422 = N + 5 - 1

10418


length(test)
str(test)



tokenize_words()


tcorp = TextReuseCorpus(paths =  flist, meta = list(id = basename(file)),skip_short = T,simplify=T,
                        tokenizer = tokenizers::tokenize_ngrams,n = 5,keep_tokens = T)

comparisons <- pairwise_compare(tcorp, jaccard_similarity)
hash_string('baseball')
test = lsh(x = tcorp,bands = )
rm(tcorp)
rm(corpus)
?lsh

comparisons

txtf = readtext(fdir)

test = as.character(txtf$text[10])
textreuse::TextReuseTextDocument(file = flist[2])
trcor = TextReuseCorpus(paths = flist[1:10],meta = list(id = basename(flist[1:10])),
                        tokenizer = tokenize_ngrams, n = 5,
                        keep_tokens = TRUE,)
str(trcor)

flist = list.files('../../../../net/tmp/tscott1/tuolumne_scratch/scratch/leverage_points/ca_text_files/',full.names = T)
txtf = readtext(flist)

str(txdt)

test_pages = c('Seattle_Washington','Beer','Whales','Shellfish','Trees','Basketball','Beyonce','Tacos','Portland_Oregon','Canada')
wiki_base = 'https://en.wikipedia.org/wiki/'

test_docs = lapply(test_pages,function(x) read_html(paste0(wiki_base,x)) %>% html_nodes('p') %>% html_text(trim=T) %>% paste(.,collapse=' '))

