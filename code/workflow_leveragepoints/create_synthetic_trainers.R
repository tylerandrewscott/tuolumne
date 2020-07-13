library(data.table)
library(tidyverse)
library(rvest)
sents = fread('scratch/leverage_points/tokenized_sentences.csv')
sents$text = gsub('(?![a-z])-\\s(?=[a-z])',"",sents$text,perl=T)
sents = sents[!grepl('\\.{2,}',sents$text),]
sents = sents[nchar(sents$text)>50 & nchar(sents$text) < 400]
#drop weird spacing
sents = sents[!grepl('\\s[a-z]\\s[a-z]\\s|\\s[A-Za-z]\\s[A-Za-z]\\s[A-Za-z]\\s',sents$text)]
sents = sents[!grepl('\\s{2,}',sents$text)]
#end in a period
sents= sents[grepl('\\.$',sents$text)]
sents = sents[!duplicated(sents$text)]
sents = sents[!grepl("\\([^\\)]+$",sents$text)]
sents = sents[!grepl("[A-Z]{8,}",sents$text)]

usgs_water_html = "https://water.usgs.gov/water-basics_glossary.html";css_water_usgs = "strong"
usgs2_water_html = 'https://water.usgs.gov/wsc/glossary.html';css_water_usgs2 = "a+ b"
usgs3_water_html = 'https://water.usgs.gov/edu/dictionary.html';css_water_usgs3 = 'p+ p b'
edwards_aquifer_water_html = 'https://www.edwardsaquifer.net/glossary.html';css_water_edwards = '.bodytext b'
mpr_water_html = 'https://www.mprnews.org/story/2011/09/12/ground-level-water-glossary';css_water_mpr = 'strong'
lenntech_water_html = 'https://www.lenntech.com/water-glossary.htm';css_water_lenntech = 'strong , strong a'
#ca_water_board = 'https://www.waterboards.ca.gov/publications_forms/available_documents/water_words.html'
aqua_science_water_html = 'https://www.aquascience.net/glossary/';css_water_agua = 'dt strong'
acwi_water_html = 'https://acwi.gov/monitoring/glossary.html';css_water_acwi = '.bib b'
gwf_water_html = 'http://www.globalwaterforum.org/learning-hub/glossary/';css_water_gwf = "strong"
usbr_water_html = 'https://www.usbr.gov/library/glossary/'; css_water_usbr = "p b"
nws_water_html = "http://www.nws.noaa.gov/om/hod/SHManual/SHMan014_glossary.htm";css_water_nws = "span strong"
nawqa_water_html = "https://water.usgs.gov/nawqa/glos.html";css_water_nawqa = 'b'
dwr_water_html = "https://water.ca.gov/Water-Basics/Glossary";css_water_dwr = ".CWPreference~ p+ p strong span"
water_sources = data.table(site = sort(grep('water_html$',ls(),value=T)),css = grep('css_water',ls(),value=T),stringsAsFactors = F)
water_term_list = lapply(1:nrow(water_sources),function(i) get(water_sources$site[i]) %>% read_html() %>% html_nodes(get(water_sources$css[i])) %>% html_text(trim=T))

all_water_terms = tolower(do.call(c,water_term_list))
all_water_terms = all_water_terms[!all_water_terms %in% letters]
all_water_terms = gsub(':','',all_water_terms)
all_water_terms = gsub(' \\(.*','',all_water_terms)
all_water_terms = gsub(' —(|\\s)$','',all_water_terms)
all_water_terms = gsub(' $','',all_water_terms)
all_water_terms = gsub('\\.$','',all_water_terms)
all_water_terms = all_water_terms[!grepl("^\\'|^\\(|^\\)",all_water_terms)]
all_water_terms = all_water_terms[all_water_terms!='']
all_water_terms= all_water_terms[!all_water_terms%in%c('mitigation','climate','weather','ion','cycle','bacteria','kinetic energy','yield','habitat','endangered species','density','nutrient','species')]
water_terms = as.data.frame(table(all_water_terms)) %>% arrange(-Freq) %>% filter(Freq>=4)
water_terms$all_water_terms = as.character(water_terms$all_water_terms)

###### hard code for water terms in sentence #####
water_sentences_usgs = usgs_water_html %>% read_html() %>% html_nodes('.content p') %>% html_text(trim=T)
water_sentences_usgs = gsub('^- ','',str_extract(water_sentences_usgs,'-\\s(.*)'))
water_sentences_usgs2 = usgs2_water_html %>% read_html() %>% html_nodes('td p') %>% html_text(trim=T)
water_sentences_usgs2 = gsub('^--','',str_extract(water_sentences_usgs2,'--(.*)'))
water_sentences_dwr = dwr_water_html %>% read_html() %>% html_nodes("strong+ span") %>% html_text(trim=T)
water_sentences_mpr = mpr_water_html  %>% read_html() %>% html_nodes('#article-body p') %>% html_text(trim=T)
water_sentences_mpr = gsub('^— ','',str_extract(water_sentences_mpr,'—\\s(.*)'))
water_sentences_nawqa = nawqa_water_html  %>% read_html() %>% html_nodes('p~ p+ p') %>% html_text(trim=T)
water_sentences_nawqa = gsub('^- ','',str_extract(water_sentences_nawqa,'-\\s(.*)'))
water_sentences_nws = nws_water_html  %>% read_html() %>% html_nodes('p+ p span') %>% html_text(trim=T)
water_sentences_nws = water_sentences_nws[!grepl(':$',water_sentences_nws )] 
water_sentences = c(water_sentences_dwr,water_sentences_mpr,water_sentences_nawqa,water_sentences_nws,water_sentences_usgs,water_sentences_usgs2)

cawb_gloss = Reduce(c,lapply(list.files('scratch/ca_water_board_glossary/',"pdf",full.names = T),function(x) tabulizer::extract_text(x)))

cawb_gloss_sents = sapply(seq_along(cawb_gloss),function(x) {
  tok = unlist(tokenizers::tokenize_sentences(cawb_gloss[x]))
  tok = gsub('^.+\\:\\s{1,}','',tok) 
  tok = tok[!grepl('^[0-9]',tok)]
})

all_cwab_gloss_sents = Reduce(c,cawb_gloss_sents)
example_sentences = c(water_sentences,all_cwab_gloss_sents)
example_sentences = iconv(example_sentences,from = 'ASCII',to = 'utf8',sub = '')


#https://pubs.usgs.gov/circ/2004/circ1268/htdocs/text-glossary.html
#http://www.nws.noaa.gov/om/hod/SHManual/SHMan014_glossary.htm


# 
# drought_html_carleton = 'https://serc.carleton.edu/eslabs/drought/drought_glossary.html';drought_css_carleton = 'dt'
# drought_html_do = 'https://drought.climateservices.it/en/glossario/';drought_css_do = '.glossaryLinkMain'
# drought_html_scpr = 'https://www.scpr.org/news/2015/05/13/51605/graywater-snowpack-a-glossary-of-drought-jargon/';drought_css_scpr = 'strong'
# drought_html_unl = 'https://drought.unl.edu/Education/DroughtforKids/Glossary.aspx';drought_css_unl = 'strong'
# drought_set = list(drought_html_carleton,drought_html_do,drought_html_scpr,drought_html_unl)
# drought_css_set = list(drought_css_carleton,drought_css_do,drought_css_scpr,drought_css_unl )
# drought_term_list = lapply(seq_along(drought_set),function(x) read_html(drought_set[[x]]) %>% 
#                              html_nodes(drought_css_set[[x]]) %>% html_text(trim=T))
# all_drought_terms = tolower(do.call(c,drought_term_list))
# all_drought_terms = all_drought_terms[!all_drought_terms %in% letters]
# all_drought_terms = gsub(' \\(.*','',all_drought_terms)
# all_drought_terms = gsub(' $','',all_drought_terms)
# all_drought_terms = gsub('\\.$','',all_drought_terms)
# all_drought_terms = gsub('\\:$','',all_drought_terms)
# all_drought_terms = all_drought_terms[!grepl("^\\'|^\\(|^\\)",all_drought_terms)]
# all_drought_terms = all_drought_terms[all_drought_terms!='']
# all_drought_terms  = all_drought_terms [!all_drought_terms  %in% 
#                                           c('dam','mitigation','glacier','indicators','jet stream','water year','scenario',"weather"  ,
#                      'groundwater',   "runoff",        'watershed',  'climate', "orographic uplifting",  "ice cap"  ,  'basin',  "limited resource"  , "irrigation"   , 'submarginal farmland',  'elevation',  'pollution',
# 'renewable resource', 'stakeholder','tributary', "cubic meters per second"  ,'condensation','erosion')]
# drought_terms = as.data.frame(table(all_drought_terms)) %>% arrange(-Freq) %>% filter(Freq>=2)
# drought_terms$all_drought_terms = as.character(drought_terms$all_drought_terms)

custom_drought_terms = c('drought','dearth',"hydrological drought",'vulnerability','severity index','water shortage','drouth',
                         'excessive pumping','groundwater level','total yield','deficit','usage restrictions','overpumping',
                         'pumping restriction','water recycling',
                         'precipitation index','snowpack','subsidence','depletion','xeriscaping','desertification','fallow','junior rights','senior rights',
                         'PDSI','PHDI','saltwater intrusion','aridification','water use','reservoir level','SPEI','dry year','wet year')

custom_climate_terms = c('climate change','global warming','greenhouse effect','carbon dioxide','CO2',
                         'ozone','methane','climate model','nighttime temperature','desertification',
                         'carbon cycle','carbon sequestration','IPCC','fossil fuels','renewable energy',
                         'climate feedback','feedback loop','adaptive capacity','sea level rise','carbon emissions',
                         'severe weather','wetter','hotter','drier','global temperature','climate projection','extreme weather',
                         'increased wildfire','climate risk','storm surge','atmospheric river','longer summer','hotter days','warmer nights')


#test = lapply(drought_terms$all_drought_terms,function(x) cbind(x,sum(grepl(paste0('\\b',x,'\\b'),sents$text))))
#data.frame(do.call(rbind,test),stringsAsFactors = F)
#length(sents$text)

#drought_terms = as.data.frame(table(all_drought_terms)) %>% arrange(-Freq) %>% filter(Freq>=3)
#all_waterdrought_terms = c(all_water_terms,all_drought_terms)

library(htmlTable)
wat = paste(sort(water_terms$all_water_terms),collapse=', ')
drou = paste(sort(custom_drought_terms),collapse=', ')
clim = paste(sort(custom_climate_terms),collapse=', ')
htmlTable::htmlTable(cbind(wat,drou,clim))


library(pbapply)
#write_csv(water_terms,'scratch/leverage_points/water_terms.csv')
#rm(list = grep('_html$',ls(),value=T));rm(list = grep('css',ls(),value=T));rm(source)
# 
# epa_climate_html = 'https://19january2017snapshot.epa.gov/climatechange/glossary-climate-change-terms_.html';css_climate_epa = 'p+ p strong'
# bbc_climate_html = 'https://www.bbc.com/news/science-environment-11833685';css_climate_bbc = '.story-body__inner strong'
# carleton_climate_html = 'https://serc.carleton.edu/eslabs/weather/glossary.html'; css_climate_carleton = 'dt'
# sercc_climate_html = 'http://www.sercc.com/glossary';css_climate_sercc = 'p~ p+ p b'
# gcrp_climate_html = 'https://www.globalchange.gov/climate-change/glossary'; css_climate_gcrp = 'dt'
# lenntech_climate_html  = 'https://www.lenntech.com/greenhouse-effect/climate-change-glossary.htm';css_climate_lenntech = 'strong , strong a'
# miami_climate_html = "http://climate.miami.edu/glossary-of-terms/"; css_climate_miami = "strong"
# nrcan_climate_html = "https://www.nrcan.gc.ca/environment/resources/publications/impacts-adaptation/reports/assessments/2008/glossary/10413";css_climate_nrcan = "strong"
# epa2_climate_html = 'https://www3.epa.gov/climatechange//kids/glossary.html';css_climate_epa2 = 'strong'
# 
# climate_sources = data.frame(site = sort(grep('climate_html$',ls(),value=T)),css = grep('css_climate',ls(),value=T),stringsAsFactors = F)
# climate_term_list = lapply(1:nrow(climate_sources),function(i) get(climate_sources$site[i]) %>% read_html() %>% html_nodes(get(climate_sources$css[i])) %>% html_text(trim=T))
# 
# all_climate_terms = tolower(do.call(c,climate_term_list))
# all_climate_terms = all_climate_terms[!all_climate_terms %in% letters]
# all_climate_terms = gsub(' \\(.*','',all_climate_terms)
# all_climate_terms = gsub(' $','',all_climate_terms)
# all_climate_terms = all_climate_terms[!grepl("^\\'|^\\(|^\\)",all_climate_terms)]
# all_climate_terms = all_climate_terms[all_climate_terms!='']
# all_climate_terms = gsub('\\:$','',all_climate_terms)
# all_climate_terms = all_climate_terms[!all_climate_terms %in% 
#                                               c('stakeholder','watershed','percolation','industrial revolution','scenario','ecosystem','storyline','joint implementation',
#                                                 "hydrologic cycle" ,    "dam",  "runoff" , "condensation"  , 'groundwater',  'erosion','latitude','parameterization','elevation','reservoir',
#                                                 'land use','uncertainty','mitigation')]
# climate_terms = as.data.frame(table(all_climate_terms)) %>% arrange(-Freq) %>% filter(Freq>=3)
# climate_terms$all_climate_terms = as.character(climate_terms$all_climate_terms)

library(stringi)

ex_sents = data.table(text = example_sentences,found = 0,stringsAsFactors = F)
sents$found = 1
combined_sents = rbind(sents,ex_sents,fill=T)
#has_climate_term = sents[,stri_detect_regex(text,pattern = paste(paste0('\\b',climate_terms$all_climate_terms,'\\b'),collapse='|'))]
#has_drought_term = sents[,stri_detect_regex(text,pattern = paste(paste0('\\b',drought_terms$all_drought_terms,'\\b'),collapse='|'))]
#has_water_term = combined_sents[,stri_detect_regex(text,pattern = paste(paste0('\\b',water_terms$all_water_terms,'\\b'),collapse='|'))]
#has_climate_term = combined_sents[,stri_detect_regex(text,pattern = paste(paste0('\\b',custom_climate_terms,'\\b'),collapse='|'))]
#has_drought_term = combined_sents[,stri_detect_regex(text,pattern = paste(paste0('\\b',custom_drought_terms,'\\b'),collapse='|'))]

all_water_matches = pbsapply(water_terms$all_water_terms,function(x) combined_sents[,stri_detect_regex(text,pattern = paste(paste0('\\b',x,'\\b'),collapse='|'))],cl = 8)
has_water_term = apply(all_water_matches,1,any)

all_climate_matches = pbsapply(seq_along(custom_climate_terms),function(x) combined_sents[,stri_detect_regex(text,pattern = paste(paste0('\\b',custom_climate_terms[x],'\\b'),collapse='|'))],cl = 8)
has_climate_term = apply(all_climate_matches,1,any)

all_drought_matches = pbsapply(seq_along(custom_drought_terms),function(x) combined_sents[,stri_detect_regex(text,pattern = paste(paste0('\\b',custom_drought_terms[x],'\\b'),collapse='|'))],cl = 8)
has_drought_term = apply(all_drought_matches,1,any)

combined_sents$X_Water = (has_water_term | combined_sents$found==0) + 0
combined_sents$X_Drought = has_drought_term + 0
combined_sents$X_Climate = has_climate_term + 0


term_set_df = do.call(rbind,list(data.frame(topic = 'climate',word = custom_climate_terms,stringsAsFactors = F),
                                 data.frame(topic = 'drought',word = custom_drought_terms,stringsAsFactors = F),
                                 data.frame(topic = 'water',word = water_terms$all_water_terms,stringsAsFactors = F)))
write_csv(term_set_df,'scratch/leverage_points/term_set.csv')


css_eia = '.glossary_content strong'
eia_alternativefuels_html = 'https://www.eia.gov/tools/glossary/index.php?id=alternative%20fuels'
eia_coal_html = 'https://www.eia.gov/tools/glossary/index.php?id=coal'
eia_naturalgas_html = 'https://www.eia.gov/tools/glossary/index.php?id=natural%20gas'
eia_electricity_html = 'https://www.eia.gov/tools/glossary/index.php?id=electricity'
eia_nuclear_html = 'https://www.eia.gov/tools/glossary/index.php?id=nuclear'
eia_petroleum_html = 'https://www.eia.gov/tools/glossary/index.php?id=nuclear'
eia_renewable_html = 'https://www.eia.gov/tools/glossary/index.php?id=renewable'
eia_set = sapply(grep('^eia.*_html',ls(),value=T),get)
eia_terms_df = data.frame(do.call(rbind,lapply(seq_along(eia_set),function(x) cbind(names(eia_set)[x],read_html(eia_set[x]) %>% html_nodes(css_eia) %>% html_text(trim=T)))),stringsAsFactors = F)
eia_terms_df = eia_terms_df %>% rename(Category = X1,Term = X2) %>% mutate(Category = gsub('^eia_|_html$','',Category),Term = gsub(':$','',Term))

#eia_terms_df[eia_terms_df$Category=='renewable',]
data.table::fwrite(sents, 'scratch/leverage_points/tagged_training_sentences.csv')
###possible water categories --- quality/habitat, supply, storage
#disaster_emdat_html = 'https://www.emdat.be/Glossary'
#https://www.unisdr.org/we/inform/terminology

need = 20000

sample_water_indices = c(which(combined_sents$found == 0),sample(which(combined_sents$X_Water==1 & combined_sents$found == 1),size = need - sum(combined_sents$found==0),replace=T))
sample_indices = unlist(lapply(grep('X_Clim|X_Drou',colnames(combined_sents)),function(x) sample(which(combined_sents[,x,with=F]==1),size = need,replace=T)))
none_indices = sample(which(rowSums(combined_sents[,grep('X_',colnames(combined_sents)),with=F])==0),need * 3,replace=T)

#coded_df = sent_df[rowSums(sent_df %>% select(contains('X_')))>0,]
coded_df = combined_sents[c(sample_water_indices,sample_indices,none_indices),]
data.table::fwrite(coded_df, 'scratch/leverage_points/auto_trainers.csv')

