library(data.table)
library(tidyverse)

es = read_csv('input/epa_master_repository/eis_record_detail.csv')
es$Proj = NA
es$EIS.Title



sents = readRDS('scratch/sample_sentences.RDS')
sents = sents[sapply(sents,class)=='data.frame']
sent_df = rbindlist(sents)
sent_df$text = as.character(sent_df$text)

sent_df$text = gsub('(?![a-z])-\\s(?=[a-z])',"",sent_df$text,perl=T)
regex_line_number_string = "([0-9]{1,2}\\s){3,}"
regex_table_dump = "(Alternative\\s[A-Z0-9]\\s){2,}"
regex_largenumber_dump = "[0-9]{5,}|[0-9]{3,}-[0-9]{3,}"
regex_internet_dump = "@|\\.com|cc:"
regex_funkychar_dump = "\\*|_"
sent_df = sent_df[!grepl(regex_line_number_string,sent_df$text)]
sent_df = sent_df[!grepl(regex_table_dump,sent_df$text)]
sent_df = sent_df[!grepl(regex_largenumber_dump,sent_df$text)]
sent_df = sent_df[!grepl(regex_funkychar_dump,sent_df$text)]
sent_df = sent_df[nchar(sent_df$text)>50 & nchar(sent_df$text) < 400]
#drop weird spacing
sent_df = sent_df[!grepl('\\s[a-z]\\s[a-z]\\s|\\s[A-Za-z]\\s[A-Za-z]\\s[A-Za-z]\\s',sent_df$text)]
sent_df = sent_df[!grepl('\\s{2,}',sent_df$text)]
#end in a period
sent_df = sent_df[grepl('\\.$',sent_df$text)]
sent_df = sent_df[!duplicated(sent_df$text)]
sent_df = sent_df[!grepl("\\([^\\)]+$",sent_df$text)]
sent_df = sent_df[!grepl("[A-Z]{8,}",sent_df$text)]

water_quality_words = c("TMDL",'nonpoint source','NPS pollution',"heavy metals","NPDES","run-off","water quantity",
                        "water quality impacts","liquid effluents",'stream health',"erosion control",
                        'effluent','effluents','groundwater','water delivery','sedimentation',
                        'hydrologic condition','snowmelt','turbidity','urban runoff','stormwater','surface runoff','pH',
                        'DO concentration','discharge water','flood event','flooding')
air_quality_words  = c('carbon monoxide','PM2.5',"PM10","PM 2.5", "PM 10","NOX",'dust','air quality',
                       'criteria pollutants','nonattainment','particulate matter','particulates','emissions')
biology_words = c('invasive species','invasives','native species','native plants','endangered species',
                  'species composition','spawn','breed','prey','survival','noxious weeds',"nests",
                  'migratory species','migratory birds','fish','juveniles','wildlife','salmon',
                  'Salmon','migration','migrate','pollinators','macroinvertebrate')
habitat_words = c('ecosystem','habitat', 'ramping','spawning area','gravel bed','wetlands','Pinyon-juniper',
                  'habitat connectivity',"wetland habitat",'riparian habitat','uplands','riparian area','grazing area',
                  "channel conditions","riparian-wetland areas","native grasses","rip-rap")

economic_words = c('economic benefits','economic development','industry','jobs','agricultural benefits',
                   'farming','commercial fishing','tourism','eco-tourism','income')

environmental_justice_words = c('Environmental justice','environmental justice','EJ','socioeconomic','low-income','minority',
                                'cultural resources','hazardous waste','toxic waste','sacred','disadvantage','poverty',
                                'epidemiology','asthma','cancer','respiratory',
                                'public health','migratory workers','public access','historic resource')

recreation_words = c('hiking trails','biking trails','biking','walking','swimming','harvest','recreational benefit','fishing','hunting','hiking trail','biking trail',
                     'recreational activity','recreational uses','resource access',"parks",'hiking trails','biking trails')

aesthetic_words = c('noise pollution','background noise','vividness','intactness','unity','viewer','light pollution','visual quality','visual character',
                    'noise','visibility','aesthetic','wilderness','viewshed','vibration','visual impact','scenic','vistas','noticeable','lighting','visible')

climate_words = c('climate model','climate change','global warming','greenhouse gas','GHG','GHGs','ocean acidification','energy use','energy consumption',
                  'greenhouse gasses','carbon emissions','oil and gas development','climate projections','sea level rise','drought','precipitation','wildfire')

none_words = c("public scoping","public awareness","solid waste management","scoping meetings","public meetings","fossils","property acquisition")

sent_df$X_Water <- grepl(paste(paste0("\\b",water_quality_words,"\\b"),collapse='|'),sent_df$text) + 0 
sent_df$X_Air <- grepl(paste(paste0("\\b",air_quality_words,"\\b"),collapse='|'),sent_df$text) + 0
sent_df$X_Biology <- grepl(paste(paste0("\\b",biology_words,"\\b"),collapse='|'),sent_df$text) + 0
sent_df$X_Habitat <- grepl(paste(paste0("\\b",habitat_words,"\\b"),collapse='|'),sent_df$text) + 0
sent_df$X_Climate_Change <- grepl(paste(paste0("\\b",climate_words,"\\b"),collapse='|'),sent_df$text) + 0 
sent_df$X_Aesthetics <- grepl(paste(paste0("\\b",aesthetic_words,"\\b"),collapse='|'),sent_df$text) + 0
sent_df$X_EJ <- grepl(paste(paste0("\\b",environmental_justice_words,"\\b"),collapse='|'),sent_df$text) + 0
sent_df$X_Recreation <- grepl(paste(paste0("\\b",recreation_words,"\\b"),collapse='|'),sent_df$text) + 0
sent_df$X_Economics <- grepl(paste(paste0("\\b",economic_words,"\\b"),collapse='|'),sent_df$text) + 0

sample_indices = unique(unlist(lapply(grep('X_',colnames(sent_df)),function(x) sample(which(sent_df[,x,with=F]==1),20000,replace=T))))
#coded_df = sent_df[rowSums(sent_df %>% select(contains('X_')))>0,]
none_indices = grep(paste(paste0("\\b",none_words,"\\b"),collapse='|'),sent_df$text)
coded_df = sent_df[c(sample_indices,none_indices),]

wiki_base = "https://en.wikipedia.org/wiki/"
#wiki pages for "none" 
wiki_pages = c("baseball","American_Revolution","genetics","airplane","stock_market","Administrative_law",
               "LeBron_James","Star_Trek",'geology','beer',"Mathematics","Poetry",'Facebook','Stadium')
page_texts = sapply(wiki_pages,function(page) paste0(wiki_base,page) %>% rvest::html() %>% rvest::html_nodes('p') %>% rvest::html_text() %>% paste0(.,collapse = ' ') %>%
                      gsub("\\[[0-9]+\\]","",.))
none_sents = tokenizers::tokenize_sentences(page_texts)

none_df = do.call(rbind,lapply(none_sents,function(none) {temp = data.frame(text = none); temp[grep('X_',colnames(sent_df),value=T)] <- 0; temp}))
none_df = none_df[sample(1:nrow(none_df),25000,replace=T),]
trainers = full_join(coded_df,none_df)
write.csv(trainers, 'input/scratch/auto_trainers.csv')
