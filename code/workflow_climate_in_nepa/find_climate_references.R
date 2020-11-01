
fl = list.files('scratch/full_text_documents/',full.names = T)

projs = fread('../eis_documents/enepa_repository/meta_data/eis_record_detail.csv')
projs = projs[grepl('Draft',Document.Type),]
projs = projs[str_extract(projs$EIS.Number,'^20(1[3-9]|20)') %in% 2013:2020,]

fl = fl[str_extract(basename(fl),'^[0-9]{1,}') %in% projs$EIS.Number]

fl = fl[!grepl('^([0-9]{8})_\\1\\.txt',basename(fl))]


climate_phrases = c('climate change','ocean acidification','sea level rise','sea-level rise','renewables',
                    'fossil fuels','global average temperature','nighttime temperature',
                    'climate mitigation','climate adaptation',
                    'renewable energy','severe weather','greenhouse gas','global warming','GHG','marine heat wave')
climate_regex = paste(climate_phrases, collapse = '|')
dir.create('scratch/climate_in_nepa/climate_segments')

drop_regex = '\\.{10,}'


for(year in 2013:2020){
  fname = paste0('scratch/climate_in_nepa/climate_segments/temp_segments_',year,'.rds')
 # if(file.exists(fname)){next}
fl_temp = fl[grepl(paste0('^',year),basename(fl))]
empty_list = list()
for(i in seq_along(fl_temp)){
text_lines = readLines(fl_temp[i])
matches = grep(climate_regex,text_lines)

if(!identical(matches,integer(0))){
matches = matches[!({matches+1} %in% matches & {matches - 1} %in% matches)]
line_seqs = lapply(grep(climate_regex,text_lines),function(x) {y = x + -100:100;y[y>0]})
text_segments = lapply(line_seqs,function(x) gsub("\\s{2,}",' ',paste(text_lines[x],collapse = ' ')))
names(text_segments) = paste0(basename(fl_temp[i]),'_l',sapply(line_seqs,min),'_',sapply(line_seqs,max))

page_names = str_extract(names(text_segments) ,'txt_l.*')
first_page = str_extract(str_extract(page_names,'_l[0-9]{1,}'),'[0-9]{1,}')
file_name = str_extract(names(text_segments),'^.*txt')
text_segments = text_segments[!duplicated(paste(file_name,first_page))]
text_segments = text_segements[grepl('\\.{10,}')]
empty_list = append(empty_list,text_segments)
}
}
saveRDS(empty_list,fname)
}
