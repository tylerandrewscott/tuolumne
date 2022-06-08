
pack = c('data.table','stringr','tidyverse','doParallel','pdftools','lubridate','koRpus')
need = pack[!pack %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(pack,require,character.only=T)
tokpars = 'scratch/tokenized_paragraphs/'
fls = list.files(tokpars)

projects = fread('scratch/boilerplate/project_candidates_eis_only.csv')
documents = fread('scratch/boilerplate/document_candidates_eis_only.csv')
fls  = fls[fls %in% gsub('PDF$|pdf$','txt',documents$File_Name )]

flesch_file = 'scratch/boilerplate/flesch_scores_by_paragraph.rds'

if(file.exists(flesch_file)){dt = readRDS(flesch_file)}
if(!file.exists(flesch_file)){
dt = data.table(File = character(),paragraph = numeric(),Flesch = numeric())
}
fls = fls[!fls %in% dt]
require(quanteda)
for(f in fls){
text = readLines(paste0(tokpars,f))
flesch = data.table(quanteda::textstat_readability(text))
dt = rbind(dt,data.table(File = f,paragraph = str_remove_all(flesch$document,'[a-z]'),Flesch = flesch$Flesch),use.names = T,fill = T)
}
saveRDS(dt,flesch_file)

