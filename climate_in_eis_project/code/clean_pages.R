
packs = c('stm','data.table','stringr','readtext','pbapply','maps','quanteda','tm','tokenizers')
sapply(packs[!sapply(packs,require,character.only = T)],install.packages)
sapply(packs,function(x) require(x,character.only = T))

redraw_corpus = TRUE
proj.dir = 'climate_in_eis_project/'
scratch.dir = 'data_products/'
input.dir = 'input/'
projs = readRDS(paste0(proj.dir,scratch.dir,'eis_metadata.RDS'))
docs = readRDS(paste0(proj.dir,scratch.dir,'eis_doc_metadata.RDS'))
corps = list.files(paste0(proj.dir,input.dir),full.names = T,pattern = 'corpus')

source('climate_in_eis_project/code/functions/FindCleanPages.R')

temp_corp = readRDS(corps[1])
dim(temp_corp)
test = FindCleanPages(file = temp_corp)
dim(test)


dim(temp_corp)
str(test)

fl = list.files(text_loc,full.names = T)
sizes = file.size(fl)
file.remove(fl[sizes==0])

flist = list.files(text_loc)
#flist = flist[flist %in% gsub('pdf$','txt',docs$File_Name)]
flist = flist[str_extract(flist,'^[0-9]{8}') %in% projs$EIS.Number]

new_floc = 'scratch/tokenized_paragraphs/'
dir.create(new_floc)

require(doParallel)
cores = 6
cl = makeCluster(cores)
doParallel::registerDoParallel(cl)
clusterExport(cl,varlist = list('FindCleanParagraphs','new_floc','text_loc'))

flist = flist[!file.exists(paste0(new_floc,gsub('pdf$|PDF$','txt',basename(flist))))]
flist = flist[file.size(paste0(text_loc,flist))>10]
flist = flist[file.exists(paste0(text_loc,flist))]


foreach(f = rev(flist)) %dopar% {FindCleanParagraphs(file = f,newdir = new_floc,olddir = text_loc)}

