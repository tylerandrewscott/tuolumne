
packs = c('stm','data.table','stringr','readtext','pbapply','maps','quanteda','tm','tokenizers')
sapply(packs[!sapply(packs,require,character.only = T)],install.packages)
sapply(packs,require)

redraw_corpus = TRUE
projs = readRDS('scratch/climate_in_nepa/eis_metadata.RDS')
docs = readRDS('scratch/climate_in_nepa/eis_doc_metadata.RDS')

text_loc = 'scratch/full_text_documents/'
fl = list.files(text_loc,full.names = T)
sizes = file.size(fl)
file.remove(fl[sizes==0])

flist = list.files(text_loc)
#flist = flist[flist %in% gsub('pdf$','txt',docs$File_Name)]
flist = flist[str_extract(flist,'^[0-9]{8}') %in% projs$EIS.Number]

new_floc = 'scratch/tokenized_paragraphs/'
dir.create(new_floc)

FindCleanParagraphs = function(file,newdir,olddir){
temp=tryCatch(readtext::readtext(paste0(olddir,file)),error = function(e) NULL)
if(!is.null(temp)){
pars = tokenizers::tokenize_paragraphs(temp$text,paragraph_break = '.\n',simplify = T)
pars <- stringr::str_replace_all(pars,'\\s{2,}',' ')
chars = nchar(pars)
periods = stringr::str_count(pars,"\\.")
numbers = stringr::str_count(pars,"[0-9]")
caps = stringr::str_count(pars,'[A-Z]')
spaces = stringr::str_count(pars,'\\s')
filtered_pars = pars[chars>400&{periods/chars}<0.1&{numbers/chars}<0.1&{caps/chars}<0.1&!grepl('http',pars)&{spaces/chars}<0.2]
new_file = paste0(newdir,basename(file))
if(length(filtered_pars)>0){
  print(new_file)
  file.create(new_file)
  fileConn<-file(new_file)
  writeLines(text = filtered_pars, fileConn)
  close(fileConn)
}
}
}
require(doParallel)
cores = 6
cl = makeCluster(cores)
doParallel::registerDoParallel(cl)
clusterExport(cl,varlist = list('FindCleanParagraphs','new_floc','text_loc'))

flist = flist[!file.exists(paste0(new_floc,gsub('pdf$|PDF$','txt',basename(flist))))]
flist = flist[file.size(paste0(text_loc,flist))>10]
flist = flist[file.exists(paste0(text_loc,flist))]


foreach(f = rev(flist)) %dopar% {FindCleanParagraphs(file = f,newdir = new_floc,olddir = text_loc)}

