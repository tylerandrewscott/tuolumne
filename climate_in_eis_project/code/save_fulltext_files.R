
require(stringr)
require(data.table)
require(pdftools)
flist= list.files('../eis_documents/enepa_repository/documents/',full.names = T,recursive = T)
flist = flist[grepl('^2020|^201[3-9]',basename(flist))]
flist = flist[grepl('PDF$|pdf$',flist)]
floc = 'scratch/full_text_documents/'
dir.create(floc)

require(parallel)


concatenate_save = function(file){
  new_file = paste0(floc,gsub('PDF$|pdf$','txt',basename(file)))
  print(new_file)
  try = tryCatch(pdf_info(file),error = function(e) NULL)
  if(!is.null(try)){
  tv = pdftools::pdf_text(file)
  temp = paste(tv,collapse = ' ')
  if(nchar(temp)>0){
    if(any(grepl('[A-Za-z]',temp))){
      file.create(new_file)
      fileConn<-file(new_file)
  writeLines(text = temp, fileConn)
  close(fileConn)
  }
  }
  }
}


require(doParallel)
cores = 4
cl = makeCluster(cores)
doParallel::registerDoParallel(cl)
clusterExport(cl,varlist = list('concatenate_save'))

fl = list.files(floc,full.names = T)
sizes = file.size(fl)
file.remove(fl[sizes==0])

flist = flist[!file.exists(paste0(floc,gsub('pdf$|PDF$','txt',basename(flist))))]

foreach(f = rev(flist)) %do% {concatenate_save(f)}

