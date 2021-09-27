if(!require(rdryad)){install.packages('rdryad',type = 'source');require(rdryad)}

corpus_file = 'eis_corpus_2013-2020.rds'
floc = 'boilerplate_project/input/'

getInputData = function(corpus_file = 'eis_corpus_2013-2020.rds',floc = 'boilerplate_project/input/'){
  if(!file.exists(paste0(floc,corpus_file))){
    download.file('https://datadryad.org/stash/downloads/file_stream/1042427',destfile = 'boilerplate_project/input/')
  }
  else{print('data are already stored locally')}
}


getInputData()



if(!file.exists(paste0(floc,corpus_file))){
  download.file('https://datadryad.org/stash/downloads/file_stream/1042427',destfile = 'boilerplate_project/input/')
}

fl = 'https://datadryad.org/stash/downloads/file_stream/1042427'
RCurl::getURL(fl)



dryad_dataset(dois = 'https://doi.org/10.25338/B80K9R')
'https://datadryad.org/stash/downloads/file_stream/1042515'

