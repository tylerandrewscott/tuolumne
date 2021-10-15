pkcs = c('rdryad','rvest','curl','stringr')
for(p in pkcs){
if(!require(p,character.only = T)){install.packages(p,type = 'source');require(p,character.only = T)}
}

project_files = str_extract(grep('csv|rds',readLines('boilerplate_project/README.md'),value = T),'[a-z].*')
project_files <- c(project_files,'README.md')

if(all(project_files %in% list.files('boilerplate_project/input/'))){print('data are already stored locally')}else{
  url = 'https://datadryad.org/stash/dataset/doi:10.25338/B80K9R'
  file_nodes = read_html(url) %>% html_nodes(css = '.c-file-group__list a') 
  go_get = sort(project_files[!project_files %in% list.files('boilerplate_project/input/')],decreasing = T)
  base_url = 'https://datadryad.org/'
  node_index = which({file_nodes %>% html_attr('title')} %in% go_get)
go_get
  for(file in go_get){
    node = file_nodes[[match(file,file_nodes %>% html_attr('title'))]]
    url = paste0(base_url,node %>% html_attr('href'))
    destination.file = paste0('boilerplate_project/input/',file)
    curl_download(url, destination.file)
  }}
    
    
