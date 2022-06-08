library(data.table)
library(rvest)
blm_manuals = 'https://www.blm.gov/policy/manuals'
sections = html_session(blm_manuals)
first = sections %>% jump_to('#ui-accordion-quickset-media_blm_policy_manuals-header-0')
chapters_a = first %>% xml2::read_html() %>% html_nodes("a") 
index=grep('^MS-[0-9]',chapters_a %>% html_text(trim=T),value=F)
manual_dt = data.table(Chapter = chapters_a[index] %>% html_text(trim=T),href = chapters_a[index] %>% html_attr("href"))
manual_dt$href = paste0('https://www.blm.gov',manual_dt$href)
manual_dt$file_names = paste0(gsub('\\s','_',gsub('\\,','',manual_dt$Chapter)),".txt")

#floc = 'scratch/eis_documents/'
nloc = 'input/agency_documents/blm_handbooks_and_manuals/'
for(f in (1:length(manual_dt$href))){
  print(f)
  new_name = paste0(nloc,manual_dt$file_names[f])
  if(file.exists(new_name)){next}
  if(httr::GET(manual_dt$href[f])$status==404){next}
  temp_read <-tryCatch( pdftools::pdf_text(manual_dt$href[f]), error = function(e) NA)
  if(!any(sapply(temp_read,is.na))&all(sapply(temp_read,function(x) x!=''))){
    #temp_read <- stringr::str_replace_all(unlist(temp_read),"[\\s]+", " ")
    temp_read <- unlist(temp_read)
    write.table(temp_read, file=new_name,
                quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
    rm(temp_read)}
  gc()
}

sections = html_session(blm_manual)
first = sections %>% jump_to('#ui-accordion-quickset-media_center_blm_policy_manuals-header-0')
chapters_a = first %>% xml2::read_html() %>% html_nodes("a") 
index=grep('policymanual',chapters_a %>% html_attr('href'),value=F)
link_dt = data.table(Chapter = chapters_a[index] %>% html_text(trim=T),href = chapters_a[index] %>% html_attr("href"))
link_dt$href = paste0('https://www.blm.gov/',link_dt$href)
library(tabulizer)
library(textreadr)
link_dt$text = lapply(link_dt$href,function(x) pdftools::pdf_text(x))
link_dt$Type = 'Manual'

blm_handbooks = 'https://www.blm.gov/policy/handbooks/'
sections = html_session(blm_handbooks)
first = sections %>% jump_to('#ui-accordion-quickset-media_blm_policy_handbooks-header-0')
chapters_a = first %>% xml2::read_html() %>% html_nodes("a") 
index=grep('^H-[0-9]',chapters_a %>% html_text(trim=T),value=F)
handbook_dt = data.table(Chapter = chapters_a[index] %>% html_text(trim=T),href = chapters_a[index] %>% html_attr("href"))
handbook_dt$href = paste0('https://www.blm.gov',handbook_dt$href)
handbook_dt$file_names = paste0(gsub('\\s','_',gsub('\\,','',handbook_dt$Chapter)),".txt")


#floc = 'scratch/eis_documents/'
nloc = 'input/agency_documents/blm_handbooks_and_manuals/'

for(f in (1:length(handbook_dt$href))[-42]){
  print(f)
  new_name = paste0(nloc,handbook_dt$file_names[f])
  if(file.exists(new_name)){next}
  if(httr::GET(handbook_dt$href[f])$status==404){next}
  temp_read <-tryCatch( pdftools::pdf_text(handbook_dt$href[f]), error = function(e) NA)
  if(!is.na(temp_read)){
    #temp_read <- stringr::str_replace_all(unlist(temp_read),"[\\s]+", " ")
    temp_read <- unlist(temp_read)
    write.table(temp_read, file=new_name,
                quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
    rm(temp_read)}
  gc()
}


