top100 = 'https://www.enr.com/toplists/2019-Top-200-Environmental-Firms-1'
top200 = 'https://www.enr.com/toplists/2019-Top-200-Environmental-Firms-2'
library(pdftools)
library(tabulizer)
library(rJava)
library(data.table)
library(rvest)
library(stringr)
tabs = '.flush-left td , th'
prepares = fread('../tuolumne/scratch/boilerplate/preparedocs.csv')

firms = rbindlist(list(read_html(top100) %>% html_nodes('table') %>% .[[2]] %>% html_table(trim=T,fill=T),
read_html(top200) %>% html_nodes('table') %>% .[[2]] %>% html_table(trim=T,fill=T)),use.names=T)

splits = str_split(firms$FIRM,',')
firms$NAME = sapply(splits,function(x) x[[1]])
firms$NAME[firms$NAME=="WHITMAN"] <- 'WHITMAN, REQUARDT & ASSOCIATES LLP'
firms$NAME[firms$NAME=="JOHNSON"] <- "JOHNSON, MIRMIRAN & THOMPSON INC."
firms$NAME[firms$NAME=="EA ENGINEERING"] <- "EA ENGINEERING, SCIENCE & TECHNOLOGY INC."

matches = lapply(firms$NAME,function(x) grep(x,toupper(prepares$text)))

prepares$Paragraph
prepares[EIS=='20130022',.(File,Paragraph,text)] 

prepares[195,]

prepares$text[1000]

prepares[,.N,by = .(EIS)][order(-N)]
splits[sapply(splits,length)==4]



firms[165,]
firms$FIRM

firms$FIRM

grep("Requardt",prepares$text,value=T)

(do.call(rbind,str_split(firms$FIRM,',')))

read_html(top200) %>% html_nodes('table')


install.packages('tabulizer')
install.packages('rJava')
read_html(top200) %>% html_nodes('table') 

DOI-BLM-WY-D030-2006-0001-EIS_docset_view.do?projectId=58344&currentPageId=77531&documentId=72255


read_html(top200) %>% html_nodes('table') 


test = list.files('agency_nepa_libraries/blm/nepa_documents/2006/')
grep('D030-2006-000',test,value=T)
test
