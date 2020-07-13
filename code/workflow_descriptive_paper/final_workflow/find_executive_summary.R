
require(boxr)
require(data.table)
require(utf8)
library(pbapply)
require(stringr)
require(pdftools)
#fwrite(data.table(File = character()),file = 'scratch/boilerplate/reference_files.csv')
#fwrite(data.table(Box_ID = character(), File = character()),file = 'scratch/files_with_references.csv')
#dir_create("~/.boxr-auth", mode = 700)
# box_auth_service()
# eis_doc_id = '94759130991'
# eis_dir = boxr::box_ls(eis_doc_id)
# agency_dirs = as.data.table(eis_dir)[name=='agency_nepa_libraries',]$id
# usfs_subd_id = as.data.table(box_ls(agency_dirs))[name=='usfs',]$id
# usfs_text_subds_id = as.data.table(box_ls(usfs_subd_id))[name=='text_as_datatable',]$id
# text_subdirs = as.data.table(box_ls(usfs_text_subds_id))

full_rec = fread('../eis_documents/enepa_repository/meta_data/eis_record_detail_V2.csv',encoding = 'UTF-8')
full_rec$Title = gsub("[^[:print:]]", "", full_rec$Title)
nonfinal = full_rec[!grepl('Final',Document.Type),]
full_rec = full_rec[grepl('Final',full_rec$Document.Type),]
full_rec$Year = str_extract(full_rec$EIS.Number,'^[0-9]{4}')
full_rec = full_rec[Year %in% 2013:2019,]
full_rec = full_rec[!grepl('^ADOPT|^WITH',full_rec$Title),]
full_rec$DEIS_NUMBER = nonfinal$EIS.Number[match(full_rec$Title,nonfinal$Title)]


full_rec$Exec_Summary_File = NA
full_rec$Exec_Summary_Start_Page = NA
full_rec$Exec_Summary_End_Page = NA
fl = list.files('../eis_documents/enepa_repository/documents/',full.names = T,recursive = T)
finfo = file.info(fl)
fl = fl[finfo$size>0]
fl = fl[!grepl('([0-9]{8})_\\1\\.pdf',fl,perl = T)]
fl_id = str_extract(basename(fl),'^[0-9]{8}')

keep = fl_id %in% full_rec$EIS.Number
fl2 = fl[keep]
fl_id2 = fl_id[keep]
exec_file = grepl('Exec|EXEC|Summary|SUMMARY',fl2)&!grepl('Appendix|APPENDIX|Errata',fl2)
full_rec$Exec_Summary_File = fl2[exec_file][match(full_rec$EIS.Number,fl_id2[exec_file])]

require(textclean)


temp = full_rec[,.(Title,EIS.Number,DEIS_NUMBER,Exec_Summary_File,Exec_Summary_Start_Page,Exec_Summary_End_Page)]
temp$YEAR = str_extract(temp$EIS.Number,'^[0-9]{4}')
temp$DOCUMENT = full_rec$Document[match(temp$EIS.Number,full_rec$EIS.Number)]


removes = which(temp$Exec_Summary_End_Page-temp$Exec_Summary_Start_Page>100)
temp$Exec_Summary_File[removes]<-NA
temp$Exec_Summary_Start_Page[removes]<-NA
temp$Exec_Summary_End_Page[removes]<-NA
summary(temp$Exec_Summary_End_Page)


exec_regex = '(^|\\s{1,})[EN]S[^0-9A-Za-z][0-9ivx]{1,3}($|\\s{1,})|Executive Summary|EXECUTIVE SUMMARY|(SUMMARY|Summary)[^0-9A-Za-z][0-9]{1,3}($|\\s{1,})'

which(temp$EIS.Number=='20190300')
temp[21,]
temp[Exec_Summary_Start_Page>200,]


#go_get = which(temp$EIS.Number %in% full_rec$EIS.Number[full_rec$Agency=='Forest Service'] & is.na(temp$Exec_Summary_File))
for(i in which(is.na(temp$Exec_Summary_File))){
  if(is.na(temp$Exec_Summary_File[i])){
    print(i)
    rm(temp_flist);rm(temp_read_list);rm(toc_list)  
    temp_flist  = fl[fl_id %in% temp$EIS.Number[i]]
    temp_flist = temp_flist[!grepl('Appendix_[L-Z]',temp_flist)]
    if(length(temp_flist)==0){next}
    temp_read_list = lapply(seq_along(temp_flist),function(f){
      #print(f)
      dfile   = temp_flist[f]
      temp_read <- tryCatch(pdf_text(dfile),error = function(e) NULL)
      if(identical(temp_read,character(0))){integer(0)}else{
      tail_pages = lapply(temp_read,function(page) tail(stringi::stri_split_lines(page,omit_empty = T)[[1]],3))
      tail_pages = lapply(tail_pages,function(x) gsub('^\\s{1,}','',x))
      head_pages = lapply(temp_read,function(page) head(stringi::stri_split_lines(page,omit_empty = T)[[1]],3))
      head_pages = lapply(head_pages,function(x) gsub('^\\s{1,}','',x))
      if(identical(tail_pages,list())){which_pages = integer(0)}else{
        which_pages = which(sapply(tail_pages,function(x) any(grepl(exec_regex,x)&!grepl('[0-9]{6,}',x))))
        which_pages_s = which(sapply(tail_pages,function(x) any(grepl('(^|\\s{1,})S[^0-9A-Za-z][0-9]{1,3}($|\\s{1,})',x)&!grepl('[0-9]{5,}',x))))
     # grep('Appendix S\\n|APPENDIX S\\n',temp_read[which_pages_s[1]])
    #  sapply(tail_pages[which_pages_s],function(x) any(grepl('Appendix S\\n|APPENDIX S\\n',x)))
      if(any(grepl("(Appendix|APPENDIX) S\\b",head_pages[which_pages_s]))){which_pages_s = integer(0)}
      if(length(which_pages)<=2 & length(which_pages_s)>4){which_pages = which_pages_s}
      }
      }
      if(identical(head_pages,list())){which_pages2 = integer(0)}else{
      which_pages2 = which(sapply(head_pages,function(x) any(grepl(paste(c(exec_regex,'(Summary|SUMMARY)($|\\n)'),collapse='|'),x)&!grepl('[0-9]{6,}',x)&!grepl("\\.{7,}",x))))}
      which_pages = sort(unique(c(which_pages,which_pages2)))
     if(!min(which_pages)<200){which_pages = integer(0)}
      which_pages
    })
  
    longest = sapply(temp_read_list,function(t){
    s <- split(t, cumsum(c(TRUE, diff(t) != 1)))
    length(s[[which.max(lengths(s))]])})
    if(any(longest>=4)){
    keep_file = which.max(sapply(temp_read_list,length))
    es_pages_orig = temp_read_list[[keep_file]]
    while(any(es_pages_orig [-length(es_pages_orig )] - es_pages_orig [-1] < (-3))){
      drop = which(es_pages_orig [-length(es_pages_orig )] - es_pages_orig [-1] < (-3)) + 1
      es_pages_orig = es_pages_orig[-drop]}
    es_pages = es_pages_orig[sapply(seq_along(es_pages_orig),function(x) min(abs(es_pages_orig[x]-es_pages_orig[-x])))<=2]
    print('success')
    temp$Exec_Summary_File[i]<-temp_flist[keep_file]
    temp$Exec_Summary_Start_Page[i]<-min(es_pages)
    temp$Exec_Summary_End_Page[i]<-max(es_pages)
    }else{print('ES pages not found')}
  }
  
  if(is.na(temp$Exec_Summary_File[i])){
    if(length(temp_flist)==0){next}
    toc_list = lapply(seq_along(temp_flist),function(f){
      dfile   = temp_flist[f]
      tryCatch(pdf_toc(dfile),error = function(e) NULL)})
    childs = lapply(seq_along(toc_list),function(t) sapply(toc_list[[t]]$children,function(x) x$title))
    need_next_level = sapply(childs,length)==1|!grepl('Summary|SUMMARY',childs)
     childs[need_next_level] <- lapply(which(need_next_level),function(x) sapply(toc_list[[x]]$children[[1]]$children,function(u) u$title))
    has_exec_sum = sapply(childs,function(x) any(grepl('EXECUTIVE SUMMARY|SUMMARY',toupper(x))))
    if(!any(has_exec_sum)&!is.na(temp$DEIS_NUMBER[i])){
      temp_flist  = fl[fl_id %in% temp$DEIS_NUMBER[i]]
    if(any(grepl('Executive',temp_flist))){temp$Exec_Summary_File[i]<-temp_flist[grepl('Executive',temp_flist)];print('found whole draft');next}
      toc_list = lapply(seq_along(temp_flist),function(f){
        dfile   = temp_flist[f]
        tryCatch(pdf_toc(dfile),error = function(e) NULL)})
      childs = lapply(seq_along(toc_list),function(t) sapply(toc_list[[t]]$children,function(x) x$title))
      has_exec_sum = sapply(childs,function(x) any(grepl('EXECUTIVE SUMMARY$',toupper(x))))
    }
    
    if(sum(has_exec_sum)>1){has_exec_sum <- has_exec_sum & !grepl('Appen|APP|Resource_Report',temp_flist)}
    if(sum(has_exec_sum)>1&any(grepl("FEIS|Final|FINAL",temp_flist[has_exec_sum]))){has_exec_sum <- has_exec_sum & grepl("FEIS|Final|FINAL",temp_flist)}
    if(sum(has_exec_sum)>1){has_exec_sum <- has_exec_sum & sapply(childs,function(x) any(x %in% c('Summary','SUMMARY','Executive Summary','EXECUTIVE SUMMARY')))}
    
    if(sum(has_exec_sum)!=1){print(paste('found',sum(has_exec_sum),'sections'));next}else{
      dfile= temp_flist[which(has_exec_sum)]
      temp_read = pdf_text(dfile)
      if(identical(temp_read,character(0))){next}
      head_pages = lapply(temp_read,function(page) head(stringi::stri_split_lines(page,omit_empty = T)[[1]],8))
      head_pages = lapply(head_pages,function(x) gsub('^\\s{1,}','',x))
      toc = toc_list[[which(has_exec_sum)]]

      if(which(has_exec_sum) %in% which(need_next_level)){childs = sapply(toc$children[[1]]$children,function(u) u$title)}else{
      childs = sapply(toc$children,function(x) x$title)}
      is_exec_section = grepl('EXECUTIVE SUMMARY|SUMMARY',toupper(childs))
      which_pages = which(sapply(head_pages,function(x) any(grepl('EXECUTIVE SUMMARY|Executive Summary|SUMMARY|^Summary',x))&
                                   all(!grepl('Table of Contents|TABLE OF CONTENTS|\\.{6,}|SUMMARY OF',x))))
      if(identical(which_pages,integer(0))){print('summary not found');next}
      which_pages = min(which_pages)
      
   
      next_section = str_squish(str_remove(childs[which(is_exec_section)+1],'.*[0-9]'))
      next_section = str_remove(next_section,'^[^A-Z]+')
      if(identical(next_section,character(0))){next}
      if(grepl('Purpose|PURPOSE',next_section)&grepl('Need|NEED',next_section)){
      which_next_pages = min(which(sapply(head_pages,function(x) any(grepl('Purpose|PURPOSE',x)&grepl('Need|NEED',x))&all(!grepl('Contents|CONTENTS',x)))))}else{
      if(any(grepl('(:|-)',next_section))){next_section = str_extract(next_section,'.+(:|-)')}else{
      next_section_words = str_extract_all(next_section,'\\w+')[[1]]
      next_section = paste(next_section_words[1:min(length(next_section_words),4)],collapse = ' ')}
        which_next_pages = which(sapply(head_pages,function(x) any(grepl(toupper(next_section),toupper(x)))&all(!grepl('Table of Contents|TABLE OF CONTENTS',x))))
      }
       if(identical(which_next_pages,integer(0))){
       which_next_pages = which(grepl(paste0(next_section,'\\n'),temp_read))}
      if(length(which_next_pages)>1){which_next_pages = min(which_next_pages[which_next_pages>which_pages])}
      where_table_contents_starts = min(which(sapply(head_pages,function(x) any(grepl('Table of Contents|TABLE OF CONTENTS|CONTENTS|Contents',x)))))
       if(where_table_contents_starts==-Inf){where_table_contents_starts=integer(0)}
     # which_next_pages = which(sapply(head_pages,function(x) any(grepl(toupper(next_section),x))))
      if(identical(which_next_pages,integer(0))|identical(where_table_contents_starts,integer(0))){print("Next section or TOC not found");next}else{
      if(which_pages<where_table_contents_starts&where_table_contents_starts<which_next_pages){
        print('success')
        temp$Exec_Summary_File[i]<-dfile
        temp$Exec_Summary_Start_Page[i] <- which_pages
        temp$Exec_Summary_End_Page[i] <- where_table_contents_starts-1
      }else{
      if(min(which_pages) < min(which_next_pages)){
        print('success')
        temp$Exec_Summary_File[i]<-dfile
        temp$Exec_Summary_Start_Page[i] <- which_pages
        temp$Exec_Summary_End_Page[i] <- which_next_pages-1
      }else{
        print('Bad find')
      }
      }
    }
    }
  }
}

#fwrite(temp,'exec_summary_locations.csv')


# 
#   
# }
# matches = pblapply(still_need[1:1000],function(i) {
#  temp_flist  = fl[fl_id %in% full_rec$EIS.Number[i]]
#  #if(length(temp_flist)>=4){
# #  temp_flist = grep('Appendix|appendix|Apdcs|II|IV|Chapter_[2-9]|Section_[2-9]|Volume_[2-9]|Part_[2-9]|Part_[1-9][0-9]|Addendum',temp_flist,invert=T,value = T)}
#   temp_read_list = lapply(seq_along(temp_flist),function(f){
#    dfile   = temp_flist[f]
#    temp_read <- tryCatch(pdf_text(dfile),error = function(e) NULL)
#    pages = lapply(temp_read,function(page) tail(stringi::stri_split_lines(page,omit_empty = T)[[1]],3))
#    pages = lapply(pages,function(x) gsub('^\\s{1,}','',x))
#    es_page_extracts =  sapply(pages,str_extract_all,'(^|\\s{4,})(E|)S(-|\\s)[0-9]{1,3}|(E|)S(-|\\s)[0-9]{1,3}(\\s{4,}|$)')
#    es_page_nums = unlist(sapply(es_page_extracts,unlist))
#    pag = es_page_nums[es_page_nums!='']
#    str_squish(pag)})
#   
#   if(max(sapply(temp_read_list,length))>=2){
#   mx = which.max(sapply(temp_read_list,length))
#   full_rec$Exec_Summary_File[i] <- temp_flist[mx]
#   }
# },cl = 4)
# 
# 
# 
# table(is.na(full_rec$Exec_Summary_File))
# 
# 
# 
#   
# which(sapply(matches,length)==44)
# table(sapply(matches,length))
# 
# which(is.na(full_rec$Exec_Summary_File))[447]
# which(sapply(matches,length)==30)
# matches
# matches
# te
#   temp_flist
#   temp_flist
# length(temp_flist)})
# temp_flist
#   #flist = flist[gsub('.pdf','.txt',flist) %in% readable]
#   
# 
#     
#     
#     pages[[8]]
#     es_page_numbers = which(sapply(pages,function(x)  {any(grepl('^(E|)S(-|\\s)[0-9]{1,3}|(E|)S(-|\\s)[0-9]{1,3}$',x,perl=T)|
#                                                              (grepl('Page',x,perl=T)&grepl('s',x,perl=T) & grepl('[0-9]',x,perl=T)))}))
#     
# es_regex_options = c('Summary of the Final EIS','Executive Summary','Summary')
# 
# 
# 
# 
# 
# 
# 
# option_end = paste0(c(es_regex_options,toupper(es_regex_options)),'\\n')
# option_start = paste0('^',c(es_regex_options,toupper(es_regex_options)))
# 
# #es_regex = '^Summary of the Final EIS|^Summary$|^SUMMARY$|^Executive Summary|^EXECUTIVE SUMMARY|\nSummary$|\nSUMMARY$|\nAbstract|\nABSTRACT|\nExecutive Summary|\nEXECUTIVE SUMMARY|Summary of Plan|Executive Summary$|EXECUTIVE SUMMARY$'
# es_regex_exclude = 'Summary Report|Summary of (?!the Final EIS|Plan)|INDEX|Index|Table of Contents|TABLE OF CONTENTS|THIS PAGE INTENTIONALLY LEFT BLANK|LIST OF FIGURES|This Page Intentionally Left Blank|This page intentionally left blank|CONTENTS|^Contents|Summary of Comment|Summary of comments|LIST OF TABLES|Summary of Content|Summary of Public Comments'
# es_regex_abstract = '^ABSTRACT|^Abstract'
# #floc = 'scratch/eis_documents/'
# #readable = list.files('scratch/eis_documents_plaintext/')
# library(pdftools)
# regex_line_number_string = "([0-9]{1,2}\\s){3,}"
# 
# #fle = list.files('input/eis_es_text/')
# 
# 
# matches = pblapply(1:nrow(full_rec),function(i) {
#   if(!is.na(full_rec$Exec_Summary_File[i])){temp_flist    = full_rec$Exec_Summary_File[i]}else{
#   temp_flist  = fl[fl_id %in% full_rec$EIS.Number[i]]}
#  
#   #flist = flist[gsub('.pdf','.txt',flist) %in% readable]
#   temp_read_list = lapply(seq_along(temp_flist),function(f){
#       dfile   = temp_flist[f]
#       temp_read <- tryCatch(pdf_text(dfile),error = function(e) NULL)
#       pages = lapply(temp_read,function(page) tail(stringi::stri_split_lines(page,omit_empty = T)[[1]],3))
#       pages = lapply(pages,function(x) gsub('^\\s{1,}','',x))
#       
#       
#       pages[8]
#       str_extract_all(pages[8],'^(E|)S(-|\\s)[0-9]{1,3}|(E|)S(-|\\s)[0-9]{1,3}$')
#       pages[[8]]
#       es_page_numbers = which(sapply(pages,function(x)  {any(grepl('^(E|)S(-|\\s)[0-9]{1,3}|(E|)S(-|\\s)[0-9]{1,3}$',x,perl=T)|
#                                                                (grepl('Page',x,perl=T)&grepl('s',x,perl=T) & grepl('[0-9]',x,perl=T)))}))
#                                 
#       
#       #!grepl(es_regex_exclude,temp_read,perl=T) & 
#                                 #sapply(pages,function(x) {all(!grepl("[^\\x00-\\x7F]",x,perl=T))|any(grepl('Page',x,perl=T)&grepl('s',x,perl=T) & grepl('[0-9]',x,perl=T))}))
#       x = pages[8]
#       
#       all(!grepl("[^\\x00-\\x7F]",x,perl=T))
#       any(grepl('Page',x,perl=T)&grepl('s',x,perl=T) & grepl('[0-9]',x,perl=T))
#       
#       sapply(pages,function(x) {all(!grepl("[^\\x00-\\x7F]",x,perl=T))|any(grepl('Page',x,perl=T)&grepl('s',x,perl=T) & grepl('[0-9]',x,perl=T))})
#   
#       pages[8]
#       es_page_numbers
#       
#       
#       dfile
#      # temp_read = str_squish(temp_read)
#       if(!is.null(temp_read)){
#         es_pages_orig = which(grepl(es_regex,temp_read,perl = T)&!grepl(es_regex_exclude,temp_read,perl = T))
#   
#         temp_read[8]
#         es_pages = es_pages_orig[es_pages_orig < 100 | (es_pages_orig + 2) %in% es_pages_orig | (es_pages_orig - 2) %in% es_pages_orig |
#                                    (es_pages_orig + 1) %in% es_pages_orig | (es_pages_orig - 1) %in% es_pages_orig]
#         es_pages = sort(c(es_pages,which(((1:length(temp_read)) + 1) %in% es_pages & ((1:length(temp_read)) -1) %in% es_pages)))
#       }
#         
#       
#         pages = lapply(temp_read,function(page) tail(stringi::stri_split_lines(page,omit_empty = T)[[1]],3))
#         pages = lapply(pages,function(x) gsub('^\\s{1,}','',x))
#         es_page_numbers = which(sapply(pages,function(x)  {any(grepl('^(E|)S(-|\\s)[0-9]{1,3}|(E|)S(-|\\s)[0-9]{1,3}$',x,perl=T)|(grepl('Page',x,perl=T)&grepl('s',x,perl=T) & grepl('[0-9]',x,perl=T)))}) & 
#                                   !grepl(es_regex_exclude,temp_read,perl=T) & 
#                                   sapply(pages,function(x) {all(!grepl("[^\\x00-\\x7F]",x,perl=T))|any(grepl('Page',x,perl=T)&grepl('s',x,perl=T) & grepl('[0-9]',x,perl=T))}))
#         
#         grepl('(E|)S(-|\\s)[0-9]{1,3}$',pages[8],perl=T)
#         
#         es_page_numbers = es_page_numbers[(es_page_numbers + 1) %in% es_page_numbers | (es_page_numbers - 1) %in% es_page_numbers]
#         es_page_numbers = es_page_numbers[es_page_numbers<200]
#         es_page_numbers = es_page_numbers[es_page_numbers<=length(temp_read)]
#         
#         
#         es_page_numbers
#         es_pages = unique(sort(c(es_pages,es_page_numbers)))
#        
#         case_page_numbers = which(sapply(pages,function(x) any(!grepl('[^ivxlc]', x,perl=T))) & 
#                                     !grepl(es_regex_exclude,temp_read,perl=T) & 
#                                     sapply(pages,function(x) all(!grepl("[^\\x00-\\x7F]",x,perl=T))))
#         case_page_numbers = case_page_numbers[(case_page_numbers + 1) %in% case_page_numbers | (case_page_numbers - 1) %in% case_page_numbers]
#         if(length(es_pages)>0){ case_page_numbers = case_page_numbers[case_page_numbers >= max(es_pages)]}
#         temp_text = NULL
#         
# 
#         if(length(es_pages)==1){es_pages = es_pages + c(0:3)}
#         if(length(es_pages)!=0 | length(case_page_numbers)!=0){
#           temp_text <- stringr::str_replace_all(unlist(temp_read)[sort(union(es_pages,case_page_numbers))],"[\\s]+", " ")}
#         
#         if(length(es_pages)==0 & length(case_page_numbers)==0 & length(es_pages_orig)>0){
#           headings= sapply(seq_along(temp_read),function(p){
#             content = unlist(stringi::stri_split_lines(temp_read[p]))
#             content[!grepl('[a-z]',content) &
#                       grepl('[A-Z]{2,}',content) & !grepl('[0-9]{2,}',content)]})
#           summary_page_num = which(grepl("EXECUTIVE SUMMARY",headings))
#           summary_page_num = summary_page_num[!summary_page_num %in% which(grepl(es_regex_exclude,temp_read,perl=T))]
#           if(length(summary_page_num)!=0){
#             final = min(which(sapply(headings,length)>0 & seq_along(headings)>summary_page_num))
#             temp_text = temp_read[summary_page_num:final]}}
#         # if(length(es_pages)==0 & length(case_page_numbers)==0 & length(es_pages_orig)==0){
#         #   last_ditch_effort = which(sapply(pages,function(x) any(grepl('(Page\\s|)1-[0-9]{1,3}$',x,perl=T))))
#         #   pages_top3 = lapply(temp_read,function(page) head(stringi::stri_split_lines(page,omit_empty = T)[[1]],3))
#         #   intro_pages = which(grepl('INTRODUCTION|Introduction',pages_top3)| sapply(pages,function(x) any(grepl('(Page\\s|)1-[0-9]{1,3}$',x,perl=T)))) 
#         #   temp_text = temp_read[intro_pages]}
#         rm(temp_read);rm(dfile)
#         if(length(temp_text)>0){
#           temp_text}}
#     })
#   },cl = 4)
# 
# 



#   
#   
#   if(length(temp_read_list)>0){
#     temp_read_list = lapply(temp_read_list,function(x) x[!is.na(x)])
#     temp_read_all = unlist(temp_read_list[!is.na(temp_read_list) & !sapply(temp_read_list,is.null)])
#     if(sum(nchar(temp_read_all))>500000){print(paste('Bad find job for EIS#',erecord$EIS.Number[i]))}
#     if(sum(nchar(temp_read_all))<=500000){
#       temp_read_all = temp_read_all[!duplicated(temp_read_all)]
#       temp_read_all = gsub(regex_line_number_string,"",temp_read_all)
#       if(length(temp_read_all)>0)
#       {write.table(temp_read_all, file=paste0('input/eis_es_text/',gsub('pdf','txt',paste0('ES_',erecord$EIS.Number[i],'.txt'))),
#                    quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")}}}
#   rm(temp_read_all);rm(temp_read_list);rm(temp_read);rm(temp_flist)
# },cl = 4)
# 
# 
# 
# 
# 
# 
# #### use page # method ####
# for(eis in still_need){
#   eis = sample(still_need,1)
#   file_set = fl[fl_id ==eis]
#   file_set = file_set[!grepl('Appendices|Appendix|APPENDIX|APPENDICES',file_set)]
#   for(doc in file_set){
#     doc = file_set[1]
#     text = pdf_text(doc)
#     line_split = str_split(text,'\n')
#     sapply(line_split,function(x) tail(x,4))
#     
#   }
#   
#   length(file_set)
# }
#   
#   text_list = lapply(file_set,pdf_text)  
#   
#   
#   str_split(text_list,'\\n')
#   str(text_list)
#   }
# 
# 
# 
# still_need
# 
# 
# 
# fl[exec_file]
# fl[exec_file][duplicated(fl_id[exec_file])]
# 
# 
# 
# fl[fl_id=='20150138']
# sapply(fl,pdf_info)
# 
# test = lapply(fl[fl_id=='20150138'],pdf_info)
# 
# test
# 
# 
# pdf_info(fl[fl_id=='20150138'])
# exec_file = grepl('Exec|EXEC',fl)&!grepl('Appendix|APPENDIX',fl)
# fl[exec_file]
# fl[exec_file][duplicated(fl_id[exec_file])]
# 
# 
# fl[!grepl('^[0-9]{8}',basename(fl))]
# 
# fl[fl_id=='20190290']
# 
# library(doParallel)
# 
# stringcombos = c('Executive Summary','Summary')
# 
# stc = paste(paste0(c(stringcombos,toupper(stringcombos))),collapse='|')
# flist = list.files('../eis_documents/enepa_repository/text_as_datatable/',full.names = T,recursive = T)
# flist = flist[grepl('^201[3-9]',basename(flist))]
# flist = flist[!grepl('^[0-9]{8}_[0-9]{8}.txt',basename(flist))]
# 
# list_of_matches = pbsapply(flist,function(f) {
#   have = F
#   while(!have){
#   temp = tryCatch(fread(f),error = function(e) NULL)
#   if(!is.null(temp)){have = T}
#   }
#   preps = any(grepl(stc,temp$text)&!grepl('Table of Contents|TABLE OF CONTENTS|DOCUMENT ORGANIZATION',temp$text))
#   preps
# },cl = 4)
# 
# keep = unlist(list_of_matches)
# 
# candidate_flist = flist[keep]
# 
# list_of_execsums = pbsapply(candidate_flist,function(f) {
#   have = F
#   while(!have){
#     temp = tryCatch(fread(f),error = function(e) NULL)
#     if(!is.null(temp)){have = T}
#   }
#   preps = any(grepl('Executive Summary|EXECUTIVE SUMMARY',temp$text)&!grepl('Table of Contents|TABLE OF CONTENTS|DOCUMENT ORGANIZATION',temp$text))
#   preps
# },cl = 4)
# 
# 
# require(pdftools)
# test = '../eis_documents/enepa_repository/documents/2015/20150280_00_OMSF_FEIS_Exec_Summary.pdf'
# tt = pdf_text(test)
# 
# 
# es_regex = '^Summary of the Final EIS|^Summary$|^SUMMARY$|^Executive Summary|^EXECUTIVE SUMMARY|\nSummary$|\nSUMMARY$|\nAbstract|\nABSTRACT|\nExecutive Summary|\nEXECUTIVE SUMMARY|Summary of Plan|Executive Summary$|EXECUTIVE SUMMARY$'
# es_regex_exclude = 'Summary Report|Summary of (?!the Final EIS|Plan)|INDEX|Index|Table of Contents|TABLE OF CONTENTS|THIS PAGE INTENTIONALLY LEFT BLANK|LIST OF FIGURES|This Page Intentionally Left Blank|This page intentionally left blank|CONTENTS|^Contents|Summary of Comment|Summary of comments|LIST OF TABLES|Summary of Content|Summary of Public Comments'
# es_regex_abstract = '^ABSTRACT|^Abstract'
# #floc = 'scratch/eis_documents/'
# #readable = list.files('scratch/eis_documents_plaintext/')
# library(pdftools)
# regex_line_number_string = "([0-9]{1,2}\\s){3,}"
# 
# fle = list.files('input/eis_es_text/')
# flen = str_extract(fle,'[0-9]{8}')
# need_still = erecord$EIS.Number[!erecord$EIS.Number %in% flen]
# 
# tt
# fl = list.files('../eis_documents/enepa_repository/documents/',full.names = T,recursive = T)
# new= gsub('\\s','_',fl)
# file.rename(fl,new)
# 
# candidate_flist[!list_of_execsums&grepl('Exec',candidate_flist)]
# 
# 
# 
# candidate_type = rep(NA,length(list_of_execsums))
# candidate_type[grepl('Exec',candidate_flist)&!grepl('Append',candidate_flist)] <- 'whole_doc'
# candidate_type[grepl('Appendix',candidate_flist)] <- 'appendix'
# 
# 
# table(is.na(candidate_type))
# 
# table(candidate_type,is.na(candidate_type))
# candidate_flist[is.na(candidate_type)]
# 
# 
# 
# 
# preps = unlist(list_of_preparers)
# saveRDS(preps,file = 'scratch/boilerplate/eis_files_with_preparer_list.txt')
#   
#   
# 
# flist = list.files('../eis_documents/agency_nepa_libraries/usfs/text_as_datatable/',full.names = T,recursive = T)
# 
# list_of_preparers = pblapply(flist,function(f) {
#   have = F
#   while(!have){
#     temp = tryCatch(fread(f),error = function(e) NULL)
#     if(!is.null(temp)){have = T}
#   }
#   preps = any(grepl(stc,temp$text)&!grepl('Table of Contents|TABLE OF CONTENTS|DOCUMENT ORGANIZATION',temp$text))
#   if(preps){f}
# },cl = 3)
# 
# preps = unlist(list_of_preparers)
# saveRDS(preps,file = 'scratch/boilerplate/usfs_files_with_preparer_list.txt')
# 
# 
# 
# flist = list.files('../eis_documents/agency_nepa_libraries/blm/text_as_datatable/',full.names = T,recursive = T)
# #flist = flist[grepl('^201[3-9]',basename(flist))]
# #flist = flist[!grepl('^[0-9]{8}_[0-9]{8}.txt',flist)]
# 
# list_of_preparers = pblapply(flist,function(f) {
#   have = F
#   while(!have){
#     temp = tryCatch(fread(f),error = function(e) NULL)
#     if(!is.null(temp)){have = T}
#   }
#   preps = any(grepl(stc,temp$text)&!grepl('Table of Contents|TABLE OF CONTENTS|DOCUMENT ORGANIZATION',temp$text))
#   if(preps){f}
# },cl = 3)
# 
# preps = unlist(list_of_preparers)
# saveRDS(preps,file = 'scratch/boilerplate/blm_files_with_preparer_list.txt')
# 
# 
# flist = list.files('../eis_documents/agency_nepa_libraries/doe/text_as_datatable/',full.names = T,recursive = T)
# #flist = flist[grepl('^201[3-9]',basename(flist))]
# #flist = flist[!grepl('^[0-9]{8}_[0-9]{8}.txt',flist)]
# 
# list_of_preparers = pblapply(flist,function(f) {
#   have = F
#   while(!have){
#     temp = tryCatch(fread(f),error = function(e) NULL)
#     if(!is.null(temp)){have = T}
#   }
#   preps = any(grepl(stc,temp$text)&!grepl('Table of Contents|TABLE OF CONTENTS|DOCUMENT ORGANIZATION',temp$text))
#   if(preps){f}
# },cl = 3)
# 
# preps = unlist(list_of_preparers)
# saveRDS(preps,file = 'scratch/boilerplate/doe_files_with_preparer_list.txt')





