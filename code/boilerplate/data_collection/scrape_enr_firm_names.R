require(rvest)
### make consult matrix ####
enr2019_1 = 'https://www.enr.com/toplists/2019-Top-200-Environmental-Firms-1'
enr2019_2 = 'https://www.enr.com/toplists/2019-Top-200-Environmental-Firms-2'
enr2018_1 = 'https://www.enr.com/toplists/2018-Top-200-Environmental-Firms-1'
enr2018_2 = 'https://www.enr.com/toplists/2018-Top-200-Environmental-Firms-2'
enr2017_1 = 'https://www.enr.com/toplists/2017-Top-200-Environmental-Firms-1'
enr2017_2 = 'https://www.enr.com/toplists/2017-Top-200-Environmental-Firms-2'
enr2015_1 = 'https://www.enr.com/toplists/2015_Top_200_Environmental_Firms1'
enr2015_2 = 'https://www.enr.com/toplists/2015_Top_200_Environmental_Firms2'
#


des2019_1 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms1'
des2019_2 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms2'
des2019_3 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms3'
des2019_4 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms4'
des2019_5 = 'https://www.enr.com/toplists/2019-Top-500-Design-Firms5'
#
des2017_1 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms1'
des2017_2 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms2'
des2017_3 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms3'
des2017_4 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms4'
des2017_5 = 'https://www.enr.com/toplists/2017-Top-500-Design-Firms5'
#

des2015_1 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms1'
des2015_2 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms2'
des2015_3 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms3'
des2015_4 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms4'
des2015_5 = 'https://www.enr.com/toplists/2015_Top_500_Design_Firms5'
#
firm_lists = ls(pattern = 'des201[579]|enr201[579]')
#
firm_lists = ls(pattern = 'des201[579]|enr201[579]')
firm_names_css_2019 = 'td:nth-child(3)'
firm_names_css_2017 = 'td:nth-child(3)'
firm_names_css_2015 = 'td:nth-child(2)'

firm_scrapes = pblapply(firm_lists,function(x) {
  if(grepl('2015',x)){
    get(x) %>% read_html() %>% html_nodes(firm_names_css_2015) %>% html_text(trim=T)
  }
  else if(grepl('2017',x)){
    get(x) %>% read_html() %>% html_nodes(firm_names_css_2017) %>% html_text(trim=T)
  }
  else if(grepl('2019',x)){
    get(x) %>% read_html() %>% html_nodes(firm_names_css_2019) %>% html_text(trim=T)
  }
})


names(firm_scrapes)<-firm_lists

saveRDS(firm_scrapes,'scratch/enr_firm_lists.RDS')