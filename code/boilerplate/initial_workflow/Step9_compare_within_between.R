
require(data.table)
require(tidyverse)

fls = list.files('scratch/boilerplate/','pagecount.RDS$',full.names = T)

flist = lapply(seq_along(fls),function(x) readRDS(fls[x])%>% mutate(type = fls[x]) %>% 
                 data.table())
fls
dt = rbind(readRDS(fls[[3]]) %>% mutate(type = str_extract(fls[[3]],'between|within')) %>% data.table(),
           readRDS(fls[[4]])%>% mutate(type = str_extract(fls[[4]],'between|within')) %>% data.table(),use.names = T,fill = T)
dt$type = str_extract(dt$type,'between|within')
dt$Agency = ifelse(is.na(dt$AGENCY_SHORT),as.character(dt$Agency_Short),as.character(dt$AGENCY_SHORT))

comp = dcast(dt[,.(PROJECT_ID,over300,total_pages,Agency,type)],PROJECT_ID + as.factor(Agency) + total_pages ~ type,value.var = 'over300')

compare_between_within = ggplot(comp,aes(x = between/total_pages,y = within/total_pages,size = total_pages)) + 
  geom_point(alpha = 0.5,pch = 21) + 
  scale_x_continuous('Proportion of pages w/ between-EIS LDA score > 300') +
  scale_y_continuous('Proportion of pages w/ within-EIS LDA score > 300') +
  theme_bw() + 
  theme(legend.position = c(0.8,0.7)) + 
  scale_size_binned_area(breaks=c(100,500,1000,10000),name = '# EIS pages') + 
  ggtitle('Between-project vs. within-project text reuse')
ggsave(compare_between_within,filename = 'output/boilerplate/figures/compare_within_between.png',dpi = 300,width = 7,height = 6,units = 'in')



fls
dt$type
head(dt)
table(dt$TYPE,is.na(dt$TYPE))
dt[TYPE=='EIS']


table(dt$AGENCY)
dt[PROJECT_ID=='11057']
dt[order(PROJECT_ID)]
