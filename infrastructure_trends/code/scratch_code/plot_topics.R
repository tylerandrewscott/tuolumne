library(tidyverse)
library(data.table)
library(ggthemes)


#### basic summary stats
n_draft_pages = sapply(list.files('input/eis_clean_pdfs',"DEIS",full.names = T),
                       function(x) {gc();pg = tabulizer::get_n_pages(x);pg})
n_final_pages = sapply(list.files('input/eis_clean_pdfs',"FEIS.pdf$",full.names = T),
                       function(x) {gc();pg = tabulizer::get_n_pages(x);pg})
meta_sheet = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSW1BL6LAW6zGiL2Ms-QC4n_H7ASJfr-pBxjvCnGB8rtaP4x08MQnmK5RHoRoYPO3syl6LubHbxCOeG/pub?output=csv'
library(lubridate)

pages_df = do.call(full_join,list(data.frame(n_draft_pages) %>% rownames_to_column(var = 'file') %>% rename(pages = n_draft_pages) %>%
  mutate(file = gsub("input/eis_clean_pdfs/",'',file,fixed=T)),
data.frame(n_final_pages) %>% rownames_to_column(var = 'file') %>% rename(pages = n_final_pages) %>%
  mutate(file = gsub("input/eis_clean_pdfs/",'',file,fixed=T)))) %>%
  filter(!grepl('DEIS1',file)) %>% mutate(file = gsub("DEIS2",'DEIS',file)) %>%
  mutate(type = str_extract(file,'[DF]EIS')) %>%
  mutate(file = gsub('_[DF]EIS\\.pdf','',file)) %>%
  filter(file!='Maricopa_Sun_Solar_Complex_HCP')

pages_df %>% spread(type,pages) %>% mutate(diff = FEIS - DEIS) %>%
  summarise_if(is.numeric,.funs = funs(min,median,mean,max)) %>% gather() %>%
  mutate(measure = str_extract(key,'_.*'),
         key = gsub('_.*','',key)) %>% spread(measure,value)

pages_df$file[!pages_df$file %in% gsub('_FEIS.*','',list.files('input/eis_comment_feis_text/'))]

meta = read_csv(meta_sheet)
year_df = meta %>% filter(`Standard Name`  %in% pages_df$file) %>% 
  filter(!duplicated(`EIS Number`)) %>%
  mutate(year = str_extract(`EIS Number`,'^2[0-9]{3}')) %>%
  select(Document,year,`Standard Name`) %>% mutate(Document = gsub("*",'',Document,fixed=T)) %>%
 filter(grepl('Final',Document)) %>%
  group_by(year) %>% summarise(n())
  
head(year_df)
pages_df$file[!pages_df$file %in% test$`Standard Name`]

read_csv('input/scratch/all_eis_clean_sentences.csv') %>% group_by(file) %>% summarise(co = n()) 
  group_by() %>%
  mutate_if(is.numeric,.funs= funs(min,median,mean,max))

group_by(Document,year) %>% summarise(n())

######
pred_dt = data.table::fread('input/scratch/classified_eis_sentences.csv')
com_dt = data.table::fread('input/scratch/classified_eis_comment_sentences.csv')

pred_dt$type = ifelse(grepl('FEIS',pred_dt$file),'FEIS','DEIS')
com_dt$type = 'Comment'
pred_dt = full_join(pred_dt,com_dt) %>% mutate(file = gsub('_[DF]EIS.*','',file))
pred_dt = data.table(pred_dt[!grepl('P_',colnames(pred_dt))],(pred_dt[,grepl('P_',colnames(pred_dt))] > 0.5) + 0)

keep_sites = pred_dt %>% filter(!duplicated(paste(file,type))) %>% group_by(file) %>% summarise(set = n()) %>% filter(set==3) %>% select(file)
pred_dt = pred_dt[pred_dt$file %in% keep_sites$file]
pred_dt$classified = rowSums(pred_dt[,grepl('P_',colnames(pred_dt)),with=F])

sum_dt = left_join(pred_dt %>% group_by(file,type) %>% summarise_at(.vars = vars(matches("P_")),.funs = list(sum)),
pred_dt %>% group_by(file,type) %>% summarise(tot_sents = n(),class_sents = sum(classified)))



###### if class sents #####
class_sum_dt <- sum_dt %>% select(-tot_sents)
class_sum_dt[,grepl('P_',colnames(class_sum_dt))] <- class_sum_dt[,grepl('P_',colnames(class_sum_dt))]/class_sum_dt$class_sents

table_dt = class_sum_dt %>% filter(type=='FEIS') %>% mutate_if(is.numeric,round,3) %>% select(-class_sents,-type,) %>% 
  gather(Topic,Prop,-file) %>% ungroup() %>% mutate(file = gsub('_Project$|_Program$','',file),
                                                    Topic = gsub('^P_','',Topic))

library(viridis)
ggplot(table_dt,aes(x = Topic,y = file,fill=Prop)) + geom_tile() + 
  scale_x_discrete(position = "top",expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_viridis(name = 'Prop. focus',option = 'B') + 
  theme(axis.title.y = element_blank(),axis.ticks=element_blank(),
        axis.text = element_text(size = 12))

class_sum_dt$type <- as.factor(class_sum_dt$type)
class_sum_dt$type <- fct_relevel(class_sum_dt$type,'DEIS','Comment','FEIS')

plot_df = class_sum_dt %>% gather('Topic','Prop',-file,-type,-class_sents)
plot_df$Topic = gsub('^P_','',plot_df$Topic)

ggplot(plot_df%>% filter(type=='FEIS'),aes(x = file,y = Prop,fill= Topic)) + geom_bar(stat = 'identity')+
  scale_y_continuous(name = 'Proportion of content',limits=c(0,0.5),expand= c(0,0)) + scale_fill_brewer(type = 'qual')  + coord_flip() + 
  theme_bw() + scale_x_discrete(name = 'test') + ggtitle('Proportion of focus by impact area for final EIS')+
  theme(axis.title.y = element_blank(),axis.ticks = element_blank(),axis.text = element_text(size = 12),
        axis.title = element_text(size =14))

#ggplot(plot_df %>% filter(type!='Comment')) %>% %>%
library(viridis)
per_change = plot_df %>% filter(type!='Comment') %>% select(-tot_sents,-class_sents) %>% spread(type,Prop) %>% 
  mutate(Perc_Change = 100 * (1 - FEIS/DEIS)) %>% select(-DEIS,-FEIS)
plot_df <- left_join(plot_df,per_change)


ggplot(plot_df %>% filter(type!='Comment'),aes(x = type,y = Prop,group = file)) + 
  geom_point() + geom_path(aes(colour=Perc_Change),alpha = 0.8,lwd=0.25) + 
  facet_wrap(~Topic) + scale_x_discrete(expand=c(0,0),labels=c('Draft','Final'),name='EIS version') + theme_bw() + 
  scale_y_continuous(name = 'Prop. focus / all classified sentences') + 
  scale_color_gradient2(limits=c(-150,150),breaks=c(-150,-50,50,150),labels=c('-150%','-50%','50%','150%'),mid = 'grey50') + 
  theme(legend.position = c(0.65,0.15),legend.direction = 'horizontal')

box_sums = class_sum_dt %>% group_by(type) %>% select(-class_sents)  %>% gather(Topic,Value,-file,-type) %>% 
  mutate(Topic = gsub('^P_','',Topic))
ggplot(box_sums,aes(x = Topic,y = Value,color=type)) + geom_boxplot() + 
  scale_color_brewer(type = 'qual',palette = 2) + theme_bw() + 
  scale_y_continuous(name = 'Prop. emphasis/all impact sentences') + 
  theme(legend.position = c(0.8,0.7),legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),axis.title=element_text(size = 12)) +
  ggtitle('Distribution of impact area focus by document type')


perc_point_changes = class_sum_dt %>% select(-class_sents) %>% gather(Topic,Prop,-type,-file) %>% group_by(file,Topic) %>% arrange(file,Topic,type) %>%
  mutate(change = 100 * (Prop-lag(Prop,1)),change_draft_to_final = 100 * (Prop - lag(Prop,2))) %>% select(-Prop)

perc_point_changes$Topic = gsub("^P_","",perc_point_changes$Topic)
g1 = ggplot(perc_point_changes %>% filter(type == 'Comment') %>% select(-change_draft_to_final)) + 
  geom_boxplot(aes(x = Topic,y=change)) + theme_bw() + scale_y_continuous(name = "Percentage point change") + 
  ggtitle("Percentage point change from draft EIS to comments")+ coord_flip()

g2 = ggplot(perc_point_changes %>% filter(type == 'FEIS') %>% select(-change_draft_to_final)) + 
  geom_boxplot(aes(x = Topic,y=change)) + theme_bw() + scale_y_continuous(name = "Percentage point change") + 
  ggtitle("Percentage point change from comments to final EIS")+ coord_flip()

g3 =  ggplot(perc_point_changes %>% filter(type == 'FEIS')) + 
  geom_boxplot(aes(x = Topic,y=change_draft_to_final)) + theme_bw() + scale_y_continuous(name = "Percentage point change") + 
  ggtitle("Percentage point change from draft EIS to final EIS") + coord_flip()

library(gridExtra)
grid.arrange(g1,g2,g3,ncol=1)

round(do.call(rbind,list(summary(perc_point_changes$change[perc_point_changes$type=='Comment'])[c(1,3,4,6)],
summary(perc_point_changes$change[perc_point_changes$type=='FEIS'])[c(1,3,4,6)],
summary(perc_point_changes$change_draft_to_final)[c(1,3,4,6)])),3)


change,-file,-type,-Topic,-change_draft_to_final

source('code/custom_functions/ggradar.R')
radar_dt = class_sum_dt
colnames(radar_dt) <- gsub("^P_","",colnames(radar_dt))
radar_dt$group = paste(radar_dt$file,radar_dt$type,sep = '_')
radar_dt = radar_dt[radar_dt$file %in% sort(unique(radar_dt$file))[c(1,2,4,6)],]
colnames(radar_dt)[8] <- 'Cultural'
radar_dt$file = gsub('_Project$|_Program$|_Policy$','',radar_dt$file)
radar_list = lapply(sort(unique(radar_dt$file)),function(x) {
  print(x)
  tdf = radar_dt[radar_dt$file ==x,c('type',grep('^[A-Z]',colnames(radar_dt),value=T))]
  ggr <- ggradar(data.frame(tdf),grid.max = 0.50,grid.mid = 0.25,label.gridline.max = TRUE,label.gridline.mid=TRUE,label.gridline.min = FALSE,
                 values.radar = c(0.0,0.25,0.50),alpha = 0.5,point_type = 21,axis.label.size = 4,plot.title = x,grid.label.size=4,legend.text.size = 4,group.point.size = 3) +
    theme(legend.position = 'bottom',legend.text = element_text(size = 10),title = element_text(size = 12))
  ggr
})
names(radar_list) <- sort(unique(radar_dt$file))

library(gridExtra)
grid.arrange(radar_list[[1]],radar_list[[2]],radar_list[[3]],radar_list[[4]],padding = unit(0.1, "line"))




#######if tot sents#####
sum_dt[,grepl('P_',colnames(sum_dt))] <- sum_dt[,grepl('P_',colnames(sum_dt))]/sum_dt$tot_sents

table_dt = sum_dt %>% filter(type=='FEIS') %>% mutate_if(is.numeric,round,3) %>% select(-tot_sents,-type,-class_sents) %>% 
  gather(Topic,Prop,-file) %>% ungroup() %>% mutate(file = gsub('_Project$|_Program$','',file),
                                                    Topic = gsub('^P_','',Topic))
library(viridis)
ggplot(table_dt,aes(x = Topic,y = file,fill=Prop)) + geom_tile() + 
  scale_x_discrete(position = "top",expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_viridis(limits = c(0,0.25),name = 'Prop. focus',option = 'B') + 
  theme(axis.title.y = element_blank(),axis.ticks=element_blank(),
        axis.text = element_text(size = 12))

sum_dt$type <- as.factor(sum_dt$type)
sum_dt$type <- fct_relevel(sum_dt$type,'DEIS','Comment','FEIS')

plot_df = sum_dt %>% gather('Topic','Prop',-file,-type,-tot_sents,-class_sents)
plot_df$Topic = gsub('^P_','',plot_df$Topic)

plot_df$file = gsub('_Project$|_Program$|_Policy$','',plot_df$file)
ggplot(plot_df%>% filter(type=='FEIS'),aes(x = file,y = Prop,fill= Topic)) + geom_bar(stat = 'identity')+
  scale_y_continuous(name = 'Proportion of content',limits=c(0,0.5),expand= c(0,0)) + scale_fill_brewer(type = 'qual')  + coord_flip() + 
  theme_bw() + scale_x_discrete(name = 'test') + ggtitle('Proportion of focus by impact area for final EIS')+
  theme(axis.title.y = element_blank(),axis.ticks = element_blank(),axis.text = element_text(size = 12),
        axis.title = element_text(size =14))

summary(plot_df$Prop[plot_df$type=='FEIS'])



library(ggradar)
library(scales)
library(tibble)

radar_dt = sum_dt
colnames(radar_dt) <- gsub("^P_","",colnames(radar_dt))
radar_dt$group = paste(radar_dt$file,radar_dt$type,sep = '_')
colnames(radar_dt)[8] <- 'Cultural'
radar_list = lapply(sort(unique(radar_dt$file)),function(x) {
  tdf = radar_dt[radar_dt$file ==x,c('type',grep('^[A-Z]',colnames(radar_dt),value=T))]
  ggr <- ggradar(data.frame(tdf),grid.max = 0.15,grid.mid = 0.05,label.gridline.max = TRUE,label.gridline.mid=TRUE,label.gridline.min = FALSE,
                 values.radar = c(0.0,0.05,0.15),alpha = 0.5,point_type = 21,axis.label.size = 4,plot.title = x,grid.label.size=3,legend.text.size = 3,group.point.size = 3) +
    theme(legend.position = 'bottom',legend.text = element_text(size = 10),title = element_text(size = 12))
  ggr
})
names(radar_list) <- sort(unique(radar_dt$file))

library(gridExtra)
do.call(grid.arrange,radar_list[c(1,2,4,6)])

change_df = plot_df
change_df %>% filter(type == 'FEIS') %>% group_by(Topic) %>% summarise_at(.vars = vars('Prop'),.funs = list(min,median,mean,max)) %>% mutate_if(is.numeric,round,2)
perc_changes = change_df %>% select(-tot_sents,-class_sents) %>% spread(type,Prop) %>% mutate(draft_to_comment_perc_change = ifelse(Comment/DEIS>1,({Comment/DEIS}-1) * 100,{(Comment/DEIS)-1}*100),
                                                                                 comment_to_feis_perc_change = ifelse(FEIS/Comment>1,({FEIS/Comment}-1) * 100,{(FEIS/Comment)-1}*100),
                                                                                 draft_to_feis_perc_change = ifelse(FEIS/DEIS>1,({FEIS/DEIS}-1) * 100,{(FEIS/DEIS)-1}*100))


draft_change_table = perc_changes %>% group_by(Topic) %>% 
  summarise_at(.vars = vars('draft_to_comment_perc_change'),.funs = list(min,median,mean,max)) %>% 
  mutate_if(is.numeric,round,2)
knitr::kable(draft_change_table)




plot_df %>% filter(file == 'Contra_Loma_Reservoir')
perc_changes %>% filter(draft_to_comment_perc_change==-100)
perc_changes


head(plot_df)






  

ggplot(perc_changes,aes(x = draft_to_comment_perc_change,colour = Topic)) + geom_density(trim=T) + facet_wrap(~Topic)




0.0722/0.0715
0.0427/0.0264


0.0427/0.0264  

0.0264 * 1.6 

0.0715 * 0.43
0.0308/0.0715  
0.10/0.0715

0.0715 *  0.4307692
0.0715 * 0.40
for (i in 1:nrow(change_df))
{
  if(change_df$type[i=='DEIS')
}

head(sum_dt)

grid.arrange(radar_list)
class(radar_list[[1]])
,facet = "~file"
ggradar

sum_dt[sum_dt$file=='Alta_East_Wind_Project',which(!colnames(sum_dt) %in% c('tot_sents','type'))]

ggplot(plot_df) + geom_density(aes(x = Prop,group = type)) + facet_wrap(~Topic)
  

  


scale_fill_brewer(type='qual')
# Example from https://github.com/ricardo-bion/ggradar

library(ggradar)
library(scales)
 mtcars %>%
   
   tibble::rownames_to_column(var = 'group') %>%
  tail(4) %>% select(1:10) -> mtcars_radar

class_df$file = gsub('_[DF]EIS\\.txt','',class_df$file)


?ggradar()
plot(p1)
library(tidyverse)
library(dplyr)
library(tidyr)
mtcars %>%
  add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) %>%
  tail(4) %>% select(1:10)
head(plot_df)
?spread
plot_df %>% spread(Topic,Prop)
ggplot(plot_df) + 
  facet_wrap(~file,scales = 'free') + theme_tufte(ticks=F) + 
  geom_density(aes(x = Topic,y  = Prop,color=type,fill=type))

plot_df$Topic

plot_df$Prop
plot_df$Prop


com_pred_dt = data.table::fread('input/scratch/classified_eis_comment_sentences.csv')
com_pred_dt$type = 'Comments'


com_pred_dt %>% group_by(file) %>% summarise(co = n()) %>% arrange(co)
all_preds = full_join(pred_dt,com_pred_dt)


all_preds %>% group_by(file, type) %>% summarise(tot_sents = n()) 

pred_dt[,grep('P_',colnames(text_dt))] <- (pred_dt[,grep('P_',colnames(text_dt))] > 0.5)+0


length(list.files('input/dump_eis_text/'))

pred_dt[,grep('P_',colnames(text_dt)),with=F] = (pred_dt[,grep('P_',colnames(text_dt)),with=F] > 0.5) + 0



= data.table((text_dt[,grep('P_',colnames(text_dt)),with=F] > 0.5) + 0)
pred_dt$file = text_dt$file

class_df = pred_dt %>% group_by(file) %>% summarise_at(.vars = vars(matches("P_")),.funs = list(sum))
class_df$Total_Sents = rowSums(class_df  %>% select(contains('P_')))
class_df[,grepl('^P_',colnames(class_df))] = class_df[,grepl('^P_',colnames(class_df))] / class_df$Total_Sents

plot_df = class_df %>% gather('Topic','Prop',-file) %>% filter(Topic!='Total_Sents')
plot_df$file = gsub('\\.txt','',plot_df$file)
ggplot(plot_df,aes(x = file,y = Prop,fill= Topic)) + geom_bar(stat = 'identity') +
  scale_y_continuous(limits=c(0,1),expand= c(0,0)) + scale_fill_brewer(type = 'qual') + coord_flip()

?summarise_each
predict <- class_model %>% keras::predict_proba(x = sequences[1:10,])


predict_generator <- function(sent_sequence, batch_size) {
  function() {
    rows <- sample(1:nrow(X_data), batch_size, replace = TRUE)
    list(X_data[rows,], Y_data[rows,])
  }
}

model %>% 
  fit_generator(sampling_generator(X_train, Y_train, batch_size = 128), 
                steps_per_epoch = nrow(X_train) / 128, epochs = 10)


keras::predict_proba(object = class_model,x = sequences[1:10,])


keras::predict_on_batch(object = class_model,x = sequences,)

keras::predict_generator(object = class_model,generator = sequences[1:10000,],steps = 100)



= predict_on_batch(object = class_model,x = sequences[1:10000,])




keras::predict_proba()
keras::text_tokenizer(text_dt$text[1],)
[sent for s in doc.sents][:10]
#predict_text = ["To achieve a more environmentally sustainable energy resource mix and meet RPS goals, LADWP must access renewable energy sources, most of which are located in more remote areas (such as the Owens Valley, Mojave Desert, Tehachapi Mountains) where limited electrical infrastructure exists."]
#predict_sequences = tokenizer.texts_to_sequences(predict_text)
#predict_sequences = pad_sequences(predict_sequences, maxlen=MAX_SEQUENCE_LENGTH)


keras::to