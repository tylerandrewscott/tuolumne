library(tidyverse)
library(data.table)
library(ggthemes)
library(lubridate)
tf_idf = readRDS('../../../../net/tmp/tscott1/tuolumne_scratch/scratch/leverage_points/focal_tfidf.RDS')
tf_idf$word = as.character(tf_idf$word)
tf_idf = tf_idf[tf_idf$word%in%c('climate change','environmental justice','extreme weather','economic development','drought'),]

figure2a = ggplot(tf_idf[tf_idf$CA==1,],aes(x = Date,y = tf,colour = word,linetype = word))+
  geom_smooth(se=F,lwd = 0.7) + 
  #facet_wrap(~word,scales='free_y',ncol = 3) + 
  scale_y_continuous(name = 'term frequency') + 
  scale_x_continuous(name = 'Federal Register publication date',breaks=seq(2012,2018,2),limits = c(2012,2019)) + 
 # scale_color_tableau(name = 'term') + 
  scale_colour_grey(name = 'term') +  scale_linetype(name = 'term') + 
   guides(colour = F,linetype= F) + 
  theme_tufte(ticks=F)  + theme(legend.position =c(0.85,0.35),
                          legend.title = element_blank(),legend.box = 'horizontal',text = element_text(size = 10,family = 'Times'))+
  #axis.text.y = element_text(size = 12),axis.title = element_text(size = 14),legend.text = element_text(size = 12),strip.text = element_text(size = 12),axis.text.x = element_text(size = 12)) +
  geom_vline(aes(xintercept = 2014),lty = 1,col = 'grey50',lwd = 0.2) +
  ggtitle('term frequency and tf-idf weights for CA water/energy project EISs') 

figure2b = ggplot(tf_idf[tf_idf$CA==1,],aes(x = Date,y = tf_idf,colour = word,linetype = word))+
 geom_smooth(se=F,lwd = 0.7) + 
  #facet_wrap(~word,scales='free_y',ncol = 3) + 
   scale_y_continuous(name = 'relative tf-idf weight by term') + 
   scale_x_continuous(name = 'Federal Register publication date',breaks=seq(2012,2018,2),limits = c(2012,2019)) + 
  scale_colour_grey(name = 'term') +   scale_linetype(name = 'term') + 
  # scale_color_tableau(name = 'term') + 
  # guides(colour = F) + 
  theme_tufte(ticks=F)  + theme( legend.title = element_blank(),legend.text = element_text(),
                                legend.spacing.y = unit(-5,'cm'),legend.position = c(0.5,.4),
                                #legend.background = element_rect(fill = alpha('white',0.75)),
                                legend.box = 'horizontal',text = element_text(size = 10,family = 'Times'))+
  #axis.text.y = element_text(size = 12),axis.title = element_text(size = 14),legend.text = element_text(size = 12),strip.text = element_text(size = 12),axis.text.x = element_text(size = 12)) +
   geom_vline(aes(xintercept = 2014),lty = 1,col = 'grey50',lwd = 0.2) +
 #  ggtitle('tf-idf term weights for CA water/energy project EISs') + 
  NULL

library(gridExtra)
saveRDS(figure2a,'figure2a.RDS')
saveRDS(figure2b,'figure2b.RDS')
figure2grob = grid.arrange(figure2a,figure2b,ncol=2)


ggsave(figure2grob,filename = 'output/leverage_points/figure2.png',dpi=500,width=6,height=3,units='in')
 

ca_sents = fread('../../../../net/tmp/tscott1/tuolumne_scratch/scratch/leverage_points/classified_ca_sentences_waterclimatedrought.csv')
#ca_ref = fread('https://docs.google.com/spreadsheets/d/e/2PACX-1vSW1BL6LAW6zGiL2Ms-QC4n_H7ASJfr-pBxjvCnGB8rtaP4x08MQnmK5RHoRoYPO3syl6LubHbxCOeG/pub?output=csv')
#ca_sents$FR_Date = ca_ref$`Federal Register Date`[match(ca_sents$EIS_Number,ca_ref$`EIS Number`)]
library(lubridate)
ca_sents$T_Water <- (ca_sents$P_Water>0.5)+0
ca_sents$T_Climate <- (ca_sents$P_Climate>0.5)+0
ca_sents$T_Water_Climate <- ((ca_sents$P_Climate>0.5) & (ca_sents$P_Water>0.5)) + 0
ca_sents$T_Drought <- (ca_sents$P_Drought>0.5)+0
ca_sents$T_Water_Drought <- ((ca_sents$P_Drought>0.5) & (ca_sents$P_Water>0.5)) + 0
props = ca_sents %>% group_by(EIS_Number,FR_Date,California,CA_Sample) %>% 
  summarise(water_sents = sum(T_Water),
            drought_sents = sum(T_Drought),
            climate_sents = sum(T_Climate),
            water_drought_sents = sum(T_Water_Drought),
            water_climate_sents = sum(T_Water_Climate),
            total_sents = n()) %>%
  mutate(prop_water = water_sents/total_sents,prop_drought = drought_sents/total_sents,
         prop_climate = climate_sents/total_sents,
         prop_water_climate = water_climate_sents/total_sents,
         prop_water_drought = water_drought_sents/total_sents)

props$Cat = ifelse(props$California==1&props$CA_Sample==1,'CA water/energy',
                   ifelse(props$California==1&props$CA_Sample==0,'CA other','US water/energy'))
props$FR_Quarter = paste(lubridate::year(props$FR_Date),lubridate::quarter(props$FR_Date),sep = '_')
props$FR_Date = ymd(props$FR_Date)

library(ggthemes)
prop_long = (props %>% dplyr::select(-contains('sents')) %>% gather(topic,prop,-Cat,-FR_Quarter,-EIS_Number,-FR_Date,-California,-CA_Sample))
prop_long = prop_long[!(prop_long$California==1& prop_long$CA_Sample==0),]
library(forcats)
prop_long$topic = as.factor(prop_long$topic)
prop_long$topic = fct_recode(prop_long$topic,'Climate' = 'prop_climate','Water resources' = 'prop_water','Drought' = 'prop_drought')
prop_long$topic = fct_rev(prop_long$topic)

library(scales)
library(ggthemes)

#show_col(grey_pal('Color Blind')(3))
cols = c('#333333','#989898','#CCCCCC')

f3= ggplot(prop_long[prop_long$FR_Date>=ymd('2012-01-01') & !prop_long$topic %in% c('prop_water_drought','prop_water_climate'),],
       aes(x = FR_Date,colour = topic,linetype = as.factor(CA_Sample))) + 
  geom_smooth(aes(y =prop * 100),se = F) + 
  scale_x_date(name = 'Federal Register date') + theme_bw() +
  scale_y_continuous(name = '% of content focus',limits=c(0,NA))  + 
  #scale_color_tableau(name = '% focus',labels= c('climate','drought','water')) + 
  theme(text = element_text(family = 'Times',size= 10),
        #legend.position = c(0.65,0.3),
        legend.box = "horizontal",
        #  legend.text = element_text(size = 12),
        legend.background = element_rect(fill = alpha('white',0.5)),
        # legend.title = element_text(size = 12),
        #axis.title.y = element_blank(),
        # axis.text = element_text(size = 12),
        axis.title.x = element_blank()) + 
  scale_color_manual(name = 'Topic',values=cols) + 
  #scale_colour_grey(name = 'Topic') + 
  ggtitle('Water, drought, and climate') + 
  scale_linetype_manual(name = 'Sample',labels = c('other US (n = 330)','California (n=113)'),values = c(3,1)) +
  #facet_wrap(~topic,ncol = 2,scales = 'free_y') + 
  #ggtitle("Climate, drought, and water foci in impact assessment") +
  geom_vline(aes(xintercept = mdy('1/1/2014')),lty = 2,col = 'grey50') + 
  guides(linetype = guide_legend(override.aes = list(col = 'black'))) 


f3b= ggplot(prop_long[prop_long$FR_Date>=ymd('2012-01-01') & prop_long$topic %in% c('Drought'),],
           aes(x = FR_Date,colour = topic,linetype = as.factor(CA_Sample))) + 
  geom_smooth(aes(y =prop * 100),se = F) + 
  scale_x_date(name = 'Federal Register date') + theme_bw() +
  scale_y_continuous(name = '% of content focus',limits=c(0,NA))  + 
  #scale_color_tableau(name = '% focus',labels= c('climate','drought','water')) + 
  theme(text = element_text(family = 'Times',size= 10),
        #legend.position = c(0.65,0.3),
        legend.box = "horizontal",
        #  legend.text = element_text(size = 12),
        legend.background = element_rect(fill = alpha('white',0.5)),
        # legend.title = element_text(size = 12),
        axis.title.y = element_blank(),
        # axis.text = element_text(size = 12),
        axis.title.x = element_blank()) + 
  scale_color_manual(name = 'Topic',values=cols[2]) + 
  ggtitle('Drought') + 
  scale_linetype_manual(name = 'Sample',labels = c('other US (n = 330)','California (n=113)'),values = c(3,1)) +
 # facet_wrap(~topic,ncol = 1,scales = 'free_y') + 
  #ggtitle("Climate, drought, and water foci in impact assessment") +
  geom_vline(aes(xintercept = mdy('1/1/2014')),lty = 2,col = 'grey50') + 
  guides(linetype = guide_legend(override.aes = list(col = 'black')))  + guides(col=FALSE,linetype=FALSE)

f3c= ggplot(prop_long[prop_long$FR_Date>=ymd('2012-01-01') & prop_long$topic %in% c('Climate'),],
            aes(x = FR_Date,colour = topic,linetype = as.factor(CA_Sample))) + 
  geom_smooth(aes(y =prop * 100),se = F) + 
  scale_x_date(name = 'Federal Register date') + theme_bw() +
  scale_y_continuous(name = '% of content focus',limits=c(0,NA))  + 
  #scale_color_tableau(name = '% focus',labels= c('climate','drought','water')) + 
  theme(text = element_text(family = 'Times',size= 10),
    #legend.position = c(0.65,0.3),
    legend.box = "horizontal",
      #  legend.text = element_text(size = 12),
        legend.background = element_rect(fill = alpha('white',0.5)),
       # legend.title = element_text(size = 12),
    axis.title.y = element_blank(),
       # axis.text = element_text(size = 12),
    axis.title.x = element_blank()) + 
  scale_color_manual(name = 'Topic',values=cols[3]) + 
  scale_linetype_manual(name = 'Sample',labels = c('other US (n = 330)','California (n=113)'),values = c(3,1)) +
  # facet_wrap(~topic,ncol = 1,scales = 'free_y') + 
  #ggtitle("Climate, drought, and water foci in impact assessment") +
  geom_vline(aes(xintercept = mdy('1/1/2014')),lty = 2,col = 'grey50') + 
 # guides(linetype = guide_legend(override.aes = list(col = 'black')))  +
 guides(col=FALSE,linetype=FALSE) + ggtitle('Climate')+
  NULL

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(f3)


library(grid)
library(gridExtra)
f3grob = grid.arrange(f3+guides(color=FALSE,linetype=FALSE),arrangeGrob(f3b,f3c,mylegend,ncol=1,nrow=3),ncol = 2,widths = c(1,1),
             top=textGrob("Climate, drought, and water foci in impact assessment",gp=gpar(fontsize=14,fontfamily = "Times")),
             bottom=textGrob("Federal Register publication date",gp=gpar(fontsize=12,fontfamily = "Times")))

ggsave(f3grob,filename = 'output/leverage_points/figure3.png',dpi=500,width=6,height=3.5,units='in')


ca_query = "https://usdmdataservices.unl.edu/api/StateStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=06&startdate=1/1/2010&enddate=1/1/2019&statisticsType=2/json"

library(jsonlite)
ca_dsci = fromJSON(ca_query)
ca_dsci = ca_dsci[ca_dsci$StatisticFormatID==1,]
ca_dsci$D0 = as.numeric(ca_dsci$D0);ca_dsci$D1 = as.numeric(ca_dsci$D1);ca_dsci$D2 = as.numeric(ca_dsci$D2)
ca_dsci$D3 = as.numeric(ca_dsci$D3);ca_dsci$D4 = as.numeric(ca_dsci$D4);
ca_dsci$ValidStart = ymd(ca_dsci$ValidStart)

figure1 =  ggplot(data = ca_dsci,aes(x = ValidStart)) +
  # geom_point(aes( y=D0),fill = 'yellow') + 
  geom_ribbon(aes(ymin=0, ymax=D0,fill = 'yellow')) + 
  geom_ribbon(aes(ymin=0, ymax=D1,fill = 'orange')) + 
  geom_ribbon(aes(ymin=0, ymax=D2,fill = 'red')) + 
  geom_ribbon(aes(ymin=0, ymax=D3,fill = 'maroon')) +
  geom_ribbon(aes(ymin=0, ymax=D4,fill = 'dark red')) + 
  theme_bw() + scale_y_continuous(name = ' % of state land area',expand = c(0,0)) + 
  scale_x_date(name = 'Weekly record',expand = c(0,0),
               date_breaks = "1 years",date_labels = '%Y') + 
  theme(legend.position = c(0.15,0.5),text = element_text(size = 12,family = 'Times'),
        axis.text = element_text(hjust = 0.75),legend.title = element_text(face = 'bold'),
        legend.background = element_rect(fill = alpha('white',0.75)))+ 
  ggtitle('California statewide drought conditions, 1/2010 to 1/2019') + 
  scale_fill_identity(labels = c('Exceptional drought','Extreme drought','Severe drought','Moderate drought','Abnormally dry'),guide = 'legend',name = 'Drought severity')

ggsave(figure1,filename = 'output/leverage_points/figure1.png',width=6,height=2.5,units='in')






ca_dsci$D0C = ca_dsci$D0 + ca_dsci$D1 + ca_dsci$D2 + ca_dsci$D3 + ca_dsci$D4
ca_dsci$D1C = ca_dsci$D1 + ca_dsci$D2 + ca_dsci$D3 + ca_dsci$D4
ca_dsci$D2C = ca_dsci$D2 + ca_dsci$D3 + ca_dsci$D4
ca_dsci$D3C = ca_dsci$D3 + ca_dsci$D4
ca_dsci$D4C = ca_dsci$D4


ca_dsci[ca_dsci$MapDate>2014&ca_dsci$MapDate<2015,]
plot(ca_dsci$MapDate,ca_dsci$D0C)

test = list.files('www.usbr.gov/',recursive = T)
list.dirs('www.usbr.gov/uc/')
list.files('www.usbr.gov/mp/nepa/')



