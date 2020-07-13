library(tidyverse)
library(data.table)
library(lubridate)

dft = fread('input/epa_master_repository/eis_record_detail_coded.csv')
dft = dft[dft$YEAR %in% c(1987:2019),]
dft2 = dft[,.N,by=.(Agency,InfraType)][order(N,decreasing = T)]
dft3 = dcast(dft2,Agency~InfraType) 
dft3$Built = replace_na(dft3$Built,0);dft3$Institutional = replace_na(dft3$Institutional,0)
dft3$Total = dft3$Built + dft3$Institutional

temp = dft3[order(Total,decreasing = T),][1:10,]
temp2 = dft3[order(Total,decreasing = T),][-c(1:10),][,lapply(.SD,sum),.SDcols=c('Built','Institutional')]
temp2$Agency = 'Other'
temp2$Total = temp2$Built + temp2$Institutional 
tt = rbind(temp,temp2,fill=T)
library(ggthemes)
library(forcats)
tt$Agency <- fct_inorder(tt$Agency)
tt2 = melt(tt,'Agency')
tt2 = tt2[tt2$variable!='Total',]
tt2$Agency <- fct_rev(tt2$Agency)
tt2$Agency <- fct_recode(tt2$Agency, 'NOAA' = 'National Oceanic and Atmospheric Administration',
                         'FERC' ='Federal Energy Regulatory Commission' )

figure1 = ggplot(tt2) +  geom_col(aes(x = Agency,y = value,fill = variable)) + theme_bw() + 
  scale_y_continuous(expand=c(0,0),name = '# FEISs published 1987-2019',
                     limits=c(0,1750),breaks=c(300,600,900,1200,1500)) +
  coord_flip() + scale_fill_tableau(name = 'Infrastructure Type') + 
  theme(legend.position = c(0.7,0.3),
        axis.title.y = element_blank(),text = element_text(family = 'Times'),
        legend.background = element_rect(fill = alpha('white',0.5)))+
        #axis.text = element_text(size = 12),legend.text = element_text(size = 12),
        #legend.title = element_text(size = 12)) + 
  ggtitle('# FEISs 1987-2019 by agency')
ggsave(figure1,filename = 'output/descriptive_eis/figure1.tiff',dpi = 500,width = 4,height=3,units = 'in')

dft2 = dft[,.N,by=.(Agency,Area)][order(N,decreasing = T)]
dft3 = dcast(dft2,Agency~Area) 
dft4 = melt(dft3,'Agency')
dft4$value[is.na(dft4$value)]<-0
top10 = dft4[,sum(value),by=.(Agency)][order(V1,decreasing = T)][1:10,]
dft4$Agency = ifelse(dft4$Agency%in%top10$Agency,dft4$Agency,'Other')
dft4$Agency = fct_relevel(dft4$Agency ,c(top10$Agency,'Other'))

dft4$value2 = ifelse(dft4$value>600,600,dft4$value)
dft4$value2[dft4$value2==0]<-NA
dft4 = dft4[,lapply(.SD,sum,na.rm=T),by = .(Agency,variable),.SDcols = c('value','value2')]
dft4$Agency <- fct_recode(dft4$Agency, 'NOAA' = 'National Oceanic and Atmospheric Administration',
                         'FERC' ='Federal Energy Regulatory Commission' )
dft4$value2[dft4$value2==0]<-NA
dft4$Agency = fct_rev(dft4$Agency)


dft[Area=='Military Operations'&Agency=='Bureau of Land Management']
summary(dft4$value2)
dft4$value[dft4$value==0] <- NA
figure2 = ggplot(dft4) + geom_point(aes(y = Agency,x= variable,size=value2,fill = value2),pch = 21) + theme_tufte(ticks=F) + 
  #scale_colour_gradient_tableau(palette = 'Purple',name = '# FEISs',na.value = 'white') + 
  scale_size_continuous(name = '# FEISs',breaks=c(1,10,50,100,200,400,600),labels = c('1',10,50,100,200,400,'600+')) + 
  scale_fill_gradient(low = 'white',high = 'black',name = '# FEISs',breaks=c(1,10,50,100,200,400,600),labels = c(1,10,50,100,200,400,'600+'),guide = 'legend')+
  theme(axis.text.x = element_text(angle = 45,hjust = 1),#,size = 12),
        #axis.text.y = element_text(size = 12),
        text = element_text(family = 'Times'),
        axis.title = element_blank()
        #legend.text = element_text(size = 12)
        ) + 
  ggtitle('FEISs 1987-2019 by agency and sector') 
figure2
ggsave(figure2,filename = 'output/descriptive_eis/figure2.tiff',dpi = 500,width = 6,height=4,units = 'in')



dft2 = dft[,.N,by=.(Area,YEAR)]
temp = dft[,.N,by=.(YEAR)]
temp$InfraType = 'All'
dft2 = dft[,.N,by=.(InfraType,YEAR)]
dft2 = rbind(dft2,temp)
figure3 = ggplot(dft2) + 
  geom_path(aes(x = YEAR,y = N, colour = InfraType,group = InfraType)) +
  geom_point(aes(x = YEAR,y = N, colour = InfraType,shape = InfraType),size = 2) +
  scale_color_colorblind(name = '') + theme_bw() + 
  geom_vline(xintercept = c(1988,1992,2000,2008),lty = 2,col = 'grey50') + 
  scale_x_continuous(breaks=seq(1987,2017,5)) + scale_y_continuous(breaks=seq(0,300,50),limits=c(0,300),expand=c(0,0),'# FEISs published')+
  theme(legend.position = c(0.85,0.8),text = element_text(family = 'Times'),
        legend.background = element_rect(fill = alpha('white',0.5)),
        #legend.text = element_text(size = 12),
        legend.title = element_blank()
       # axis.text = element_text(size = 12),
        #axis.title = element_text(size = 12)
       ) + 
  guides(shape = FALSE,colour = guide_legend(override.aes = list(shape = c(16,17,15)))) +
  NULL

figure3
ggsave(figure3,filename = 'output/descriptive_eis/figure3.tiff',dpi = 500,width = 6,height=4,units = 'in')



dft2 = dft[,.N,by=.(Agency,YEAR)][order(N,decreasing = T)]
dft3 = dcast(dft2,Agency~YEAR) 
dft4 = melt(dft3,'Agency')
dft4$value[is.na(dft4$value)]<-0
top10 = dft4[,sum(value),by=.(Agency)][order(V1,decreasing = T)][1:10,]
dft4$Agency = ifelse(dft4$Agency%in%top10$Agency,dft4$Agency,'Other')
dft4$Agency = fct_relevel(dft4$Agency ,c(top10$Agency,'Other'))
dft4 = dft4[,lapply(.SD,sum,na.rm=T),by = .(Agency,variable),.SDcols = c('value')]
dft4$variable <- as.numeric(as.character(dft4$variable))

dft4$value2 = ifelse(dft4$value == 0,NA,dft4$value)


my_breaks <- function(x) { if (max(x) <=20) seq(0, 20, 5) else seq(0, 80, 20) }

dft4$Agency <- fct_rev(dft4$Agency)
dft4$Agency <- fct_recode(dft4$Agency,'NOAA' = 'National Oceanic and Atmospheric Administration',
                           'FERC' = 'Federal Energy Regulatory Commission')

figure4 = ggplot(dft4) + ggtitle('FEISs by agency over time') + 
  geom_tile(aes(x = variable,y = Agency,fill = value)) + 
  scale_fill_viridis_c(direction = -1,name = '# FEISs published',option = 'E')  +
  scale_x_continuous(name = "Year",limits=c(1987,2019),expand = c(0,0)) +
  theme_tufte(ticks = T) +
  theme(text = element_text(family = 'Times'),legend.position = 'bottom',
        axis.title.y = element_blank())+
  NULL
ggsave(figure4,filename = 'output/descriptive_eis/figure4.tiff',dpi = 500,width = 6,height=4,units = 'in')


  
