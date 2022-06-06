packs = c('sf','ggparty','rpart','partykit','caret','tidyverse','ROCR','stringr','data.table','vip','ggthemes','forcats','texreg','pROC')
have = sapply(packs,require,character.only = T)
lapply(packs[!have],install.packages)
sapply(packs[!have],require,character.only = T)
set.seed(1984)
dir = 'Documents/GitHub/tuolumne/transmission_lines/'
flist = list.files(paste0(dir,'data_products/'),pattern = 'finaldataset',full.name = T)
flist = flist[as.numeric(str_extract(flist,'[0-9]{1,}')) <= 2000]
flist <- data.table(flist,as.numeric(str_extract(flist,'[0-9]{1,}')))[order(V2)]$flist
names(flist) <- paste0(str_extract(flist,'[0-9]{1,}'),'m')
files1<-lapply(flist,readRDS)
names(files1)<-names(flist)

shapedistance<-readRDS(paste0(dir,"data_products/shapedistance.rds"))
blmdoeshape2<-files1 %>% bind_rows(.id="buffer")  %>% dplyr::filter(yearnum>2004)
blmdoeshape2<-left_join(blmdoeshape2,shapedistance)
blmdoeshape2$existing<-blmdoeshape2$sum_total
temp<-rbind(data.frame(year=2000:2008,party="R"),data.frame(year=2009:2016, party="D")) %>% rbind(data.frame(year=2017:2020, party="R"))
temp$party<-as.factor(temp$party)
blmdoeshape2$`Pres. Party`<-plyr::mapvalues(as.character(blmdoeshape2$YEAR), 
                                            from=as.character(temp$year),to=as.character(temp$party))
blmdoeshape2$`Pres. Party`<-as.factor(blmdoeshape2$`Pres. Party`)

outresult3<-lapply(names(files1),function(X){
  modeldatasubst<-blmdoeshape2  %>% as.data.frame() %>% 
    mutate(existing=as.factor(existing>=3),distance=distance/1000,newarea=as.numeric(newarea),
           "Population Density"=population10/newarea,"Critical Habitat"=crithabcount,"Water NRI&WSR"=surfacewater.nri) %>% 
    plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance"))%>% 
    dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`NPS Ownership`,`FWS Ownership`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Democrat`,`Voter Rate`,Existing,Distance,buffer,`Pres. Party`) %>% 
    filter(buffer==X) %>%  select(-buffer)
  formula1<-`Study Type`~.
  rpart(formula1,data=modeldatasubst)
})
names(outresult3)<-names(files1)

outresult3_sensitive<-lapply(names(files1),function(X){
  modeldatasubst<-blmdoeshape2  %>% as.data.frame() %>% 
    mutate(existing=as.factor(existing>=3),distance=distance/1000,newarea=as.numeric(newarea),"Population Density"=population10/newarea,"Critical Habitat"=crithabcount,"Water NRI&WSR"=surfacewater.nri) %>% 
    plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance"))%>% 
    dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`NPS Ownership`,`FWS Ownership`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Democrat`,`Voter Rate`,Existing,buffer,`Pres. Party`) %>% filter(buffer==X) %>% 
    select(-buffer)
  formula1<-`Study Type`~.
  rpart(formula1,data=modeldatasubst)
})

names(outresult3_sensitive)<-names(files1)



categories<-c("Distance"="Type and Size",
              "Voter Rate"="Context",
              "Population Density"="Context",
              "Critical Habitat"='Regulatory',
              "USFS Ownership"='Regulatory',
              "State Ownership"='Regulatory',
              "Tribal Ownership"='Regulatory',
              "Water NRI&WSR"='Regulatory',
              "NPS Ownership"='Regulatory',
              "FWS Ownership"='Regulatory',
              "Democrat"='Context',
              "Pres. Party"='Context')
vi_scores<-outresult3$`1000m` %>% vip::vi()
vi_scores<-vi_scores %>% mutate(Type=plyr::revalue(Variable,categories))
vi_scores$Variable<-ordered(vi_scores$Variable,vi_scores$Variable)

#vi scores for main model (1km)
vi_scores %>% ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+theme(legend.position=c(.5,.7),text=element_text(size=20))

vi_scores_sensitive<-outresult3_sensitive$`1000m` %>% vip::vi()
vi_scores_sensitive<-vi_scores_sensitive %>% mutate(Type=plyr::revalue(Variable,categories))
vi_scores_sensitive$Variable<-ordered(vi_scores_sensitive$Variable,vi_scores_sensitive$Variable)

#vi_scores_1km to no distance
gg_compare = rbind(vi_scores %>% mutate(model="Distance variable included"),
                   vi_scores_sensitive %>% mutate(model="No distance variable")) %>% 
  ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+
  coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+facet_wrap(~model)+
  theme(legend.position = c(0.9,0.8))

ggsave(gg_compare,filename = paste0(dir,'output/figure5.png'),dpi =300)


#plot vi results
temp<-lapply(outresult3, function(X) X %>% prune(cp=.01) %>% vip::vi())
names(temp)<-names(outresult3)


temp<-temp %>% bind_rows(.id="buffer")  %>% mutate(Type=plyr::revalue(Variable,categories),Variable=ordered(as.character(Variable),c(as.character(vi_scores$Variable),"NPS Ownership","Existing"))) 

temp$buffer[temp$buffer=='1000m'] <- '1km'
temp$buffer[temp$buffer=='2000m'] <-'2km'
temp$buffer <- fct_relevel(temp$buffer,'50m','250m','500m','1km','2km')
gg_compare_buffer <- temp %>% ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+theme(legend.position=c(.85,.2),text=element_text(size=20))+facet_wrap(~buffer)

ggsave(plot=gg_compare_buffer,width=8.6,height = 6,units = 'in',
       filename = paste0(dir,'output/figure4.png'),dpi = 450)

#vi scores full no distance
temp<-lapply(outresult3_sensitive, function(X) X %>% prune(cp=.01) %>% vip::vi())
names(temp)<-names(outresult3_sensitive)
temp<-temp %>% bind_rows(.id="buffer")  %>% mutate(Type=plyr::revalue(Variable,categories),Variable=ordered(as.character(Variable),c(as.character(vi_scores_sensitive$Variable),"Existing","State Ownership","Pres. Party","FWS Ownership"))) 
temp %>% ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+theme(legend.position=c(.8,.2),text=element_text(size=20))+facet_wrap(~buffer)

#partytree

temp<-ggparty(outresult3$`1000m` %>% as.party) 
subfarme<-data.frame("from"=temp$data$breaks_label %>% stringr::str_extract(.,"[0-9.]+"),"to"=temp$data$breaks_label %>% stringr::str_extract(.,"[0-9.]+") %>% as.numeric() %>% round(.,2))
temp$data$breaks_label<-sapply(1:nrow(subfarme), function(X) gsub(subfarme$from[X],subfarme$to[X],temp$data$breaks_label[X]))

temp2<-temp+
  geom_edge() +
  geom_edge_label(mapping = aes(label = !!sym("breaks_label"))) +
  #geom_node_label(aes(label = splitvar), ids = "inner") +
  geom_node_label(aes(label = paste0(splitvar,", N = ", nodesize)),ids="inner")+
  geom_node_label(aes(label = paste0("N = ", nodesize)),ids="terminal")+
  # identical to  geom_node_splitvar() +
  geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = `Study Type`),position = position_fill()),scale_fill_manual(values=c("grey","black")),theme_minimal(),xlab(""),ylab("proportion")),
                 # draw individual legend for each plot
                 shared_legend = T)+
  theme(text=element_text(size=20)) 

ggsave(paste0(dir,"output/figure3.png"),plot=temp2,height=7,width=10,dpi=450)

#plot map of lines
lineplot <- ggplot(tidycensus::state_laea)+geom_sf(fill="white",colour="black",size=.1)+geom_sf(data=blmdoeshape2 %>% dplyr::filter(yearnum>2004) %>% st_simplify(),aes(fill=Doc_Type,colour=Doc_Type))+ggthemes::theme_map()+scale_fill_manual(name="Study Type", values=c("#FFC20A","#0C7BDC"))+scale_colour_manual(name="Study Type",values=c("#FFC20A","#0C7BDC"))
ggsave(plot = lineplot,filename = paste0(dir,"output/figure2.png"),dpi=450)
##Logit Model
modeldatasubst_logit<-blmdoeshape2  %>% 
  as.data.frame() %>% 
  mutate(existing=as.factor(existing>=3),
         distance=distance/1000,newarea=log(as.numeric(newarea+1)),"Population Density"=population10/newarea,"Critical Habitat"=crithabcount,"Water NRI&WSR"=surfacewater.nri) %>%
  plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance km")) %>%
  dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`NPS Ownership`,`FWS Ownership`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Voter Rate`,Existing,`Distance km`,buffer,`Pres. Party`) %>% 
  filter(buffer=='1000m') %>% 
  select(-buffer) 

formula1<-`Study Type`~. - Existing - `FWS Ownership` - `NPS Ownership`
fake = lm(formula1,data = modeldatasubst_logit)

linearForm <- update.formula(fake,~. - `Distance km` + log(`Distance km`+0.01))
quadForm <- update.formula(fake,~. - `Distance km` + log(`Distance km`+0.01) + I(log(`Distance km`+0.01)^2))

binLinear = glm(linearForm,data = modeldatasubst_logit,family=binomial(link = 'logit'))
binQuad = glm(quadForm,data = modeldatasubst_logit,family=binomial(link = 'logit'))


library(tidyr)
library(texreg)
tr.coef <- extract(binLinear)
tr.coef@se <- numeric()
tr.coef@pvalues <- numeric()
tr.coef@model.name <- "coef"
tr.pv <- extract(binLinear)
tr.pv@coef <- tr.pv@pvalues
tr.pv@se <- numeric()
tr.pv@pvalues <- numeric()
tr.pv@model.name <- "p-val"
tr.pv@gof <- numeric()
tr.pv@gof.decimal <- logical()
tr.pv@gof.names <- character()


tr2.coef <- extract(binQuad)
tr2.coef@se <- numeric()
tr2.coef@pvalues <- numeric()
tr2.coef@model.name <- "coef"
tr2.pv <- extract(binQuad)
tr2.pv@coef <- tr2.pv@pvalues
tr2.pv@se <- numeric()
tr2.pv@pvalues <- numeric()
tr2.pv@model.name <- "p-val"
tr2.pv@gof <- numeric()
tr2.pv@gof.decimal <- logical()
tr2.pv@gof.names <- character()
htmlreg(list(tr.coef,  tr.pv,
             tr2.coef, tr2.pv),
        custom.header = list("Model 1" = 1:2,
                             "Model 2" = 3:4),
        custom.note = "",file = paste0(dir,'output/compare_logits.html'))






#ggplot(modeldatasubst_logit)+geom_density(aes(x=`Distance ln`,y=`USFS Ownership`,colour=`Study Type`,group=`Study Type`))+theme_bw()


#testd<-ggplot(data=modeldatasubst_logit)+geom_point(aes(x=log(`Distance km`+1),y=`Voter Rate`, colour=`Study Type`))+geom_vline(aes(xintercept=log(data.frame(outresult3[[4]]$splits)$index[1]+1)))+geom_vline(aes(xintercept=log(data.frame(outresult3[[4]]$splits)$index[1]+1)))+geom_vline(aes(xintercept=log(data.frame(outresult3[[4]]$splits)$index[3]+1)))+geom_linerange(aes(x=4,y=.33,xmin=log(49),xmax=8))+geom_linerange(aes(x=4,y=.5,xmin=log(49),xmax=8))+theme_bw()+geom_point(data=filter(modeldatasubst_logit, `Voter Rate`>.33,log(`Distance km`+1)>3.9,`Voter Rate`<.5,`Population Density`>=.0000166),aes(x=log(`Distance km`+1),y=`Voter Rate`,shape="Density bin"),size=.5)+geom_point(data=filter(modeldatasubst_logit, `Voter Rate`>.33,log(`Distance km`+1)>3.9,`Voter Rate`<.5,`Population Density`>=.0000166,`Population Density`>=.0000166,`State Ownership`==T),aes(x=log(`Distance km`+1),y=`Voter Rate`,shape="State Bin"),size=2)+scale_shape_manual(name="5th and 6th splits",values=c(2,5))+xlab("distance km ln+1")+ggthemes::scale_colour_tableau()


library(ROCR)


blmdoeshape2$buffer
X = "1000m"
modeldatasubst<-blmdoeshape2  %>% as.data.frame() %>% 
  mutate(existing=as.factor(existing>=3),distance=distance/1000,newarea=as.numeric(newarea),
         "Population Density"=population10/newarea,"Critical Habitat"=crithabcount,"Water NRI&WSR"=surfacewater.nri) %>% 
  plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance"))%>% 
  dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`NPS Ownership`,`FWS Ownership`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Democrat`,`Voter Rate`,Existing,Distance,buffer,`Pres. Party`) %>% 
  filter(buffer==X) %>%  select(-buffer)
formula1<-`Study Type`~.
partmod_5split <- rpart(formula1,data=modeldatasubst,minsplit = 5)

EIS<-(modeldatasubst_logit$`Study Type`=='EIS')+0
keep <- !is.na(modeldatasubst_logit$`Distance km`)
treeROC <- roc(EIS,predict(outresult3$`1000m`, type = "prob")[, 2])


tree5splitROC <- roc(EIS,predict(partmod_5split, type = "prob")[, 2])
logitROC <- roc(EIS[keep],predict(binQuad))

lbs = paste0(c('logit (auc = ','tree (20 split) (auc = ','tree (5 split) (auc = '),
             round(c(logitROC$auc,treeROC$auc,tree5splitROC$auc),3),
             c(')'))

gg_roc = ggplot() + 
  geom_path(aes(y = treeROC$sensitivities,x = 1-treeROC$specificities,col = 'tree (20-split)')) +
  geom_path(aes(y = tree5splitROC$sensitivities,x = 1-tree5splitROC$specificities,col = 'tree (5-split)')) +
  geom_path(aes(y = logitROC$sensitivities,
                x = 1-logitROC$specificities,col = 'logit')) +
  theme_bw() + theme(legend.position = c(0.8,0.4)) +
  geom_abline(lty = 2,col = 'grey40')+
  xlab('false positive rate') + ylab('true positive rate') +
  scale_color_colorblind(name = 'model',labels = lbs) + 
  ggtitle('ROC curves for classification tree and logit model')

ggsave(gg_roc,filename = paste0(dir,'output/figurea2.png'),dpi = 300,
       height = 5.5, width = 7,units = 'in')




pl <- modeldatasubst_logit$`Study Type`[!is.na(modeldatasubst_logit$`Distance km`)]
pred.logit <- prediction(binQuad$fitted.values, pl)
pred.logit<-performance(pred.logit, "tpr", "fpr")



pred <- prediction(predict(outresult3$`1000m`, type = "prob")[, 2],EIS)
pred.tree<-performance(pred, "tpr", "fpr")
library(pROC)



pred_sensitive <- prediction(predict(outresult3_sensitive$`1000m`, type = "prob")[, 2], modeldatasubst_logit$`Study Type`)
pred.tree_sensitive<-performance(pred_sensitive, "tpr", "fpr")

pl <- modeldatasubst_logit$`Study Type`[!is.na(modeldatasubst_logit$`Distance km`)]
pred.logit <- prediction(binQuad$fitted.values, pl)
pred.logit<-performance(pred.logit, "tpr", "fpr")

rbind(data.frame(model="tree","x"=pred.tree@x.values[[1]],"y"=pred.tree@y.values[[1]]),
      data.frame(model="logit","x"=pred.logit@x.values[[1]],"y"=pred.logit@y.values[[1]])) %>% 
  #rbind(data.frame(model="tree no distance","x"=pred.tree_sensitive@x.values[[1]],"y"=pred.tree_sensitive@y.values[[1]])) %>% 
  ggplot()+geom_line(aes(x=x,y=y,group=model,colour=model),size=2)+
  theme_bw()+xlab("false positive rate")+ylab("true positive rate")+
  scale_colour_discrete() + geom_abline(a = 0,b = 1,lty = 2,col = 'grey')


stargazer::stargazer(logit1,type="html",report="vcsp",out="logit.sensitivity.html",single.row=T,star.cutoffs = c(.05, .01,.001))


X = '1000m'
md<-blmdoeshape2  %>% as.data.frame() %>% 
  mutate(existing=as.factor(existing>=3),distance=distance/1000,newarea=as.numeric(newarea),
         "Population Density"=population10/newarea,"Critical Habitat"=crithabcount,"Water NRI&WSR"=surfacewater.nri) %>% 
  plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance"))%>% 
  dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`NPS Ownership`,`FWS Ownership`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Democrat`,`Voter Rate`,Existing,Distance,buffer,`Pres. Party`) %>% 
  filter(buffer==X) %>%  select(-buffer)

library(mgcv)
md$EIS <- (md$`Study Type`=='EIS')+0

md <- data.table(md)
md[Distance < 0.5,][,.N,by=.(EIS)]

md$Cat <- case_when(md$Distance<0.5 ~ '<0.5km',
                    md$Distance>=0.5&md$Distance<50 ~ '0.5 to 50km',
                    md$Distance>=50&md$Distance<200 ~ '50km to 200km',
                    md$Distance>=200 ~ '>200km')

md<-md[!is.na(Distance),]
md$Cat <- as.factor(md$Cat)
md$Cat <- forcats::fct_relevel(md$Cat,'<0.5km','0.5 to 50km','50km to 200km','>200km')

lplot<-ggplot(data = md) + 
  geom_bar(aes(x = Cat,fill = as.factor(EIS)),position = 'dodge')+scale_fill_manual(name="Study Type", values=c("#FFC20A","#0C7BDC"),labels=c('EA','EIS')) +
  theme_bw() + theme(legend.position = c(0.8,0.8)) +
  ggtitle('count of EA and EIS sample by distance category') +
  xlab('Tranmission line corridor length')

ggsave(lplot,filename = paste0(dir,'output/figure_a1.png'),dpi = 300,
       height = 5.5, width = 7,units = 'in')


ggplot(md,aes(y = log(Distance+0.1),x = as.factor(EIS))) +
  geom_boxplot()