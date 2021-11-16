
library(plyr)
library(dplyr)
library(sf)
library(rpart)
library(partykit)
library(caret)
library(ggplot2)
library(ROCR)
set.seed(1984)
files1<-lapply(list("eis_ea_finaldataset50.rds","eis_ea_finaldataset250.rds","eis_ea_finaldataset500.rds","eis_ea_finaldataset1000.rds","eis_ea_finaldataset2000.rds"),readRDS)
names(files1)<-c(50,250,500,1000,2000)


shapedistance<-readRDS("shapedistance.rds")
blmdoeshape2<-files1 %>% bind_rows(.id="buffer")  %>% dplyr::filter(yearnum>2004)
blmdoeshape2<-left_join(blmdoeshape2,shapedistance)
blmdoeshape2$existing<-blmdoeshape2$sum_total
temp<-rbind(data.frame(year=2000:2008,party="R"),data.frame(year=2009:2016, party="D")) %>% rbind(data.frame(year=2017:2020, party="R"))
temp$party<-as.factor(temp$party)
blmdoeshape2$`Pres. Party`<-mapvalues(as.character(blmdoeshape2$YEAR), from=as.character(temp$year),to=as.character(temp$party))
blmdoeshape2$`Pres. Party`<-as.factor(blmdoeshape2$`Pres. Party`)

outresult3<-lapply(names(files1),function(X){
  modeldatasubst<-blmdoeshape2  %>% as.data.frame() %>% mutate(existing=as.factor(existing>=3),distance=distance/1000,newarea=as.numeric(newarea),"Population Density"=population10/newarea,"Critical Habitat"=crithabcount,"Water NRI&WSR"=surfacewater.nri) %>% plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance"))%>% dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`NPS Ownership`,`FWS Ownership`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Democrat`,`Voter Rate`,Existing,Distance,buffer,`Pres. Party`) %>% filter(buffer==X) %>% select(-buffer)
  formula1<-`Study Type`~.
  rpart(formula1,data=modeldatasubst)
})

outresult3_sensitive<-lapply(names(files1),function(X){
  modeldatasubst<-blmdoeshape2  %>% as.data.frame() %>% mutate(existing=as.factor(existing>=3),distance=distance/1000,newarea=as.numeric(newarea),"Population Density"=population10/newarea,"Critical Habitat"=crithabcount,"Water NRI&WSR"=surfacewater.nri) %>% plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance"))%>% dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`NPS Ownership`,`FWS Ownership`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Democrat`,`Voter Rate`,Existing,buffer,`Pres. Party`) %>% filter(buffer==X) %>% select(-buffer)
 
  formula1<-`Study Type`~.
  rpart(formula1,data=modeldatasubst)
})

names(outresult3_sensitive)<-paste(c(50,250,500,1000,2000),"m")

names(outresult3)<-paste(c(50,250,500,1000,2000),"m")

library(ggplot2)
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
vi_scores<-outresult3[[4]] %>% vip::vi()
vi_scores<-vi_scores %>% mutate(Type=revalue(Variable,categories))
vi_scores$Variable<-ordered(vi_scores$Variable,vi_scores$Variable)

#vi scores for main model (1km)
vi_scores %>% ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+theme(legend.position=c(.5,.7),text=element_text(size=20))

vi_scores_sensitive<-outresult3_sensitive[[4]] %>% vip::vi()
vi_scores_sensitive<-vi_scores_sensitive %>% mutate(Type=revalue(Variable,categories))
vi_scores_sensitive$Variable<-ordered(vi_scores_sensitive$Variable,vi_scores_sensitive$Variable)
  
#vi_scores_1km to no distance
rbind(vi_scores %>% mutate(model="Distance variable included"),vi_scores_sensitive %>% mutate(model="No distance variable")) %>% ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+facet_wrap(~model)


#plot vi results
temp<-lapply(1:5, function(X) outresult3[[X]] %>% prune(cp=.01) %>% vip::vi())
names(temp)<-c("  50m"," 250m"," 500m","1km","2km")
temp<-temp %>% bind_rows(.id="buffer")  %>% mutate(Type=revalue(Variable,categories),Variable=ordered(as.character(Variable),c(as.character(vi_scores$Variable),"NPS Ownership","Existing"))) 
temp %>% ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+theme(legend.position=c(.8,.2),text=element_text(size=20))+facet_wrap(~buffer)

#vi scores full no distance
temp<-lapply(1:5, function(X) outresult3_sensitive[[X]] %>% prune(cp=.01) %>% vip::vi())
names(temp)<-c("  50m"," 250m"," 500m","1km","2km")
temp<-temp %>% bind_rows(.id="buffer")  %>% mutate(Type=revalue(Variable,categories),Variable=ordered(as.character(Variable),c(as.character(vi_scores_sensitive$Variable),"Existing","State Ownership","Pres. Party","FWS Ownership"))) 
temp
temp %>% ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+theme(legend.position=c(.8,.2),text=element_text(size=20))+facet_wrap(~buffer)

#partytree
library(ggparty)
temp<-ggparty(outresult3[[4]] %>% as.party) 
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
                 shared_legend = T
  )+theme(text=element_text(size=20)) 

temp2

ggsave("treeout.png",plot=temp2,height=7,width=10,dpi=300)


library(ggparty)

#plot map of lines
ggplot(tidycensus::state_laea)+geom_sf(fill="white",colour="black",size=.1)+geom_sf(data=blmdoeshape2 %>% dplyr::filter(yearnum>2004) %>% st_simplify(),aes(fill=Doc_Type,colour=Doc_Type))+ggthemes::theme_map()+scale_fill_manual(name="Study Type", values=c("grey","black"))+scale_colour_manual(name="Study Type",values=c("grey","black"))
ggsave("linemap.png",dpi=300)


##Logit Model

modeldatasubst_logit<-blmdoeshape2  %>% as.data.frame() %>% mutate(existing=as.factor(existing>=3),distance=distance/1000,newarea=log(as.numeric(newarea+1)),"Population Density"=population10/newarea,"Critical Habitat"=crithabcount,"Water NRI&WSR"=surfacewater.nri) %>% plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance km","distance2"="Distance Sq."))%>% dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`NPS Ownership`,`FWS Ownership`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Voter Rate`,Existing,`Distance km`,buffer,`Pres. Party`) %>% filter(buffer==1000) %>% select(-buffer) %>% mutate(Existing=ifelse(is.na(Existing),FALSE,T))

formula1<-`Study Type`~.

modeldatasubst_logit2<-blmdoeshape2  %>% as.data.frame() %>% mutate(existing=as.factor(existing>=3),distance=distance/1000,newarea=log(as.numeric(newarea+1)),"Population Density"=population10/newarea,"Critical Habitat"=crithabcount,"Water NRI&WSR"=surfacewater.nri) %>% plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance km","distance2"="Distance Sq.")) %>% dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Voter Rate`,`Distance km`,buffer,`Pres. Party`) %>% filter(buffer==1000) %>% select(-buffer)

logit1<-glm(formula1, data=modeldatasubst_logit2, family=binomial(link="logit"))

ggplot(modeldatasubst_logit)+geom_density(aes(x=`Distance ln`,fill=`Study Type`,group=`Study Type`))+facet_wrap(~`Study Type`)

ggplot(modeldatasubst_logit)+geom_density(aes(x=log(`Distance km`+1),colour=`Study Type`,group=`Study Type`,group=`Study Type`,fill=`Study Type`),alpha=.2)+theme_bw()+xlab("Distribution of Distance in km, ln")+geom_vline(data=filter(modeldatasubst_logit, `Study Type`=="EIS"),aes(xintercept=median(log(`Distance km`+1),na.rm=T),colour="EIS"),lty=2)+geom_vline(data=filter(modeldatasubst_logit, `Study Type`=="EA"),aes(xintercept=median(log(`Distance km`+1),na.rm=T),colour="EA"),lty=2)+theme(panel.background = element_rect(fill="white"))+geom_vline(aes(xintercept=log(49)),lty=2)+ggthemes::scale_colour_tableau()+ggthemes::scale_fill_tableau()

ggplot(modeldatasubst_logit)+geom_density(aes(x=`Distance km`,colour=`Study Type`,group=`Study Type`,group=`Study Type`),fill="white")+theme_bw()+xlab("Distribution of Distance in km, ln")+geom_vline(data=filter(modeldatasubst_logit, `Study Type`=="EIS"),aes(xintercept=median(`Distance km`,na.rm=T),colour="EIS"),lty=2)+geom_vline(data=filter(modeldatasubst_logit, `Study Type`=="EA"),aes(xintercept=median(`Distance km`,na.rm=T),colour="EA"),lty=2)+theme(panel.background = element_rect(fill="white"))


ggplot(modeldatasubst_logit)+geom_density(aes(x=`Distance ln`,y=`USFS Ownership`,colour=`Study Type`,group=`Study Type`))+theme_bw()


testd<-ggplot(data=modeldatasubst_logit)+geom_point(aes(x=log(`Distance km`+1),y=`Voter Rate`, colour=`Study Type`))+geom_vline(aes(xintercept=log(data.frame(outresult3[[4]]$splits)$index[1]+1)))+geom_vline(aes(xintercept=log(data.frame(outresult3[[4]]$splits)$index[1]+1)))+geom_vline(aes(xintercept=log(data.frame(outresult3[[4]]$splits)$index[3]+1)))+geom_linerange(aes(x=4,y=.33,xmin=log(49),xmax=8))+geom_linerange(aes(x=4,y=.5,xmin=log(49),xmax=8))+theme_bw()+geom_point(data=filter(modeldatasubst_logit, `Voter Rate`>.33,log(`Distance km`+1)>3.9,`Voter Rate`<.5,`Population Density`>=.0000166),aes(x=log(`Distance km`+1),y=`Voter Rate`,shape="Density bin"),size=.5)+geom_point(data=filter(modeldatasubst_logit, `Voter Rate`>.33,log(`Distance km`+1)>3.9,`Voter Rate`<.5,`Population Density`>=.0000166,`Population Density`>=.0000166,`State Ownership`==T),aes(x=log(`Distance km`+1),y=`Voter Rate`,shape="State Bin"),size=2)+scale_shape_manual(name="5th and 6th splits",values=c(2,5))+xlab("distance km ln+1")+ggthemes::scale_colour_tableau()

library(ROCR)
pred <- prediction(predict(outresult3[[4]], type = "prob")[, 2], modeldatasubst_logit$`Study Type`)
pred.tree<-performance(pred, "tpr", "fpr")

pred_sensitive <- prediction(predict(outresult3_sensitive[[4]], type = "prob")[, 2], modeldatasubst_logit$`Study Type`)
pred.tree_sensitive<-performance(pred_sensitive, "tpr", "fpr")

pred.logit <- prediction(logit1$fitted.values,na.omit(modeldatasubst_logit)$`Study Type`)
pred.logit<-performance(pred.logit, "tpr", "fpr")

rbind(data.frame(model="tree","x"=pred.tree@x.values[[1]],"y"=pred.tree@y.values[[1]]),data.frame(model="logit","x"=pred.logit@x.values[[1]],"y"=pred.logit@y.values[[1]])) %>% rbind(data.frame(model="tree no distance","x"=pred.tree_sensitive@x.values[[1]],"y"=pred.tree_sensitive@y.values[[1]])) %>% ggplot()+geom_line(aes(x=x,y=y,group=model,colour=model),size=2)+theme_bw()+xlab("fpr")+ylab("tpr")+scale_colour_discrete()


stargazer::stargazer(logit1,type="html",report="vcsp",out="logit.sensitivity.html",single.row=T,star.cutoffs = c(.05, .01,.001))
