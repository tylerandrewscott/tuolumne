
library(plyr)
library(dplyr)
library(sf)
library(rpart)
library(partykit)
library(caret)
set.seed(1984)
files1<-lapply(list("eis_ea_finaldataset50.rds","eis_ea_finaldataset250.rds","eis_ea_finaldataset500.rds","eis_ea_finaldataset1000.rds","eis_ea_finaldataset2000.rds"),readRDS)
names(files1)<-c(50,250,500,1000,2000)


shapedistance<-readRDS("shapedistance.rds")
shapedistance
blmdoeshape2<-files1 %>% bind_rows(.id="buffer")  %>% dplyr::filter(yearnum>2004)
blmdoeshape2<-left_join(blmdoeshape2,shapedistance)
blmdoeshape2$existing<-blmdoeshape2$sum_total
temp<-rbind(data.frame(year=2000:2008,party="R"),data.frame(year=2009:2016, party="D")) %>% rbind(data.frame(year=2017:2020, party="R"))
temp$party<-as.factor(temp$party)
blmdoeshape2$`Pres. Party`<-mapvalues(as.character(blmdoeshape2$YEAR), from=as.character(temp$year),to=as.character(temp$party))
blmdoeshape2$`Pres. Party`<-as.factor(blmdoeshape2$`Pres. Party`)

outresult3<-lapply(names(files1),function(X){
  modeldatasubst<-blmdoeshape2  %>% as.data.frame() %>% mutate(existing=as.factor(existing>=3),distance=log(distance+1),newarea=as.numeric(newarea),"Population Density"=population10/newarea,"Critical Habitat"=log(crithabcount+1),"Water NRI&WSR"=log(surfacewater.nri+1)) %>% plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance"))%>% dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`NPS Ownership`,`FWS Ownership`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Democrat`,`Voter Rate`,Existing,Distance,buffer,`Pres. Party`) %>% filter(buffer==X) %>% select(-buffer)
  
  modeldatasubst<-blmdoeshape2  %>% as.data.frame() %>% mutate(existing=as.factor(existing>=3),distance=distance/1000,newarea=as.numeric(newarea),"Population Density"=population10/newarea,"Critical Habitat"=crithabcount,"Water NRI&WSR"=surfacewater.nri) %>% plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance"))%>% dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`NPS Ownership`,`FWS Ownership`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Democrat`,`Voter Rate`,Existing,Distance,buffer,`Pres. Party`) %>% filter(buffer==X) %>% select(-buffer)
  
  #modeldatasubst<-modeldatasubst %>% mutate_if(is.numeric,scale)
#  modeldatasubst$Distance<-log(modeldatasubst$Distance+1)
  # modeldatasubst$`Voter Rate`<-log(modeldatasubst$Distance+1)
  #modeldatasubst$`Population Density`<-log(modeldatasubst$`Population Density`+1)
  formula1<-`Study Type`~.
  #formula2<-dummyVars(~`Region`,data=modeldatasubst)
  #dumbdata<-predict(formula2, newdata=modeldatasubst) %>% as.data.frame() %>% cbind(modeldatasubst)
  #dumbdata<-dumbdata %>% dplyr::select(-Region)
  #test1<-rpart(`Study Type`~.,data=recipes::recipe(formula=`Study Type`~.,data=dumbdata) %>% bake(dumbdata), method = 'class',control=rpart.control(xval=10,cp=0,minbucket=5,minsplit=10))
  #test1<-rpart(formula1,data=modeldatasubst, method = 'class',control=rpart.control(xval=10,cp=0.01,minbucket=10,minsplit=10))
  #  partykit::ctree(formula1,data=filter(modeldatasubst,is.na(`Study Type`)==F))
 # test1<-partykit::ctree(formula1,data=modeldatasubst,control=ctree_control(minbucket=5,minsplit=10,alpha=.1,intersplit=F,testtype = "Bonferroni"))
  
  test2<-rpart(formula1,data=modeldatasubst)
  
  test2
  #test1 <- prune(test1 , cp = 0.01)
  #  test1 %>% as.party() %>% plot()
  #  test1a<-test1
  # if(bin_continuous==T){
  #   modeldatasubst2<-modeldatasubst
  # resplit<-test1$splits %>% as.data.frame() %>% mutate(id=rownames(.))  %>% arrange(-improve) %>% mutate(id=gsub("\\.[0-9]+","",id)) %>%  .[match(unique(.$id),.$id),] %>% filter(index!=.5) %>% mutate(id=gsub("\\."," ",id)) 
  #   resplit<-test1$splits %>% as.data.frame() %>% mutate(id=rownames(.))  %>% arrange(-improve) %>% mutate(id=gsub("\\.[0-9]+","",id)) %>% filter(index!=.5) %>% mutate(id=gsub("\\."," ",id)) 
  #   for(K in resplit$id %>% unique()){
  #    equant<-ecdf(modeldatasubst[K][,1])
  #   temp<-cut(modeldatasubst[K][,1],c(0,filter(resplit, id==K) %>% select(index) %>% unique() %>% .[,1],max(modeldatasubst[K][,1],na.rm=T))) %>% stringr::str_extract(.,"[0-9\\.]+,") %>% gsub(",","",.) %>% equant() %>% round(2)
  #  modeldatasubst2[K][,1]<-temp %>% as.character() 
  # modeldatasubst2[K][is.na(modeldatasubst2[K][,1])==F,1]<-paste0("q", modeldatasubst2[K][is.na(modeldatasubst2[K][,1])==F,1]) 
  #modeldatasubst2[K][,1]<-as.ordered(modeldatasubst2[K][,1])
  #}
  # res1<-lapply(1:nrow(resplit), function(X) {modeldatasubst %>% select(resplit$id[X]) %>% .[,1]>resplit$index[X]})
  #names(res1)<-paste(resplit$id,">",round(resplit$index,2))
  #res1<-bind_rows(res1)
  #modeldatasubst2<-modeldatasubst2%>% select(unique(row.names(test1$splits)),`Study Type`) %>% select(-resplit$id) %>% cbind(res1) 
  #   test1<-rpart(formula1,data=modeldatasubst2, method = 'class',control=rpart.control(xval=10,minbucket=10,minsplit=10))
  #    test1<- prune(test1 , cp = 0.01)
  #  }
})
outresult3_sensitive<-lapply(names(files1),function(X){
 
  modeldatasubst<-blmdoeshape2  %>% as.data.frame() %>% mutate(existing=as.factor(existing>=3),distance=distance/1000,newarea=as.numeric(newarea),"Population Density"=population10/newarea,"Critical Habitat"=crithabcount,"Water NRI&WSR"=surfacewater.nri) %>% plyr::rename(.,c("own.NPS"="NPS Ownership","own.FWS"='FWS Ownership',"own.USFS"="USFS Ownership","own.trib"="Tribal Ownership","own.SLB"="State Ownership","newarea"="Polygon Area","yearnum"="Year","Doc_Type"="Study Type","DH"="Democrat","existing"="Existing","VoterRate"="Voter Rate","distance"="Distance"))%>% dplyr::select(`Study Type`,`Population Density`,`Critical Habitat`,`Water NRI&WSR`,`NPS Ownership`,`FWS Ownership`,`USFS Ownership`,`Tribal Ownership`,`State Ownership`,`Democrat`,`Voter Rate`,Existing,buffer,`Pres. Party`) %>% filter(buffer==X) %>% select(-buffer)
  
  #modeldatasubst<-modeldatasubst %>% mutate_if(is.numeric,scale)
  #  modeldatasubst$Distance<-log(modeldatasubst$Distance+1)
  # modeldatasubst$`Voter Rate`<-log(modeldatasubst$Distance+1)
  #modeldatasubst$`Population Density`<-log(modeldatasubst$`Population Density`+1)
  formula1<-`Study Type`~.
  #formula2<-dummyVars(~`Region`,data=modeldatasubst)
  #dumbdata<-predict(formula2, newdata=modeldatasubst) %>% as.data.frame() %>% cbind(modeldatasubst)
  #dumbdata<-dumbdata %>% dplyr::select(-Region)
  #test1<-rpart(`Study Type`~.,data=recipes::recipe(formula=`Study Type`~.,data=dumbdata) %>% bake(dumbdata), method = 'class',control=rpart.control(xval=10,cp=0,minbucket=5,minsplit=10))
  #test1<-rpart(formula1,data=modeldatasubst, method = 'class',control=rpart.control(xval=10,cp=0.01,minbucket=10,minsplit=10))
  #  partykit::ctree(formula1,data=filter(modeldatasubst,is.na(`Study Type`)==F))
  # test1<-partykit::ctree(formula1,data=modeldatasubst,control=ctree_control(minbucket=5,minsplit=10,alpha=.1,intersplit=F,testtype = "Bonferroni"))
  
  test2<-rpart(formula1,data=modeldatasubst)
  
  test2
  #test1 <- prune(test1 , cp = 0.01)
  #  test1 %>% as.party() %>% plot()
  #  test1a<-test1
  # if(bin_continuous==T){
  #   modeldatasubst2<-modeldatasubst
  # resplit<-test1$splits %>% as.data.frame() %>% mutate(id=rownames(.))  %>% arrange(-improve) %>% mutate(id=gsub("\\.[0-9]+","",id)) %>%  .[match(unique(.$id),.$id),] %>% filter(index!=.5) %>% mutate(id=gsub("\\."," ",id)) 
  #   resplit<-test1$splits %>% as.data.frame() %>% mutate(id=rownames(.))  %>% arrange(-improve) %>% mutate(id=gsub("\\.[0-9]+","",id)) %>% filter(index!=.5) %>% mutate(id=gsub("\\."," ",id)) 
  #   for(K in resplit$id %>% unique()){
  #    equant<-ecdf(modeldatasubst[K][,1])
  #   temp<-cut(modeldatasubst[K][,1],c(0,filter(resplit, id==K) %>% select(index) %>% unique() %>% .[,1],max(modeldatasubst[K][,1],na.rm=T))) %>% stringr::str_extract(.,"[0-9\\.]+,") %>% gsub(",","",.) %>% equant() %>% round(2)
  #  modeldatasubst2[K][,1]<-temp %>% as.character() 
  # modeldatasubst2[K][is.na(modeldatasubst2[K][,1])==F,1]<-paste0("q", modeldatasubst2[K][is.na(modeldatasubst2[K][,1])==F,1]) 
  #modeldatasubst2[K][,1]<-as.ordered(modeldatasubst2[K][,1])
  #}
  # res1<-lapply(1:nrow(resplit), function(X) {modeldatasubst %>% select(resplit$id[X]) %>% .[,1]>resplit$index[X]})
  #names(res1)<-paste(resplit$id,">",round(resplit$index,2))
  #res1<-bind_rows(res1)
  #modeldatasubst2<-modeldatasubst2%>% select(unique(row.names(test1$splits)),`Study Type`) %>% select(-resplit$id) %>% cbind(res1) 
  #   test1<-rpart(formula1,data=modeldatasubst2, method = 'class',control=rpart.control(xval=10,minbucket=10,minsplit=10))
  #    test1<- prune(test1 , cp = 0.01)
  #  }
})

outresult3_sensitive[[4]] %>% prune(cp=.01) %>% partykit::as.party() %>% plot()

outresult3_sensitive[[4]] %>% prune(cp=.01) %>% partykit::as.party() %>% plot()
names(outresult3_sensitive)<-paste(c(50,250,500,1000,2000),"m")



outresult3[[5]] %>% prune(cp=.01) %>% partykit::as.party() %>% plot()
names(outresult3)<-paste(c(50,250,500,1000,2000),"m")
outresult3[[3]] %>% vip::vi()
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
vi_scores %>% ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+theme(legend.position=c(.5,.7),text=element_text(size=20))

vi_scores_sensitive<-outresult3_sensitive[[4]] %>% vip::vi()
vi_scores_sensitive<-vi_scores_sensitive %>% mutate(Type=revalue(Variable,categories))
vi_scores_sensitive$Variable<-ordered(vi_scores_sensitive$Variable,vi_scores_sensitive$Variable)
vi_scores_sensitive %>% ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+theme(legend.position=c(.5,.7),text=element_text(size=20))


temp<-lapply(1:5, function(X) outresult3[[X]] %>% prune(cp=.01) %>% vip::vi())
names(temp)<-c("  50m"," 250m"," 500m","1km","2km")
temp<-temp %>% bind_rows(.id="buffer")  %>% mutate(Type=revalue(Variable,categories),Variable=ordered(as.character(Variable),c(as.character(vi_scores$Variable),"NPS Ownership","Existing"))) 
temp
temp %>% ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+theme(legend.position=c(.8,.2),text=element_text(size=20))+facet_wrap(~buffer)


outresult3_sensitive[[5]]
temp<-lapply(1:5, function(X) outresult3_sensitive[[X]] %>% prune(cp=.01) %>% vip::vi())
names(temp)<-c("  50m"," 250m"," 500m","1km","2km")
temp<-temp %>% bind_rows(.id="buffer")  %>% mutate(Type=revalue(Variable,categories),Variable=ordered(as.character(Variable),c(as.character(vi_scores_sensitive$Variable),"Existing","State Ownership","Pres. Party","FWS Ownership"))) 
temp
temp %>% ggplot()+geom_bar(aes(x=Variable,y=Importance,fill=Type),stat="identity")+coord_flip()+ggthemes::scale_fill_tableau()+theme_minimal()+theme(legend.position=c(.8,.2),text=element_text(size=20))+facet_wrap(~buffer)


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






smallmodel<-outresult3[[1]][[2]] %>% prune(cp=.01) %>% partykit::as.party() %>% ggparty()

outresult3[[5]][[2]] %>% prune(cp=.01) %>% partykit::as.party() %>% plot()

library(ggparty)

subfarme2<-data.frame("from"=smallmodel$data$breaks_label %>% stringr::str_extract(.,"[0-9.]+"),"to"=smallmodel$data$breaks_label %>% stringr::str_extract(.,"[0-9.]+") %>% as.numeric() %>% round(.,2))
smallmodel$data$breaks_label<-sapply(1:nrow(subfarme2), function(X) gsub(subfarme2$from[X],subfarme2$to[X],smallmodel$data$breaks_label[X]))


temp3<- smallmodel+
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

temp3

ggplot(tidycensus::state_laea)+geom_sf(fill="white",colour="black",size=.1)+geom_sf(data=blmdoeshape2 %>% dplyr::filter(yearnum>2004) %>% st_simplify(),aes(fill=Doc_Type,colour=Doc_Type))+ggthemes::theme_map()+scale_fill_manual(name="Study Type", values=c("grey","black"))+scale_colour_manual(name="Study Type",values=c("grey","black"))
ggsave("linemap.png",dpi=300)
blmdoeshape2$buffer
blmdoeshape2 %>% filter(buffer=='250') %>% filter(own.USFS==T) %>%  xtabs(~c(distance>50000)+Doc_Type,data=.)
blmdoeshape2$Doc_Type
blmdoeshape2 %>% filter(buffer=='1000') %>% as.data.frame() %>% xtabs(~`Pres. Party`+Doc_Type, data=.)
blmdoeshape2 %>% filter(buffer=='1000') %>% as.data.frame() %>% select(distance) %>% summary()
