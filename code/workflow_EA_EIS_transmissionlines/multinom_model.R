
test = doe_fls[grepl('mitigated',tolower(doe_fls$text))&grepl('fonsi',tolower(doe_fls$text)),]

test$text[1]

sapply(doe_current$ID,function(x) grepl(x,fls))


fls
doe_fls


grep('mitig',tolower(doe_current$page_description),value=T)
read_html(x) %>% html_nodes('#main_content p') %>% html_text(trim=T)

doe_current$URL


doe_recs$Proj_Type = NA
doe_recs$Proj_Type[sapply(doe_recs$proj_description,function(x) any(grepl('anadromous fish|sustainable use|watershed management|wildlife|habitat|salmon|wetland mitigation|restoration',tolower(x))))] <- 'Wildlife/Habitat'
doe_recs$Proj_Type[sapply(doe_recs$proj_description,function(x) any(grepl('non-federal access|sales|purchase|supplement|as a cooperating agency|exchange contracts|operations problems|operation.*and maintenance|programmatic|financing|marketed|cost-effective|reliability|power system problem|planning|utility industry|rate schedule|load fluctuations|operate|vegetation management|herbicides|operations and management|modify existing|rebuild|control.*vegetation',tolower(x))))] <- 'Operations/Maintenace'
doe_recs$Proj_Type[sapply(doe_recs$proj_description,function(x) any(grepl('energy project|transmission project|developing|proposed.*transmission line|proposed.*facility|proposed.*plant|facility location|construct|to interconnect|approval of an interconnection|provide a connection|new.*transmission|interconnecting|build and operate|proposed.*project',tolower(x))))] <- 'New Construction/Installation'
doe_recs$Proj_Type[sapply(doe_recs$proj_description,function(x) any(grepl('install additional|reinforcing|upgrade|improvements|widening|increase|replace',tolower(x))))] <- 'Upgrade/Expansion'


doe_current

#doe_cx_reds = readRDS('input/agency_nepa/doe/cx_dt.RDS')

#doe_cx_reds$Agency = 'Other'
#doe_cx_reds$Agency[grep('BPA|Bonneville|BONNEVILLE',doe_cx_reds$temp_info)] <- 'BPA'
#doe_cx_reds$Agency[grep('WAPA|Western Area|WESTERN AREA',doe_cx_reds$temp_info)] <- 'WAPA'
#bpa_wapa_filter = readRDS('input/agency_nepa/doe/rpa_cx_dt.RDS')
#bpa_wapa_filter = bpa_wapa_filter[bpa_wapa_filter$Power_Agency %in% c("BPA","WAPA_DSW",'WAPA_RM','WAPA_SN','WAPA_UGP'),]
#doe_cx_reds = doe_cx_reds[doe_cx_reds$temp_url %in% bpa_wapa_filter$href,]
#doe_cx_reds$Agency = ifelse(doe_cx_reds$temp_url %in% bpa_wapa_filter$href[bpa_wapa_filter$Power_Agency=='BPA'],'BPA','WAPA')
#doe_cx_reds$YEAR = year(mdy(doe_cx_reds$temp_dates))
#doe_recs$Agency = 'Other'
#doe_recs$Agency[grepl('WAPA|Western Area',doe_recs$proj_description)] <- 'WAPA'
#doe_recs$Agency[grepl('BPA|Bonneville',doe_recs$proj_description)] <- 'BPA'
#doe_recs = doe_recs[doe_recs$Agency!='Other',]
#setnames(doe_cx_reds,'CX_ID','DOE_ID')
#doe_recs = doe_recs[Agency!='Other',]
#wapa_bpa = rbind(doe_recs,doe_cx_reds,fill=T)

doe = readRDS('input/agency_nepa/doe/doefiles_cfips.rds')
doe = doe[doe$NEPA_ID %in% doe_recs$DOE_ID,]
doe = doe[sapply(doe$CFIPS,length)>0,]
doe$Title = doe_recs$Title[match(doe$NEPA_ID,doe_recs$DOE_ID)]
#doe$Title[is.na(doe$Title)] <- doe_cx_reds$temp_info[match(doe$NEPA_ID[is.na(doe$Title)],doe_cx_reds$DOE_ID)]
doe$Agency = doe_recs$Agency[match(doe$NEPA_ID,doe_recs$DOE_ID)]
#doe$Agency[is.na(doe$Agency)] <- doe_cx_reds$Agency[match(doe$NEPA_ID[is.na(doe$Agency)],doe_cx_reds$DOE_ID)]
doe$YEAR = doe_recs$YEAR[match(doe$NEPA_ID,doe_recs$DOE_ID)]
#doe$YEAR[is.na(doe$YEAR)] <- year(mdy(doe_cx_reds$temp_dates[match(doe$NEPA_ID[is.na(doe$YEAR)],doe_cx_reds$DOE_ID)]))
doe$Proj_Type = doe_recs$Proj_Type[match(doe$NEPA_ID,doe_recs$DOE_ID)]
doe = data.table(doe)
setnames(doe,'NEPA_ID','ID')

table(doe$Proj_Type[doe$YEAR%in%2010:2018],doe$Agency[doe$YEAR%in%2010:2018])

blm = readRDS("input/agency_nepa/blm/blm_scrape_dataset_cfips.RDS")
table(blm$Doc.Type)/nrow(blm)
blm$Program[is.na(blm$Program)] <- as.character(blm$Program.s.[is.na(blm$Program)])
#rd1<-readRDS("blm_scrape_dataset_cfips.RDS")
#blm$CFIPS<-lapply(blm$CFIPS,function(X) unlist(X,recursive=T))
#blm<-blm[-which(sapply(blm$CFIPS,class)!="character"),] %>% tidyr::unnest(CFIPS)
blm$YEAR<-stringr::str_split(blm$NEPA..,"-",simplify=T,6)[,5]
blm<-blm[,c("NEPA..","CFIPS","YEAR","Project.Name","Doc.Type","Description"),with=F]
colnames(blm)<-c("ID","CFIPS","YEAR","Title","outcome","temp_info")
blm$Agency<-"BLM"
blm = blm[blm$outcome%in%c('EA','EIS'),]
blm$Proj_Type = NA
blm$Proj_Type[sapply(blm$temp_info,function(x) any(grepl('anadromous fish|sustainable use|watershed management|wildlife|habitat|salmon|wetland mitigation|restoration',tolower(x))))] <- 'Wildlife/Habitat'
blm$Proj_Type[sapply(blm$temp_info,function(x) any(grepl('non-federal access|sales|purchase|supplement|as a cooperating agency|exchange contracts|operations problems|operation.*and maintenance|programmatic|financing|marketed|cost-effective|reliability|power system problem|planning|utility industry|rate schedule|load fluctuations|operate|vegetation management|herbicides|operations and management|modify existing|rebuild|control.*vegetation',tolower(x))))] <- 'Operations/Maintenace'
blm$Proj_Type[sapply(blm$temp_info,function(x) any(grepl('applied for a ROW|to install|energy project|transmission project|developing|proposed.*transmission line|proposed.*facility|proposed.*plant|facility location|construct|to interconnect|approval of an interconnection|provide a connection|new.*transmission|interconnecting|build and operate|proposed.*project',tolower(x))))] <- 'New Construction/Installation'
blm$Proj_Type[sapply(blm$temp_info,function(x) any(grepl('install additional|reinforcing|upgrade|improvements|widening|increase|replace',tolower(x))))] <- 'Upgrade/Expansion'


table(is.na(blm$Proj_Type))
blm$temp_info[is.na(blm$Proj_Type)]


blm$temp_info[is.na(blm$Proj_Type)]
table(is.na(blm$Proj_Type))
doe = data.table(doe)
setnames(doe,'NEPA_ID','ID')
setnames(doe,'doc_type','Type')
setnames(blm,'outcome','Type')

all_projs = rbind(doe[,.(ID,Title,CFIPS,Agency,Type,YEAR,Category)],
blm[,.(ID,CFIPS,Title,Agency,Type,YEAR)])
all_projs$YEAR = as.numeric(all_projs$YEAR)
all_projs = all_projs[all_projs$YEAR>=2010,]

full = readRDS('input/full.rds')
full<-full %>% mutate(perc_urban=c(A57AA+.01)/c(AV0AA+.01),perc_black=c(B18AB+.01)/c(AV0AA+.01),perc_asianpi=c(B18AD+.01)/c(AV0AA+.01),perc_hispanic=c(A35AA+.01)/c(AV0AA+.01),STATE=as.character(STATE))
full<-full %>% mutate_at(vars(stringr::str_which(colnames(full),"OWN|77|IUCN")), ~replace(., is.na(.), 0))
fb1<-nnet::multinom(outcome~c(OWN_TRIB>0)+c(OWN_USFS>0)+OWN_FWS+c(OWN_BLM>0)+IUCN.mammal+IUCN.bird+FEDREG.list+perc_black+perc_hispanic+FEDREG.list+log(AV0AA)+casecounts+value+as.numeric(YEAR)+agency+STATE+log(as.numeric(ALAND))+log(as.numeric(AWATER)),data=full)
library(lxctk)
devtools::install_github('lixiangchun/lxctk')
library(lxctk)

blm_url = 'https://gis.blm.gov/EGISDownload/LayerPackages/BLM_National_Surface_Management_Agency.zip'
td = tempdir()
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(blm_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
blm_land <- st_read(fpath)
blm_land  <- st_transform(blm_land ,crs = st_crs(albersNA))
blm_land   <- st_make_valid(blm_land )
class(fb1)
summary(fb1)
class(fb1)
plot.nnet(fb1,pos.col='darkgreen',neg.col='darkblue',alpha.val=0.7,rel.rsc=15,
          circle.cex=10,cex=1.4,circle.col='brown',all.in='Sepal W.',all.out='v')
nnet::

test = left_join(all_projs,fll)

f
all_projs$CFIPS
fll$CFIPS
head(fll)
head(test)
table(duplicated(fll$ID))

yeah, table(is.na(match(all_projs$ID,fll$ID)))

sapply(seq_along(all_projs$CFIPS),function(x) all_projs$CFIPS[[x]])
fll$CFIPS


library(multinomINLA)
test = nnet::multinom(Type ~ 1 + YEAR + Agency, data = all_projs)


summary(test)
multinomINLA(Type ~ 1 + YEAR + Agency, data = all_projs)
table(all_projs$YEAR,all_projs$Agency)
multinomINLA::
multinomINLA(Type ~ 1, data = all_projs)

doe$doc_type
all_projs$ID
head(all_projs)


summary(test)
citation('multinomINLA')
head(doe)
rbind(doe,blm)

names(doe)
doe





str_extract('')
doe


head(blm)
colnames(doe)[c(1,3,5)]<-c("ID","Title","outcome")
colnames(doe)


full<-rbind(rd1,doe1[c("ID","CFIPS","YEAR","Title","outcome","temp_info","agency")])
full<-join(full,as.data.frame(shapefile),by=c("CFIPS","YEAR"),type="left")


full$temp_info %>% paste0(.,full$Title) %>% tidytext::unnest_tokens() 



library(brms)
rm(shapefile)
rm(rd1)
rm(prop1)
rm(list=ls()[-4])

saveRDS(full,"full.rds")
full<-readRDS("full.rds")

full1<-na.omit(as.data.frame(full)[c("outcome","agency","OWN_TRIB","OWN_USFS","OWN_FWS","OWN_BLM","IUCN.mammal","IUCN.bird","FEDREG.list","perc_black","perc_hispanic","AV0AA","casecounts","value","YEAR","stp","STATE","ALAND","AWATER")])
full1$outcome<-as.character(full1$outcome)
full1$YEAR<-as.numeric(full1$YEAR)
table(full$outcome)
rm(list=ls()[-c(4:5)])
fit1 <- brm(outcome~agency+c(OWN_TRIB>0)+c(OWN_USFS>0)+OWN_FWS+c(OWN_BLM>0)+IUCN.mammal+IUCN.bird+FEDREG.list+perc_black+perc_hispanic+FEDREG.list+log(AV0AA)+casecounts+value+as.numeric(YEAR)+(1|stp)+(1|STATE)+log(as.numeric(ALAND))+log(as.numeric(AWATER)),data=full1,family=categorical())
colnames(full1)


readRDS("doefiles_cfips.rds")
fit1 <- brm(OUTCOME~1,data=fullBLM,family=categorical())











blm$Description
doe_recs$DOE_ID[any(grepl('BPA|Bonneville|BONNEVILLE',doe_recs$proj_description))]

blm[993,]

any(doe$NEPA_ID %in% doe_recs$DOE_ID[any(grepl('BPA|Bonneville|BONNEVILLE',doe_recs$proj_description))])

doe$NEPA_ID
doe_recs$DOE_ID







doe_recs
test = fread('input/agency_nepa/doe/bpa_nepa_record.csv')
test = test[test$NEPA!='CATEX',]

test$Title <- gsub('\\s','',test$Title)
test$ID = str_extract(test$Title,'(EA|EIS)-[0-9]{1,}')

library(stringr)



test

test = readRDS('input/agency_nepa/doe/doe_ea_eis_doc_record.RDS')


test

dim(bpa_wapa_filter )

test
temp = doe_cx_reds[doe_cx_reds$Agency!='Other',]


doe_cx_reds[doe_cx_reds$temp_url %in% test$href[which(!test$href %in% temp$temp_url)],]


test$NEPA


table(doe$IN_SAMPLE)

library(plyr)
library(dplyr)

doe1<-left_join(readRDS("input/agency_nepa/doe/doe_ea_eis_record.RDS")[c("DOE_ID","NEPA","Title")],ddply(readRDS("input/agency_nepa/doe/doe_ea_eis_doc_record.RDS"), .(DOE_ID),summarize,temp_date=max(lubridate::mdy(Doc_Date))))
doe2<-readRDS("input/agency_nepa/doe/cx_dt.RDS")[c("temp_title","temp_dates","temp_info","CX_ID")] 
colnames(doe2)<-c("Title","temp_date","temp_info","DOE_ID")
doe2$NEPA<-"CX"
doe1<-rbind.fill(doe1,doe2)
doe1<-plyr::rename(doe1,c("DOE_ID"="NEPA_ID"))
doe1<-join(doe1,readRDS("input/agency_nepa/doe/doefiles_cfips.rds")[c("NEPA_ID","CFIPS")])
doe1$YEAR<-lubridate::year(lubridate::ymd(doe1$temp_date))
doe1<-doe1[c("NEPA_ID","CFIPS","YEAR","Title","temp_info","NEPA")]
doe1<-doe1[-which(sapply(doe1$CFIPS,class)!="character"),] %>% tidyr::unnest(CFIPS)

shapefile<-readRDS("SHAPEFILE2.rds")
shapefile<-as.data.frame(shapefile)
shapefile<-shapefile[,-which(colnames(shapefile)=="geometry")]
head(shapefile)
shapefile$CFIPS<-paste0(stringr::str_pad(shapefile$STATEFP.x,2,"left",0),stringr::str_pad(shapefile$COUNTYFP.x,3,"left",0))




#state random
scale(FEDREG.list)[,1]+
  scale(salmonST)[,1]+
  WildandScenicRiver+
  scale(IUCN.amph)[,1]+
  #endangered amphibian
  scale(IUCN.bird)[,1]+
  #endangered bird
  scale(IUCN.mammal)[,1]+
  #endangered mammal
  scale(as.numeric(YEAR))[,1]
#year random
scale(AV0AA)[,1]+
  #countypopulation
  scale(B18AA/AV0AA)[,1]+
  #white %
  scale(A35AA/AV0AA)[,1]+
  #hispanic %
  scale(CL6AA/AV0AA)[,1]+
  #poverty %
  scale(as.numeric(ALAND))[,1]+
  #county area1
  #FishWS ownership
  scale(OWN_BLM)[,1]+
  #BLM ownership
  scale(OWN_USFS)[,1]+
  scale(OWN_TRIBE)[,1]+
  #USFS ownership
  #USBR reclamation Ownership
  scale(log(dam.storage+1))[,1]+
  #USBR reclamation Ownership
  scale(log(height+1))[,1], data=fullBLM)
````