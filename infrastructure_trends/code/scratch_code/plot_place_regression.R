library(tidyverse)
library(stringr)
library(tigris)
library(ggthemes)
library(jsonlite)
library(censusapi)
library(pbapply)
library(tidyverse)
library(tidycensus)
library(data.table)
library(lubridate)
library(sf)
options(tigris_use_cache = TRUE)

rm(list=ls())
full_rec = read_csv('input/epa_master_repository/eis_record_detail.csv')
full_rec = full_rec[!is.na(full_rec$EIS.Number),]
full_rec = full_rec[full_rec$Document.Type=='Final',]
full_rec$Year = str_extract(full_rec$EIS.Number,'^[0-9]{4}')

class = read_csv('input/scratch/classified_eis_sentences_9topic.csv')
class[,grepl('P',colnames(class))] <- (class[,grepl('P',colnames(class))] > 0.5) + 0
class$EIS.Number = str_extract(class$file,'[0-9]{8}')

epa_df = read_csv('input/epa_master_repository/eis_record_detail.csv')
epa_df = epa_df[!grepl('(\\W|^)VOID|(\\W|^)ADOPTION',toupper(epa_df$Title),perl=T),]
#epa_df$YEAR = year(mdy(epa_df$Federal.Register.Date))
epa_df$YEAR = str_extract(epa_df$EIS.Number,'^[0-9]{4}')
epa_df$YEAR[is.na(epa_df$EIS.Number)] <- year(mdy(epa_df$`Federal.Register.Date`[is.na(epa_df$EIS.Number)]))
epa_df = epa_df[epa_df$YEAR%in%c(2012:2018),]
epa_df$TYPE_SET = NA
epa_df$Document = toupper(epa_df$Document)
epa_df$TYPE_SET[grepl('FINAL',epa_df$Document)&grepl('SUPP',epa_df$Document)] <- 'FINAL_SUPPLEMENT'
epa_df$TYPE_SET[grepl('FINAL',epa_df$Document)&!grepl('SUPP',epa_df$Document)] <- 'FINAL'
epa_df$TYPE_SET[grepl('DRAFT',epa_df$Document)&grepl('SUPP',epa_df$Document)] <- 'DRAFT_SUPPLEMENT'
epa_df$TYPE_SET[grepl('DRAFT',epa_df$Document)&!grepl('SUPP',epa_df$Document)] <- 'DRAFT'
epa_df$TYPE_SET[grepl('ADOPTION',epa_df$Title)|epa_df$Document=='ADOPTION'] <- 'ADOPTION'
epa_df = epa_df %>% filter(!is.na(TYPE_SET))
epa_df$Title = gsub('\\s{2,}',' ',epa_df$Title)
epa_df=epa_df[sapply(str_split(gsub('NB','NE',epa_df$State.or.Territory),'-'),function(x) any(x %in% state.abb)),]
epa_df$InfraType <- NA

built_indicators = c("I-[0-9]{1,3}|I[0-9]{1,3}|US(-|\\s)[0-9]{1,3}","SR(-|\\s)[0-9]{1,3}","S\\.R\\.\\s[0-9]{1,3}","Project","Completion","Facilit","Widening","Bypass","Extension","Special Use Permits","Bridge",
                     "Prison","Improvements","Expansion","Resort","Development","Extraction","Broadband","Project","Train","Station","Construction","Special Use Permit","Correctional","Parkway","Loop",'Freeway','Connector',
                     "404","Section 10","Mine","Highway","Base","Harbor","Rail","Port","Dam","Reservoir","Hydroelectric","Pipeline","Transmission","Plant","Reactor","Coal","Interstate","Crossing","Corridor","Route","Addition","Powerline",
                     "Airport","Observatory","Rocket","Collider","Experimental","Research Facility","Plutonium",'Interchange','Reconstruction','Street','Road','Expressway','Improvement',"Special-Use Permits","Communication Site","Hwy",
                     "Regulating Works","Solar","Oil and Gas",
                     "Dock","Floodway","Terminal","Ship","Channel","Dredging",
                     "Special-Use-Permits")
inst_indicators = c("Remedial","Program","Conservation","Maintenance","Activities","Protection",
                    "Deploy","Cleanup","Combat","Grazing","Operational","Standards","Efficiency","Mining","IMPLEMENTATION",
                    "Sonar","Salvage","Access","Proposal","Lease","Refuge","Laboratory","Safety","Control","Leasing","Fuels Reduction","Designation","Recovery",
                    "Selection","Reforestation","Removal",'Exchange',"Allotments","Species","Conveyance","Transfer",
                    "Vehicle Use","Trail System","Rehabilitation",'Rangeland',"Treatment","New Information","Investigation","Transfers","Water Surplus",
                    "Renewals","Water System","WaterFix","Long-Term","Reevaluation","Land Transfer",
                    "Management","Plan","Planning","Study","Assessment","Disposal","Reuse","Sale","Conservation","Restoration",
                    "Catch Limit","signatory",'Signatory','Hunt','Bycatch','Habitat','Continued')

epa_df$InfraType[epa_df$Agency == 'Federal Highway Administration'] <- 'Built' 
epa_df$InfraType[grepl(paste0(built_indicators,collapse='|'),epa_df$Title,perl=F)] <- 'Built'
epa_df$InfraType[grepl(paste(inst_indicators,collapse='|'),epa_df$Title,perl=F)] <- 'Institutional'

class$Agency = epa_df$Agency[match(class$EIS.Number,epa_df$EIS.Number)] 
class_df = class %>% gather(Topic,Value,-text,-file,-EIS.Number,-Agency) %>% group_by(Agency,Topic) %>% summarise(Topic_Value = sum(Value))

k = "b5a388cd6162590fc939335ddc45787bcc61778c"
tidycensus::census_api_key(k)
#vn = list("NAME","GEOID","B02001_001E","B02001_002E","B06011_001E","B07013_001E","B07013_002E","B16010_001E","B16010_041E","B25035_001E","B25077_001E")
vn = c("B02001_001E","B02001_002E","B06011_001E","B07013_001E","B07013_002E","B16010_001E",
       "B16010_041E","B25035_001E","B25077_001E",'B17001_002')
vn2 = c('GEOID',vn)


acs_2010 = get_acs(geography = "place", variables = vn,year = 2010,#county = yfips$FIPS[i],
                   geometry = FALSE,keep_geo_vars = F)
acs_2010$Year = 2010
acs_2011 = get_acs(geography = "place", variables = vn,year = 2011,#county = yfips$FIPS[i],
                   geometry = FALSE,keep_geo_vars = F)
acs_2011$Year = 2011

acs_dt = data.table(rbind(acs_2010,acs_2011))
acs_dt$variable = fct_recode(acs_dt$variable,"Total_Population" = "B02001_001",
                             # "Poverty_Rate" = "DP03_0119E",
                             "White_Population" = "B02001_002" ,
                             "Median_Income" = "B06011_001",
                             "Household_Pop" = "B07013_001",
                             "Household_Owner_Occupied" = "B07013_002",
                             "Pop_Over_25"= "B16010_001",
                             "Pop_Bach" = "B16010_041",
                             "Median Year Structure Built" = "B25035_001",
                             "Median_Home_Value" = "B25077_001",
                             "Income_Under_Poverty_Status" = 'B17001_002')
#unique(acs_dt$variable)
#acs_dt$estimate[acs_dt$variable=='Poverty_Rate2']

acs_spread = dcast(acs_dt,GEOID + Year ~variable,value.var = 'estimate')
#acs_spread = acs_spread[!grepl('^72',acs_spread$GEOID),]
acs_spread = acs_spread[Year==2011,] 
acs_spread$Poverty_Rate <- (acs_spread$Income_Under_Poverty_Status/acs_spread$Total_Population)

census_places = fread('spatial_input/2016_Gaz_place_national.txt')
setnames(census_places,c('GEOID'),c('PLACE_FIPS'))
census_places$PLACE_FIPS <-formatC(census_places$PLACE_FIPS,width=max(nchar(census_places$PLACE_FIPS)),flag=0)

class$YEAR = str_extract(class$EIS.Number,'[0-9]{4}')
class = left_join(class,class %>% group_by(file) %>% summarise(total_sents = n()))

#count_by_topic_and_eis = class %>% gather(Topic,Value,-text,-file,-EIS.Number,-Agency,-YEAR,-total_sents) %>% group_by(EIS.Number,Topic) %>% summarise(Total_Value = sum(Value))
#emph_by_eis_and_topic = left_join(count_by_topic_and_eis,class %>% select(EIS.Number,file,total_sents,YEAR,Agency) %>% filter(!duplicated(.))) %>% mutate(Ratio_Value = Total_Value/total_sents)
#emph_by_eis_and_topic = emph_by_eis_and_topic %>% rename(EIS_Number = EIS.Number)

###### regression model
library(tidyverse)
library(ggthemes)

place_eis = readRDS('scratch/descriptive_paper/fips_place_matches.RDS')
place_eis$Agency = full_rec$Agency[match(place_eis$EIS_Number,full_rec$EIS.Number)]
place_eis$InfraType = epa_df$InfraType[match(place_eis$EIS_Number,epa_df$EIS.Number)]
place_eis = data.table(place_eis)
place_eis = place_eis[!is.na(PLACE_FIPS),]

total <- place_eis[,.N,by=PLACE_FIPS] 
type <- dcast(place_eis[,.N,by=.(PLACE_FIPS,InfraType)] ,PLACE_FIPS~InfraType)
setkey(total,'PLACE_FIPS')
setkey(type,'PLACE_FIPS')
place_total <- type[total,]
place_total$All = replace_na(place_total$N,value=0)
place_total$Built = replace_na(place_total$Built,value=0)
place_total$Institutional = replace_na(place_total$Institutional,value=0)

state_places = list.files('spatial_input/ftp2.census.gov/geo/tiger/TIGER2016/PLACE/','shp$',full.names = T)
places_list = lapply(state_places,function(x) {st_read(x)})
TA <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0"
places_sf = do.call(rbind,places_list)
places_sf = st_transform(places_sf,TA)
places_sf <- places_sf[!grepl('CDP',places_sf$NAMELSAD),]
places_sf$PLACE_FIPS <- paste0(places_sf$STATEFP,places_sf$PLACEFP)
places_sf <- places_sf[!places_sf$STATEFP%in%c('60','69','72','78'),]
places_sf$Total_Area <- st_area(places_sf)

library(sf)
brown = read_csv('input/epa_frs_data/ACRES_12-18-18.CSV')
point_browns = lapply(seq_along(brown$LATITUDE83),function(x) st_point(c(brown$LONGITUDE83[x],brown$LATITUDE83[x])))
nad83 <- "+proj=longlat +datum=NAD83"
brownfields <- st_sfc(point_browns,crs = st_crs(nad83))
TA <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0"
brownfields <- st_transform(brownfields,TA)
brownfields =sf::st_bind_cols(brownfields,brown)
places_sf$Brownfields_Count <- sapply(st_intersects(places_sf,brownfields),length)
places_sf$STATE <- tigris::fips_codes$state[match(places_sf$STATEFP,tigris::fips_codes$state_code)]

counties_sf <- counties(class= 'sf')
counties_sf <- st_transform(counties_sf,TA)
over_counties <- st_intersects(places_sf,counties_sf)
places_sf$CFIPS_List <- sapply(over_counties,function(x) counties_sf$GEOID[x])
nada = fread('input/NADA/2011nata_national_cancerrisk_by_tract_source.csv')
nada$FIPS <- formatC(nada$FIPS,width=5,flag = '0')
places_sf$`Total Cancer Risk (per million)` <- sapply(places_sf$CFIPS_List,function(x) mean(nada$`Total Cancer Risk (per million)`[nada$FIPS %in% x],na.rm=T))
czma = st_read('spatial_input/czma/CZMP_counties_2009/CZMP_counties_2009.shp')
czma$CFIPS = paste0(czma$STATEFP,czma$COUNTYFP)
places_sf$CZMA_County <- sapply(places_sf$CFIPS_List,function(x) all(x %in% czma$CFIPS)) + 0

places_sf = left_join(places_sf,place_total)

places_sf$EIS_Mention_Count[is.na(places_sf$EIS_Mention_Count)] <- 0
places_sf$All[is.na(places_sf$All)] <- 0
places_sf$Built[is.na(places_sf$Built)] <- 0
places_sf$Institutional[is.na(places_sf$Institutional)] <- 0
setnames(acs_spread,'GEOID','PLACE_FIPS')
#setkey(places_sf,'PLACE_FIPS')
#setkey(acs_spread,'PLACE_FIPS')
places_sf = left_join(places_sf,acs_spread)
places_sf$PERC_MINORITY = 1 - places_sf$White_Population/places_sf$Total_Population
places_sf$PERC_POV = places_sf$Poverty_Rate
places_sf$POP_DENSITY = places_sf$Total_Population/places_sf$ALAND
places_sf$Ln_Total_Area = log(as.numeric(places_sf$Total_Area))
places_sf$Ln_Total_Population = log(as.numeric(places_sf$Total_Population))
places_sf$Ln_Median_Home_Value = log(as.numeric(places_sf$Median_Home_Value))
places_sf = places_sf[!is.na(places_sf$PERC_POV)&!is.na(places_sf$PERC_MINORITY)&!is.na(places_sf$Median_Home_Value),]
places_sf$Airborne_Cancer_Risk_PPM <- places_sf$`Total Cancer Risk (per million)`



mod_num_vars = c("Ln_Total_Area" , "Ln_Total_Population",'POP_DENSITY',
                 'PERC_MINORITY',"Poverty_Rate",'Ln_Median_Home_Value',
                 "Brownfields_Count","Airborne_Cancer_Risk_PPM")

place_dt <- as.data.table(places_sf[,!colnames(places_sf) %in% c('CFIPS_List')])
std_mod_vars = paste0('std_',mod_num_vars)
place_dt[,(std_mod_vars):=lapply(.SD,scale),.SDcols = mod_num_vars]
mod_bin_vars = c("CZMA_County")
bin_vars = paste0('bin_',mod_bin_vars)
place_dt[,(bin_vars):=lapply(.SD,as.factor),.SDcols = mod_bin_vars]
place_dt$lat <- as.numeric(as.vector(place_dt$INTPTLAT))
place_dt$lon <- as.numeric(as.vector(place_dt$INTPTLON))
form_base = paste(c('All','Built','Institutional'),paste(grep('^std|^bin',names(place_dt),value=T),collapse='+'),sep='~')
library(INLA)
library(nngeo)
library(spdep)

pcprior <- list(prec = list(prior = "pc.prec",param = c(3, 0.01)))
re_set = "+ f(STATE,model='iid',hyper = pcprior) + 
f(inla.group(lat),model='rw2',hyper=pcprior) +
f(inla.group(lon),model='rw2',hyper=pcprior)"
forms <- paste0(form_base,re_set)

mod_results = lapply(forms,function(x) {inla(as.formula(x),
                                             data=place_dt,family='nbinomial',verbose = F,
                                             control.predictor=list(compute=TRUE),
                                             control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE),
                                             control.fixed = list(prec.intercept=1),num.threads = 8)})

mod_results_zinfnb = lapply(forms,function(x) {inla(as.formula(x),
                                             data=place_dt,family='zeroinflatednbinomial0',verbose = F,
                                             control.predictor=list(compute=TRUE),
                                             control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE),
                                             control.fixed = list(prec.intercept=1),num.threads = 8)})

summary(mod_results[[1]])
summary(mod_results_zinfnb[[1]])
sapply(mod_results,function(x) x$waic$waic)
sapply(mod_results_zinfnb,function(x) x$waic$waic)

places_4nn <- st_nn(st_geometry(places_sf),st_geometry(places_sf),k = 4,sparse = T,maxdist = 100*1000)
test <- knearneigh(places_sf, k=4, longlat = NULL, RANN=TRUE)
summary()
+ f(inla.group(lat),model='rw2',hyper=pcprior) + f(inla.group(lon),model='rw1',hyper=pcprior)" 
#+ f(CFIPS_ID,model='besag',graph='county.adj',hyper = pcprior)"

summary(mod)

place_mods <- lapply(forms[3],function(x) 
  inla(form_base,data=place_dt,family='gaussian',verbose = T,
       control.predictor=list(compute=TRUE),
       control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE),
       control.fixed = list(prec.intercept=1),num.threads = 8))

class(place_dt)

place_dt$std_Airborne_Cancer_Risk_PPM
place_dt$std_Brownfields_Count
place_dt$std_Ln_Median_Home_Value


place_dt$EIS

summary(test)


form_base[1]

place_dt[,grep('^std|^bin|EIS_Mention_Count',names(place_dt)),with=F]


place_dt$bin_CZMA_County

inla(EIS_Mention_Count ~ bin_CZMA_County ~ std_Ln_Total_Area,data = place_dt)


place_dt[,grep('^std|^bin|EIS_Mention_Count',names(place_dt)),with=F]


place_dt[place_dt$Total_Population<100,]
summary(exp(place_dt$Ln_Total_Population))
summary(test)
summary(test2)
test$waic$waic
test2$waic$waic

mod0A <- glm(as.formula(paste0(form_base[1],"+ STATE")),data = place_dt,family = '')
mod0B <- glm.nb(as.formula(paste0(form_base[1],"+ STATE")),data = place_dt)


mod1 <- glm.nb(as.formula(paste0(form_base[2],"+ STATE")),data = place_dt)
mod2 <- glm.nb(as.formula(paste0(form_base[3],"+ STATE")),data = place_dt)
htmlreg(list(mod0,mod1,mod2),custom.model.names = c('All projects','Built','Institutional'),omit.coef = 'STATE',single.row = T)#file = 'scratch/descriptive_paper/reg_project_type_table.html',)




TA <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0"
hab_poly = st_read('spatial_input/Critical_Habitat_USGS/CRITHAB_POLY.shp')
hab_poly = st_transform(hab_poly,TA)
hab_poly = st_make_valid(hab_poly)
hab_inters = st_intersects(places_sf ,hab_poly)
hab_line = st_read('spatial_input/Critical_Habitat_USGS/CRITHAB_LINE.shp')



library(viridis)
ggplot() + geom_sf(data = places_sf[places_sf$STATEFP=='06',],aes(fill = EIS_Mention_Count,colour = EIS_Mention_Count)) + 
  scale_fill_viridis(direction = -1)



#county_count$METRO =ifelse(grepl('Nonmetro',rural_urban$Description[match(county_count$CFIPS,rural_urban$CFIPS)]),'Non-metro','Metro')


hab_inter_area = pbsapply(seq_along(hab_inters),function(x) sum(st_area(st_intersection(us_county[x,],hab_poly[hab_inters[[x]],]))),cl = 8)
us_county$Prop_CH_Poly = as.numeric(hab_inter_area / st_area(us_county))



library(stringr)
surface_water = readRDS('scratch/descriptive_paper/county_surface_water_area.RDS')
stream_length = readRDS('scratch/descriptive_paper/county_stream_length.RDS')
wetlands_prop = readRDS('scratch/descriptive_paper/county_wetlands_proportion.RDS')
urban_prop = readRDS('scratch/descriptive_paper/county_urban_proportion.RDS')

county_count$Total_Stream_Length_km = as.numeric(stream_length$Total_Stream_Length_km[match(county_count$CFIPS,stream_length$CFIPS)])
county_count$Surface_Water_Area_km2 = as.numeric(surface_water$Surface_Water_Area_km2[match(county_count$CFIPS,surface_water$CFIPS)])
county_count$Wetlands_Prop = as.numeric(wetlands_prop$Prop_Wetlands[match(county_count$CFIPS,as.character(wetlands_prop$CFIPS))])
county_count$Urban_Prop = as.numeric(urban_prop$Prop_Urban[match(county_count$CFIPS,urban_prop$CFIPS)])




ggplot() + 
  geom_sf(data = places_sf[places_sf$STATEFP=='53',],fill = 'grey50') +
  geom_sf(data = brownfields[brownfields$STATE_NAME == 'WASHINGTON',],col = 'red')



table(brownfields_in)
places_sf[which(brownfields_in==331),]

test <- st_covered_by(brownfields,places_sf)

places_sf
st_coordinates(places_sf)
st_coordinates(brownfields)


test

table(sapply(test,length))

point_browns
st_sfc(point_browns)
point_browns
class(point_browns)


point_browns

st_point(brown$LATITUDE83[x],brown$LONGITUDE83[x])

st_sfc(brown$LATITUDE83,brown$LONGITUDE83)

?st_sfc
brownfield_locs <- cbind(brown$LATITUDE83,brown$LONGITUDE83)

sf::as_Spatial(brownfield_locs)
sf::
  
  brown$State_FIPS = formatC(state.fips$fips[match(brown$STD_STATE_CODE,state.fips$abb)],width = 2,flag=0)
brown$CFIPS = ifelse(!is.na(brown$STD_COUNTY_FIPS),brown$STD_COUNTY_FIPS,brown$FIPS_CODE)
brown$CFIPS[!is.na(brown$CFIPS) & nchar(brown$CFIPS)==3] <- paste0(brown$State_FIPS[!is.na(brown$CFIPS) & nchar(brown$CFIPS)==3],brown$CFIPS[!is.na(brown$CFIPS) & nchar(brown$CFIPS)==3]) 
county_brownfields = brown %>% group_by(CFIPS) %>% summarise(brownfield_county = n())
county_count$Brownfields_Count = county_brownfields$brownfield_county[match(county_count$CFIPS,county_brownfields$CFIPS)]
county_count$Brownfields_Count = ifelse(is.na(county_count$Brownfields_Count),0,county_count$Brownfields_Count)

super = read_csv('input/epa_frs_data/SEMS_12-18-18.CSV')
super$State_FIPS = formatC(state.fips$fips[match(super$STD_STATE_CODE,state.fips$abb)],width = 2,flag=0)
super$CFIPS = ifelse(!is.na(super$STD_COUNTY_FIPS),super$STD_COUNTY_FIPS,super$FIPS_CODE)
super$CFIPS[!is.na(super$CFIPS) & nchar(super$CFIPS)==3] <- paste0(super$State_FIPS[!is.na(super$CFIPS) & nchar(super$CFIPS)==3],super$CFIPS[!is.na(super$CFIPS) & nchar(super$CFIPS)==3]) 
county_superfunds = super %>% group_by(CFIPS) %>% summarise(superfund_county = n())
county_count$Superfund_Site_Count = county_superfunds$superfund_county[match(county_count$CFIPS,county_superfunds$CFIPS)]
county_count$Superfund_Site_Count = ifelse(is.na(county_count$Superfund_Site_Count),0,county_count$Superfund_Site_Count)

cog = fread('input/cog/COG_2012_ORG13.ST05P.csv')
cog$CFIPS = formatC(cog$`GC.target-geo-id2`,width=5,flag=0)
cog$Num_Local_Govs = replace_na(cog$special_districts,value = 00) + replace_na(cog$municipal,value = 0)+replace_na(cog$town_township,value = 0)
county_count$Local_Gov_Num = cog$Num_Local_Govs[match(county_count$CFIPS,cog$CFIPS)]

nada = fread('input/NADA/2011nata_national_cancerrisk_by_tract_source.csv')
head(nada)
nada = nada[nada$TRACT==0,]
nada$FIPS = formatC(nada$FIPS,width=5,flag = 0)

county_count$Tot_Cancer_Risk_PPM = nada$`Total Cancer Risk (per million)`[match(county_count$CFIPS,nada$FIPS)]
county_count$Point_Stationary_Cancer_Risk_PPM = nada$`Point stationary (PT) Cancer Risk (per million)`[match(county_count$CFIPS,nada$FIPS)]

ch_values = readRDS('scratch/county_ch_data.RDS')

county_count$Length_CH_Line_M <- ch_values$Length_CH_Line_M[match(county_count$CFIPS,ch_values$CFIPS)]
county_count$Prop_CH_Poly <- ch_values$Prop_CH_Poly[match(county_count$CFIPS,ch_values$CFIPS)]

library(lubridate)

place_dt = data.table(place_mention_count)
place_dt$Ln_Total_Population = log(place_dt$Total_Population)
place_dt$Ln_Total_Area = log(place_dt$ALAND)
place_dt$Ln_Median_Home_Value <- log(place_dt$Median_Home_Value)
place_dt$Wetlands_Perc <- place_dt$Wetlands_Prop*100
place_dt$Urban_Perc <- place_dt$Urban_Prop*100
place_dt$Poverty_Rate<-as.numeric(place_dt$PERC_POV)
place_dt$Perc_Nonwhite <- place_dt$PERC_MINORITY*100
place_dt$Length_CH_Line_km <- place_dt$Length_CH_Line_M/1000
place_dt$Critical_Habitat = {(place_dt$Prop_CH_Poly>0 | place_dt$Length_CH_Line_km>0) + 0}


county_dt$CZMA_County = (county_dt$CFIPS %in% czma$CFIPS) + 0
county_dt$Urban_County = (county_dt$Urban_Perc > 10) + 0
county_dt$Wetlands_Prop[is.na(county_dt$Wetlands_Prop)]<-0

county_dt$Local_Gov_Num[is.na(county_dt$Local_Gov_Num)] <- 0

mod_num_vars = c("Ln_Total_Area" ,"Wetlands_Prop", "Ln_Total_Population",
                 "Local_Gov_Num" ,#"Housing_Units_Per_SQM" ,
                 'PERC_MINORITY',"Poverty_Rate",'Ln_Median_Home_Value',
                 "Brownfields_Count", 'Tot_Cancer_Risk_PPM')
mod_num_vars = c("Ln_Total_Area" ,"POP_DENSITY",
                 'PERC_MINORITY',"PERC_POV",'Ln_Median_Home_Value')
std_mod_vars = paste0('std_',mod_num_vars)
mod_bin_vars = c("Urban_County","CZMA_County", "Critical_Habitat")
bin_vars = paste0('bin_',mod_bin_vars)
place_dt[,(std_mod_vars):=lapply(.SD,scale),.SDcols = mod_num_vars]
county_dt[,(bin_vars):=lapply(.SD,as.factor),.SDcols = mod_bin_vars]
place_dt$STATE = fips_codes$state[match(place_dt$STATEFP,fips_codes$state_code)]

form_base = paste(c('EIS_Mention_Count','Built','Institutional'),paste(grep('^std|^bin',names(place_dt),value=T),collapse='+'),sep='~')



mod0 <- glm.nb(as.formula(paste0(form_base[1],"+ STATE")),data = place_dt)
mod1 <- glm.nb(as.formula(paste0(form_base[2],"+ STATE")),data = place_dt)
mod2 <- glm.nb(as.formula(paste0(form_base[3],"+ STATE")),data = place_dt)
htmlreg(list(mod0,mod1,mod2),custom.model.names = c('All projects','Built','Institutional'),omit.coef = 'STATE',single.row = T)#file = 'scratch/descriptive_paper/reg_project_type_table.html',)



plot_model(mod1,type = 'pred', terms = c("std_Poverty_Rate [-2,0,2]", "std_PERC_MINORITY [-2,0,2]", "bin_Urban_County")) + 
  theme_bw() + 
  #scale_colour_tableau(labels=c('0% urban','100% urban')) + 
  #  scale_fill_tableau(labels=c('0% urban','100% urban')) + 
  theme(legend.position = c(0.8,0.8),legend.title = element_blank(), legend.text=element_text(size = 12))






#point_obs = readRDS('scratch/descriptive_paper/fips_matches_with_place_points.RDS')
#point_obs = point_obs[point_obs$PRIM_LAT_DEC!=0,]
#point_obs = point_obs[point_obs$PRIM_LONG_DEC<0,]
#giant_merge = left_join(point_obs,emph_by_eis_and_topic)
#giant_merge = giant_merge[!is.na(giant_merge$Ratio_Value) & giant_merge$Ratio_Value>0,]
#giant_merge = giant_merge[!giant_merge$STATE_ALPHA %in% c('PR','AK','HI'),]
#giant_merge$Topic = as.factor(giant_merge$Topic)

library(forcats)
#giant_merge$Topic = fct_recode(giant_merge$Topic,'Economic resources' = 'P_Economics','Air quality' = 'P_Air','Water resources' = 'P_Water','Environmental aesthetics' = 'P_Aesthetics', 'Outdoor recreation' = 'P_Recreation',
#                               'Environmental justice' = 'P_EJ','Biological resources' = 'P_Biology','Climate change' = 'P_Climate_Change','Habitat' = 'P_Habitat')

library(maps)
state_tigris = tigris::states()
state_map_df = map_data(state_tigris[state_tigris$STUSPS %in% c(state.abb,'DC','PR'),])
state_map_df = state_map_df[state_map_df$long<0 & !state_map_df$region%in%c("Alaska",'Hawaii','Puerto Rico'),]
figure7 <- ggplot() + 
  #geom_polygon(data=state_map_df,aes(y = lat,x = long,group =group),fill = 'white') + 
  geom_point(data = giant_merge,aes(colour=Ratio_Value,y = PRIM_LAT_DEC,x = PRIM_LONG_DEC),pch=21) + 
  facet_wrap(~Topic) + scale_colour_viridis_c(option = 'A',direction = -1,breaks=c(0.1,0.2,0.3,0.4),labels=c('10%','20%','30%','40%'),name = 'Topical emphasis') + theme_map() +
  theme(legend.background = element_rect(fill = alpha('white',0.3)),strip.text = element_text(size = 14),legend.text = element_text(size =12),legend.title = element_text(size = 12))


library(gstat)
library(sp)
library(geoR);library(fields);library(maps)



#giant_merge$Agency[!giant_merge$Agency %in% top_10$Agency] <- 'Other'
#ggplot() + 
#geom_polygon(data=state_map_df,aes(y = lat,x = long,group =group),fill = 'white') + 
#  geom_point(data = giant_merge,aes(y = PRIM_LAT_DEC,x = PRIM_LONG_DEC),pch=21,alpha=0.25) + 
#  facet_wrap(~Agency) + theme_map() +
#  theme(legend.background = element_rect(fill = alpha('white',0.3)),strip.text = element_text(size = 14),legend.text = element_text(size =12),legend.title = element_text(size = 12))

top_emph = giant_merge  %>% filter(!duplicated(paste(EIS_Number,Topic))) %>% group_by(Topic) %>% top_n(10,Ratio_Value) %>% select(Ratio_Value,YEAR,Agency,EIS_Number,Topic)
top_emph$Title = epa_df$Title[match(top_emph$EIS_Number,epa_df$EIS.Number)]
top_emph$States = epa_df$State.or.Territory[match(top_emph$EIS_Number,epa_df$EIS.Number)]

rank_table = top_emph %>% arrange(Topic,-Ratio_Value) %>% mutate(Rank = rep(1:10)) %>% select(-Ratio_Value,-YEAR) %>% 
  mutate(TT = paste(Agency,States,Title,sep =' | ')) %>% select(-EIS_Number,-Title,-Agency,-States) %>%
  spread(Topic,TT)




form_inters = paste0(form_base,"+ STATE + std_Poverty_Rate*std_PERC_MINORITY*bin_Urban_County")
modA <- glm.nb(as.formula(form_inters[1]),data = county_dt)
modB <- glm.nb(as.formula(form_inters[2]),data = county_dt)
modC <- glm.nb(as.formula(form_inters[3]),data = county_dt)
htmlreg(list(modA,modB,modC),custom.model.names = c('All projects','Built','Institutional'),omit.coef = 'STATE',file = 'scratch/descriptive_paper/reg_interactions_table.html',single.row = T)


plot_model(modB,type = 'pred', terms = c("std_Poverty_Rate [-2,0,2]", "std_PERC_MINORITY [-2,0,2]", "bin_Urban_County")) + 
  theme_bw() + 
  #scale_colour_tableau(labels=c('0% urban','100% urban')) + 
  #  scale_fill_tableau(labels=c('0% urban','100% urban')) + 
  theme(legend.position = c(0.8,0.8),legend.title = element_blank(), legend.text=element_text(size = 12))

marg = margins(mod2)
cf = summary(marg)


cvars = grep('^std|^bin',names(county_dt),value=T)

colSums(is.na(county_dt[,..cvars]))
cor(county_dt[,..cvars])


cor(county_dt[,grep('^std|^bin',names(county_dt))])

margins::
  cor(county_dt$std_Brownfields_Count,county_dt$std_Tot_Cancer_Risk_PPM)


screenreg(list(mod0,mod1,mod2),omit.coef = 'STATE')


margins(mod1)






library(texreg)
screenreg(mod0)


margins_summary(mod0)
ggplot(m)


library(margins)
summary(margins(test))
margins::marginal_effects()
install.packages('margins')

pchisq(2 * (logLik(mod0) - logLik(mod1)), df = 1, lower.tail = FALSE)


?sjplot
names(county_dt)




summary(county_dt$std_Urban_Prop)

plot(mod0)
summary(mod0)

mod0c <- lme4::lmer(as.formula(paste0(form_base,'+STATEFP')),data = county_dt)
mod0d <- lme4::glmer(as.formula(paste0(form_base,'+(1|STATEFP)')),data = county_dt,
                     family = poisson(link='log'))

mod0e <- glmer.nb(as.formula(paste0(form_base,'+(1|STATEFP)')),data = county_dt)

summary(county_dt$total_county_eis_count<40)
summary(mod0)

summary(mod0)
summary(m1 <- glm.nb(as.formula(paste0(form_base,'+STATEFP')), data = county_dt))
summary(m2 <- glm.nb(as.formula(paste0(form_base,'+STATEFP + std_PERC_POV:std_Urban_Prop')), data = county_dt))
(est <- cbind(Estimate = coef(m1), confint(m1)))
summary(m2)

sjplot(m2)
summary(county_dt$Urban_Prop)



m2 <- glm(as.formula(form_base), family = "poisson", data = county_dt)
pchisq(2 * (logLik(m1) - logLik(m2)), df = 1, lower.tail = FALSE)


#library(INLA)





summary(m1)

plot_model(m2)


m2$coefficients
library(sjPlot)
table(county_dt$total_county_eis_count==0)


mod0 = lm(total_county_eis_count~PERC_MINORITY + PERC_POV + METRO + log(TOTAL_POPULATION),data = county_count)
mod1 = lm(total_county_eis_count~PERC_MINORITY*PERC_POV + METRO + log(TOTAL_POPULATION),data = county_count)
mod2 = glm(total_county_eis_count~PERC_MINORITY*PERC_POV + METRO + log(TOTAL_POPULATION),data = county_count,family = poisson(link='log'))
mod3 = glm(total_county_eis_count~PERC_MINORITY*PERC_POV*METRO + log(TOTAL_POPULATION),data = county_count,family = poisson(link='log'))
mod3b = lme4::glmer(total_county_eis_count~ PERC_MINORITY + PERC_POV + METRO + log(TOTAL_POPULATION) + (1|STATEFP),data = county_count,family = poisson(link='log'))





us_county = readOGR('../yampa/spatial_input/tl_2016_us_county/','tl_2016_us_county')
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0") 
us_county = spTransform(us_county,TA)
us_county@data$CFIPS = paste0(us_county@data$STATEFP,us_county@data$COUNTYFP)
states = tigris::states()
us_county@data$State = states@data$NAME[match(us_county@data$STATEFP,states@data$STATEFP)]


summary(m2)
save.image('scratch.RData')

Critical habitat

# local governments
Infrastructure History
EJ


plot_model(m2, type = "pred", terms = c( "std_PERC_POV","std_Urban_Prop"),ncol = 1) + theme_sjplot()


names(county_count)

summary(mod3)
library(lme4)
plot_model(mod3, type = "pred", terms = c( "PERC_POV","PERC_MINORITY",'METRO'),ncol = 1) + theme_sjplot() + 
  theme(legend.position =c(0.8,0.8),strip.text = element_text(size = 14),axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),legend.text = element_text(size = 12),legend.title = element_text(size = 14)) + 
  ggtitle('Predicted # of EISs for 2013-2018 period by county') + 
  scale_color_tableau(name = '% minority',labels = c("0%","16%",'32%')) + scale_x_continuous("% families below poverty line") +
  scale_y_continuous(name = 'Predicted marginal effect')

plot_model(mod3, type = "pred", terms = c( "PERC_MINORITY","PERC_POV",'METRO'),ncol = 1) + theme_sjplot() + 
  theme(legend.position =c(0.8,0.8),strip.text = element_text(size = 14),axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),legend.text = element_text(size = 12),legend.title = element_text(size = 14)) + 
  ggtitle('Predicted # of EISs for 2013-2018 period by county') + 
  #scale_color_tableau(name = 'Poverty level',labels = c("6%","12%",'18%')) + 
  scale_x_continuous("% non-white population") +
  scale_y_continuous(name = 'Predicted marginal effect')

library(sjPlot)
library(sjmisc)


topic_reg = giant_merge  %>% filter(!duplicated(EIS_Number)) %>% arrange(-Ratio_Value) %>% select(EIS_Number,STATE_ALPHA,Ratio_Value,COUNTY_NAME,YEAR,Topic,Agency)
topic_reg$Ratio_Value = topic_reg$Ratio_Value * 100

temp_reg_df = data.frame(EIS_Number=unique(county_eis$EIS_Number[county_eis$CFIPS %in% county_tigris@data$GEOID[county_tigris$STATE_NAME %in% c(state.name,'District of Columbia')]]),stringsAsFactors = F)
temp_reg_df$PERC_MINORITY = sapply(temp_reg_df$EIS_Number,function(x){
  cis = county_eis[county_eis$EIS_Number == x,]
  cis$PERC_MINORITY = acs_minority$PERC_MINORITY[match(cis$CFIPS,acs_minority$CFIPS)]
  weighted.mean(cis$PERC_MINORITY,cis$Mentions,na.rm=T)})
temp_reg_df$PERC_POV = sapply(temp_reg_df$EIS_Number,function(x){
  cis = county_eis[county_eis$EIS_Number == x,]
  cis$PERC_POV = acs_poverty$PERC_POV[match(cis$CFIPS,acs_poverty$CFIPS)]
  weighted.mean(cis$PERC_POV,cis$Mentions,na.rm=T)}) 
temp_reg_df$Population = sapply(temp_reg_df$EIS_Number,function(x){
  cis = county_eis[county_eis$EIS_Number == x,]
  cis$POPULATION = acs_minority$TOTAL_POPULATION[match(cis$CFIPS,acs_minority$CFIPS)]
  weighted.mean(cis$POPULATION,cis$Mentions,na.rm=T)}) 

temp_reg_df$METRO =  sapply(temp_reg_df$EIS_Number,function(x){
  cis = county_eis[county_eis$EIS_Number == x,]
  cis$Metro = ifelse(grepl('Nonmetro',rural_urban$Description[match(cis$CFIPS,rural_urban$CFIPS)]),0,1)
  weighted.mean(cis$Metro,cis$Mentions,na.rm=T)}) 

temp_reg_df = left_join(temp_reg_df,emph_by_eis_and_topic )
temp_reg_df$Ratio_Value = 100 * temp_reg_df$Ratio_Value
temp_reg_df = temp_reg_df[!is.na(temp_reg_df$Topic),]
temp_reg_df$Agency[!temp_reg_df$Agency %in% top_10$Agency] <- 'Other'
temp_reg_df$METRO = ifelse(round(temp_reg_df$METRO)=='1','Metro','Non-metro')

temp_reg_df$Topic = fct_recode(temp_reg_df$Topic,'Economic resources' = 'P_Economics','Air quality' = 'P_Air','Water resources' = 'P_Water','Environmental aesthetics' = 'P_Aesthetics', 'Outdoor recreation' = 'P_Recreation',
                               'Environmental justice' = 'P_EJ','Biological resources' = 'P_Biology','Climate change' = 'P_Climate_Change','Habitat' = 'P_Habitat')


mod_set = lapply(unique(temp_reg_df$Topic),function(x) {
  lm(Ratio_Value ~ Agency + PERC_MINORITY*PERC_POV*METRO,data = temp_reg_df[temp_reg_df$Topic==x,])
})
names(mod_set) <- unique(temp_reg_df$Topic)


plot_set = lapply(names(mod_set),function(x) {
  gg = plot_model(mod_set[[x]],type = "pred", terms = c( "PERC_POV","PERC_MINORITY"),fullrange = F) + 
    theme_sjplot() + ggtitle(x) + scale_color_tableau(name = "% non-white population",labels = c("7%","21%",'35%')) + 
    scale_fill_tableau(name = "% non-white population",labels = c("7%","21%",'35%')) +
    scale_y_continuous(limits = c(-2,12),breaks=seq(0,10,2))+
    theme(legend.position = c(0.8,0.1),axis.title = element_blank()) + 
    # guides(colour = FALSE) +
    NULL; gg })
library(gridExtra)

grid.arrange(grobs = plot_set,ncol=3,left = "Predicted emphasis level",
             bottom = '% households in poverty')




county_habitat_df = left_join(ch_props_df,hab_length_by_county)
library(tidyverse)
library(tigris)
library(sf)
library(maps)
fip_ref = tigris::fips_codes
fip_ref$CFIPS = paste0(fip_ref$state_code,fip_ref$county_code)
fip_ref$State_File = gsub('\\s','_',fip_ref$state_name)
fip_ref$dsn = paste0('spatial_input/nhd_states/NHD_H_',fip_ref$State_File,'_State_Shape/Shape/')
us_county@data$SUPS = fip_ref$state[match(us_county@data$GEOID,fip_ref$CFIPS)]



state_wetland_sets = list.files('spatial_input/nwi/IA_shapefile_wetlands/IA_shapefile_wetlands',pattern = "IA_Wetlands_[NEWS]",full.names = T)
state_wetland_sets = grep('shp$',state_wetland_sets,value=T)
state_wetland_list = lapply(state_wetland_sets,st_read)
state_wetlands_bind = do.call(rbind,state_wetland_list)

temp_counties = us_county[grep('19',us_county@data$STATEFP),]
county_sf = st_as_sf(temp_counties)
county_sf = st_transform(county_sf,st_crs(state_wetlands_bind))
temp = st_intersection(county_sf,state_wetlands_bind)
test = st_area(temp)



county_wetlands_list = pblapply(seq_along(state.abb),function(x) {
  print(state.abb[x])
  print(file.exists(grep(state.abb[x],state_wetlands,value=T)))})

grep(state.abb,state_wetlands,value=T)
sapply(state.abb,function(x) file.exists(grep(x,state_wetlands,value=T)))


temp_wetlands = st_read(grep(state.abb[x],state_wetlands,value=T))
temp_counties = us_county[grepl(state.abb[x],us_county@data$SUPS),]
county_sf = st_as_sf(temp_counties)
county_sf = st_transform(county_sf,st_crs(temp_wetlands))
tdf = data.frame(CFIPS = county_sf$CFIPS,Prop_Wetlands = pbsapply(1:nrow(county_sf),function(f) 
  as.numeric(sum(st_area(st_intersection(x = county_sf[f,],y = temp_wetlands))) / st_area(county_sf[f,])),cl = 10))})

test = st_intersection(temp_wetlands,county_sf)




state_wetland_sets = list.files('spatial_input/nwi/CA_shapefile_wetlands/CA_shapefile_wetlands/',pattern = "CA_Wetlands_[NEWS]",full.names = T)
t1 = st_read(state_wetland_sets[grepl('shp$',state_wetland_sets)][1])
t2 = st_read(state_wetland_sets[grepl('shp$',state_wetland_sets)][2])
t3 = st_read(state_wetland_sets[grepl('shp$',state_wetland_sets)][3])
t4 = st_read(state_wetland_sets[grepl('shp$',state_wetland_sets)][4])

t1u = st_union(t1)
t2u = st_union(t2)


test = st_read('spatial_input/nwi/IA_shapefile_wetlands/IA_shapefile_wetlands/IA_Wetlands_West.shp')
ggplot() + geom_sf(data = t1)
ggplot2::qplot(data = t1,geom = 'sf')

test = st_join(t1,t2)

Reduce(st_union,list(t1,t2,t3,t4))


county_sf$wetlands_cover = tt

tt
ggplot() + geom_sf(data = county_sf,aes(fill = wetlands_cover))
plot_sf(county_sf)
sapply(seq_along(county_sf),function(f) print(f))
test_inter = st_intersection(x = county_sf,y = temp_wetlands)





?st_union
ggplot() + geom_sf(data = test)
ggplot() + geom_sf(data = county_sf)
ggplot() + geom_sf(data = test_inter)
str(test_inter)
st_length()
sum(st_area(test_inter)) 
tt = st_union(test_inter)

ss = {st_area(tt)/st_area(county_sf)}
as.numeric(st_area(tt)/st_area(county_sf))
51261259/1569537096
st_area(county_sf)
st_area(county_sf)
sum(st_area(test_inter)) 
st_crs(county_sf)

length(test)
dim(county_sf)
plot_sf(county_sf[grepl('01027',county_sf$GEOID),])

plot_sf(county_sf[1:10,])
county_sf[grepl('01027',county_sf$GEOID),]
grepl('01027',county_sf$GEOID)
st_length()
ggplot() + geom_sf(data = test)
test2 = readOGR('spatial_input/nwi/AL_shapefile_wetlands/AL_shapefile_wetlands/','AL_Wetlands')

plot(test2)
fip_ref$wetlands_file[[1]]
test = readOGR('spatial_input/nhd_states/NHD_H_Vermont_State_Shape/Shape/','NHDFlowline')



table(test@data$FCode)
table(test@data$Visibility)

46000


plot(test)
table(test@data$Visibility)

sapply(seq_along(fip_ref$CFIPS),function(x) 
  list.files(paste0('spatial_input/nhd_states/NHD_H_',fip_ref$State_File[x],'State_Shape/',recursive = T),value=T))


test = unlist(sapply(gsub('\\s','_',fip_ref$state_name),function(x) grep(x,list.files('spatial_input/nhd_states/'),value=T)))
head(test)

length(test)
dim(fip_ref)


st_read('spatial_input/nhd_states/NHD_H_Alabama_State_Shape.zip')


write_lines(paste0("wget ","ftp://128.104.224.198/State-Downloads/",
                   state.abb,
                   "_shapefile_wetlands.zip"),'wetlands_queries.txt')

AZ_shapefile_wetlands.zip"




for(i in 1:nrow(fip_ref))
{
  
  state_whd_temp = readOGR('spatial_input/nhd_states/NHD_H_Alabama_State_Shape.zip')  
  
  
}




us_county@data$STATEFP



nhdp = sf::st_read('spatial_input/NHD_H_National_GDB.gdb/','NHDFlowline')

wetlands = sf::st_read('spatial_input/Wetlands-Data.kml')
plot(wetlands)

write_lines(x = paste0(
"https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/State/HighResolution/Shape/NHD_H_",
gsub('\\s','_',state.name),
"_State_Shape.zip"),'nhd_queries.txt')

st_layers('spatial_input/NHD_H_National_GDB.gdb/')

nhdp2 = readOGR('spatial_input/NHDPlusV21_National_Seamless.gpkg')
class(nhdp2)

class(nhdp$Shape)
plot(nhdp$Shape,type = 'l')

?st_read
class(test)

names(test)

length(test)
head(test[[1]]@)
names(test[[1]])



plot(test[[2]])


test = gIntersection(hab_line[1,],us_county,byid = T)


hab_line_buff = gBuffer(hab_line,width=0,byid = T)



gLength(hab_line[1,])
plot(us_county[us_county@data$STATEFP==13,])
plot(hab_line[1,],col='red',add=T)
proj4string(hab_line)

plot(test,col='blue',add=T)
gLength(test,byid = T)
hab_poly = gBuffer(hab_poly,byid = T)
combined_hab_poly = gUnaryUnion(hab_poly)
combined_hab_lines = gLineMerge(hab_line)
all_hab = gUnion(combined_hab_lines,combined_hab_poly)


gIntersect(us_county[1,],)

hab_line = spTransform(hab_line,CRS(proj4string(us_county)))
library(rgeos)

plot

sects = gIntersection(hab_poly,us_county,byid = T)


downl
library(ggpubr)

ggf = ggarrange(plot_set[[1]],plot_set[[2]],plot_set[[3]],
plot_set[[4]],plot_set[[5]],plot_set[[6]],
plot_set[[7]],plot_set[[8]],plot_set[[9]],
ncol=3, nrow=3, common.legend = TRUE, legend="bottom") 

annotate_figure(ggf,
top = text_grob("Impact area emphasis by county attributes", size = 14),
bottom = text_grob("% households under poverty line in county"),
left = text_grob("Predicted emphasis level", rot = 90),
fig.lab.face = "bold")


stp = tigris::states()
stp = stp[stp@data$STUSPS %in% state.abb & !stp@data$STUSPS %in% c('AK','HI'),]
stp <- spTransform(stp, CRS("+init=epsg:4269"))
stp_df = fortify(stp)
ggplot() + 
geom_polygon(data=stp_df,aes(y = lat,x = long,group =group),fill = 'white',col = 'black') +
geom_point(data = giant_merge[giant_merge$Topic=='Climate change',],aes(colour=Ratio_Value,y = PRIM_LAT_DEC,x = PRIM_LONG_DEC),pch=21,alpha=0.5) + 
facet_wrap(~Topic) + 
scale_colour_gradient(breaks=c(0.0,0.1,0.2,0.3,0.4),labels=c('0%','10%','20%','30%','40%'),limits = c(0,0.4),name = 'Topical emphasis',high = "#132B43", low = "#56B1F7") + 
theme_map() +
  theme(legend.background = element_rect(fill = alpha('white',0.3)),strip.text = element_text(size = 14),legend.text = element_text(size =12),legend.title = element_text(size = 12),
        legend.position=c(0.1,0.1))
?scale_colour_gradient
proj4string(stp)
proj4string()
?scale_color_gradient
