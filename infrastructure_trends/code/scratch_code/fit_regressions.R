library(tidyverse)
library(stringr)
library(tigris)
library(lubridate)
library(INLA)
library(data.table)

full_rec = read_csv('input/epa_master_repository/eis_record_detail_coded.csv')
full_rec = full_rec[!is.na(full_rec$EIS.Number),]
full_rec = full_rec[full_rec$Document.Type=='Final',]
full_rec$Year = str_extract(full_rec$EIS.Number,'^[0-9]{4}')

class = read_csv('input/scratch/classified_eis_sentences_9topic.csv')
class[,grepl('P',colnames(class))] <- (class[,grepl('P',colnames(class))] > 0.5) + 0
class$EIS.Number = str_extract(class$file,'[0-9]{8}')

class$Agency = full_rec$Agency[match(class$EIS.Number,full_rec$EIS.Number)] 
class_df = class %>% gather(Topic,Value,-text,-file,-EIS.Number,-Agency) %>% group_by(Agency,Topic) %>% summarise(Topic_Value = sum(Value))
library(ggthemes)
library(jsonlite)
library(censusapi)
library(pbapply)
library(tidyverse)
library(tidycensus)
library(data.table)
k = "b5a388cd6162590fc939335ddc45787bcc61778c"
tidycensus::census_api_key(k)
#vn = list("NAME","GEOID","B02001_001E","B02001_002E","B06011_001E","B07013_001E","B07013_002E","B16010_001E","B16010_041E","B25035_001E","B25077_001E")
vn = c("B02001_001E","B02001_002E","B06011_001E","B07013_001E","B07013_002E","B16010_001E",
       "B16010_041E","B25035_001E","B25077_001E","B17001_002E")
vn2 = c('GEOID',vn)

#us_place <- get_acs(geography = "place", variables = vn, geometry = FALSE,year = 2010,output = 'wide')
#get_acs(table = 'DP01',geography='place',)
#test = get_acs(geography = "place", variables = 'B02001_001E',year = 2010,state = 'WA',#county = yfips$FIPS[i],
#                          geometry = FALSE,survey = 'acs5',keep_geo_vars = F)

acs_2010 = get_acs(geography = "county", variables = vn,year = 2010,#county = yfips$FIPS[i],
                   geometry = FALSE,survey = 'acs5',keep_geo_vars = F)
acs_2010$Year = 2010

acs_2011 = get_acs(geography = "county", variables = vn,year = 2011,#county = yfips$FIPS[i],
        geometry = FALSE,survey = 'acs5',keep_geo_vars = F)
acs_2011$Year = 2011

acs_dt = data.table(rbind(acs_2010,acs_2011))
acs_dt$variable = fct_recode(acs_dt$variable,"Total_Population" = "B02001_001",
                             "Income_Under_Poverty_Status" = 'B17001_002',
                             "White_Population" = "B02001_002" ,
                             "Median_Income" = "B06011_001",
                             "Household_Pop" = "B07013_001",
                             "Household_Owner_Occupied" = "B07013_002",
                             "Pop_Over_25"= "B16010_001",
                             "Pop_Bach" = "B16010_041",
                             "Median Year Structure Built" = "B25035_001",
                             "Median_Home_Value" = "B25077_001")

acs_spread = dcast(acs_dt,GEOID + Year ~variable,value.var = 'estimate')
acs_spread = acs_spread[!grepl('^72',acs_spread$GEOID),]
acs_spread = acs_spread[Year==2011,] 


pov_query = 'https://api.census.gov/data/timeseries/poverty/saipe?get=NAME,SAEPOVALL_PT,SAEPOVALL_MOE,SAEPOVRTALL_MOE,SAEPOVRTALL_PT&for=county:*&time=2011'
county_pov = jsonlite::fromJSON(pov_query,simplifyDataFrame = T)
county_pov = data.table(county_pov)
setnames(county_pov,names(county_pov),as.character(county_pov[1,]))
county_pov = county_pov[-1,]
county_pov$CFIPS = paste0(county_pov$state,county_pov$county)
county_pov = county_pov[,.(CFIPS,SAEPOVRTALL_PT)]
setnames(county_pov,'SAEPOVRTALL_PT','Poverty_Rate')
setnames(acs_spread,'GEOID','CFIPS')
setkey(acs_spread,CFIPS)
setkey(county_pov,CFIPS)
acs_spread <- county_pov[acs_spread,]

######
library(tigris)
county_tigris = tigris::counties(class = 'sf')
county_eis = readRDS('../../../../net/tmp/tscott1/tuolumne_scratch/scratch/descriptive_paper/ngaz_matches_with_cfips.RDS')
county_eis$Agency = full_rec$Agency[match(county_eis$EIS_Number,full_rec$EIS.Number)]
county_eis$InfraType = full_rec$InfraType[match(county_eis$EIS_Number,full_rec$EIS.Number)]
county_eis <- county_eis[!duplicated(paste(county_eis$EIS_Number,county_eis$CFIPS)),]
total_count = county_eis[,.N,by = .(CFIPS)]
count_by_type = (dcast(county_eis[,.N,by = .(CFIPS,InfraType)],CFIPS~InfraType))

setkey(total_count,CFIPS)
setkey(count_by_type,CFIPS)
county_count = count_by_type[total_count,]
county_dt = data.table(county_tigris)
setnames(county_dt,'GEOID','CFIPS')
setkey(county_dt,CFIPS)
county_dt <- county_count[county_dt,]
county_dt$Institutional <- replace_na(county_dt$Institutional,0)
county_dt$Built <- replace_na(county_dt$Built,0)
county_dt$N <- replace_na(county_dt$N,0)

###### regression model
library(tidyverse)
library(ggthemes)
setkey(county_dt,CFIPS)
setkey(acs_spread,CFIPS)
county_dt <- acs_spread[county_dt,]

rural_urban = readxl::read_excel('input/acs/ruralurbancodes2013.xls') %>% dplyr::select(-County_Name) %>% dplyr::rename(CFIPS = FIPS)

density = fread('input/acs/DEC_10_SF1_GCTPH1.US05PR.csv')
density$CFIPS = formatC(density$`GCT_STUB.target-geo-id2`,width=5,flag=0)
density$Housing_Units_Per_SQM = density$SUBHD0402
density$Land_Area_SQM = density$SUBHD0303

county_dt$PERC_MINORITY = 1 - county_dt$White_Population/county_dt$Total_Population
county_dt$PERC_POV = county_dt$Poverty_Rate
county_dt$Housing_Units_Per_SQM = density$Housing_Units_Per_SQM[match(county_dt$CFIPS,density$CFIPS)]
county_dt$Land_Area_SQM = density$Land_Area_SQM[match(county_dt$CFIPS,density$CFIPS)]
#county_count$METRO =ifelse(grepl('Nonmetro',rural_urban$Description[match(county_count$CFIPS,rural_urban$CFIPS)]),'Non-metro','Metro')
county_dt = county_dt[!grepl('^72|^78|^66|^60|^69',county_dt$CFIPS),]

county_dt = county_dt[!is.na(county_dt$PERC_POV)&!is.na(county_dt$PERC_MINORITY),]
county_dt$STATEFP = str_extract(county_dt$CFIPS,'^[0-9]{2}')
library(stringr)

### add air quality data
aqi_df = do.call(rbind,lapply(list.files('input/epa_aqi_data/',full.names = T),read_csv)) %>% filter(Year %in% 2007:2012)
aqi_df = aqi_df[aqi_df$State %in% state.name,]
aqi_df$Days_Unhealthy_or_Worse = aqi_df$`Unhealthy Days` + aqi_df$`Unhealthy for Sensitive Groups Days` + aqi_df$`Very Unhealthy Days` + aqi_df$`Hazardous Days`
county_tigris$CFIPS = formatC(county_tigris$GEOID,width=5,flag=0)

county_tigris$State = fips_codes$state_name[match(county_tigris$STATEFP,fips_codes$state_code)]
aqi_df$CFIPS = county_tigris$CFIPS[match(paste(aqi_df$County,aqi_df$State),paste(county_tigris$NAME,county_tigris$State))]
nomatch = which(is.na(aqi_df$CFIPS))

county_tigris$NAMELSAD <- str_to_title(county_tigris$NAMELSAD)
aqi_df$CFIPS[nomatch] <- county_tigris$CFIPS[match(aqi_df$County[nomatch],county_tigris$NAMELSAD)]
aqi_df$CFIPS[aqi_df$County=="Baltimore (City)"] <- 24510
aqi_df$CFIPS[aqi_df$County=="Saint Louis"&aqi_df$State=='Missouri'] <- 29189
aqi_df$CFIPS[aqi_df$County=="Sainte Genevieve"&aqi_df$State=='Missouri'] <- 29186
aqi_df$CFIPS[aqi_df$County=="Saint Louis"&aqi_df$State=='Minnesota'] <- 27137
aqi_df$CFIPS[aqi_df$County=="Saint Charles"&aqi_df$State=='Missouri'] <- 29183
aqi_df$CFIPS[aqi_df$County=="Saint Clair"&aqi_df$State=='Illinois'] <- 17163
aqi_df$CFIPS[aqi_df$County=="Charles"&aqi_df$State=='Virginia'] <- 51036
aqi_df$CFIPS[aqi_df$County=="Dona Ana"&aqi_df$State=='New Mexico'] <- 35013
aqi_df$CFIPS[aqi_df$County=="La Salle"&aqi_df$State=='Illinois'] <- 17099
aqi_df$CFIPS[aqi_df$County=="Wrangell Petersburg"&aqi_df$State=='Alaska'] <- 02275

aqi_summary = aqi_df %>% group_by(CFIPS) %>% summarise(sum(Days_Unhealthy_or_Worse,na.rm = T),AQI_Days = sum(`Days with AQI`,na.rm=T) ,Good_AQI_Days = sum(`Good Days`,na.rm=T)) %>%
  mutate(Prop_Good_AQI_Days_07_12 = Good_AQI_Days/AQI_Days)
county_dt$Prop_Good_AQI_Days_07_12 <- aqi_summary$Prop_Good_AQI_Days_07_12[match(county_dt$CFIPS,aqi_summary$CFIPS)]

scratch_loc = '../../../../net/tmp/tscott1/tuolumne_scratch/'
surface_water = readRDS(paste0(scratch_loc,'scratch/descriptive_paper/county_surface_water_area.RDS'))
stream_length = readRDS(paste0(scratch_loc,'scratch/descriptive_paper/county_stream_length.RDS'))
wetlands_prop = readRDS(paste0(scratch_loc,'scratch/descriptive_paper/county_wetlands_proportion.RDS'))
urban_prop = readRDS(paste0(scratch_loc,'scratch/descriptive_paper/county_urban_proportion.RDS'))

county_dt$Total_Stream_Length_km = as.numeric(stream_length$Total_Stream_Length_km[match(county_dt$CFIPS,stream_length$CFIPS)])
county_dt$Surface_Water_Area_km2 = as.numeric(surface_water$Surface_Water_Area_km2[match(county_dt$CFIPS,surface_water$CFIPS)])
county_dt$Wetlands_Prop = as.numeric(wetlands_prop$Prop_Wetlands[match(county_dt$CFIPS,as.character(wetlands_prop$CFIPS))])
county_dt$Urban_Prop = as.numeric(urban_prop$Prop_Urban[match(county_dt$CFIPS,urban_prop$CFIPS)])
brown = read_csv('input/epa_frs_data/ACRES_12-18-18.CSV')
brown$State_FIPS = formatC(maps::state.fips$fips[match(brown$STD_STATE_CODE,maps::state.fips$abb)],width = 2,flag=0)
brown$CFIPS = ifelse(!is.na(brown$STD_COUNTY_FIPS),brown$STD_COUNTY_FIPS,brown$FIPS_CODE)
brown$CFIPS[!is.na(brown$CFIPS) & nchar(brown$CFIPS)==3] <- paste0(brown$State_FIPS[!is.na(brown$CFIPS) & nchar(brown$CFIPS)==3],brown$CFIPS[!is.na(brown$CFIPS) & nchar(brown$CFIPS)==3]) 
county_brownfields = brown %>% dplyr::group_by(CFIPS) %>% dplyr::summarise(brownfield_county = n())
county_dt$Brownfields_Count = county_brownfields$brownfield_county[match(county_dt$CFIPS,county_brownfields$CFIPS)]
county_dt$Brownfields_Count = ifelse(is.na(county_dt$Brownfields_Count),0,county_dt$Brownfields_Count)


super = read_csv('input/epa_frs_data/SEMS_12-18-18.CSV')
super$State_FIPS = formatC(maps::state.fips$fips[match(super$STD_STATE_CODE,maps::state.fips$abb)],width = 2,flag=0)
super$CFIPS = ifelse(!is.na(super$STD_COUNTY_FIPS),super$STD_COUNTY_FIPS,super$FIPS_CODE)
super$CFIPS[!is.na(super$CFIPS) & nchar(super$CFIPS)==3] <- paste0(super$State_FIPS[!is.na(super$CFIPS) & nchar(super$CFIPS)==3],super$CFIPS[!is.na(super$CFIPS) & nchar(super$CFIPS)==3]) 
county_superfunds = super %>% group_by(CFIPS) %>% dplyr::summarise(superfund_county = dplyr::n())
county_dt$Superfund_Site_Count = county_superfunds$superfund_county[match(county_dt$CFIPS,county_superfunds$CFIPS)]
county_dt$Superfund_Site_Count = ifelse(is.na(county_dt$Superfund_Site_Count),0,county_dt$Superfund_Site_Count)

cog = fread('input/cog/COG_2012_ORG13.ST05P.csv')
cog$CFIPS = formatC(cog$`GC.target-geo-id2`,width=5,flag=0)
cog$Num_Local_Govs = replace_na(cog$special_districts,value = 00) + replace_na(cog$municipal,value = 0)+replace_na(cog$town_township,value = 0)
county_dt$Local_Gov_Num = cog$Num_Local_Govs[match(county_dt$CFIPS,cog$CFIPS)]

nada = fread('input/NADA/2011nata_national_cancerrisk_by_tract_source.csv')
nada = nada[nada$TRACT==0,]
nada$FIPS = formatC(nada$FIPS,width=5,flag = 0)

county_dt$Tot_Cancer_Risk_PPM = nada$`Total Cancer Risk (per million)`[match(county_dt$CFIPS,nada$FIPS)]
county_dt$Point_Stationary_Cancer_Risk_PPM = nada$`Point stationary (PT) Cancer Risk (per million)`[match(county_dt$CFIPS,nada$FIPS)]

ch_values = readRDS(paste0(scratch_loc,'scratch/county_ch_data.RDS'))
county_dt$Length_CH_Line_M <- ch_values$Length_CH_Line_M[match(county_dt$CFIPS,ch_values$CFIPS)]
county_dt$Prop_CH_Poly <- ch_values$Prop_CH_Poly[match(county_dt$CFIPS,ch_values$CFIPS)]

library(lubridate)
county_dt$Ln_Total_Population = log(county_dt$Total_Population)
county_dt$Ln_Total_Area = log(county_dt$Land_Area_SQM)
county_dt$Ln_Median_Home_Value <- log(county_dt$Median_Home_Value)
county_dt$Wetlands_Perc <- county_dt$Wetlands_Prop*100
county_dt$Urban_Perc <- county_dt$Urban_Prop*100
county_dt$Poverty_Rate<-as.numeric(county_dt$PERC_POV)
county_dt$Perc_Nonwhite <- county_dt$PERC_MINORITY*100
county_dt$Length_CH_Line_km <- county_dt$Length_CH_Line_M/1000
county_dt$Critical_Habitat = {(county_dt$Prop_CH_Poly>0 | county_dt$Length_CH_Line_km>0) + 0}
library(sf)

dloc = 'https://pubs.usgs.gov/of/2013/1284/data/CZMP_counties_2009.zip'
temp <- tempfile()
download.file(dloc,temp)
cat = unzip(temp)
fname <- (grep('shp',cat,value=T))
czma <- st_read(fname)

czma$CFIPS = paste0(czma$STATEFP,czma$COUNTYFP)
county_dt$CZMA_County = (county_dt$CFIPS %in% czma$CFIPS) + 0
county_dt$Urban_County = (county_dt$Urban_Perc > 10) + 0
county_dt$Wetlands_Prop[is.na(county_dt$Wetlands_Prop)]<-0

county_dt$Local_Gov_Num[is.na(county_dt$Local_Gov_Num)] <- 0

mod_num_vars = c("Ln_Total_Area" ,"Wetlands_Prop", "Ln_Total_Population",
             "Local_Gov_Num" ,#"Housing_Units_Per_SQM" ,
            'PERC_MINORITY',"Poverty_Rate",'Ln_Median_Home_Value',
             "Brownfields_Count", 'Tot_Cancer_Risk_PPM')

std_mod_vars = paste0('std_',mod_num_vars)
mod_bin_vars = c("Urban_County","CZMA_County", "Critical_Habitat")
bin_vars = paste0('bin_',mod_bin_vars)
county_dt[,(std_mod_vars):=lapply(.SD,scale),.SDcols = mod_num_vars]
county_dt[,(bin_vars):=lapply(.SD,as.factor),.SDcols = mod_bin_vars]
county_dt$STATE = fips_codes$state[match(county_dt$STATEFP,fips_codes$state_code)]

form_base = paste(c('N','Built','Institutional'),paste(c(mod_bin_vars,mod_num_vars),collapse='+'),sep='~')
library(lme4)
library(INLA)
#Create adjacency matrix
require(spdep)
county_fips_set <- data.table(CFIPS = county_tigris$CFIPS,CFIPS_ID = seq_along(county_tigris$CFIPS))
county.nb <- poly2nb(county_tigris,row.names = county_tigris$CFIPS,queen=T)
#Convert the adjacency matrix into a file in the INLA format
nb2INLA("county.adj", county.nb)
county_dt$CFIPS_ID <- county_fips_set$CFIPS_ID[match(county_dt$CFIPS,county_fips_set$CFIPS)]
pcprior <- list(prec = list(prior = "pc.prec",param = c(3, 0.01)))
re_set = "+ f(STATE,model='iid',hyper = pcprior) + f(CFIPS_ID,model='besag',graph='county.adj',hyper = pcprior)"
form0 <- as.formula(paste0(form_base[1],re_set))
family_list = c('poisson','zeroinflatedpoisson0','zeroinflatedpoisson1','nbinomial','zeroinflatednbinomial0','zeroinflatednbinomial1')


mod_list <- lapply(family_list,function(x) {
  print(x)
tryCatch({inla(form0,data=county_dt,family=x,verbose = F,
           control.predictor=list(compute=TRUE),
           control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE),
           control.fixed = list(prec.intercept=1),num.threads = 4)},error = function(e) NULL)})

saveRDS(object = mod_list,file = paste0(scratch_loc,'../../../../net/tmp/tscott1/tuolumne_scratch/scratch/descriptive_paper/mods_functional_form.RDS'))

cbind(sapply(mod_list,function(x) {x$waic$waic}),
      sapply(mod_list,function(x) {x$dic$dic}))

overdisp_post <- inla.tmarginal(fun = function(x) 1/x, marg = mod_list[[4]]$marginals.hyperpar$`size for the nbinomial observations (1/overdispersion)`)
round(inla.qmarginal(c(0.025,0.975),overdisp_post),4)

family_set = family_list[(sapply(mod_list,function(x) {x$waic$waic}))>-Inf]
mod_subset = mod_list[(sapply(mod_list,function(x) {x$waic$waic}))>-Inf]
best_reference = family_set[which.min(sapply(mod_subset,function(x) {x$waic$waic}))]

best_reference <- 'poisson'

county_dt$PERC_MINORITY = county_dt$PERC_MINORITY*100
county_dt$Wetlands_Prop  = county_dt$Wetlands_Prop * 100
                                
dv_forms = sapply(form_base,function(f) as.formula(paste0(f,re_set)))
mod_dv_list <- lapply(dv_forms,function(x) {
  print(x)
  inla(x,data=county_dt,family=best_reference,verbose = F,
       control.predictor=list(compute=TRUE),
       control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE),
       control.fixed = list(prec.intercept=1),num.threads = 8)})

saveRDS(object = mod_dv_list,file = paste0(scratch_loc,'scratch/descriptive_paper/mods_baseline.RDS'))


interaction = '+ std_Poverty_Rate*std_PERC_MINORITY'
interaction_forms = sapply(form_base,function(f) as.formula(paste0(f,re_set,interaction)))
mod_interaction_list <- lapply(interaction_forms,function(x) {
  #print(x)
  inla(x,data=county_dt,family=best_reference,verbose = F,
       control.predictor=list(compute=TRUE),
       control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE),
       control.fixed = list(prec.intercept=1),num.threads = 4)})

sapply(mod_dv_list,function(x) x$waic$waic)
sapply(mod_interaction_list,function(x) x$waic$waic)

summary(mod_dv_list[[1]])
summary(mod_interaction_list[[1]])
n = 30

summary(mod_interaction_list_lc$Institutional)
hqs = quantile(county_dt$std_Ln_Median_Home_Value,c(0.05,0.95))
lcdf = expand.grid(home_values = seq(hqs[1],hqs[2],length.out = n),pov_rates = quantile(county_dt$std_Poverty_Rate,c(0.10,0.5,0.90)))
## net effect of educate + racewhite:educate 

lc = inla.make.lincombs(std_Ln_Median_Home_Value = lcdf$home_values, 
                        "std_Poverty_Rate:std_Ln_Median_Home_Value" = lcdf$pov_rates * lcdf$home_values) 

mod_interaction_list_lc <- lapply(interaction_forms,function(x) {
  #print(x)
  print(x)
  inla(x,data=county_dt,family=best_reference,verbose = F,
       control.predictor=list(compute=TRUE), lincomb = lc,
       control.compute=list( cpo=TRUE,waic=TRUE),
       control.fixed = list(prec.intercept=1),num.threads = 4)})

names(mod_interaction_list_lc ) <- c("All",'Built','Institutional')

interdf = do.call(rbind,
        lapply(seq_along(mod_interaction_list_lc),function(x) data.frame(mod_interaction_list_lc[[x]]$summary.lincomb.derived,pov_rates = lcdf$pov_rates,home_values = lcdf$home_values,
                                                                         Model = names(mod_interaction_list_lc )[x])))

library(ggthemes)
labq = paste0('(',c('10th','50th','90th'),')')
prates = unique(sort(county_dt$Poverty_Rate[county_dt$std_Poverty_Rate%in%c(quantile(county_dt$std_Poverty_Rate,c(0.10,0.5,0.90)))]))
hvals = unique(sort(county_dt$std_Ln_Median_Home_Value[county_dt$std_Ln_Median_Home_Value%in%c(quantile(county_dt$std_Ln_Median_Home_Value,c(0.01,0.99)))]))
hvals_label = unique(sort(county_dt$Median_Home_Value[county_dt$std_Ln_Median_Home_Value%in%c(quantile(county_dt$std_Ln_Median_Home_Value,seq(.01,.91,.10)))]))






ggplot(data = interdf,aes(x = home_values,colour = as.factor(pov_rates),fill = as.factor(pov_rates))) +
  geom_path(aes(y = mean),lwd = 2) +
  geom_errorbar(aes(ymin = `X0.025quant`,ymax = `X0.975quant`),alpha = 0.5) + 
  facet_wrap(~Model,ncol = 2)  +
  theme(legend.position = c(0.75,0.25),legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),strip.text = element_text(size = 14)) + 
  #  scale_x_continuous(name = 'scale(ln(median home value))',breaks=hvals,labels=hvals_label) + 
  scale_y_continuous(name = 'parameter estimate')


  scale_fill_tableau(labels=paste(paste0(prates,'%'),labq,sep=' '),name = 'Poverty rate (quantile)') + 
  scale_colour_tableau(labels=paste(paste0(prates,'%'),labq,sep=' '),name = 'Poverty rate  (quantile)') + theme_bw() + 
  theme(legend.position = c(0.75,0.25),legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),strip.text = element_text(size = 14)) + 
#  scale_x_continuous(name = 'scale(ln(median home value))',breaks=hvals,labels=hvals_label) + 
  scale_y_continuous(name = 'parameter estimate')









sort(unique(county_dt$Ln_Median_Home_Value[county_dt$std_Ln_Median_Home_Value %in% quantile(county_dt$std_Ln_Median_Home_Value,c(0.05,.5,0.95))]))


quantile(test,c(0.1,0.9))
quantile(stest,c(0.1,0.9))

which
length(test)
length(stest)
cbind(test,stest)[90:92,]

ggplot() + geom_point(aes(x = county_dt$Poverty_Rate,y = county_dt$Median_Home_Value))
seq_along(mod_interaction_list_lc)

summary(county_dt$std_Ln_Median_Home_Value)
summary(county_dt$std_Poverty_Rate)
std_Poverty_Rate <- rep(seq(-2,2,0.25),)
std_Ln_Median_Home_Value <- rep(seq(-2,2,0.05),2)
bin_Urban_County <- rep(c(0,1),each = seq(-2,2,0.05))

unlist(sapply(mod_dv_list,function(x) x$waic$waic))
unlist(sapply(mod_interaction_list,function(x) x$waic$waic))




test = mod_interaction_list_lc[[1]]$summary.lincomb.derived
test

plot(density(mod_interaction_list_lc[[1]]$summary.lincomb.derived$mean))

cbind(sapply(mod_dv_list,function(x) x$waic$waic),
      sapply(mod_interaction_list2,function(x) x$waic$waic))
mod_interaction_list[[1]]$model.matrix[,'']
r = inla(y ~ 1 + racewhite*educate, 
         data = data.frame(racewhite, educate), 
         control.predictor = list(compute=TRUE)) 




interaction2 = '+ std_Poverty_Rate*std_Ln_Median_Home_Value*bin_Urban_County'



interaction_forms2 = sapply(form_base,function(f) as.formula(paste0(f,re_set,interaction2)))
mod_interaction_list2 <- lapply(interaction_forms2,function(x) {
  #print(x)
  inla(x,data=county_dt,family=best_reference,verbose = T,
       control.predictor=list(compute=TRUE),
       control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE),
       control.fixed = list(prec.intercept=1),num.threads = 4)})

sapply(mod_dv_list,function(x) x$waic$waic)
sapply(mod_interaction_list2 ,function(x) x$waic$waic)

cbind(sapply(mod_dv_list,function(x) x$waic$waic),
  sapply(mod_interaction_list ,function(x) x$waic$waic))

saveRDS(object = mod_interaction_list ,file = paste0(scratch_loc,'scratch/descriptive_paper/mods_interaction.RDS'))
