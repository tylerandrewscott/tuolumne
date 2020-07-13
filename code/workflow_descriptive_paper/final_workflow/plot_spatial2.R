library(plyr)
library(tidyverse)
library(sp)
library(rgeos)
library(rgdal)
library(tigris)
library(usmap)
library(data.table)
library(ggthemes)
library(sf)

library(maps)
full_rec = fread('input/epa_master_repository/eis_record_detail_coded.csv')
full_rec = full_rec[!is.na(full_rec$EIS.Number),]
full_rec = full_rec[full_rec$Document.Type=='Final',]
full_rec$Year = str_extract(full_rec$EIS.Number,'^[0-9]{4}')

county_map = plot_usmap(regions = "counties",
                  include = c(), data = data.frame(), values = "values",
                  theme = theme_map(), lines = "black", labels = FALSE,
                  label_color = "black")
smap = plot_usmap(regions = "states",
                        include = c(), data = data.frame(), values = "values",
                        theme = theme_map(), lines = "black", labels = FALSE,
                        label_color = "black")


county_eis =readRDS('input/ngaz_matches_V3.RDS')
county_eis = county_eis[grepl('[0-9]{8}',PROJECT_ID),]
county_eis = county_eis[,.N,by=.(PROJECT_ID,CFIPS)]
setnames(county_eis,c('N','PROJECT_ID'),c('Mentions','EIS.Number'))

county_eis$Agency = full_rec$Agency[match(county_eis$EIS_Number,full_rec$EIS.Number)]

keep_agencies = c("U.S. Army Corps of Engineers","Federal Highway Administration",
                  "National Park Service",'Bureau of Land Management',  "Forest Service")
county_eis$Agency_Group = ifelse(county_eis$Agency %in% keep_agencies,county_eis$Agency ,'Other')

county_eis <- data.table(county_eis)


county_eis_count = county_eis[, .N,by = .(CFIPS)]
setnames(county_eis_count,'N','EIS_Count')
county_map$data$EIS_Count <- county_eis_count$EIS_Count[match(county_map$data$fips,county_eis_count$CFIPS)] 
figure5 = county_map +
  geom_polygon(aes(x = x,y = y,group = group, fill=EIS_Count)) + 
  geom_polygon(aes(x = x,y = y,group = group,col = 'empty'),fill = NA,lwd=0.1) + 
  scale_color_manual(values = 'grey50',labels=c('= 0'),name = NULL)+
  scale_fill_viridis_c(name = '# of FEISs',na.value = 'white',direction = -1,begin = 0.1,)+ 
  ggtitle('Final EISs by county, 2013-2019') +
  # geom_polygon(data = smap$data,aes(y = lat,x =long,group = group),col='grey30',fill=NA) + 
  theme(title = element_text(size = 14),legend.title = element_text(size = 12),
        legend.text=element_text(size = 12),legend.position = c(0.86,0.03),
        legend.box.margin = margin(t = -1),
        legend.background = element_rect(fill = alpha('white',0.2))) +
  guides(fill = guide_colourbar(order = 1),colour = guide_legend(order = 2))
figure5

ggsave(figure5,filename = 'output/descriptive_eis/figure5.tiff',dpi = 500,width=6,height=5,units = 'in')


county_eis_count_by_agency = county_eis[, .N,by = .(CFIPS,Agency_Group)]
setkey(county_eis_count_by_agency,'CFIPS')
county_map$data$CFIPS <- county_map$data$fips
county_map$data <- full_join(county_map$data,county_eis_count_by_agency)

county_map$data$Agency_Group <- as.factor(county_map$data$Agency_Group)
county_map$data$Agency_Group <- fct_relevel(county_map$data$Agency_Group,'Other',after=6)


match_points = readRDS('input/ngaz_matches_V3.RDS')
match_points = match_points[grepl('[0-9]{8}',PROJECT_ID),]
setnames(match_points,'PROJECT_ID','EIS_Number')
match_points = match_points[grepl('201[3-9]',EIS_Number),]

class = readRDS('scratch/descriptive_eis/predicted_es_sentence_topics.RDS')
class$EIS_Number <- str_extract(class$file,'^[0-9]{8}')

bin <- function(x) {ifelse(x>0.5,1,0)}
class = class[ , grep('P',colnames(class),value=T):=lapply(.SD,bin ), .SDcols = grep('P',colnames(class),value=T)]
class$EIS_Number = str_extract(basename(class$file),'^[0-9]{8}')

total = class[,.N,by=.(EIS_Number)]

categ = class[,lapply(.SD,sum ), .SDcols = grep('P',colnames(class),value=T),by=.(EIS_Number)]
setkey(total,EIS_Number)
setkey(categ,EIS_Number)
total = categ[total,]
total = total[,grep('P',colnames(total),value=T):=lapply(.SD,function(x) x / N), .SDcols = grep('P',colnames(total),value=T)]


match_points = match_points[EIS_Number %in% total$EIS_Number,]
setkey(match_points,EIS_Number)
setkey(total,EIS_Number)

pvals = total[match_points,]
#pvals <- pvals[!duplicated(pvals),]
library(viridis)
tstates = tigris::states(class = 'sf',year = '2017')
pvals[,doc_id:=NULL]
pvals[,sentence_id:=NULL]
pvals[,FEATURE_ID:=NULL]
pvals[,token:=NULL]
pvals[,token_id:=NULL]
pvals[,entity_type:=NULL]


#pgath = gather(pvals,Cat,Prob,-EIS_Number,-N,-doc_id,-sentence_id,-PRIM_LAT_DEC,-PRIM_LONG_DEC,-token,-entity_type,-CFIPS,-FEATURE_ID)

pgath = melt(pvals,id.vars = c('EIS_Number','PRIM_LAT_DEC','PRIM_LONG_DEC','N','STATEFP','CFIPS'),variable.name = 'Cat',value.name = 'Prob')

county_tigris = tigris::counties(class = 'sf',year = '2016')
pgath$STATEFP <- county_tigris$STATEFP[match(pgath$CFIPS,county_tigris$GEOID)]
pgath <- pgath[!pgath$STATEFP %in% c('02','15','72','66','60','69'),]
pgath <- pgath[pgath$PRIM_LONG_DEC!=0 & pgath$PRIM_LAT_DEC!=0,]
pgath$Cat <- as.factor(pgath$Cat)

pgath$Cat <- fct_recode(pgath$Cat,'Habitat' = 'P_Habitat' ,'Environmental justice' = 'P_EJ','Recreation' = 'P_Recreation','Biological resources' = 'P_Biology',
                        'Air quality' = 'P_Air','Water resources' = 'P_Water','Aesthetics' = 'P_Aesthetics','Economics' = 'P_Economics','Climate change' = 'P_Climate_Change')

library(data.table)
library(ggplot2)
library(automap)
library(plyr)
library(akima)

# Data munging

coord_vars = c("PRIM_LAT_DEC","PRIM_LONG_DEC")
data_vars <- 'Prob'
pgath = data.table(pgath)
pgath = pgath[!is.na(PRIM_LAT_DEC),]

sp_points = SpatialPoints(pgath[,coord_vars,with = F])
sp_df = SpatialPointsDataFrame(sp_points, pgath[,data_vars,drop=FALSE,with = F])

nad83 = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
albersNA <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")

krig_dt = data.table(pgath[!is.na(pgath$Cat),])

krig_list_dt = split(krig_dt,f = krig_dt$Cat)


county_sp = tigris::counties(year = 2014)
county_sp = spTransform(county_sp,albersNA)
county_sp = county_sp[county_sp@data$GEOID %in% pgath$CFIPS,]

ptsreg <- spsample(county_sp[!grepl('^02|^15',county_sp@data$GEOID),], 10000, type = "regular")   # Define the ouput grid - 4000 points in polygons extent
require(parallel)

krig_result_list = mclapply(names(krig_list_dt),function(x){
temp_data = krig_list_dt[[x]]
temp_data = temp_data[!duplicated(temp_data),]
temp_data[,N:=NULL]
temp_data[,sentence_id:=NULL]
temp_data[,entity_type:=NULL]
temp_data = temp_data[!duplicated(temp_data),]
temp_data$PRIM_LONG_DEC = temp_data$PRIM_LONG_DEC + runif(length(temp_data$PRIM_LONG_DEC),min=-0.001,max=0.001)
temp_data$PRIM_LAT_DEC = temp_data$PRIM_LAT_DEC + runif(length(temp_data$PRIM_LAT_DEC),min=-0.001,max=0.001)
coordinates(temp_data) <- ~ PRIM_LONG_DEC + PRIM_LAT_DEC
proj4string(temp_data) <- nad83
temp_data <- spTransform(temp_data,albersNA)
Krig_result = autoKrige(Prob~1,input_data = temp_data, new_data = ptsreg)$krige_output
Krig_df = as.data.table(Krig_result)
names(Krig_df) = c("longitude","latitude", "p_pred","p_var","p_stdev")
Krig_df$Category = x
Krig_df},mc.cores=4,mc.preschedule = T,mc.cleanup = T)

saveRDS(krig_result_list,'krig_results.RDS')

all_krig_df = do.call(rbind,krig_result_list)
figure6 = ggplot() + ggtitle('Relative emphasis of impact areas in executive summaries') + 
  geom_raster(data=all_krig_df, aes(x=longitude, y=latitude,fill=p_pred)) + 
  scale_fill_viridis_c(option = 'magma',direction = -1,name  = '% focus') +
  facet_wrap(~Category) + theme_minimal() + 
  theme(panel.grid = element_line(colour = 'transparent'),axis.text=element_blank(),
        strip.text = element_text(size= 12),
        axis.title = element_blank(),text= element_text(family = 'Times'))
ggsave(figure6,filename = 'output/descriptive_eis/figure6.tiff',dpi = 500,width = 6,height=5)



figure6B = ggplot() + ggtitle('Relative emphasis of impact areas in executive summaries') + 
  geom_raster(data=all_krig_df, aes(x=longitude, y=latitude,fill=p_pred)) + 
  scale_fill_viridis_c(option = 'magma',direction = -1,name  = '% focus') +
  facet_wrap(~Category,scales = 'free') + theme_minimal() + 
  theme(panel.grid = element_line(colour = 'transparent'),axis.text=element_blank(),
        strip.text = element_text(size= 12),
        axis.title = element_blank(),text= element_text(family = 'Times'))
ggsave(figure6B,filename = 'output/descriptive_eis/figure6B.tiff',dpi = 500,width = 6,height=5)


ggplot(data=inside_points, aes(x=PRIM_LONG_DEC, y=PRIM_LAT_DEC)) + 
  geom_tile(aes(fill=Prob)) + 
  stat_contour(aes(z=Prob)) +
  coord_equal() + 
  scale_fill_gradient2(low="blue",mid="white",high="red", midpoint=mean(inside_points$Prob)) +
  borders




ngaz = fread('input/gazetteers/NationalFile_20181001.txt',sep = '|')
head(ngaz)
library(ggvoronoi)


head(pvals)

library(forcats)
giant_merge$Topic = fct_recode(giant_merge$Topic,'Economic resources' = 'P_Economics','Air quality' = 'P_Air','Water resources' = 'P_Water','Environmental aesthetics' = 'P_Aesthetics', 'Outdoor recreation' = 'P_Recreation',
                               'Environmental justice' = 'P_EJ','Biological resources' = 'P_Biology','Climate change' = 'P_Climate_Change','Habitat' = 'P_Habitat')
library(maps)
state_tigris = tigris::states()
state_map_df = map_data(state_tigris[state_tigris$STUSPS %in% c(state.abb,'DC','PR'),])
state_map_df = state_map_df[state_map_df$long<0 & !state_map_df$region%in%c("Alaska",'Hawaii','Puerto Rico'),]
figure7 <- ggplot() + 
  #geom_polygon(data=state_map_df,aes(y = lat,x = long,group =group),fill = 'white') + 
  geom_point(data = giant_merge,aes(colour=Ratio_Value,y = PRIM_LAT_DEC,x = PRIM_LONG_DEC),pch=21) + 
  facet_wrap(~Topic) + scale_colour_viridis_c(option = 'A',direction = -1,breaks=c(0.1,0.2,0.3,0.4),labels=c('10%','20%','30%','40%'),name = 'Topical emphasis') + theme_map() +
  theme(legend.background = element_rect(fill = alpha('white',0.3)),strip.text = element_text(size = 14),legend.text = element_text(size =12),legend.title = element_text(size = 12))



match_pointsplace_eis = readRDS('scratch/descriptive_paper/fips_place_matches.RDS')
library(sf)
TA <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0"
state_places = list.files('spatial_input/ftp2.census.gov/geo/tiger/TIGER2016/PLACE/','shp$',full.names = T)
places_list = lapply(state_places,function(x) {st_read(x)})
places_sf = do.call(rbind,places_list)
places_sf = st_transform(places_sf,TA)
places_sf <- places_sf[!grepl('CDP',places_sf$NAMELSAD),]
places_sf$PLACE_FIPS <- paste0(places_sf$STATEFP,places_sf$PLACEFP)
places_sf <- places_sf[!places_sf$STATEFP%in%c('60','69','72','78'),]

place_count = place_eis[,.N,by=.(PLACE_FIPS)][!is.na(PLACE_FIPS),]
places_sf$EIS_Count = place_count$N[match(places_sf$PLACE_FIPS,place_count$PLACE_FIPS)]

ggplot() + geom_sf(data = places_sf,aes(fill=EIS_Count,colour=EIS_Count)) + scale_fill_viridis(option = 'B')


###################
###################
####################


county_eis[,county_mentions := sum(Mentions),by = .(CFIPS)]
county_eis[,county_freq_by_agency := .N,by = .(CFIPS,Agency_Group)]
county_eis[,county_mentions_by_agency := sum(Mentions),by = .(CFIPS,Agency_Group)]


county_eis[order(CFIPS),][grepl('^06',CFIPS),]
county_map$data$EIS_count = county_eis$county_freq[match(county_map$data$fips,county_freq$CFIPS)] 
county_map$data$EIS_mentions = county_eis$county_mentions[match(county_map$data$fips,county_freq$CFIPS)] 



keep_agencies = c("U.S. Army Corps of Engineers","Federal Highway Administration",
                  "National Park Service",'Bureau of Land Management',  "Forest Service")
county_map2$data$Agency_Group = ifelse(county_map2$data$Agency %in% keep_agencies,county_map2$data$Agency ,'Other')


#county_map$data$EIS_count[is.na(county_map$data$EIS_count)] <- 0




county_freq_by_agency = county_eis %>% group_by(CFIPS,Agency_Group) %>% summarise(county_freq_agency = n())
county_map2 = county_map
county_map2$data = full_join(county_map2$data,county_freq_by_agency %>%rename(fips = CFIPS))


eis_state = do.call(rbind,lapply(seq_along(full_rec$EIS.Number),function(x)
  data.frame(STATE = unlist(str_split(full_rec$State.or.Territory[x],pattern = '-')),EIS.Number = full_rec$EIS.Number[x],
             Agency = full_rec$Agency[x],stringsAsFactors = F)))
eis_state$YEAR = str_extract(as.character(eis_state$EIS.Number),'^[0-9]{4}')
state_totals = eis_state %>% group_by(STATE) %>% summarise(FEIS_Since_1987 = n())
smap$data$EIS_Since_1987 = state_totals$FEIS_Since_1987[match(smap$data$abbr,state_totals$STATE)]
smap + geom_polygon(aes(x = long,y = lat,group = group, fill=EIS_Since_1987),col='grey50') + 
  scale_fill_viridis_c(name = 'FEIS by state') + ggtitle('FEIS pertaining to each state since 1987') + 
  theme(legend.position = c(0.9,0.1))




county_tigris@data$AREA = as.numeric(county_tigris@data$ALAND) + as.numeric(county_tigris@data$AWATER)
county_tigris@data$EIS_COUNT =  county_freq$county_freq[match(county_tigris@data$GEOID,county_freq$CFIPS)]
county_tigris@data$EIS_COUNT[is.na(county_tigris@data$EIS_COUNT)] <- 0
library(choroplethr)
data(df_pop_county)

df_pop_county$region = formatC(df_pop_county$region,width = 5,flag="0",format = 'f',digits = 0)

county_tigris@data$POPULATION = df_pop_county$value[match(county_tigris@data$GEOID,df_pop_county$region)]


county_tigris@data[!county_tigris$GEOID %in% df_pop_county$region,]

ggplot(data = county_tigris@data) + geom_point(aes(x = log(POPULATION),y = log(AREA),size = EIS_COUNT),pch=21)

plot(ch_poly)

ch_line = readOGR('spatial_input/Critical_Habitat_USGS','CRITHAB_LINE')
ch_line <- spTransform(ch, CRS("+init=epsg:4269"))
ch_line_fortify = fortify(ch_line)
ch_poly = readOGR('spatial_input/Critical_Habitat_USGS','CRITHAB_POLY')
ch_poly <- spTransform(ch_poly, CRS("+init=epsg:4269"))
ch_poly_fortify = fortify(ch_poly)

state_nad83 = spTransform(state_tigris,CRS("+init=epsg:4269"))
state_fortify = fortify(state_nad83[state_nad83@data$STUSPS %in% state.abb[!state.abb%in%c('AK','HI')],])

ch_poly_fortify = ch_poly_fortify[ch_poly_fortify$long>(-125)&ch_poly_fortify$long<(-66)& ch_poly_fortify$lat>23&ch_poly_fortify$long<50,]


(-124.848974, 24.396308) - (-66.885444, 49.384358)

library(ggplot2)
gg1 = ggplot() + ggtitle('Critical habitat areas for threatened and endangered species') + 
  geom_path(data = ch_line_fortify,aes(y = lat,x=long,group = group),lwd=1,col = 'grey50',alpha=0.3) + 
  geom_polygon(data = ch_poly_fortify,aes(y = lat,x=long,group = group),lwd=1,col = 'grey50',fill='grey50',alpha=0.3) + 
  geom_polygon(data = ch_poly_fortify,aes(y = lat,x=long,group = group),lwd=1,col = 'grey50',fill='grey50') + 
  theme_map() + 
  #geom_polygon(data=state_map_df,aes(y = lat,x = long,group =group),fill = 'white') + 
 # geom_point(data = giant_merge[giant_merge$Topic %in% c('Biological resources','Habitat'),],aes(colour=Ratio_Value,y = PRIM_LAT_DEC,x = PRIM_LONG_DEC),pch=21,alpha=0.5) + 
  #scale_colour_viridis_c(option = 'A',direction = -1,breaks=c(0.1,0.2,0.3,0.4),labels=c('10%','20%','30%','40%'),name = 'Topical emphasis') + theme_map() +
  theme(legend.background = element_rect(fill = alpha('white',0.3)),strip.text = element_text(size = 14),legend.text = element_text(size =12),legend.title = element_text(size = 12))

gg2 = ggplot()  + ggtitle('Habitat resource emphasis overlaid on critical habitat') + 
  geom_path(data = ch_line_fortify,aes(y = lat,x=long,group = group),lwd=1,col = 'grey50',alpha=0.3) + 
  geom_polygon(data = ch_poly_fortify,aes(y = lat,x=long,group = group),lwd=1,col = 'grey50',fill='grey50',alpha=0.3) + 
  theme_map() + 
  #geom_polygon(data=state_map_df,aes(y = lat,x = long,group =group),fill = 'white') + 
  geom_point(data = giant_merge[giant_merge$Topic %in% c('Habitat'),],aes(colour=Ratio_Value,y = PRIM_LAT_DEC,x = PRIM_LONG_DEC),pch=21,alpha=0.9) + 
  scale_colour_viridis_c(option = 'A',direction = -1,breaks=c(0.1,0.2,0.3,0.4),limits=c(0,.4),labels=c('10%','20%','30%','40%'),name = 'Topical emphasis') + theme_map() +
  theme(legend.background = element_rect(fill = alpha('white',0.3)),strip.text = element_text(size = 14),legend.text = element_text(size =12),legend.title = element_text(size = 12))
gg3 = ggplot()  + ggtitle('Biological resource emphasis overlaid on critical habitat') + 
  geom_path(data = ch_line_fortify,aes(y = lat,x=long,group = group),lwd=1,col = 'grey50',alpha=0.3) + 
  geom_polygon(data = ch_poly_fortify,aes(y = lat,x=long,group = group),lwd=1,col = 'grey50',fill='grey50',alpha=0.3) + 
  theme_map() + 
  #geom_polygon(data=state_map_df,aes(y = lat,x = long,group =group),fill = 'white') + 
  geom_point(data = giant_merge[giant_merge$Topic %in% c('Biological resources'),],aes(colour=Ratio_Value,y = PRIM_LAT_DEC,x = PRIM_LONG_DEC),pch=21,alpha=0.9) + 
  scale_colour_viridis_c(option = 'A',direction = -1,breaks=c(0.1,0.2,0.3,0.4),limits=c(0,.4),labels=c('10%','20%','30%','40%'),name = 'Topical emphasis') + theme_map() +
  theme(legend.background = element_rect(fill = alpha('white',0.3)),strip.text = element_text(size = 14),legend.text = element_text(size =12),legend.title = element_text(size = 12))

grid.arrange(gg1,gg2,gg3,ncol=1)




# change the degree symbol to a space
x$lat = gsub('°', ' ', x$lat)
x$long = gsub('°', ' ', x$long)


ggplot(point_obs,aes(x = PRIM_LONG_DEC,y = PRIM_LAT_DEC)) + geom_point()

# convert from decimal minutes to decimal degrees
measurements::conv_unit(point_obs$PRIM_LONG_DMS, from = 'deg_min_sec', to = 'dec_deg')
cbind(measurements::conv_unit(gsub('[A-Z]','',point_obs$PRIM_LONG_DMS), from = 'deg_min_sec', to = 'dec_deg'),point_obs$PRIM_LONG_DEC)

table(
test = point_obs[1:100,]
library(measurements)
conv_unit(gsub('[A-Z]','',test$PRIMARY_LAT_DMS),from = 'deg_min_sec',to = 'dec_deg')

head(point_obs)

cbind(measurements::conv_unit(gsub('[A-Z]','',point_obs$PRIM_LONG_DMS), from = 'deg_min_sec', to = 'dec_deg'),point_obs$PRIM_LONG_DMS)




