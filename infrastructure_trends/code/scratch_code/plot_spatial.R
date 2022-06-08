library(plyr)
library(tidyverse)
library(sp)
library(rgeos)
library(rgdal)
library(tigris)
library(usmap)
library(data.table)
library(ggthemes)
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

county_eis = readRDS('input/ngaz_matches_V3.RDS')
county_eis = county_eis[grepl('201[0-9]{5}',PROJECT_ID),]
setnames(county_eis,'PROJECT_ID','EIS_Number')
county_eis = county_eis[,.N,by=.(EIS_Number,CFIPS)]
setnames(county_eis,'N','Mentions')

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

county_map$data$Agency_Group
c('Forest Service','Bureau of Land Management','Army Corps of Engineers','FERC')

ggplot(data= county_map$data[!is.na(county_map$data$N),]) + 
  geom_polygon(aes(x = long,y = lat,group = group, fill=N)) + 
  facet_wrap(~Agency_Group,ncol=3,scales = 'free') + 
  scale_fill_viridis_c(name = '# of FEISs')+ ggtitle('Final EISs by county and five most active agencies, 2013-2018') + 
  theme_map() +
  geom_polygon(data = smap$data,aes(y = lat,x =long,group = group),col='grey30',fill=NA) + 
  theme(title = element_text(size = 14),legend.title = element_text(size = 12),
        legend.text=element_text(size = 12),legend.position = 'bottom',strip.background=element_blank(),
        legend.justification = 'center',
        legend.background = element_rect(fill = alpha('white',0.2)),strip.text = element_text(size =12))


match_points =  readRDS('input/ngaz_matches_V3.RDS')
match_points = match_points[grepl('^201[0-9][0-9]{4}',PROJECT_ID),]

class = readRDS('scratch/descriptive_eis/predicted_es_sentence_topics.RDS')
bin <- function(x) {ifelse(x>0.5,1,0)}
class = class[ , grep('P',colnames(class),value=T):=lapply(.SD,bin ), .SDcols = grep('P',colnames(class),value=T)]
class$EIS_Number = str_extract(class$file,'[0-9]{8}')

total = class[,.N,by=.(EIS_Number)]
categ = class[,lapply(.SD,sum ), .SDcols = grep('P',colnames(class),value=T),by=.(EIS_Number)]
setkey(total,EIS_Number)
setkey(categ,EIS_Number)
total = categ[total,]
total = total[,grep('P',colnames(total),value=T):=lapply(.SD,function(x) x / N), .SDcols = grep('P',colnames(total),value=T)]
setnames(match_points,'PROJECT_ID','EIS_Number')
setkey(match_points,EIS_Number)
setkey(total,EIS_Number)
pvals = total[match_points,]
#pvals <- pvals[!duplicated(pvals),]
library(viridis)
tstates = tigris::states(class = 'sf')

pgath = gather(pvals,Cat,Prob,-EIS_Number,-N,-doc_id,-sentence_id,-PRIM_LAT_DEC,-PRIM_LONG_DEC,-token,-entity_type,-CFIPS,-FEATURE_ID)
county_tigris = tigris::counties(class = 'sf',year = '2016')
pgath$STATEFP <- county_tigris$STATEFP[match(pgath$CFIPS,county_tigris$GEOID)]
pgath <- pgath[!pgath$STATEFP %in% c('02','15','72','66','60','69'),]
pgath <- pgath[pgath$PRIM_LONG_DEC!=0 & pgath$PRIM_LAT_DEC!=0,]
pgath$Cat <- as.factor(pgath$Cat)


pgath$Cat <- fct_recode(pgath$Cat,'Habitat' = 'P_Habitat' ,'Environmental justice' = 'P_EJ','Recreation' = 'P_Recreation','Biological resources' = 'P_Biology',
                        'Air quality' = 'P_Air','Water resources' = 'P_Water','Aesthetics' = 'P_Aesthetics','Economics' = 'P_Economics','Climate change' = 'P_Climate_Change')

test = pgath[pgath$Cat=='Habitat',]

library(data.table)
library(ggplot2)
library(automap)
library(plyr)
library(akima)

# Data munging

coord_vars = c("PRIM_LAT_DEC","PRIM_LONG_DEC")
data_vars <- 'Prob'
test = test[!is.na(test$PRIM_LAT_DEC),]
sp_points = SpatialPoints(test[,coord_vars])
sp_df = SpatialPointsDataFrame(sp_points, test[,data_vars,drop=FALSE])

library(fields)
library(pbapply)
krig_dt = data.table(pgath[!is.na(pgath$Cat),])
krig_list_dt = split(krig_dt,f = krig_dt$Cat)
krig_list = pblapply(krig_list_dt,function(x) Krig(x = as.matrix(x[,.(PRIM_LONG_DEC,PRIM_LAT_DEC)]),Y = x$Prob),cl = 4)


test = Krig(x = as.matrix(krig_list_dt[[1]][,.(PRIM_LONG_DEC,PRIM_LAT_DEC)]),Y = krig_list_dt[[1]]$Prob)
str(test)

ggplot() + geom_point(aes(col = test$fitted.values[tt],x = test$xM[,1],y = test$xM[,2]))




surface(test, type="I",nx=128,ny=128)
surface(test,type="C", nx=128, ny=128)
US( add=TRUE)
us_states = tigris::states(class = 'sf')
us_states = st_transform(us_states,st_crs(county_tigris))

?surface.Krig
surface()
length(test)
dim(krig_list_dt[[1]])
test$N
str(test)
tt = in.poly(xp = test$x,xd = st_coordinates(us_states)[,(1:2)])
table(tt)
ggplot() + 
  #geom_sf(data= tstates,fill = 'white') + 
  geom_point(data = krig_list_dt[[1]],
             aes(x = PRIM_LONG_DEC,y = PRIM_LAT_DEC,colour = Prob),pch = 21,alpha =0.5) + 
  scale_color_viridis(option = 'C',direction = -1,name ='Prop. emphasis')  +  theme_map() +
  facet_wrap(~Cat,ncol = 3)  + theme(legend.position = 'bottom',legend.direction = 'horizontal',
                                     legend.justification = 'center',strip.text = element_text(size = 12),legend.text = element_text(size = 12),legend.title = element_text(size = 12))





points( test$x)
title("Estimated ozone surface")


str(test)
plot(test)
Krig(x, Y, cov.function = "stationary.cov", lambda = NA, df
     = NA, GCV = FALSE, Z = NULL, cost = 1, knots = NA,
     weights = NULL, m = 2, nstep.cv = 200, scale.type =
       "user", x.center = rep(0, ncol(x)), x.scale = rep(1,
                                                         ncol(x)), rho = NA, sigma2 = NA, method = "REML",
     verbose = FALSE, mean.obj = NA, sd.obj = NA,
     null.function = "Krig.null.function", wght.function =
       NULL, offset = 0, na.rm = TRUE, cov.args = NULL,
     chol.args = NULL, null.args = NULL, wght.args = NULL,
     W = NULL, give.warnings = TRUE, ...)


# Clip the grid to the state regions
map_base_data = map_data("state")
colnames(map_base_data)[match(c("long","lat"),colnames(map_base_data))] =  c("PRIM_LAT_DEC","PRIM_LONG_DEC")
foo = function(x) {
  state = unique(x$region)
  print(state)
  Polygons(list(Polygon(x[, c("PRIM_LAT_DEC","PRIM_LONG_DEC")])),ID=state)
}

state_pg = SpatialPolygons(dlply(map_base_data, .(region), foo))

# Set up map plot
map_base_aesthetics = aes(x=PRIM_LONG_DEC, y=PRIM_LAT_DEC, group=group)
map_base = geom_polygon(data=map_base_data, map_base_aesthetics)
borders = geom_polygon(data=map_base_data, map_base_aesthetics, color="black", fill=NA)
library(akima)

# Do spline interpolation with the akima package
fld = with(test, akima::interp(x=PRIM_LONG_DEC, y=PRIM_LAT_DEC, z = Prob, duplicate="median",
                            xo=seq(min(map_base_data$PRIM_LONG_DEC), max(map_base_data$PRIM_LONG_DEC), length = 1000),
                            yo=seq(min(map_base_data$PRIM_LAT_DEC), max(map_base_data$PRIM_LAT_DEC), length = 1000),extrap=TRUE, linear=FALSE))


melt_x = rep(fld$x, times=length(fld$y))
melt_y = rep(fld$y, each=length(fld$x))
melt_z = as.vector(fld$z)
level_data = data.frame(PRIM_LONG_DEC=melt_x, PRIM_LAT_DEC=melt_y, Prob=melt_z)

interp_data = na.omit(level_data)
interp_data
grid_points = SpatialPoints(interp_data[,2:1])
in_points = !is.na(over(grid_points,state_pg))
inside_points = interp_data[in_points, ]

ggplot(data=inside_points, aes(x=PRIM_LONG_DEC, y=PRIM_LAT_DEC)) + 
  geom_tile(aes(fill=Prob)) + 
  stat_contour(aes(z=Prob)) +
  coord_equal() + 
  scale_fill_gradient2(low="blue",mid="white",high="red", midpoint=mean(inside_points$Prob)) +
  borders


figure6 = ggplot() + 
  #geom_sf(data= tstates,fill = 'white') + 
  geom_point(data = pgath[!is.na(pgath$Cat),],
             aes(x = PRIM_LONG_DEC,y = PRIM_LAT_DEC,colour = Prob),pch = 21,alpha =0.5) + 
  scale_color_viridis(option = 'C',direction = -1,name ='Prop. emphasis')  +  theme_map() +
  facet_wrap(~Cat,ncol = 3)  + theme(legend.position = 'bottom',legend.direction = 'horizontal',
                                     legend.justification = 'center',strip.text = element_text(size = 12),legend.text = element_text(size = 12),legend.title = element_text(size = 12))
figure6


ggsave(figure6,height = 3,width = 4, units = 'in',filename = 'output/rpr/figure6.tiff',dpi = 300)
ggplot() +
  geom_raster(data = DSM_HARV_df , aes(x = x, y = y,
                                       fill = fct_elevation_2)) + 
  scale_fill_manual(values = my_col, name = "Elevation") + 
  coord_quickmap()

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




