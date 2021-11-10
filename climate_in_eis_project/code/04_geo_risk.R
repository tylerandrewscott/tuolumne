pack = c('tigris','maptools','acs','pbapply','data.table',
         'sf','doParallel','tidyverse','ggthemes','gridExtra')
need = pack[!pack %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(pack,require,character.only=T)

projects = readRDS('climate_in_eis_project/data_products/deis_metadata_with_covariates.RDS')
documents = readRDS('climate_in_eis_project/data_products/deis_doc_metadata.RDS')

geo_file = readRDS('climate_in_eis_project/data_products/geo_temp.rds')

geo_sf = sf::st_as_sf(geo_file,coords = c('PRIM_LONG_DEC','PRIM_LAT_DEC'))
geo_sf = st_set_crs(geo_sf,4269)

td = tempdir()
hazip = 'https://hazards.fema.gov/nri/Content/StaticDocuments/DataDownload//NRI_Table_Counties/NRI_Table_Counties.zip'
tf = tempfile(tmpdir=td, fileext=".zip")
curl::curl_download(hazip, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('Counties.csv$',fname$Name,value=T))
haz = fread(fpath)

counties = tigris::counties(cb = F,year = 2017,class = 'sf')
point_in_county = st_join(geo_sf,counties,join = st_within)
point_in_county$GEOID <- formatC(point_in_county$GEOID,width = 5,flag = '0')
setnames(haz,'STCOFIPS','GEOID')
haz$GEOID <- formatC(haz$GEOID,width = 5,flag = '0')
point_in_county_haz <- merge(point_in_county,haz,all.x = T)
point_in_county_haz <- point_in_county_haz[!is.na(point_in_county_haz$RISK_SCORE),]
pcents = point_in_county_haz %>% group_by(PROJECT_ID) %>% 
  summarise(
    project_RISK = mean(RISK_SCORE),
    project_EAL = mean(EAL_SCORE),
    project_CR = mean(RESL_SCORE),
    project_SVI = mean(SOVI_SCORE)) %>% st_centroid()

pcents_df_only = pcents %>% as.data.frame() %>% select(-geometry)
saveRDS(pcents_df_only,file = 'climate_in_eis_project/data_products/project_risks.RDS')



states = tigris::states(cb = F,year = 2017,class = 'sf')
states = states[states$STUSPS %in% c(state.abb,'DC'),]
states = st_transform(states,st_crs(pcents))
state_shift = tigris::shift_geometry(states)
counties = st_transform(counties,st_crs(pcents))
county_shift = tigris::shift_geometry(counties[counties$STATEFP %in% state_shift$STATEFP,])
county_haz <- inner_join(county_shift,haz)






pshift = tigris::shift_geometry(pcents)


gg_haz = ggplot() + geom_sf(data = state_shift,fill = alpha('white',0)) + theme_map() +
  scale_color_viridis_c(option = 'E')

g1 = gg_haz + geom_sf(data = pshift,aes(colour = project_EAL),alpha = 0.5)
g2 = gg_haz + geom_sf(data = pshift,aes(colour = project_SVI),alpha = 0.5)
g3 = gg_haz + geom_sf(data = pshift,aes(colour = project_CR),alpha = 0.5)
ggsave(plot = grid.arrange(g1,g2,g3,ncol = 2),
       filename = 'climate_in_eis_project/output/figure1_hazard.pdf',
       height = 7,width = 7,units = 'in',dpi = 300)



ggtest = ggplot() + 
  geom_sf(data = state_shift,col = 'blue') + 
  geom_sf(data = county_shift,col = 'red')
ggsave(filename = 'test.pdf',plot = ggtest)




class(county_haz)
ggplot() + 
  
  county_coords
ggplot() + 
  geom_sf(data = counties[counties$STATEFP=='02',])





ggplot() + 
  geom_sf(data = states) + 
  geom_sf(data = pcents,aes(col = project_haz)) + 
  theme_map() + 
  scale_color_viridis_c(option = 'B',name = 'Aggregate risk score')


str(haz)

names(haz)

psplits <- split(point_in_county,point_in_county$PROJECT_ID)
psplits <- lapply(psplits,function(x) x %>% select(geometry))
pcents = lapply(psplits,st_centroid)


project_centroids = do.call(rbind,pcents)

dim(project_centroids)
length(psplits)

ggplot() + geom_sf(data = project_centroids,aes(fill = ))

st_centroid(point_in_county)
st_


rscores = point_in_county_haz %>% 
  group_by(PROJECT_ID) %>% 
  summarise(project_risk_score = mean(RISK_SCORE))


library(ggthemes)
ggplot() + geom_sf(data = rscores,aes(col = project_risk_score)) + 
  scale_color_viridis_c(option = 'B') + theme_map()


haz[!haz$GEOID %in% point_in_county$GEOID,]

test = point_in_county[point_in_county$GEOID %in% haz$GEOID,]
table(projects$EIS.Number %in% test$PROJECT_ID)
projects[!EIS.Number %in% test$PROJECT_ID,]$EIS.Number



geo_file[PROJECT_ID=='20130002',]
projects[EIS.Number=='20130002']
documents[EIS.Number=='20130002']


# find points within polygons
tree_in_tract <- st_join(tree_sf, tract_sf, join = st_within)




# count trees per census tract
tree_tract_count <- count(as_tibble(tree_in_tract), boro_ct2010) %>%
  print()


dict = fread(file.path(td,grep('Dictionary',fname$Name,value = T)))

grep('(V|v)uln',dict$`Field Alias`,value = T)

plot(haz$SOVI_VALUE,haz$RISK_SCORE)

plot(haz$RISK_SCORE,haz$EAL_SCORE)
str(haz)


test = merge(counties,haz)
ggplot(data = test) + geom_sf()



tigris::counties()
cor(haz[,.(EAL_SCORE,SOVI_SCORE,RESL_SCORE)])

summary(haz[,.(RISK_SCORE,EAL_SCORE,SOVI_SCORE,RESL_SCORE)])
grep('SCORE',names(haz),value = T)

ggplot() + geom_point(data = geo_file,aes(x = PRIM_LONG_DEC,y = PRIM_LAT_DEC))
geo_file


dim(haz)
