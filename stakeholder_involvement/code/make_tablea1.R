water_quality_words = c("TMDL",'nonpoint source','NPS pollution',"heavy metals","NPDES","run-off","water quantity",
                        "water quality impacts","liquid effluents",'stream health',"erosion control",
                        'effluent','effluents','groundwater','water delivery','sedimentation',
                        'hydrologic condition','snowmelt','turbidity','urban runoff','stormwater','surface runoff','pH',
                        'DO concentration','discharge water','flood event','flooding')
air_quality_words  = c('carbon monoxide','PM2.5',"PM10","PM 2.5", "PM 10","NOX",'dust','air quality',
                       'criteria pollutants','nonattainment','particulate matter','particulates','emissions')
biology_words = c('invasive species','invasives','native species','native plants','endangered species',
                  'species composition','spawn','breed','prey','survival','noxious weeds',"nests",
                  'migratory species','migratory birds','fish','juveniles','wildlife','salmon',
                  'Salmon','migration','migrate','pollinators','macroinvertebrate')
habitat_words = c('ecosystem','habitat', 'ramping','spawning area','gravel bed','wetlands','Pinyon-juniper',
                  'habitat connectivity',"wetland habitat",'riparian habitat','uplands','riparian area','grazing area',
                  "channel conditions","riparian-wetland areas","native grasses","rip-rap")
sociocultural_words = c('Environmental justice','environmental justice','EJ','socioeconomic','low-income','minority','recreational activity','recreational uses','resource access',
                        'economic benefits','cultural resources','recreational benefit','hazardous waste','toxic waste','fishing','hunting','hiking trail','biking trail',
                        'hiking trails','biking trails','sacred',
                        'public health','migratory workers','public access','historic resource','biking','walking','swimming','harvest')
aesthetic_words = c('noise pollution','background noise','vividness','intactness','unity','viewer','light pollution','visual quality','visual character',"parks",
                    'noise','visibility','aesthetic','wilderness','viewshed','vibration','visual impact','scenic','vistas','noticeable','lighting','visible')
climate_words = c('climate model','climate change','global warming','greenhouse gas','GHG','GHGs','ocean acidification','energy use','energy consumption',
                  'greenhouse gasses','carbon emissions','oil and gas development','climate projections','sea level rise','drought','precipitation','wildfire')
none_words = c("public scoping","public awareness","solid waste management","scoping meetings","public meetings","fossils","property acquisition")

library(htmlTable)
test = data.table(Category = c('Air quality','Water quality','Biological','Habitat','Climate change','Socio-cultural','Aesthetic'))
test$Words = list(air_quality_words,water_quality_words,biology_words,habitat_words,climate_words,sociocultural_words,aesthetic_words)

htmlTable(test)
test = readtext::readtext(list.files('input/eis_text/',full.names = T))

grepl('EIR',test$text)
test[37,]

sum_dt$class_prop = sum_dt$class_sents/sum_dt$tot_sents

tapply(sum_dt$class_prop,sum_dt$type,mean)

list.files('../../../../net/tmp/tscott1/tuolumne_scratch/scratch/')


