
require(data.table)
require(tidyverse)
require(ggrepel)
proj.dir = 'climate_in_eis_project/'
scratch_loc = paste0(proj.dir,'data_products/')
projs = readRDS(paste0(scratch_loc,'deis_metadata.RDS'))
projs$Title[projs$EIS.Number=='20200101'] <- "Environmental Impact Statement for Interim Storage Partners LLC License Application for a Consolidated Interim Storage Facility for Spent Nuclear Fuel in Andrews County, Texas"
projs$PROJECT_TYPE <- NA
projs$PROJECT_TYPE[grepl('Restoration\\b|Remediation\\b|Residual|Fuels Management|Homebasing|Fire Recovery|Disposal of Spent Nuclear Fuel|Charleston Harbor Post 45|SR 997|Lambert Houses Redevelopment|Recapitalization of Infrastructure|LA 3127 Connection|Versatile Test Reactor|Eden Landing Ecological Reserve|Kachess Drought Relief|South San Francisco Bay Shoreline Phase I|Fort Wainwright Disposition|Electrical Upgrades|Pacific Crest Trail|Storm Damage Reduction|Operational Efficiency|Mobile Harbor, Mobile|Headquarters Consolidation|Construction and Demonstration|Reconstruction|Interceptor Site|Dakota Alternate Water Supply|Beasley Pond Analysis Area|American River Common Features|Basin Regional Water System|Rangeland Management|NJ Transitgrid Traction Power System|Proposal to Relocate|U-4738|East Rockaway Inlet to Rockaway Inlet and Jamaica Bay|Storm Damage Risk Reduction|Amite River and Tributarie|Gulf Intracoastal Waterway|Matagorda Ship Channel|Address the Presence|Habitat Protection and Enhancement|Habitat Enhancement|Benefit Native Species|Invasive Plant Management|Public Motor Vehicle Use|Travel Management|Vegetative Management|Sea Range|Savannah River Site|Interstate\\s[0-9]|US\\s[0-9]{1,3}|Salvage|Reforestation|Scorpion Pier Replacement|Project\\b|Projects\\b|Building|Transmission Line|Improvement|Airport|Invasive Plant Treatment|Removal|Embarkation|Expansion|Campus|Modernization|Corridor|Bypass|LNG|Crossing|I-[0-9]{1,}|I[0-9]{1,}|Connector|Railroad|Xpressway|Rail\\b|Highway|Route|Expressway|Mine\\b|Extension|Facility|Station|Addition|Interchange|Maintenance|Operations|Management (?!Plan)|Program\\b|Control\\b|Training|Testing|Programs\\b|Beddown|Vegetation Management|Research',projs$Title,perl = T)]<-'Direct Action/Operations'
projs$PROJECT_TYPE[grepl('Plan\\b|Plans\\b|RMP|Blueprint|Strategy|Initiatives|Feasibility|Resources Investigation|Storage Investigation|Study|Policy Review|Risk Management|Management Planning|Re-Evaluation|Sage-Grouse Planning|Experimental|Rangeland Analysis|Grazing Analysis|Strategic Initiative Landscape Initiative|National Wildlife Refuge Complex|Special Use Airspace Optimization|NEC FUTURE|Proposed Geological and Geophysical Activities|Lynx Analysis Units|Test Releases|Airspace Optimization|Comprehensive Airspace Initiative',projs$Title,perl = T)]<-'Planning/Study'
projs$PROJECT_TYPE[grepl('License|Permit|Exchange|Lease\\b|Leasing\\b|Acquisition|Gondola|Lee Canyon EIS|Long-Term Water Transfers|River Islands At Lathrop  Phase 2B|Proposed New Bridge|Mackey Road Relocation|Ambler Road|Communication Site|Amoruso Ranch|Rangeland Allotments|Fee-to-Trust Transfer|Lower Bois D|Interconnection of the Grande Prairie Wind Farm|Spaceport Camden|Hallets Point Rezoning|BP Cherry Point Dock|License Application|HCP|Bulk Terminals|Wind Energy Center|SpaceX|Timber Sale|Everglades Agricultural Area A-1 Shallow Flow Equalization Basin|East Bay Hills Hazardous Fire Risk Reduction|Earthquake Ready Burnside Bridge|Uinta Basin Railway|Grazing Authorization|Grazing Allotments|Solar Energy Center|Impoundment Closure|Summer Uses EIS|Commercial.*Dredging|Landfill|Leases|Oil and Gas|Lignite Mining|Request to Hunt',projs$Title,perl = T)]<-'Permit/Lease/License'
projs$PROJECT_TYPE[grepl('Programmatic|PROGRAMMATIC|Regulation|Rule\\b|Standards\\b|Designation\\b|National Marine Sanctuary|Focal Areas Withdrawal|Rulemaking|A National Approach|Issuing Annual Catch Limits|Determination|Amendment|Enhancing Protections|Generic\\b|Areas and Boundaries|Reduce the Incidental Bycatch',projs$Title, perl = T)]<-'Rule/Program'
projs$PROJECT_TYPE[projs$Title %in% c('Starry Goat','Ragged Ruby',"Flat Country","Gold Butterfly","Trout Creek","Youngs Rock Rigdon" )]<- 'Direct Action/Operations'

projs = projs[!duplicated(EIS.Number),]
projs$PROJECT_TOPIC <- NA
projs$PROJECT_TOPIC[grepl('Solar|Wind|Transmission|Hydropower|Renewable|Hydroelectric|Nuclear.*Licenses|Engineered High Energy Crop Programs|Renewal of Nuclear Plants|Hawaii Clean Energy|Construct Power Line Replacement|Electrical Line Upgrade|West of Devers Upgrade Project|Electric Station|San Juan Basin Energy Connect Project|Pumped Storage|Yuba River Development Project|Energy Storage Project|License Renewal of Nuclear Plants|Nuclear Power|Test Reactor',projs$Title)] <- 'Electric Generation/Transmission'
projs$PROJECT_TOPIC[grepl('Oil|Gas|LNG|Pipeline|Keystone XL|Xpress|Nanushuk Project|Coal Combustion|Fossil Plant|East 300 Upgrade Project|Millennium Bulk Terminals-Longview|Marcus Hook Electric Compression Project|Ash Impoundment|FutureGen 2.0 Project|Carbon Capture and Sequestration Project|Liquefaction|Evangeline Pass Expansion Project|Enhancement by Compression Project|Delivery Lateral|Riley Ridge to Natrona Project|Willow Master Development|Bull Mountain Unit Master Development Plan|National Petroleum Reserve|Calcasieu Pass|Mountain Valley Project|Jordan Cove|Gulf XPress|Atlantic Sunrise|Southgate Project|Rayne Xpress|Algonquin Incremental Market Project|Northeast Supply Enhancement Project',projs$Title)|grepl('Ocean Energy',projs$Lead.Agency)] <- 'Fossil Fuels'
projs$PROJECT_TOPIC[grepl('Highway|Rail|Transportation|Transit',projs$Lead.Agency)|grepl('Airport|Proposed New Bridge|Rail Support Facility|Kinston Bypass|US-275|Nebraska Highway 12|Intermodal Container Transfer Facility|BNSF Railway Bridge|Bog Creek Road Project',projs$Title)] <- 'Transportation'
projs$PROJECT_TOPIC[grepl('United States Army|Navy|Air Force|Marine Corp|Space',projs$Lead.Agency)|grepl('SpaceX|Spaceport|Plutonium Pit Production|NIH Chilled Water System|Cutter Acquisition|Interceptor Site|Radioisotope|Research Program Facility|Polar Icebreaker|Geologic Repository|Public Safety Broadband|Marine Data Acquisition|Spent Nuclear Fuel|Waste Confidence|Early Site Permit|Determinations of Nonregulated Status|Determination of Non-regulated|Determination of Nonregulated Status|Genetically Engineered|Sheep Analysis|Experimental Forest|Research Station Project|Microreactor|Fuel Fabrication Facility',projs$Title)] <- 'Military, Space & Research'
projs$PROJECT_TOPIC[grepl('Mine\\b|Mine$|Mineral|Mining|Uranium|Copper Project|Ore Body Development Project|Gold Project|Stream Protection Rule|Quarry Expansion|Potash|Greater Phoenix Project|Mines Plan of Operations|South Quarry|Deep South Expansion Project|Mackay Optimization Project|Quarries Expansion',projs$Title)] <- 'Mining'

projs$PROJECT_TOPIC[grepl('Fish Passage|Juvenile Salmonids|Parking Improvement|Backcountry Management|Burning Man|The Great Lakes and Mississippi River Interbasin Study|Dam Test Release|Over Snow Vehicle Use|Bighorn Sheep Management|Cattle Canyon Improvements Project|Over-(S|s)now Vehicle|Motorized Trail|Lynx Analysis|Rehabilitation Project| Management Project|Trail System Project|Public Motor Vehicle Use|Salmon in the Lower Klamath River|Warm Springs Management Plan|Archeological Resources|Wilderness Stewardship|Moose-Wilson Corridor|Travel Management|Travel Plan|Trail$|Vaccination|Wild and Scenic|Sage-grouse Habitat|National Seashore|Restoration|Reintroduction|Ungulate|Park.*Transportation|Habitat Enhancement|National Lakeshore|Public Access|Wildlife Refuge|National Monument|Outfitter|Habitat Conservation Plan|Invasive|Eradication|Abundance|Carcass Management|Cooperative Control Program|Feral Swine|Plant Pests|Tamarisk Removal|National Battlefield|Wilderness Study|Recreation Area|Deer Management|Vehicle Management|Goat Management|Presence of Wolves|Historical Park|Wilderness Management Plan|Off-Road Management|Backcountry Access',projs$Title)] <- 'Parks & Wildlife'
projs$PROJECT_TOPIC[grepl('Allotment|Grazing|Resource Management Plan|Road Relocation|Central Everglades Planning Project|Geothermal Leasing|Drainage Basin Improvement Project|Fuelbreak|Rangeland|General Management Plan|Combined Operational Plan|Fuel Breaks|Utility Corridor|Colorado General Investigation Study|Resource Mangement Plan|RMP|East Bay Hills Hazardous Fire Risk Reduction|Land Use Plan|Sagebrush Focal Areas Withdrawal|Eden Landing Ecological Reserve|Vegetation Treatments Using Aminopyralid|Sage-Grouse Planning|Non-native Plant Management|Land Protection Plan|2019 Draft Integrated Resource Plan|Standards for Growing|Watershed Plan and EIS|Salinity Control Project|Remediation of Area IV and the Northern Buffer Zone|Aquaculture Management Program',projs$Title)] <- 'Other Land Management'
projs$PROJECT_TOPIC[grepl('Timber|Restoration|Hazardous Fuels Reduction|Forest Health|Plan for the National Forests|Fuels Management|Recovery|Protection Project|Reforestation|Fire Hazard Reduction',projs$Title)] <- 'Forestry'
projs$PROJECT_TOPIC[projs$Agency=='Forest Service'&grepl('Management Plan|Plan Amendment|Plan Revision|Vegetation Management Project|Weed Management|Salvage|Revised Forest Plan|Forest Management Project|Late-Successional Reserve',projs$Title)] <- 'Forestry'

projs$PROJECT_TOPIC[grepl('Canal\\b|Water Resources|Ship Channel|River System Operations|Container Terminal Improvements|Upper Barataria Basin|Lake Pontchartrain and Vicinity|Radial Gates Installation|Rockaway Inlet and Jamaica Bay|North DeSoto|New Jersey Back Bays|Tammany Parish|West Bank and Vicinity|Blanchard River Watershed Study|Shoreline Stabilization|Mobile Harbor|Shallow Flow Equalization Basin|South San Francisco Bay Shoreline Phase I|River Basin Project|Berryesssa Creek Project|Southport Sacramento|Lower San Joaquin River Project|Coastal Storm Risk Management|Beach Nourishment|Navigation Study|Flood Damage Reduction|Navigation Feasibility|Dredged Material Management Plan|West Sacramento Project|Water Control Manual|Watery System|Dam Safety|Cherry Point Dock|Dike Dam|Gulfport Expansion|American River Common Features|Harbor Post|Flood Risk Management|Floodway|Harbor Navigation|Water Transfers|Water Supply Project|Deepwater Port|Southern Flow Corridor Project|Discharge Project|San Luis Low Point|Regional Water System|Rio Grande Project|Salinity Control Program|Pure Water San Diego|Basin Feasibility|Alternate Water Supply|Water Shortage|Central Arizona Project|Locks\\b|Levee|Sisk Dam|Glen Canyon Dam|Recycled Water|Central Valley Project|Basin Storage|Groundwater Replenishment Facility|Storm Damage|Diversion|Lock\\b|Collection System|Gulf Outlet|Channel Improvement|Embarkation|Pier Replacement|Dredging|Harbor Project|Flood Control|Reservoir',projs$Title)] <- 'Public Works'
projs$PROJECT_TOPIC[grepl('Fishery|Marine Sanctuar(y|ies)|Gulf of the Farallones Boundary Expansion|National Estuarine Research Reserve|Fisheries|Large Whale Take|Summer Flounder|Dolphins|Coral Reef Conservation|Hatchery|Ocean Dredged Material|Breach Management Plan',projs$Title)|grepl('Marine Fisher',projs$Lead.Agency)] <- 'Marine Management'
projs$PROJECT_TOPIC[grepl('Recreation Enhancement|Fee-(T|t)o-Trust|Exchange|River Islands At Lathrop|Amoruso Ranch|Southwest Coastal Louisiana Project|Westbrook  Project|Placer Vineyards Specific Plan|South Central Coast Louisiana Draft Feasibility Study|Cordova Hills|Houses Redevelopment|Resiliency Initiatives for Tottenville Shoreline|Ohio Creek Watershed Project|Master Plan Project|Houses Redevelopment|Resort\\b|Trust Acquisition|Road Improvement Project|Expansion and Modernization|Lab Medicine Building|Breckenridge Summer Uses|Golden Peak Improvements 2016|Land Acquisition|Multi-Season Recreation Projects|Modernization and Expansion|2018 Federal Research Center Master Plan|Communication Site|Specific Plan$|Area Improvement|Rezoning|Casino|Campus|Headquarters|Ski Area|\\bSki\\b|National Flood Insurance Program|Federal Building|Sonoran Valley Parkway Project|Ambler Road|Lee Canyon EIS|Gondola|Floating Houses Policy Review',projs$Title)] <- 'Buildings & Real Estate'

projs$PROJECT_TOPIC[projs$Title %in% c("Townsend Project", "Magone Project" ,"South Revilla Integrated Resource Project","Hwy 46 Project","Rulemaking for Alaska Roadless Areas",
                                       "Flat Country" ,"Craggy Vegetation Management" ,"Wrangell Island Project"   ,"Lolo Insect & Disease Project",
                                       "4FRI Rim Country Project","Spruce Beetle Epidemic and Aspen Decline Management Response" ,"Galton Vegetation Management",
                                       "Medicine Bow Landscape Vegetation Analysis (LaVA) Project","Beasley Pond Analysis Area" ,"Cliff Knox Project","Youngs Rock Rigdon" ,
                                       "Elk Late-Successional Reserve Enhancement Project"  ,"East Hills Project DRAFT Environmental Impact Statement" ,"Lower Yaak, O'Brien, Sheep Project"  ,
                                       "Becker Integrated Resource Project" ,"Telegraph Vegetation Project" ,"Tenmile South Helena Project","Green Mountain Project" ,"Ringo Project"     ,"Black Hills Resilient Landscapes Project",
                                       "Stonewall Vegetation Project" ,"Central Tongass Project","HiLo Project","Goose Project","Gold Butterfly" ,"Beaver Creek Project"  ,"Jess Project","Buckhorn Project","Little Boulder Project"  , "Prince of Wales Landscape Level Analysis Project"   ,"Trout Creek" ,"Starry Goat" ,"North Savery Project"  ,"Ragged Ruby")] <- 'Forestry'

projs$PROJECT_TOPIC[projs$Title %in% c("Ballville Dam Project"  ,"Management of Conflicts Associated with Double-crested Cormorants"  , "City of San Diego Vernal Pool HCP EIS" ,  
  "Regulations Governing Take of Migratory Birds","Programmatic - Eagle Rule Revision","Butte Regional Conservation Plan" ,
  "National Bison Range Draft Comprehensive Conservation Plan, Environmental Impact Statement, and Compatibility Determinations",
  "Long-term Conservation Strategy for the Marbled Murrelet"    ,"Placer County Conservation Program Environmental Impact Statement/Environmental Impact Report"   ,
  "Butte Regional Conservation Plan" ,"Proposed Revision to the Nonessential Experimental Population of the Mexican Wolf (Canis lupus baileyi)")] <- 'Parks & Wildlife'


projs$STATE = NA
projs$STATE[projs$State.or.Territory %in% state.abb] <- projs$State.or.Territory[projs$State.or.Territory %in% state.abb]
projs$STATE[projs$State.or.Territory=='DC'] <- 'DC'
projs$STATE[projs$State.or.Territory=='PR'] <- 'PR'
projs$STATE[grepl('Henrys Fork',projs$Title)]<-'WY' 
projs$STATE[grepl('Alabama-Coosa',projs$Title)] <- 'Multi'
projs$STATE[grepl('-',projs$State.or.Territory)] <- 'Multi'
projs$STATE[is.na(projs$STATE)] <- 'National'

ideology_sheet = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSOSX--qpOSyBZcW3OWeWRmiC2-eC74un7fZYXFCzrW8FNB1FOQFMaIq-CW8hMIoBqtZMXYR05UA7Lu/pub?output=csv'
ideol = fread(ideology_sheet)
subs = str_split(ideol$epaStandIn,', ')

projs$index = match(projs$Lead.Agency,ideol$epaName)
projs = data.table(projs,ideol[unlist(sapply(projs$Lead.Agency,function(x) c(grep(x,ideol$epaName),grep(x,ideol$epaStandIn)))),])

projs = data.table(left_join(projs,projs[,list(mean(skills_rating),mean(ideo_rating)),by=.(AGENCY)]))
projs$DEC_DATE = decimal_date(mdy(projs$Federal.Register.Date))-2013

projs$ABBREV[projs$AGENCY=='Nuclear Regulatory Commission'] <- 'NRC'
projs$ABBREV[projs$AGENCY=='Department of Defense'] <- 'DoD'
projs$ABBREV[projs$AGENCY=='Department of Commerce'] <- 'DoC'
projs$ABBREV[projs$AGENCY=='Bureau of Indian Affairs'] <- 'BIA'
projs$ABBREV[projs$AGENCY=='Department of Housing and Urban Development'] <- 'HUD'
projs$ABBREV[projs$AGENCY=='U.S. Army Corps of Engineers'] <- 'ACOE'
projs$ABBREV[projs$AGENCY=='Federal Highway Administration'] <- 'FHWA'
projs$ABBREV[projs$AGENCY=='Federal Energy Regulatory Commission'] <- 'FERC'
projs$ABBREV[projs$AGENCY=='General Services Administration'] <- 'GSA'
projs$ABBREV[projs$AGENCY=='Department of Health and Human Services'] <- 'HHS'
projs$ABBREV[projs$AGENCY=='Department of Homeland Security'] <- 'DHS'
projs$ABBREV[projs$AGENCY=="Fish and Wildlife Service"  ] <- 'FWS'
projs$ABBREV[projs$AGENCY=="National Park Service"  ] <- 'NPS'
projs$ABBREV[projs$AGENCY=="Forest Service"  ] <- 'FS'
projs$ABBREV[projs$AGENCY=="Bureau of Reclamation" ] <- 'BR'
projs$ABBREV[projs$AGENCY=="Department of Interior (other)" ] <- 'DoI (other)'
projs$ABBREV[projs$AGENCY=="USDA (non-FS)" ] <- 'DoA (other)'
projs$ABBREV[projs$AGENCY=="Department of Transportation (other)" ] <- 'DoT (other)'
projs$ABBREV[projs$AGENCY=="Tennessee Valley Authority"  ] <- 'TVA'
projs$ABBREV[projs$AGENCY=="Bureau of Land Management" ] <- 'BLM'
projs$ABBREV[projs$AGENCY=="Department of Energy" ] <- 'DoE'
projs$ABBREV[projs$AGENCY== "National Oceanic and Atmospheric Administration"  ] <- 'NOAA'


projs[is.na(PROJECT_TYPE),]$Title
table(is.na(projs$PROJECT_TYPE),is.na(projs$PROJECT_TOPIC))

figure1 = ggplot(projs[!duplicated(AGENCY),]) + 
  geom_hline(yintercept = projs[ABBREV=='TVA']$V2[1],lty=2)+
  geom_label(x = -1,y = projs[ABBREV=='TVA']$V2[1],label='TVA')+
  geom_point(aes(x = ideo_rating,y = skills_rating)) + 
  geom_text_repel(aes(x = ideo_rating,y = skills_rating,label = ABBREV)) + 
  theme_bw() + xlab('Ideological rating (liberal < conservative)') + ylab('Workforce skill (less < more)') + 
  ggtitle('Ideology and workforce skill by agency','(using Richardson et al. 2018 scores)') +
  labs(caption = "*TVA missing ideology score")

dir.create('climate_in_eis_project/output/')
ggsave(plot = figure1,filename = 'climate_in_eis_project/output/figure1.png',dpi = 350,width = 5,height = 4.5, units = 'in')

saveRDS(projs,paste0(scratch_loc,'deis_metadata_with_covariates.RDS'))
