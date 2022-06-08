library(lubridate)
library(data.table)
library(tidyverse)
fdt = fread('input/epa_master_repository/eis_record_detail.csv')
fdt = fdt[fdt$Document.Type=='Final',]
fdt = fdt[!grepl('WITHDRAWN|ADOPTED|ADOPTION|CANCELLED|VOIDED|Withdrawn',fdt$Title),]
fdt$Year = year(mdy(fdt$Federal.Register.Date))
fdt$Small_Sample = fdt$Year %in% 2013:2018
fdt$Have_EIS_Document = paste0('ES_',fdt$EIS.Number,'.txt') %in% list.files('input/eis_es_text/')


fdt %>% filter(Small_Sample) %>% group_by(Agency,Have_EIS_Document) %>% summarise(co = n()) %>% spread(Have_EIS_Document,co) %>% 
  mutate(`FALSE` = replace_na(`FALSE`,0),`TRUE` = replace_na(`TRUE`,0)) %>% mutate(total = `TRUE` + `FALSE`) %>% arrange(-total) %>%
  mutate(obs_prop = `TRUE`/total)
 
fdt$Area = NA
fdt$Area[grepl(paste(paste0('\\b',c('Ski','Resort','Golf','Boating','Marina','Guest Ranch','Snowmobile','Recreation Complex','Entertainment Arena','Trail Easement',
                                    'Amusement Park','Cemetery','Museum','Stadium','Slalom Venue','Horse Racing','Interpretive Center','Convention Center',
                                    'Arts and Entertainment','National Historical Park Management Plan','National Monument Management Plan','Trail Project',
                                    'Trails Management Plan',
                                    'Guide Operations','Visitor Center','Off-[Rr]oad Vehicle Management Plan','Dog Management Plan','Trail Improvement',
                                    'Trail Construction','Trails Project','Subsistence Hunt','Trail System','Snowbowl Expansion','Forest Road Special-Use-Permit for Access',
                                    'Trail Modification Project','Vessel Quotas','Personal Watercraft','Visitor Use and Facilities','Access Project',
                                    'Recreational Development','Helicopter Landings','Recreational Trails','Motorized Trail','National Trail Study','Permanent Tribal Land',
                                    'Vehicle Management Plan','Restore National Mall','Natural and Cultural Resource','New Concession Contract','Tennis Center',
                                    'Shoreline Outfitter','Snowbowl Facilities Improvements','Backcountry Recreation Area','Motorized Vehicular Access',
                                    'Recreation Master Plan','Visitor Services Plans','Amphitheatre','Motorized Travel Plan','Trail Management Plan','Visitor Access',
                                    'Recreation Residences','Gullah Culture Preservation','Multi-Use Trails System','Over-Snow','National Memorial  Designation',
                                    'Park Comprehensive Design Plan','Heritage Center',"Park Project  Construction and Operation",'Authorize Harvest',
                                    'Recreational River and Greenbelt Area','Temporary Work of Art','South Denali Implementation Plan','backcountry Management Plan',
                                    'Provide a Public Recreational Area','Enhancing the Recreational Experience','Recreation Enhancement Project','Roads and Trails Systems Development',
                                    'Subsistence','Multi-Season Recreation','Designated Routes Project','Parking Improvements','Reestablish Road Access','Mountain Improvements Project',
                                    'Facility Development Plan','Development and Use','Use of Marine Mammals',   'Subsistence Harvest','Trail Modifications',
                                    "Heavenly Epic Discovery Project"  ,'Hoodoo Master Plan','Outfitter/Guide','Designated Routes and Areas for Motor Vehicle Use',
                                    'Pack Stock Management','Recreation Development','Recreation Area Management Plan','National Scenic Trail','Campaign Trail',
                                    'Guide Permit Issuance','Stock Permit Reissuance','Stock Outfitter','Recreational Lake and Complex','Winter Use Plan',"Golden Peak Improvements 2016",
                                    'Hikers','Hiking','Equestrian','Recreational Use','Recreational Opportunities','Skiing','Tours','Historic'),'\\b'),collapse='|'),fdt$Title)] <- 'Recreation/Culture'
fdt$Area[is.na(fdt$Area) & fdt$Agency=='National Park Service' & grepl('Development Concept Plan|Development Concept Plans',fdt$Title)] <- 'Recreation/Culture'



fdt$Area[grepl(paste(paste0('\\b',c('Levee','Lock','Reservoir Expansion','Dam Raise','Groundwater Development','Dam Safety','Dam and Reservoir',
                                    'Floodwater Retarding','Reservoir Project','Groundwater Storage','Sediment Diversion','Flood Control','Water Collection System',
                                    'Flood Protection Plan','Flood Risk Management Project','Flood Protection Project','Water Control Manual','Clean Water Coalition Systems',
                                    'Delta Islands and Levees',
                                    'Local Flood Protection','Flood Plain Protection Project','Water Storage Reallocation','Flood Risk Management Projecct',
                                    'Southport Sacramento River Early Implementation Project','Interbasin Transfer Project','Improve Existing Water Storage',
                                    'Retention of Floodwaters','Dam Outlet','Desalination Plant','Wastewater Treatment','Multi-Purpose Water Development',
                                    'Groundwater Replenishment','Flood Remediation','Water Quality and Reliability','Dam and Reservior','Reservoir Operating Plan',
                                   'Dam Project','Flow Corridor','Reservoir Enlargement','Nonpoint Pollution Control','Water Treatment Residuals Management',
                                    'Erosion Control','Water Storage Project','Water Delivery System','Wastewater','Flood Prevention','Water Supply Reservoir',
                                    'Flood Control Project','Flood Damage Reduction','Concrete Dam','Water Expansion','Aqueduct System','Flooding and Water Resource',
                                    'Streambed Alteration','Adequate Water Supplies','Clean Water Program','Sediment Removal Project','Restoration of Water Storage',
                                   'Newlands Project  Operating Criteria','Musselshell Analysis Area  Implementation',"North Fork Wells of Eagle Creek" ,
                                    'Central Valley Project','Canal Lining','Pump Station Project','Drawdown Project','Water Level Management Plan','Erosion Response Project',
                                    'Dam Continued Operation and Maintenance','Water Conservation and Supply Study','Flood Reduction Plan','Response to Seepage',
                                    'Water Conservation and Supply','Stormwater Treatment Areas','Flood Damage Prevention Study','Flood Risk Management',
                                    'Integrated Recharge and Recovery','Increase Flood Protection','Watershed Management Program','Northern Integrated Supply Project',
                                    'Agricultural Drainage','Water Surplus','Dam Repair and Modification','Water Right Acquisition','Valley Water Project',
                                    'Canal Maintenance','Pumping Plant','Water Transfers','Reservoir Storage','Reservoir and Dam','Stream Maintenance and Management Plan',
                                    'Basewide Water Infrastructure','Tertiary Treatment Plant','Sediment Management Plan','San Joaquin River Agreement Project',
                                    'Bay-Delta Program','Dam Component','Water Resources Investigation','Drainage Area','Sewage Effluent','Shoreline Stabilization',
                                    'Municipal Watershed Project','Reservoirs Stabilization','Dam Reconstruction','Commercial Nuclear Fuel','Locks and Dams',
                                    'Water Rights Acquisition','Inadvertent Overrun','Alternative Intake Project','Water Operations Review','Creek Reservoir','Conveyance and Disinfection System',
                                   'Sustainable Water Project','Long-Term River Management','Rural Water','Stream Protection Rule','Aqueduct and Collection System',
                                    'Lower Basin Shortages','Instream Flow','Water Storage Feasibility','Aquifer Storage','Spillway Replacement','River Basin 201 Facilities Plan  Upgrading and Expansion',
                                    'New Water Storage Reservoir','Groundwater Basin','Modifying Water Flow','Long-Term Excess Capacity','Water Production Facilities',
                                    'Pool Bypass','Pure Water San Diego Program','Water Quality Improvement Program','Watershed Remedial Plan','River Common Features General Reevaluation Report',
                                    'Stormwater Drainage Systems','Canal Reconstruction Project','Diversion Rehabilitation','Hydrologic Manipulation','Sewer Project','Waste Water Discharges',
                                    'Shoreline Management Initiative','Conjunctive Use Project','Watershed Project','Watershed Plan','Joint Water Pollution Control Plant',
                                    'Reservoir Operations Study','Reservoir System Improvement','Dam Leakage Resolution','Agricultural Area Reservoir','Non-Potable Water Distribution',
                                   'Freshwater Diversion Project','Water Resources Study','Moffat Collection System Project','West Sacramento Project','Jenning Randolph Lake',
                                   'Area Flood Risk Management','Reduction of Flood Damages','Flood Protection','Hurricane Protection','River System Operation Review',
                                    'Recycled Water Program','Regional Water System','Operating Agreement for the Rio Grande Project','Shallow Flow Equalization Basin',
                                    'Dam Operation  Implementation','Water Recycling','Safety of Dams','Dam and Lake Project','Agricultural Water','Drainage Basin Improvement',
                                    'River Operating','Dike','Floodway','Water Supply','Reservoirs Maintenance','Irrigation','Emergency Water Storage Project','San Joaquin River Basin Lower San Joaquin River',
                                    'Water Transfer'),'\\b'),collapse='|'),fdt$Title)] <- 'Water Works'
fdt$Area[is.na(fdt$Area) & fdt$Agency=='Forest Service' & grepl('Reservoir',fdt$Title)& grepl('Construction',fdt$Title)] <- 'Water Works'


fdt$Area[grepl(paste(paste0('\\b',c('Transmission','Energy','Power Plant','Pipeline','Hydroelectric','Hydropower','Power Station','Hydro Project','Cogeneration',
                                    'Gas System','Nuclear Plant Conversion','Turbine Plant','Refinery','Electric Generating Plant','Cooling Water','Windplant',
                                    'Lassen Lodge','Liberty Development and Production Plan','TRANSMISSION','Pipelines Project','GasPort','Demand for Power','West of Devers Upgrade Proect',
                                    'Nuclear Power Plants Operating Licenses','Decommissioning of Nuclear Facilities','Early Site Permit','Amoco Carbon Dioxide Projects',
                                    'Integrated Gasification Combined-Cycle (IGCC) Project','Resource Contingency Program  Construction and Operation','Ocotillo Sol Project',
                                    'Renewal of the Operating Licenses','Generating Station','Substation','Intertie','Power Marketing Program','Shutdown of the River Water System',
                                    'Savannah River Site  Continued Operation','Powerline','OIL AND GAS DEVELOPMENT','Exploratory Well Drilling','Delivery of the Canadian Entitlement',
                                    'Electric Generating Station','Clinch River Nuclear Site','Decommissioning of NRC-Licensed Nuclear Facilities','Integrated Gasification',
                                    'Power Sale Contract','Power Sales','Electric Power Requirements','Clean Power Link','FutureGen','Voltage Support Project',"Riley Ridge to Natrona" ,
                                    'Liquefaction','Electrical Line Upgrade','Nuclear Plant Units','Nuclear Station Units','New Nuclear Reactors',"Nanushuk Project" ,
                                    'Right-of-Way Grant and Drilling Permit','Geological and Geophysical Activities','Generation Station','Floating Production  Storage and Offloading Systems',
                                    'Pipe Line Easement','Combustion Turbines','Methanol','License Renewal of Nuclear','Power Reactor','Electric Reliability Plan',
                                    'Petroleum','Power Line','Geothermal',"Solar","Wind",'Coal','Oil','Gas',"LNG"),'\\b'),collapse='|'),fdt$Title)] <- 'Energy'
fdt$Area[grepl('Nuclear Plan',fdt$Title)&grepl('Renewal|Operating License',fdt$Title)] <- 'Energy'
fdt$Area[is.na(fdt$Area)&fdt$Agency=='Federal Energy Regulatory Commission'] <- 'Energy'

fdt$Area[grepl(paste(paste0('\\b',c('Mine','Mining','Underground Mines','Tailings Impoundment','Uranium Project','Gold Project','Greater Phoenix Project','Uranium Mill Trailings',
                                    'Mineral Withdrawal','Hardrock Mineral','Tailings Disposal','Uranium Recovery','Leach Uranium','Mineral Material Sales','Uranium Leasing',
                                    'Trailings Reclamation','Phosphate Leasing','Uranium Mill Tailings','Copper Leach Project','In-Situ Recover','Federal Mineral Estate','Locatable  Mineral Operations',
                                    'Sodium Minerals'),'\\b'),collapse='|'),fdt$Title)] <- 'Mining'

fdt$Area[grepl(paste(paste0('\\b',c('Drug','Illegal','Correctional Facility','Detention Facility','Correctional Institution','Detention Center','House Felons','Northern Border Activities',
                                    'Correctional Complex','Penitentiary','Prison','Justice','Correction Complex','Courthouse','Detainees','Private Correctional Facilities',
                                    'Automatic Identification System Project','Juvenile Hall Expansion','Local Emergency First Responders','Joint Task Force',
                                    'Border Infrastructure','Border Facility','Access Restriction','Biodefense Analysis and Countermeasures Center',
                                    'Border Station'),'\\b'),collapse='|'),fdt$Title)] <- 'Law Enforcement/Security'

fdt$Area[grepl(paste(paste0('\\b',c('Highway','US-[0-9]{1,3}','I-[0-9]{1,3}','SR [0-9]{1,3}','S\\.R\\. [0-9]{1,3}','U\\.S\\. Route','US Route','US [0-9]{1,3}','Interstate [0-9]{1,3}','Bridge','Rapid Transit','Bridge Plaza',
                                    'Interstate Corridor','Rail','Train','Airport','Turnpike','Terminal Expansion','Traffic','Ferry','Expressway',
                                    'Port Everglades Expansion','Road Widening','Railway','Road Closure','Freeway Gap','Barge Terminals','Harbor Project','Wilmington Harbor Long-term Plan',
                                    'Container Terminal','Bridges','Intermodal Transit Center','Freeway Improvements','Road Extension','Parkway','Icebreaking Operation',
                                    'Road Improvement Project','Deep-Draft Terminal','River Crossing','Interchange','Trafficway','Main Track','Road Project','Casotte Channel',
                                    'Bypass Corridor','Interchange Project',paste0(state.abb,'-[0-9]{1,3}'),'Mass Transit','Corridor Location','Transfer Station',
                                    'Link Extension','Metrorail','Tunnel Reconstruction','Bypass Improvements','Tunnel','Harbor Deepening','Streetcar Line Reintroduction',
                                    'Freight','Harbor Post','Intermodal','Multimodal','Pier Replacement','Road Relocation','Marine Terminal Complex','Transit Project',
                                    'Wharf Extension','Six County Association of Governments Construction and Operation',
                                    'Arterial Travel Route','Realignment and Reconstruction','Anchorages and Channels','Dams Project','Track Project','Road Realignment',
                                    'Service Pier Extension at Naval Base', 'Naval Weapons Station Earle Trestle Replacement','High-Capacity Transit','Great Lakes and Mississippi River Interbasin Study',
                                    'Aircraft Flight Patterns','Licensing Launch Vehicles','Radar Approach Control Facility','Channel Maintenance Plan','Aggregate Terminal Project',
                                    'Ship Channel Improvement','Small Boat Harbor Facility','Cruise Terminal Project','Container Handling Facility','Permanent Terminal',
                                    'Public Port Facilities','Channel Improvement','Navigational Safety Improvements','Port of Gulfport Expansion','ix County Association of Governments Construction and Operation',
                                    "Coast Corridor Improvements" ,'Railroad Relocation Planning','Union Station Run-Through Tracks','Railline','Metro System','Subway Improvement',
                                    'Deepwater Port','Dredging','Dredged','Navigation','Road Construction','Railroad Line','Freeway Bypass','Railroad Project','Connecting Channels',
                                    'Transportation','Travel Management','Transit System','Transit Improvements'),'\\b'),collapse='|'),fdt$Title)] <- 'Transportation'

fdt$Area[is.na(fdt$Area)&fdt$Agency%in%c('Federal Highway Administration','California Department of Transit','Federal Transit Administration')&grepl('North|South|East|West|Reconstruction|Relocation|Interstate|Improvement|Right-of-Way|Arterial|Loop|Crossing|Section|Road|Widening|Avenue|Route|Corridor|Bypass|Boulevard|Viaduct|Road Realignment|Seismic Improvement|Road Improvement|Freeway|Extension|Connector|Roadways',fdt$Title)] <- 'Transportation'

fdt$Area[grepl(paste(paste0('\\b',c('Laboratory','Research','Experimental','Observatory','Rocket','Medical Isotopes',
                                    'Launch Site','Engine Technology Support','Pilot Project','Astrophysical','Prototype Project',
                                    'Advanced Science and Technology','Space Launch Complex','Temperature Control Study','MARS Exploration',
                                    'National Bureau of Standards Reactor','Medical Radioisotope','Prototype Rubber Extraction',
                                    'Doppler Weather Radar','Antarctica','Scientific Ocean Drilling',
                                    'Genetic Resource Center','Antarctia','Antartica',
                                    'Advanced Technology Demonstrator','Mars Exploration','Space Station Freedom','Galileo Mission',
                                    'Mission to Pluto','Advanced Radioisotope Power Systems','Constellation Program','Spacecraft Exploration','Rights and Responsibilities of Researchers',
                                    'Super Collider','Environmental Technology and Engineering','Reentry Vehicles','Space Nuclear','Productivty Study',
                                    'Laboratories'),'\\b'),collapse='|'),fdt$Title)] <- 'Science/Research'

#'Navy','Naval','Army','Marine Corps','Air Force',

fdt$Area[grepl(paste(paste0('\\b',c('Combat','Range Complex','Garrison Support','Military Training','Deployment',
                                    'Wing  Relocation','Wing Relocation','Missile Wing Deactivation','Training Complex','Army Transformation',
                                    'Gunnery Range Expansion','Beddown','Airspace Training Initiative','Fighter Aircraft','Advanced Amphibious Assault Vehicle',
                                    'Composite Wing Establishment','Range Safety Zones','Airfield Operations','Swimmer Interdiction',
                                    'Range Operations and Training','Operational Efficiency','Military Operations Area','Fighter Squadrons',
                                    'Test and Training Range','Aircraft Conversion','Enhanced Training Project','Training Range Development',
                                    'Radar System','Fleet','Battle Course','Special Operations','Homeporting Facilities','Pinecastle Range',
                                    'Military Readiness','Bombing Range','Towed Array','Missile Range','Undersea Warfare','Structure Realignment',
                                    'Training Site','Strategic Defense Initiative','Annual Training Facilities','Battle Area Complex','Training Mission and Mission Support',
                                    'Integrated Deepwater System Project','Aerial Gunnery Range','Battlefield Protection','Ground-Based Midcourse Defense',
                                    'Stationing and Training','Strategic Target System','Modernization and Repair','Military Operations Areas','Shock Trail','US Nuclear Weapon Complex',
                                    'Structures Realignment','Training and Operations','Maneuver Center','Training Complex  Construction','Presidential Aircraft Recapitalization Program',
                                    'Decommissioning of the Ballistic Missile Defense System','Navy Training Activities','Military Relocation','Nevada Test Site','Regional Airspace Strategic Initiative',
                                    'Improve Mission Effectiveness','Air Force Reserve Mission Change','Intelligence  Surveillance','Conducting a Shock Tril','Training Initiative',
                                    'Microwave Landing System','Military Operational Increases','Testing and Training','Increased Flight','Cutter Acquisition','Activities and Exercises',
                                    'Ford Island Development Program','Submarine','ROTHR','Electromagnetic Pulse Radiation','Electronic Warfare','Military Operating Area',
                                    'Homeporting','Defense Deployment','Basing','Proving Ground','Explosive Ordnance','Training and Testing','Relocation of the.+Training Unit',
                                    'Extended Test Range', 'Missle Range', 'Southeast Alaska Acoustic Measurement Facility','National Guard Training Center','Mission.+Termination',
                                    'Training Areas and Facilities Upgrading'),'\\b'),collapse='|'),fdt$Title)] <- 'Military Operations'

fdt$Area[grepl(paste(paste0('\\b',c('Fishery Management','Rangeland Management','Vegetation','Watershed Improvement','Total Allowable Catch',
                                    'Forest Health Improvement','Poor Forest Health','Canyons Analysis Area','Rangeland Allotments',
                                    'Forest Management','Fuel Reduction','Reduce Fuels','Hatchery','Land Exchange','Land and Resource Management Plan',
                                    'Timber Harvest','Timber Harvesting','Wildfire Salvage','Fire Salvage','Fuels Reduction','Post Fire Hazard Reduction',
                                    'Rangeland Analysis','Insect & Disease','Fuelbreak','Burning Project','Salvage Project','Reforestation',
                                    'Resource Area Management Plan  Implementation','Land and Mineral Management Plan','Use of Domestic Sheep',
                                    'Multiple Use Management Plan  Implementation','Fire Management Plan','Fuels and Wildlife Project',
                                    'Rangeland Ecosystem Management','Nursery Pest Management','Vegetative Management','Annual Catch Limits',
                                    'Forest Plan Amendment','Forest Health Project','Fuel Treatment','Gypsy Moth','Improve Forest Health',
                                    'Resilient Landscapes Project','Landscape Restoration','Fuels Management',
                                    'Landscape Area Treatments','Integrated Resource Project',
                                    'Post-Fire Project','Seed Orchards Pest Management Plan','Salvage Sale','Harvesting Dead and Damaged Timber',
                                    'Salvage Harvest','Forest Health Project','Harvest Trees','Harvest Merchantable Timber','Forest Health',
                                    'Maintain Vegetative Diversity','Pest Control Management','Forest Plan Revision','Fires Value Recovery',
                                    'Harvest Timber','Implement Silvicultural Activities','Wildfire Recovery Project','Pest Management Plan',
                                    'White-tailed Deer Management Plan','Fire Management  Plan','National Battlefield','Coastal Program',
                                    'Goose Management Plan','Fishery','Coastal Management Program','Coastal Zone Mgmt. Program','White-Tailed Deer Management Plan',
                                    'Carcass Management','Landscape Restoration Management','Spruce Beetle Epidemic','Multi-Resource Project',
                                    'Coastal Management Program','Coastal Zone Management','Fishery  Management Plan','Salvage and Treat Down and Damaged Timber',
                                    'National Marine Sanctuary Management Plan','Boundary Expansion','National Marine Sanctuary Management Plan',
                                    'National Marine Sanctuary  Establishment','National Marine Sanctuary  Implementation','Fire Recovery and Associated Activities',
                                    'National Marine Sanctuary Establishment','Reef Fish Amendment','National Marine Sanctuary  Management Plan',
                                    'National Marine Sanctuaries Expansion','Comprehensive Ecosystem-Base Amendment 1','Harvest and Old Growth Regeneration',
                                    'National Marine Sanctuaries  Proposes','Fishing Regulations','Resources Management Plan  Implementation',
                                    'Acreage Limitation','Natural Resource Plan','Floodplain Buyout','Mountain Beetle Epidemics','Fuel Loading Reduction',
                                    'Area Management Plan  Implementation','Fuel Breaks','Defensible Fuel Profile Zones','Comprehensive Management Plan','FishRefuge Master Plan',
                                    'Forest  Land and Resource Mgmt. Plan','Grasshopper Cooperative Mgmt. Program','Cricket Suppression Program','Monument General Management',
                                    'Boll Weevil Cooperative Control Program','Forest Recovery Project','Salvage Trees and Rehabilitate Lands','Land Acquisition',
                                    'Revised Operation and Development Plan','Exchange Federal and Non-Federal Lands','Storm Damage Protection',
                                    'Watershed Protection and Flood Plan','\\(NRAA\\) Comprehensive Management Plan','National Park Management Plan','Landscape Management Program',
                                    "Adaptive Management Area Plan",'Multiple Use Management Project','Multiple-Use Management Projects','Forest Restoration','Fire Retardant Project',
                                    'Grasslands  Land and Resource Mgmt. Plan','National Marine Sanctuary  Designation','Shoreline Management Plan','Land Change Project',
                                    'National Wildlife Refuge  Implementation','Land and Resources Management Plan','Landscape and Watershed Assessment Project',
                                    'National Marine Sanctuary Comprehensive Management Plan','National Marine Sanctuary Final Management Plan','Achieve and Maintain Desired Conditions',
                                    'Post Burn Management','Timber Salvage','Salvage Project','Fire Recovery Project','Salvage Timber','Pine Beetle','Tussock Moth',
                                    'National Seashore Final General Mangement Plan','White-Tailed Deer Management  Plan','UPPER DELAWARE SCENIC AND RECREATIONAL RIVER',
                                    'Recreation Area  Comprehensive Management Plan','Proposed Land Use Plan Amendments','Land Use Plan Amendment','Rangeland Project',
                                    'Reservoir Area Management Plan','National Preserve Public Access','Invasive Plant','Invasive Species',
                                    'Resource Area Management Plan','California Desert Conservation Area Plan Amendments   Implementation',
                                    'National Monument and Preserve Proposed Management Plan Amendment',
                                    'Beach Nourishment Plan','BEACH EROSION CONTROL PROJECT', 'Coastal Storm Risk','Weed Pest Management Plan','Final Hazardous Fire Risk Reduction',
                                    'Integrated Treatment of Noxious and Invasive Weeds','Noxious Weed','Invasive Weed','Infestation','Coastal Improvements Program','Harvest Specifications',
                                    'North Fork St. Joe River Project','Access Route on East Mountain','Protective Berm and Dune Construction','Recreation Area Master Plan',
                                    'Implementation for the Future Management of Land and Water Resource','Martin Run Project','Shoreline Restoration','Recreation Area Resource Protection Study',
                                      'Watershed/Fire Recovery','Vegetative Treatments','Burned Area Management','Land Tenure Adjustments Project','Resources Management Plan','Management Plan  Implementation',
                                    'Reservation Protection Project','Multi-Purpose Project','Recreation Management Area Plan','Storm Damage Risk Reduction','Shoreline Management Project',
                                    'Storm Damage Reduction','Hurricance Protection Project','Shore Protection','Bank Protection','Shoreline Protection Project','Offshore Breakwaters','Hurricane Risk Reduction Project',
                                    'Prescribed Burning','Prescribed Fire','Wildfire Protection Project','Thin Project','Land Transfers','Roadless Areas Rulemaking','Hurricane and Storm  Damage Reduction',
                                    'Forest Plan Riparian Amendment','Sevier River Resource Area','Ecosystem Analyses Area  Amendment','Forest Plan Amendments','Land Transfer','Breach Management Plan',
                                    'Harvesting Program','Landscape Level Analysis Project','Forest Resiliency Project','Achieve and Maintain Desired Condition','Fill of Wetlands','General Management and Develop Concept Plans','Development  Use and Management Plan',
                                    'Shoreline Phase I','Beach Shoreline Management Project', 'Shoreline Restoration and Infrastructure Protection','Integrated Resource Project','SurgeProtection',
                                    'Habitat Preservation Area','National Wildlife Refuge Master Plan','Initial Stewardship Plan','Final CCP','REFUGE MASTER','Wildlife Refuge Act','Center Master Plan',
                                    'Riverway Cooperative Management  Plan','Recreation Area  Lake Management Plan','Rural Landscape Management Program ','Heritage Area Management Action Plan',
                                    'General Management Plan','Allotment','Fisheries','Grazing','Integrated Pest Management','Forest Inventory','Harvesting Timber','Fuels Reduction','Management of Burros',
                                    'National Monument Management  Plan  Implementation','Floodplain Area Study','Special Area Management Plan','Update and Consolidate Management Plans',
                                    'Timber Sales','Timber Sale','Land Management','Resource Management'),'\\b'),collapse='|'),fdt$Title)] <- 'Land/Resource Management'
fdt$Area[grepl('Management Plan  Implementation|Travel Plan|Management Project|Restore and/or Maintain|Management Direction  Implementation',fdt$Title) & fdt$Agency=='Forest Service'] <- 'Land/Resource Management'
fdt$Area[grepl('Rocky Mountain Arsenal National Wildlife Refuge|San Luis Valley National Wildlife Refuge Complex',fdt$Title) & fdt$Agency=="Fish and Wildlife Service"] <- 'Land/Resource Management'

fdt$Area[fdt$Title %in% c("East Hills Project" ,"HiLo Project"  ,'Galena Project',"Big Thorne Project","Cedar-Thom Project" ,"Trout Creek",
                          "North Savery Project" ,"Plan Revision for the Coconino National Forest","Green Mountain Project" ,"Tenmile - South Helena Project" ,
                          "Starry Goat" ,"Buckhorn Project","Beaver Creek",'Wrangell Island Project',"Beasley Pond Analysis Area"  ,"Hwy 46",
                          "North Savery Project","Lower Yaak, OBrien, Sheep Project","Ringo FEIS & FPA",
                          "Jess Project" ,"Montanore Project" ,"Goose Project","Lakewood Southeast Project"  ,"Magone Project" ,"Townsend Project",
                          "Rim Fire Recovery","Little Boulder",'1986 - 2000 Nantahala and Pisgah National Forests', "Southwest Coastal Louisiana" ,"Sabine Pass to Galveston Bay"   )] <- 'Land/Resource Management'

fdt$Area[grepl(paste(paste0('\\b',c('Conservation Plan','Habitat Conservation','Habitat Restoration','Protected Area',"Wilderness Area",'Wild and Scenic',
                                    'Ecosystem Restoration','Critical Habitat','Aquatic Habitat','Wetlands Project','Elk Management Plan',
                                    'Lagoon Enhancement','Fire Restoration','Ecosystem Management Project','Restoration Project','Restoration Planning Program',
                                    'Roadless Area Conservation','Goat Management Plan','Presence of Wolves','Ecological Restoration Plan','Grizzly Bear Recovery',
                                    'Wilderness Management','Ecosystem Rehabilitation','Restore Historical Pre-1850 Forest Conditions','Coral Restoration',
                                    'Whale Take Reduction Plan','Marine Mammal Health and Stranding','Implementing Conservation and Management','Wilderness Designation',
                                    'Wilderness Study','Restoration of Native Species','Restoration and Management','No-Take','Ship Strike','Remote Vaccination','Fish and Wildlife Mitigation and Recovery',
                                    'Restoration Enhancement','Invasive Plants','Northern Goshawk','Spotted Owl','Watershed Rehabilitation','Restoration of the Mariposa Grove',
                                    'Fish Passage Improvement Project','Fish Passage Project','Outflow Temperature Control','Ecosystem Recovery Project', 'Sparrow Protection',
                                    'Habitat Enhancement Project','Oyster Restoration','Aquatic and Terrestrial Restoration','Species Conservation Habitat','Restoration of Fish and Wild Habitat',
                                    'Wildlife Habitat  Improvement','Ecosystem Management','Fish Screen and Habitat Improvement','Wilderness Recommendation','Emergent Sandbar Habitat',
                                    'Comprehensive Conservation Mgmt. Plan','Wilderness Review','Ecosystem  Restoration Feasibility Study','Wetland Restoration','Central Everglades Planning Project',
                                    'Fish Management Plan','Benefit Native Species','Salmon Flow Measures','Environmental Restoration','Restoration and Conservation','Habitat Project',
                                    'Enhancement of Threatened and Endangered Species','Reserve Enhancement Project','Dry Forest Restoration','Recovery Implementation Program',
                                    'Salmon Restoration Activities','Reintroduction','Invasive Spartina','Reduce Predation','Double crested Cormorant','Eagle Rule Revision',
                                    'Creek Restoration','Tamarisk Removal','Klamath Facilities Removal','Repopulation','Tansy Ragwort Control','Riparian and Aquaatic Habitat Management',
                                    'National Wildlife Refuge Management Plan','Wildlife Refuge Establishment','Estuary Restoration','Eradication','Diverse Habitat','Coho Restoration Program',
                                    'Wilderness Recommendations','Conservation Management Plan','Endangered Species Management and Protection Plan','Missouri River Recovery Management Plan',
                                    'Ballast Water Discharge','Marine Protected Species','Protected Living Marine Resource','Protect Adult Salmon','Basin Restoration Program',
                                    'Wilderness Stewardship Plan','Conservation Reserve Program Implementation and Expansion','Ensure Viability','Management of Desert Tortoise Habitat',
                                    'Water Enchancement','Grassland Bypass Project','Long-Term Water Service Contract','Grizzly Bear Conservation','Mountain Plover','Wolf River Ecosystem',
                                    'Valley Rehabilitation','Watershed Protection Project','Winter Elk Management','Prairie Dog Conservation','Wildlife Mitigation Program',
                                    'Wetland Restoration Project','Non-Native','Watershed Protection Plan','Central and Southern Florida Project',
                                    'Restoration Plan','Natural Resource Damage Assessment','Monk Seal Recovery','Management and Recovery',
                                    'Migration','Conservation Program','Riparian Restoration','Breach Restoration'),'\\b'),collapse='|'),fdt$Title)] <- 'Conservation/Restoration'

fdt$Area[grepl(paste(paste0('\\b',c('Solid Waste','Hazardous Waste','Nuclear Waste','Material Disposal','High-Level Waste','Depleted Uranium','Uranium Enrichment',
                                    'Disposal of Stockpiled Chemical Agents','Disposal of Chemical Agents','Tritium','Neutralization','Spent Nuclear Fuel','Contaminated Material Cleanup',
                                    'Waste Management Facilities','Stockpile Stewardship','Interim Management of Nuclear Materials','Disposition of Surplus Weapons',
                                    'Nuclear Service Center Decommissioning','Processing and Storage Highly Enriched Uranium','Fernald Environmental Management','Mercury Management Project',
                                    'Mixed Oxide','In Situ Recovery Project','Sequoyah Fuels Corporation Site','Destruction of Chemical Munitions','Fissile Materials',
                                    'Dry Cargo Residue','Ash Impoundment Closure','Chemical Weapons Destruction Technologies','Plutonium Production Reactors Decommissioning',
                                    'Safe Interim Storage','Waste Remediation Systems','Waste Management Project','Remedial Action','Reduced Radiation Exposure','Non-Stockpile Chemical',
                                    'Plutonium Residues','Gasification Project','Biorefinery Project','Sequestration Project','Mercury Storage Project','Cleanup of Radioactive Contaminated Material',
                                    'Placement of Fill Material','Stockpile Disposal','Cleanup Program','Metal Recycling Facility','By-Product Recovery Plants','Storage of Nuclear',
                                    'Disposal Site','Dredge Material Management Plan','Low-Level Radioactive','Tank Wastes Disposal','Plutonium Solutions Stabilization',
                                    'Landfill','Waste Disposal'),'\\b'),collapse='|'),fdt$Title)] <- 'Waste Storage/Disposal'

fdt$Area[fdt$Agency == 'Community Development Block Grant'|grepl(paste(paste0('\\b',c('Business Development','Disposal and Reuse  Implementation','Property Exchange',
                                      'Development Project','Business Park Development', 'Redevelopment','Rezoning','Motel-Restaurant','Land Use and Development Plan',
                                      'Road Access Requests','Property Master Planning','Real Property Master Plan','Housing Privatization','Emission Standards',"Emissions Standards",
                                      'Low Standard Road Access','Master Development Plan','Road Easement','Convey an Easement','Easement','Mertarvik Infrastructure Development',
                                      'Crop Assistance Program','Emergency Funding to Farmers and Ranchers','Replacement of Damaged Public Schools',
                                      'Standards.+Human Consumption',
                                      'National Flood Insurance Program','Resource Recovery Park','Fuel Efficiency Standards','Corporate Average Fuel Economy',
                                      'Incidental Take','Coastal and Social Resiliency','Rebuild by Design','HOPE SF Master Plan','Seawater Air Conditioning Project',
                                      'Apartment Complex Development','Plaza Development','Proposed land Use Development','Mixed-Use Commercial Development',
                                      "Mather Specific Plan Project","Elverta Specific Plan Project"  ,"Placer Vineyards Specific Plan" ,"Suncreek Specific Plan" ,'Sierra Vista Specific Plan',"Cordova Hills" ,
                                      'Business Park','Trust Acquisition','Fee-To-Trust','Residential Development','Neighborhood Project','Fruit Fly Cooperative Control',
                                      'Mixed-Use Developmental District','Comprehensive Development Plan','Urban Renewal','Wood Articles Importation','Cumulative Effects.+Large-Scale Development',
                                      'Proposed Acquisition and Demolition of Building','Commercial and Marine Development','Nonregulated Status','Disposal Boundary Project',
                                      'Solid Wood Packing Material','Swine Damage Management','Use of Genetically Engineered Fruit Fly','Communication Site',"Westbrook Project",
                                      'Planned Community Development','Residential Development Project','Subdivision','Road Access Project','Public Safety Broadband Network',
                                        'Industrial Park Development','Floating Houses','Fee to Trust','Fee-to-Trust'),'\\b'),collapse='|'),fdt$Title)] <- 'Development/Trade'

fdt$Area[grepl(paste(paste0('\\b',c('Campus Consolidation','FDA HQ','Campus Integration','Education Campus','Port of Entry Modernization',
                                    'Annex Consolidation','Federal Center Consolidation','Acquisition of Additional Property','Port of Entry',
                                    'Federal Center Master Site Plan','Disposal and Reuse','Site Acquisition','Facility Consolidation','Medical Facilities Development',
                                    'Headquarters Consolidation','Computing Center Expansion','Federal Building','Federal Office','Grow the Army',
                                    'Federal Central Site Plan','Armed Forces Recreation','Activity Realignment','Housing Area','Land Conveyance',
                                    'Conveyance of Lands into Trust',
                                    'Disposal of Portions','Project Conveyance','Incoming Mail Facility','Homebasing','Family Housing Development','Barracks',
                                    'Air Force Base (AFB) Closure','Base Closure','Air Force Base \\(AFB\\) Realignment','Center-wide Operations','Activity Closures  Realignment',
                                    'New Administrative and Support Buildings','School Relocation','Base Closure','Base Deactivation','Marine Corps Base Camp  Expansion',
                                    'Depot Closure','Base  Closure and Relocation','Land Acquisition Program','Other Construction Activities','Transfer and Reuse',
                                    'Marine Barracks', 'Base Closure and Realignment','Family Housing Construction Project', 'Base Realignment','Fort Polk Louisiana Realignment',
                                    'Security Training Center','Surplus Federal Real Property','Financing Administration Consolidation','Grow the Force',
                                    'Retirement Home','Public Sale','Property Disposal','Transfer Ownership of Property','National Office Consolidation',
                                    'Reconfigure and Relocate Facilities','Base Reuse and Development','Naval Air Station Realignment','Military Family Housing',
                                    'Naval Air Station \\(NAS\\) Realignment', 'Explosives Handling Wharf','Replacement Facility Construction','Disposition',
                                    'Basewide Utilities Infrastructure','Air Force Base \\(AFB\\) Closure','Coaxial Cable Removal Project','Naval Base Development',
                                    'Federal Office Building Construction','Consolidation of Facilities','Federal Center','Central Records Complex','Public Land Withdrawal',
                                    'Chilled Water Systems Improvements','Master Plan.*Campus','Utilities Upgrade Project','Campus Development','Campus',
                                    'Area B of the Presidio','Mail Facility Complex Development','medical Center',
                                    'VA Medical Center','Medical Center','Regional Office Center Relocation','Facilities Relocation and Development','Coastal Campus',
                                    'Federal Center Construction','Federal Building Construction'),'\\b'),collapse='|'),fdt$Title)] <- 'Gov. Facilities/Siting'

fdt$Area[is.na(fdt$Area)&fdt$Agency%in%c('General Services Administration')&grepl('Corridor|Bypass|Expansion|Consolidation|New Facility',fdt$Title)] <- 'Gov. Facilities/Siting'
fdt$Area[is.na(fdt$Area)&fdt$Agency%in%c('United States Navy','United States Air Force','United States Army')&grepl('Base|Station',fdt$Title)&grepl('Realignment',fdt$Title)] <- 'Gov. Facilities/Siting'



#build, plan, or manage
fdt$InfraType = NA
built_indicators = c("I-[0-9]{1,3}|I[0-9]{1,3}|US(-|\\s)[0-9]{1,3}","SR(-|\\s)[0-9]{1,3}","S\\.R\\.\\s[0-9]{1,3}","Project","Completion","Facilit","Widening","Bypass","Extension","Special Use Permits","Bridge",
                     "Prison","Improvements","Expansion","Resort","Development","Extraction","Broadband","Project","Train","Station","Construction","Special Use Permit","Correctional","Parkway","Loop",'Freeway','Connector',
                     "404","Section 10","Mine","Highway","Base","Harbor","Rail","Port","Dam","Reservoir","Hydroelectric","Pipeline","Transmission","Plant","Reactor","Coal","Interstate","Crossing","Corridor","Route","Addition","Powerline",
                     "Airport","Observatory","Rocket","Collider","Experimental","Research Facility","Plutonium",'Interchange','Reconstruction','Street','Road','Expressway','Improvement',"Special-Use Permits","Communication Site","Hwy",
                     "Regulating Works","Solar","Oil and Gas",'Facility Development','Extension','Interchange','Site','Center','Complex','Redevelopment','Embarkation','Tunnel','REBUILD','Rebuild',
                     "Dock","Floodway","Terminal","Ship","Channel","Dredging",'OIL AND GAS DEVELOPMENT','CONSTRUCTION','Wind Farm','Shoreline','Right-of-Way','Business Park','Replacement','Campus',
                     "Special-Use-Permits",'Upgrade Proect',"Riley Ridge to Natrona",'North Fork Wells of Eagle Creek')
inst_indicators = c("Remedial","Program","Conservation","Maintenance","Activities","Protection",'Remediation','Transfer','Reintroduction','Rule-Making','Presence','Use of','Risk Reduction',
                    'Fee to Trust','Fee-to-Trust','MASTER','Delivery','Icebreaking Operation','Disposition','Harvest','Nonregulated','Policy Review','Homebasing','Preservation',
                    "Deploy","Cleanup","Combat","Grazing","Operational","Standards","Efficiency","Mining","IMPLEMENTATION",'Study','Initiative','Amendment','Guidelines','PROGRAMMATIC',
                    "Sonar","Salvage","Access","Proposal","Lease","Refuge","Laboratory","Safety","Control","Leasing","Fuels Reduction","Designation","Recovery",'Incidental Take',
                    "Selection","Reforestation","Removal",'Exchange',"Allotments","Species","Conveyance","Transfer",'Restrict','Measure','Flow','Reallocat','Licens','Relocation','Landings',
                    'Regulatory Changes','Realignment','Rulemaking','SCENIC AND RECREATIONAL RIVER','Analysis Area','Estuarine Research Reserve','Termination','Demonstration',
                    "Vehicle Use","Trail System","Rehabilitation",'Rangeland',"Treatment","New Information","Investigation","Transfers","Water Surplus",'Renewal','LEGISLATIVE',
                    "Renewals","Water System","WaterFix","Long-Term","Reevaluation","Land Transfer",'Resource','Consolidation','Operating Area','Testing','Agreement','Within','Rezoning',
                    
                    "Management","Plan","Planning","Study","Assessment","Disposal","Reuse","Sale","Conservation","Restoration",'Determinations','Evaluation','Improve Capabilities','Range-Wide',
                    "Catch Limit","signatory",'Signatory','Hunt','Bycatch','Habitat','Continued','Drainage Area','River Basin',
                    "Sabine Pass to Galveston Bay",'Southern Pine Beetle',  "1986 - 2000 Nantahala and Pisgah National Forests",'Cordova Hills','Southwest Coastal Louisiana')

fdt$InfraType[grepl(paste0(built_indicators,collapse='|'),fdt$Title,perl=F)] <- 'Built'
fdt$InfraType[grepl(paste(inst_indicators,collapse='|'),fdt$Title,perl=F)] <- 'Institutional'
fdt$InfraType[is.na(fdt$InfraType)&fdt$Agency%in% c("Federal Highway Administration" ,"Federal Energy Regulatory Commission")] <- 'Built'
fdt$InfraType[is.na(fdt$InfraType)&fdt$Agency%in% c("Forest Service")] <- 'Institutional'

fwrite(fdt,'input/epa_master_repository/eis_record_detail_coded.csv')




