
require(data.table)
scratch_loc = 'scratch/climate_in_nepa/'
projs = readRDS(paste0(scratch_loc,'eis_metadata.RDS'))

ideol_dt = fread(paste0(scratch_loc,'dataverse_files/rcl_ideology_estimates.csv'))
skill_dt = fread(paste0(scratch_loc,'dataverse_files/rcl_skills_estimates-1.csv'))
idskill_dt = merge(ideol_dt,skill_dt)

test = unique(toupper(projs$Lead.Agency))

table(test %in% toupper(idskill_dt$agency))

test[!test %in% toupper(idskill_dt$agency)]


idskill_dt

test = unique(toupper(projs$AGENCY)) 
test[!test %in% ipdt$OPMname]


ipdt
grep('FOREST',toupper(ipdt$agency),value = T)
ipdt$OPMname[ipdt$OPMname=="DEPARTMENT OF TRANSPORTATION"] <- "DEPARTMENT OF TRANSPORTATION (OTHER)"
ipdt$OPMname[ipdt$OPMname==""] 