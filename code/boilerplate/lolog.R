if(!require(data.table)){install.packages('data.table');require(data.table)}
if(!require(statnet)){install.packages('statnet');require(statnet)}
if(!require(lolog)){install.packages('lolog');require(lolog)}
if(!require(parallel)){install.packages('parallel');require(parallel)}
if(!require(doParallel)){install.packages('doParallel');require(doParallel)}
if(!require(lubridate)){install.packages('lubridate');require(lubridate)}

floc = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/'
eis = fread(paste0(floc,'eis_used.csv'))
gloc = paste0(floc,'ngaz_matches.RDS')
gmatches = readRDS(gloc)

cl = makeCluster(8)
registerDoParallel(cl)


rg = rgraph(nrow(eis),mode = 'graph',tprob = 0.01)
nt = as.network(rg,directed= F)
network.vertex.names(nt) <- eis$EIS.Number

eis$FR_DDate = decimal_date(mdy(eis$Federal.Register.Date))
nt %v% 'Agency' <- eis$Agency[match(network.vertex.names(nt),eis$EIS.Number)]
nt %v% 'FR_Date' <- eis$FR_DDate[match(network.vertex.names(nt),eis$EIS.Number)]
nt %v% 'EIS' <- network.vertex.names(nt)
mod = lolog(nt~edges + gwdegree(0.25) + gwesp(0.25) + constraint(boundedDegree(0L, 10L)),cl = cl,nsamp = 5000,maxStepSize = 5,verbose = 3)
stopCluster(cl)

library(spacyr)
spacy_parse()
