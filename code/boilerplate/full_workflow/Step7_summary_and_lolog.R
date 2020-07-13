
library(Matrix)
library(statnet)
library(data.table)
library(stringr)
library(lolog)
library(textreuse)
install.packages('textreuse')
text_storage = 'input/filtered_text_files/'
flist = list.files(text_storage)
lda_result_file = 'scratch/boilerplate/lda_combo_results.RDS'
lda = readRDS(lda_result_file)
lda$score = as.numeric(lda$score)
lda$a_id = str_remove(lda$a,'--.*')
lda$b_id = str_remove(lda$b,'--.*')
lda$p1 = as.numeric(gsub('--','',str_extract(lda$a,'--[0-9]{1,}$')))
lda$p2 = as.numeric(gsub('--','',str_extract(lda$b,'--[0-9]{1,}$')))
lda$f1 = paste0(str_remove(lda$a,'--[0-9]{1,}$'),'.txt')
lda$f2 = paste0(str_remove(lda$b,'--[0-9]{1,}$'),'.txt')

sample_score = 800
show_example = lda[score>sample_score&a_id!=b_id,][a_id %in% projects_used$PROJECT_ID[projects_used$PROJECT_TYPE=='EIS'] & b_id %in% projects_used$PROJECT_ID[projects_used$PROJECT_TYPE=='EIS'],]
library(htmlTable)
sm = 30
#> show_example$a[sm]
#[1] "20130046--20130046_Mid Fork American FEIS FERC 2079.pdf--335"
#> show_example$b[sm]
#[1] "20130378--20130378_Toledo Bend Hydro Project FEIS.pdf--199"
file1 = fread(paste0(text_storage,show_example[sm,]$f1),sep = '\t')
file2 = fread(paste0(text_storage,show_example[sm,]$f2),sep = '\t')
page1 = file1[Project_File_Par==show_example[sm,]$a,]
page2 = file2[Project_File_Par==show_example[sm,]$b,]
sample_local = align_local(page1$text,page2$text)
htmlTable(cbind(sample_local$a_edits,sample_local$b_edits))

# countn = lda[,.N,by = .(a_id,b_id)]
# score90q = lda[,quantile(score,0.90),by = .(a_id,b_id)]
# score95q = lda[,quantile(score,0.95),by = .(a_id,b_id)]
# scoreMax = lda[,max(score),by = .(a_id,b_id)]
# align800 = lda[score>800,][!duplicated(paste(a_id,b_id)),]

library(lolog)
library(sf)

epa_record = fread('scratch/eis_record_detail.csv')
epa_record = epa_record[grepl('^201[3-9]',epa_record$EIS.Number),]
epa_record = epa_record[epa_record$Document.Type=='Final',]
epa_record[,.N,by = .(Agency)][order(-N)]
epa_record$Agency_Short = epa_record$Agency
epa_record$Agency_Short[epa_record$Agency=='Federal Energy Regulatory Commission'] <- 'FERC'
epa_record$Agency_Short[epa_record$Agency=="National Oceanic and Atmospheric Administration"] <- 'NOAA'
epa_record$Agency_Short[epa_record$Agency=="National Park Service"] <- 'NPS'
epa_record$Agency_Short[epa_record$Agency=="Bureau of Land Management"] <- 'BLM'
epa_record$Agency_Short[epa_record$Agency=="Forest Service"] <- 'FS'
epa_record$Agency_Short[epa_record$Agency=="Bureau of Indian Affairs"] <- 'BIA'
epa_record$Agency_Short[epa_record$Agency=="United States Air Force"] <- 'USAF'
epa_record$Agency_Short[epa_record$Agency=="United States Navy"] <- 'USN'
epa_record$Agency_Short[epa_record$Agency=="U.S. Army Corps of Engineers"] <- 'ACOE'
epa_record$Agency_Short[epa_record$Agency=="Nuclear Regulatory Commission"] <- 'NRC'
epa_record$Agency_Short[epa_record$Agency=="Fish and Wildlife Service"] <- 'FWS'
epa_record$Agency_Short[epa_record$Agency=="Federal Highway Administration"] <- 'FHA'
epa_record$Agency_Short[epa_record$Agency=="Federal Railroad Administration" ] <- 'FRA'
epa_record$Agency_Short[epa_record$Agency=="National Marine Fisheries Service"  ] <- 'NMFS'
epa_record$Agency_Short[epa_record$Agency== "Department of Energy"   ] <- 'DOE'
epa_record$Agency_Short[epa_record$Agency== "United States Army"   ] <-  'US Army'
epa_record$Agency_Short[epa_record$Agency== "Federal Transit Administration"   ] <-  'FTA'
epa_record$Agency_Short[epa_record$Agency== "Bureau of Reclamation"   ] <-  'BR'

project_candidates = fread('scratch/boilerplate/project_candidates.csv')
projects_used = fread('scratch/boilerplate/projects_used.csv')

eis_used = projects_used[PROJECT_TYPE == 'EIS',]
doe_used = projects_used[AGENCY == 'Department of Energy',]
blm_used = projects_used[AGENCY == 'Bureau of Land Management',]
usfs_used = projects_used[AGENCY == 'Forest Service',]

lda_eis = lda[a_id %in% eis_used$PROJECT_ID&b_id %in% eis_used$PROJECT_ID,]
lda_doe = lda[a_id %in% doe_used$PROJECT_ID&b_id %in% doe_used$PROJECT_ID,]
lda_blm = lda[a_id %in% blm_used$PROJECT_ID&b_id %in% blm_used$PROJECT_ID,]
lda_usfs = lda[a_id %in% usfs_used$PROJECT_ID&b_id %in% usfs_used$PROJECT_ID,]

n_eis = nrow(eis_used)
n_blm = nrow(blm_used)
n_usfs = nrow(usfs_used)
n_doe = nrow(doe_used)


eis_ids = sort(eis_used$PROJECT_ID)
blm_ids = sort(blm_used$PROJECT_ID)
doe_ids = sort(doe_used$PROJECT_ID)
usfs_ids = sort(usfs_used$PROJECT_ID)

eis_blank_mat = matrix(0,ncol = n_eis,nrow = n_eis,dimnames = list(eis_ids,eis_ids))
doe_blank_mat = matrix(0,ncol = n_doe,nrow = n_doe,dimnames = list(doe_ids,doe_ids))
blm_blank_mat = matrix(0,ncol = n_blm,nrow = n_blm,dimnames = list(blm_ids,blm_ids))
usfs_blank_mat = matrix(0,ncol = n_usfs,nrow = n_usfs,dimnames = list(usfs_ids,usfs_ids))


######## sensitivity test for eis tie threshold #######
thresh = expand.grid(score_threshold = seq(200,1200,100),page_threshold = 1:10)
nodes = sort(eis_ids)
library(statnet)
dens_list = mapply(function(s,t) {
tpar = lda_eis[score>s & a_id!=b_id,.N,by = .(a_id,b_id)][N>=t,][,.(a_id,b_id)]
eis_net = network.initialize(n = length(nodes),directed = F,bipartite = F,hyper = F,multiple = F,loops = F)
network.vertex.names(eis_net) <- nodes
edgelist = tpar
network::add.edges.network(eis_net,tail = match(edgelist$a_id,network.vertex.names(eis_net)),
                           head = match(edgelist$b_id,network.vertex.names(eis_net)))
network.density(eis_net)
},s = thresh$score_threshold,t = thresh$page_threshold)
thresh$dens = dens_list
thresh = data.table(thresh)
library(tidyverse)
thresh$keep = ifelse(thresh$score_threshold==700&thresh$page_threshold==3,1,0)
ggplot() + 
  geom_tile(data = thresh[score_threshold<=900&page_threshold<=6,],
            aes(x = score_threshold,y = page_threshold,fill = dens,colour = as.factor(keep)),lwd = 1) + 
  scale_fill_viridis_c(name = 'density') + theme_minimal() + 
  scale_color_manual(values = c(NA,'grey50'),labels = c('','Primary'),name = '') +  
  ggtitle('Network density by tie assignment threshold') + 
  scale_x_continuous(expand =  c(0,0),name = 'LDA score threshold') + 
  scale_y_continuous(expand = c(0,0),name = '# pages threshold') + 
  guides(colour = guide_legend(override.aes = list(fill = NA)))
library(lubridate)


######### EIS analysis #########
s = 700;t = 3
tpar = lda_eis[score>s & a_id!=b_id,.N,by = .(a_id,b_id)][N>=t,][,.(a_id,b_id)]
eis_nodes = sort(eis_ids)
eis_nodes = sort(blm_ids)
eis_net = network.initialize(n = length(eis_nodes),directed = F,bipartite = F,hyper = F,multiple = F,loops = F)
network.vertex.names(eis_net) <- eis_nodes
edgelist = tpar
network::add.edges.network(eis_net,tail = match(edgelist$a_id,network.vertex.names(eis_net)),
                           head = match(edgelist$b_id,network.vertex.names(eis_net)))
network.density(eis_net)
network.size(eis_net) * {network.size(eis_net)-1} / 2
eis_net %v% 'Agency' <- epa_record$Agency[match(network.vertex.names(eis_net),epa_record$EIS.Number)]
eis_net %v% 'Agency_Short' <- epa_record$Agency_Short[match(network.vertex.names(eis_net),epa_record$EIS.Number)]
eis_net %v% 'Fed_Reg_Date' <- decimal_date(mdy(epa_record$Federal.Register.Date[match(network.vertex.names(eis_net),epa_record$EIS.Number)]))
eis_net %v% 'Year' <- (eis_net %v% 'Fed_Reg_Date')-2013
eis_net %v% 'EIS' <- network.vertex.names(eis_net)


######### BLM analysis #########
s = 800;t = 4
blm_meta = fread('scratch/blm_document_record.csv')
docs_used = fread('scratch/boilerplate/documents_used.csv')
docs_used = docs_used[AGENCY=="Bureau of Land Management",]
blm_meta = blm_meta[blm_meta$File_Name %in% docs_used$FILE_NAME,]
docs_used$Date = blm_meta$`Release Date`[match(docs_used$FILE_NAME,blm_meta$File_Name)]
docs_used$Date = mdy(docs_used$Date)
docs_used$Dec_Date = decimal_date(docs_used$Date)
decdate = docs_used[,mean(Dec_Date),by = .(PROJECT_ID)]

epa_record$Federal.Register.DecDate  = decimal_date(mdy(epa_record$Federal.Register.Date))
tpar = lda_blm[score>s & a_id!=b_id,.N,by = .(a_id,b_id)][N>=t,][,.(a_id,b_id)]
blm_nodes = sort(blm_ids)
blm_net = network.initialize(n = length(blm_nodes),directed = F,bipartite = F,hyper = F,multiple = F,loops = F)
network.vertex.names(blm_net) <- blm_nodes
edgelist = tpar
network::add.edges.network(blm_net,tail = match(edgelist$a_id,network.vertex.names(blm_net)),
                           head = match(edgelist$b_id,network.vertex.names(blm_net)))
network.density(blm_net)
network.size(blm_net) * {network.size(blm_net)-1} / 2

blm_net %v% 'Date' <- ifelse(blm_net %v% 'vertex.names'  %in% blm_meta$NEPA_ID,decdate$V1[match(blm_net %v% 'vertex.names' ,decdate$PROJECT_ID)],
       epa_record$Federal.Register.DecDate[match(blm_net %v% 'vertex.names',epa_record$EIS.Number)])

repl_i = which(is.na(blm_net %v% 'Date'))
repl_date = as.numeric(str_extract({blm_net %v% 'vertex.names'}[repl_i],'20[1-2][0-9]'))
set.vertex.attribute(blm_net,'Date',repl_date,v = repl_i)
blm_net %v% 'Year' <- floor(blm_net %v% 'Date') - 2013
blm_net %v% 'PROJECT_ID' <- network.vertex.names(blm_net)
blm_net %v% 'PROJECT_TYPE' <- ifelse(grepl('[0-9]{8}',blm_net %v% 'vertex.names'),'EIS','EA')


######### USFS analysis #########
s = 700;t = 3
usfs_meta = fread('scratch/forest_service_project_detail.csv',na.strings = "")
tpar = lda_usfs[score>s & a_id!=b_id,.N,by = .(a_id,b_id)][N>=t,][,.(a_id,b_id)]
usfs_nodes = sort(usfs_ids)
usfs_net = network.initialize(n = length(usfs_nodes),directed = F,bipartite = F,hyper = F,multiple = F,loops = F)
network.vertex.names(usfs_net) <- usfs_nodes
edgelist = tpar
network::add.edges.network(usfs_net,tail = match(edgelist$a_id,network.vertex.names(usfs_net)),
                           head = match(edgelist$b_id,network.vertex.names(usfs_net)))
network.density(usfs_net)
network.size(usfs_net) * {network.size(usfs_net)-1} / 2
usfs_net %v% 'Date' <- decimal_date(ymd(usfs_meta$`Decision Signed Date`[match(usfs_net %v% 'vertex.names',usfs_meta$Proj_Num)]))
usfs_net %v% 'Year' <- floor(usfs_net %v% 'Date') - 2013  
usfs_net %v% 'PROJECT_ID' <- network.vertex.names(usfs_net)
usfs_net %v% 'PROJECT_TYPE' <- ifelse(grepl('^[0-9]{8}',usfs_net %v% 'vertex.names'),'EIS','EA')


######### DOE analysis #########
s = 700;t = 3
tpar = lda_doe[score>s & a_id!=b_id,.N,by = .(a_id,b_id)][N>=t,][,.(a_id,b_id)]
doe_nodes = sort(doe_ids)
doe_net = network.initialize(n = length(doe_nodes),directed = F,bipartite = F,hyper = F,multiple = F,loops = F)
network.vertex.names(doe_net) <- doe_nodes
edgelist = tpar
network::add.edges.network(doe_net,tail = match(edgelist$a_id,network.vertex.names(doe_net)),
                           head = match(edgelist$b_id,network.vertex.names(doe_net)))
network.density(doe_net)
network.size(doe_net) * {network.size(doe_net)-1} / 2
doe_net %v% 'Fed_Reg_Date' <- decimal_date(mdy(epa_record$Federal.Register.Date[match(network.vertex.names(doe_net),epa_record$doe.Number)]))
doe_net %v% 'Year' <- (doe_net %v% 'Fed_Reg_Date')-2013
doe_net %v% 'doe' <- network.vertex.names(doe_net)


geo = readRDS(paste0('scratch/','ngaz_matches.RDS'))
geo = geo[!is.na(PRIM_LONG_DEC)&!is.na(PRIM_LAT_DEC),]
geo = geo[PRIM_LAT_DEC>16,]
geo$PRIM_LONG_DEC[geo$PRIM_LONG_DEC>170] <- geo$PRIM_LONG_DEC[geo$PRIM_LONG_DEC>170] * -1
geo_eis = geo[PROJECT_ID %in% network.vertex.names(eis_net),]
geo_blm = geo[PROJECT_ID %in% network.vertex.names(blm_net),]
geo_usfs = geo[PROJECT_ID %in% network.vertex.names(usfs_net),]
geo_doe = geo[PROJECT_ID %in% network.vertex.names(doe_net),]



EIS_centroids = geo_eis[,list(mean(PRIM_LONG_DEC),mean(PRIM_LAT_DEC)),by = .(PROJECT_ID)]
eis__dist = dist(as.matrix(EIS_centroids[,.(V1,V2)]),upper = T)
dist_mat = log(as.matrix(dist)+0.0001)
#ggplot(EIS_centroids) + geom_point(aes(x = V1,y = V2))
rownames(dist_mat) = colnames(dist_mat) = EIS_centroids$PROJECT_ID
eis_net_sub = get.inducedSubgraph(eis_net,v = which(network.vertex.names(eis_net) %in% rownames(dist_mat)))
eis_order = rank(eis_net %v% 'EIS')
eis_order_sub = rank(eis_net_sub %v% 'EIS')









cores = detectCores() - 2
cl = makeCluster(cores)
registerDoParallel(cl)

mod1 = lolog(eis_net ~ edges + gwdegree(0.25) + gwesp(0.25) + nodeMatch('Agency') + 
               absDiff('Fed_Reg_Date')|eis_order,nsamp = 2500,verbose=T, maxIter = 200,maxStepSize = 5,cluster = cl)
summary(mod1)
mod2 = lolog(eis_net_sub ~ edges + gwdegree(0.25) + gwesp(0.25) + nodeMatch('Agency') + absDiff('Fed_Reg_Date')  + 
               edgeCov(dist_mat,'log_dist')|eis_order_sub,nsamp = 1000,verbose=T,maxStepSize = 5,cluster = cl)
summary(mod2)


saveRDS(eis_or,paste0(floc,'diffnet.RDS'))
saveRDS(diffnet_sub,paste0(floc,'diffnet_sub.RDS'))


stopCluster(cl)
library(texreg)
mlist = list(mod1,mod2)
#htmlreg(list(mod1,mod2),file = 'output/boilerplate/lolog_model.html')
saveRDS(object = mlist ,file = paste0(floc,'lolog_modlist.RDS'))









#plocs = geo[,.(PRIM_LONG_DEC,PRIM_LAT_DEC)] %>% as.data.frame %>%  sf::st_as_sf(coords = c(1,2))
#plocs$EIS = geo$EIS_Number

#diag(dist_mat) <- NA






lda_blm = lda[a_id %in% blm_used$PROJECT_ID&b_id %in% blm_used$PROJECT_ID,]
n_blm = nrow(blm_used)
blm_ids = sort(blm_used$PROJECT_ID)
blm_blank_mat = matrix(0,ncol = n_blm,nrow = n_blm,dimnames = list(blm_ids,blm_ids))

s = 800;t = 4
tpar = lda_blm[score>s & a_id!=b_id,.N,by = .(a_id,b_id)][N>=t,][,.(a_id,b_id)]
lda_blm[score>s & a_id!=b_id,.N,by = .(a_id,b_id)][N>=t,][,.(a_id,b_id)]
blm_mat = blm_blank_mat
blm_mat[tpar$a_id,tpar$b_id]<-1
blm_mat[tpar$b_id,tpar$a_id]<-1
blm_net = as.network(blm_mat,matrix.type = 'adjacency',directed = F)
network.density(blm_net)
blm_net



as.network()

eis_used[,.N,by = .(AGENCY)][order(-N),]
table(usfs_used$PROJECT_TYPE)
        )
str_remove(file1,'--.*')
projects_used[PROJECT_ID %in% c(file1$PROJECT_ID,file2$PROJECT_ID),]

sample_local$score

align_local(file1$text[show_example[sm,]$p1],
file2$text[show_example[sm,]$p2])









show_example[2,]$b
summary(lda$score)

projects_used = fread('scratch/boilerplate/projects_used.csv')
files_used = fread('scratch/boilerplate/documents_used.csv')
files_used$text_name = paste0(paste(files_used$PROJECT_ID,files_used$FILE_NAME,sep = '--'),'.txt')



max_by_pair = lda[,max(score),by = .(a_id,b_id)]

d = 2


f1 = paste0(text_storage,show_example$f1[d])
f2 = paste0(text_storage,show_example$f2[d])
text1 = fread(f1)$text[show_example$p1[d]]
text2 = fread(f2)$text[show_example$p2[d]]

text1
text2

f1
f2

as.data.table(textreuse::align_local(text1,text2))
show_example




if(!require(data.table)){install.packages(data.table);require(data.table)}
if(!require(rvest)){install.packages(rvest);require(rvest)}
if(!require(curl)){install.packages(curl);require(curl)}
if(!require(tidyverse)){install.packages(tidyverse);require(tidyverse)}
if(!require(textreuse)){install.packages(textreuse);require(textreuse)}
if(!require(stringr)){install.packages(stringr);require(stringr)}
if(!require(doParallel)){install.packages(doParallel);require(doParallel)}
if(!require(pdftools)){install.packages(pdftools);require(pdftools)}
if(!require(pdfsearch)){install.packages(pdfsearch);require(pdfsearch)}
if(!require(htmlTable)){install.packages(htmlTable);require(htmlTable)}

if(!require(lolog)){install.packages(lolog);require(lolog)}
if(!require(network)){install.packages(network);require(network)}
if(!require(sna)){install.packages(sna);require(sna)}
if(!require(statnet)){install.packages(statnet);require(statnet)}
if(!require(lolog)){install.packages(lolog);require(lolog)}
if(!require(lubridate)){install.packages(lubridate);require(lubridate)}
if(!require(texreg)){install.packages(texreg);require(texreg)}
if(!require(htmlTable)){install.packages(htmlTable);require(htmlTable)}

floc = '../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/'
cosim_mat = readRDS(paste0(floc,'eis_25page_chunk_cosim_matrix.RDS'))
eis = fread(paste0(floc,'eis_used.csv'))
cosdt = as.data.table(cosim_mat)
cosdt$EIS1 = str_extract(cosdt$document1,'[0-9]{8}')
cosdt$EIS2 = str_extract(cosdt$document2,'[0-9]{8}')
cosdt = cosdt[EIS1 %in% eis$EIS.Number& EIS2 %in% eis$EIS.Number,]


agency_table = htmlTable(eis[,.N,by = .(Agency)][order(-N)],rnames=F,caption = 'FEISs by agency, 2013-present')


#samecosim = cosdt[EIS1==EIS2,]
diffcosim = cosdt[EIS1!=EIS2,]

maxcosim = diffcosim[,max(cosine),by = .(EIS1,EIS2)][order(-V1)]
maxcosim$tie = (maxcosim$V1>0.25) + 0















