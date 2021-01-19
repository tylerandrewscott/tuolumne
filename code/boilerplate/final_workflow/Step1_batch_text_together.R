
library(data.table)
library(tidyverse)
#setwd('Box/tuolumne/')
            
docgroups = fread('scratch/boilerplate/document_candidates_eis_only.csv')


projects = fread('scratch/boilerplate/project_candidates_eis_only.csv')
docgroups = docgroups[!duplicated(docgroups)]

flist = list.files('../eis_documents/enepa_repository/text_as_datatable/',recursive = T,full.names = T)
#flist = list.files('../../../Desktop/text_as_datatable/',recursive = T,full.names = T)
base.file = basename(flist)


eis_docs = docgroups
eis_docs$FILE_LOC <- dirname(flist)[match(gsub('pdf$','txt',eis_docs$FILE_NAME),base.file)]


#eis_docs[!file.exists(paste('../eis_documents/',eis_docs$FILE_LOC,eis_docs$FILE_NAME,sep = '/')),]
have_pdf = eis_docs[file.exists(paste(eis_docs$FILE_LOC,gsub('pdf$','txt',eis_docs$FILE_NAME),sep = '/')),] 

nms = colnames(fread(paste(have_pdf[1]$FILE_LOC,gsub('pdf$','txt',have_pdf$FILE_NAME[1]),sep = '/')))

big_eis_text = data.table(numeric(),character(),character())
colnames(big_eis_text) <- c(nms,'File')
dir.create('scratch/boilerplate/big_text_files')
fname = 'scratch/boilerplate/big_text_files/big_eis_text.txt'
start_over<-FALSE
if(!file.exists(fname)|start_over){fwrite(x = big_eis_text ,file = fname,sep = '\t');already = big_eis_text}
if(file.exists(fname)&!start_over){ already = fread(fname,sep ='\t');already = already[File %in% gsub('pdf$','txt',docgroups$FILE_NAME),]}

still_need = have_pdf[!gsub('pdf$','txt',have_pdf$FILE_NAME) %in% already$File]

finfo = file.info(paste(still_need$FILE_LOC,gsub('pdf$','txt',still_need$FILE_NAME),sep = '/'))
still_need = still_need[finfo$size>10,]


tiles = dplyr::ntile(1:nrow(still_need),max(floor(nrow(still_need)/25),1))
uq_tiles = unique(tiles)
tfiles = paste(still_need$FILE_LOC,gsub('pdf$','txt',still_need$FILE_NAME),sep = '/')

print(length(tfiles))
if(length(tfiles)!=0){
for(u in uq_tiles){
  print(u)
  temp_fname_list = tfiles[uq_tiles==u]
  combined_files <- rbindlist(pbapply::pblapply(temp_fname_list,function(x) {xx<-fread(x,sep = '\t',);xx$File = basename(x);xx}))
  fwrite(x = combined_files,file = fname,append = T,verbose = F,sep = '\t')
}
}


wholefile = fread(fname,sep = '\t')
wholefile = wholefile[!duplicated(wholefile)]

wholefile[,PID:= str_remove(File,'(--|_).*')]
wholefile = wholefile[PID %in% projects$PROJECT_ID,]
wholefile = wholefile[!grepl('pdf.txt$',File),]
wholefile[,PID:=NULL]
fwrite(wholefile,fname,sep = '\t')
fwrite(wholefile[,.(text)],'scratch/boilerplate/big_text_files/big_eis_text_only.txt',sep = '\t')
fwrite(wholefile[,.(File,Page)],'scratch/boilerplate/big_text_files/big_eis_metadata.txt',sep = '\t')
saveRDS(wholefile,gsub('txt','rds',fname))



# 

# 
# fwrite(docgroups ,file = 'scratch/boilerplate/document_candidates.csv')
# 
# projects = projects[!PROJECT_ID %in% bia_ids]
# fwrite(projects ,file = 'scratch/boilerplate/project_candidates.csv')
# 
# 
# docs = fread('scratch/boilerplate/document_candidates.csv')
# docs$FILE_NAME <- gsub('_{1,}','_',docs$FILE_NAME)
# docs = docs[!duplicated(docs)]
# docs = docs[PROJECT_ID!='29099']
# docs = docs[PROJECT_ID!='34565']
# docs = docs[PROJECT_ID!='DOI-BLM-CA-C060-2017-0067-EA',]
# docs = docs[PROJECT_ID!='DOI-BLM-WY-P000-2016-0001-EA',]
# docs = docs[PROJECT_ID!='DOI-BLM-CA-C060-2018-0064-EA',]
# docs = docs[FILE_NAME!='DOI-BLM-UT-G010-2016-0023-EA--2016-0023-EApdf.txt',]
# docs = docs[FILE_NAME!='48884--48884_103699_FSPLT3_4318610.txt',]
# docs = docs[FILE_NAME!='EA-1849-S-1--EA-1849-S-1-2014.txt',]
# docs = docs[FILE_NAME!='42542--42542_96785_FSPLT3_1653493.txt',]
# docs = docs[PROJECT_ID!='52446'|FILE_NAME=='52446--52446_107604_FSPLT3_4396800.txt',]
# docs = docs[PROJECT_ID!='51876'|FILE_NAME=='51876--51876_106953_FSPLT3_4510414.txt',]
# docs = docs[PROJECT_ID!='48918'|FILE_NAME=='48918--48918_103741_FSPLT3_4107141.txt',]
# docs = docs[PROJECT_ID!='47978'|FILE_NAME=='47978--47978_102754_FSPLT3_3105953.txt',]
# docs = docs[PROJECT_ID!='DOI-BLM-CO-N010-2016-0015-EA'|FILE_NAME=='DOI-BLM-CO-N010-2016-0015-EA--TMA1_FEA_112718_Final.txt',]
# docs = docs[PROJECT_ID!='EA-2070'|FILE_NAME=='EA-2070--EA-2070_FORGE%20EA_20180329_508-2.txt',]
# docs = docs[!FILE_NAME%in% c('44189--44189_98564_FSPLT3_2984355.txt',
#                              '45650--45650_100166_FSPLT3_2579923.txt',
#                              '46498--46498_101151_FSPLT3_3906847.txt',
#                              '48644--48644_103449_FSPLT3_4301226.txt',
#                              '39348--39348_88686_FSPLT3_1451085.txt',
#                              '43080--43080_97340_FSPLT3_2937817.txt',
#                              '46624--46624_101291_FSPLT3_2562471.txt',
#                              '50643--50643_105558_FSPLT3_4661502.txt',
#                              '28443--28443_57831_FSPLT3_2285893.txt',
#                              'DOI-BLM-NV-S010-2014-0066-EA--Lone_Mountain_Community_Pit_Sand_&_Gravel_Sales_PDF.txt',
#                              '53048--53048_108254_FSPLT3_4634360.txt',
#                              '53048--53048_108254_FSPLT3_4634355.txt',
#                              '53048--53048_108254_FSPLT3_4630484.txt',
#                              '45123--45123_99614_FSPLT3_2982914.txt',
#                              '40841--40841_92933_FSPLT3_2354859.txt',
#                              '40207--40207_91215_FSPLT3_1662936.txt',
#                              'DOI-BLM-UT-G010-2016-0075-EA--EA_2016-0075-EA_20170324_for_comment.txt',
#                              'DOI-BLM-ORWA-B050-2016-0003-EA--DiamondCraterLandExchange_EA_11_14_2016.txt',
#                              'DOI-BLM-ORWA-R040-2014-0009-EA--Thunder_Mountain_Quarry_Expansion_Decision_Document.txt',
#                              'DOI-BLM-ORWA-R040-2014-0009-EA--160719_TMQE_EA_FONSI_Draft.txt',
#                              'DOI-BLM-UT-G010-2018-0017-EA--2018.05.14_-_SUWA_-_Augusi_EA_Comments.txt',
#                              'DOI-BLM-UT-G010-2018-0017-EA--2018-0017-EA.txt',
#                              'DOI-BLM-UT-G020-2017-0030-EA--URARA_EA_Comments_for_December_2017_Lease_Sale_(July_23,_2017).txt',
#                              'DOI-BLM-UT-G020-2017-0030-EA--2017_Oil_-_Gas_Leasing_EA_-_Public_Review_-_6.22.17.txt',
#                              'DOI-BLM-CO-N050-2019-0040-EA--NWD_EA_Comment_Sept2019.txt',
#                              'EA-1956--EA-1956-DEA-2014.txt',
#                              'DOI-BLM-UT-W010-2019-0001-EA--SLFO_OG_DOI-BLM-UT-W010-2019-0001-EA_(Box_Elder)_01-25-2019.txt',
#                              'EA-1329-S1--ea-1329-s1-wildfire-hazard-reduction-forest-health-improvement-program-04-2019.txt')]
# 
# docs = docs[PROJECT_ID!='29099']
# docs = docs[PROJECT_ID!='34565']
# docs = docs[PROJECT_ID!='DOI-BLM-AZ-P020-2019-0013-EA',]
# docs = docs[PROJECT_ID!='DOI-BLM-CA-C060-2017-0067-EA',]
# docs = docs[PROJECT_ID!='DOI-BLM-WY-P000-2016-0001-EA',]
# docs = docs[PROJECT_ID!='DOI-BLM-MT-C030-2019-0086-EA',]
# 
# docs = docs[PROJECT_ID!='DOI-BLM-CA-C060-2018-0064-EA',]
# docs = docs[FILE_NAME!='40683--40683_92468_FSPLT3_2420262.txt',]
# docs = docs[FILE_NAME!='55086--55086_110441_FSPLT3_4527420.txt',]
# docs = docs[FILE_NAME!='DOI-BLM-NM-P020-2019-0113-EA--Arena_Roja_Pad_1,_2,_CTB,_EL,_FL,_Roads_EA.txt',]
# 
# docs = docs[FILE_NAME!='EA-2078--NPTH_EA_%20draft%20NPTH%20EA_10_12_2018.txt',]
# 
# docs = docs[PROJECT_ID!='20190296',]
# docs = docs[PROJECT_ID!='DOI-BLM-NV-B010-2019-0012-EA',]
# docs = docs[FILE_NAME!='DOI-BLM-UT-G010-2016-0023-EA--2016-0023-EApdf.txt',]
# docs = docs[FILE_NAME!='48884--48884_103699_FSPLT3_4318610.txt',]
# docs = docs[FILE_NAME!='EA-1849-S-1--EA-1849-S-1-2014.txt',]
# docs = docs[FILE_NAME!='42542--42542_96785_FSPLT3_1653493.txt',]
# docs = docs[PROJECT_ID!='52446'|FILE_NAME=='52446--52446_107604_FSPLT3_4396800.txt',]
# docs = docs[PROJECT_ID!='51876'|FILE_NAME=='51876--51876_106953_FSPLT3_4510414.txt',]
# docs = docs[PROJECT_ID!='48918'|FILE_NAME=='48918--48918_103741_FSPLT3_4107141.txt',]
# docs = docs[PROJECT_ID!='47978'|FILE_NAME=='47978--47978_102754_FSPLT3_3105953.txt',]
# docs = docs[PROJECT_ID!='DOI-BLM-UT-Y020-2016-0042-EA'|FILE_NAME=='DOI-BLM-UT-Y020-2016-0042-EA--2016.09.16_2017OGLS_FINAL.txt',]
# 
# docs = docs[PROJECT_ID!='DOI-BLM-MT-L002-2017-0004-EA'|FILE_NAME=='DOI-BLM-MT-L002-2017-0004-EA--Hiline_Environmental_Analysis_March_13_2018_Oil_and_Gas_Lease_Sale.txt',]
# 
# 
# docs = docs[PROJECT_ID!='DOI-BLM-NM-0000-2017-0006-EA'|FILE_NAME=='DOI-BLM-NM-0000-2017-0006-EA--2017-09-20_DOI-BLM-NM-0000-2017-0006-EA_EA.txt',]
# 
# docs = docs[PROJECT_ID!='DOI-BLM-AZ-A010-2018-0032-EA'|FILE_NAME=='DOI-BLM-AZ-A010-2018-0032-EA--Wolfhole_Lake_and_Lizard_EA_7-16.txt',]
# docs = docs[PROJECT_ID!='DOI-BLM-CO-N010-2016-0015-EA'|FILE_NAME=='DOI-BLM-CO-N010-2016-0015-EA--TMA1_FEA_112718_Final.txt',]
# docs = docs[PROJECT_ID!='EA-2070'|FILE_NAME=='EA-2070--EA-2070_FORGE%20EA_20180329_508-2.txt',]
# docs = docs[!FILE_NAME%in% c('44189--44189_98564_FSPLT3_2984355.txt',
#                              '45650--45650_100166_FSPLT3_2579923.txt',
#                              '46498--46498_101151_FSPLT3_3906847.txt',
#                              '54880--54880_110226_FSPLT3_4650867.txt',
#                              '48644--48644_103449_FSPLT3_4301226.txt',
#                              '39348--39348_88686_FSPLT3_1451085.txt',
#                              '43080--43080_97340_FSPLT3_2937817.txt',
#                              '45313--45313_99812_FSPLT3_2395562.txt',
#                              'DOI-BLM-NV-B020-2018-0017-EA--201804_OilGasLeaseEA_508compliant.txt',
#                              'DOI-BLM-NV-E000-2017-0017-EA--2018_O&G_EA_508Final_(1).txt',
#                              'DOI-BLM-UT-W010-2018-0018-EA--Sep18_SLFO_OG_EA_7-25-18.txt',
#                              'DOI-BLM-UT-W010-2018-0018-EA--Sep18_SLFO_OG_EA_3-29-18.txt',
#                              'DOI-BLM-WY-D030-2017-0179-EA--DOI-BLM-WY-D030-2017-0179-EA_SM_Federal_19-19_and_20-19_OilWells.txt',
#                              'DOI-BLM-WY-D030-2016-0084-EA--DOI-BLM-WY-D030-2016-0084-EA_Fed_2-12_and_Fed_5-7.txt',
#                              'DOI-BLM-WY-0000-2019-0007-EA--20190507.WY.WEGvZinke.SupplementalEA.postPC_final.3.txt',
#                              '43556--43556_97857_FSPLT3_2549561.txt',
#                              'DOI-BLM-UT-Y020-2016-0020-EA--2016.09.06_UT-Y020-2016-0020-EA_Red_Canyon_Road_Re-route.txt',
#                              'DOI-BLM-UT-0000-2018-0001-EA--Sep18_SLFO_OG_EA_3-29-18.txt',
#                              'DOI-BLM-WY-0000-2017-0005-EA--183Q_WRBBD_EA_ver1.txt',
#                              'DOI-BLM-NV-C010-2018-0009-EA--Final_FONSI&LME_EA.508_Compliance_Checked.txt',
#                              'DOI-BLM-NV-C010-2015-0004-EA--EA_Chapters4-References.txt',
#                              'DOI-BLM-NV-C010-2015-0004-EA--EA_Chapters1-2.txt',
#                              'DOI-BLM-NV-C010-2015-0004-EA--EA_Chapter3.txt',
#                              'DOI-BLM-NV-C010-2015-0004-EA--Cow_Clan_Dixie_Proposed_Decision_Signed_3-16-2017.txt',
#                              'DOI-BLM-ORWA-W020-2016-0014-EA--20181107_Sinlahekin_EA.txt',
#                              'DOI-BLM-UT-W010-2018-0026-EA--SLFO_OG_DOI-BLM-UT-W010-2018-0026-EA_(Rich-Morgan).txt',
#                              'DOI-BLM-UT-0000-2018-0001-EA--2018-03-29_Sep_18_FFO_OG_Draft_EA.txt',
#                              'DOI-BLM-UT-0000-2018-0001-EA--2018-07-26_-_Sep18_SLFO_OG_EA_7-25-18.txt',
#                              'DOI-BLM-UT-W010-2015-0023-EA--EA_Plan_Amendment_Eastern_Lake_Mts_Target_Shoot.txt',
#                              'DOI-BLM-UT-0000-2018-0001-EA--2018-07-26_-_Leasing_EA_Price_Richfield.txt',
#                              'DOI-BLM-UT-0000-2018-0001-EA--2018-07-26_-_September_2018_Fillmore_Oil_and_Gas_Lease_EA.txt',
#                              'DOI-BLM-NV-E030-2013-0007-EA--MarysRiverExp_EA_-_Chapter_4_and_5_-_released_3-24-2014.txt',
#                              'DOI-BLM-NV-E030-2013-0007-EA--MarysRiverExp_EA_-_Chapter_3_released_3-24-2014.txt',
#                              'DOI-BLM-CA-D050-2017-0037-EA--488400_010_PerditoEA_CAS_20170922.txt',
#                              'DOI-BLM-NV-E030-2013-0007-EA--MarysRiverExp_EA_-_Chapter_1_and_2_released_3-24-2014.txt',
#                              '46112--46112_100697_FSPLT3_2462995.txt',
#                              '36561--36561_80595_FSPLT3_4102006.txt',
#                              '36561--36561_80595_FSPLT3_3993093.txt',
#                              '48151--48151_102931_FSPLT3_2718999.txt',
#                              '50518--50518_105405_FSPLT3_4641813.txt',
#                              '50518--50518_105405_FSPLT3_4638980.txt',
#                              '48151--48151_102931_FSPLT3_2718999.txt',
#                              '40893--40893_93067_FSPLT3_4383388.txt',
#                              'DOI-BLM-NM-F010-2018-0042-EA--2019.04.22_2018-0042_EA_Final.txt',
#                              'DOI-BLM-ID-B010-2018-0004-EA--Environmental_Assessment.txt',
#                              'DOI-BLM-ID-B010-2016-0041-EA--DOI-BLM-ID-B010-2016-0041-EA_Bennett_Mountain_North_Grazing_Permit_Renewals.txt',
#                              'DOI-BLM-CA-D050-2017-0037-EA--488400_010_PerditoEA_CAS_20170922.txt',
#                              '46624--46624_101291_FSPLT3_2562471.txt',
#                              '50643--50643_105558_FSPLT3_4661502.txt',
#                              '28443--28443_57831_FSPLT3_2285893.txt',
#                              '45739--45739_100276_FSPLT3_2426594.txt',
#                              '45850--45850_100402_FSPLT3_3109325.txt',
#                              'DOI-BLM-AZ-G020-2015-0009-EA--Keystone_Peak_EA_021915_Final.txt',
#                              '42886--42886_97139_FSPLT3_2552815.txt',
#                              '36707--36707_81011_FSPLT3_3032131.txt',
#                              'DOI-BLM-NM-P020-2018-0047-EA--EA_Stateline_Permian_Phase_3_Gathering_Project_20170915_to_BLM_draft.txt',
#                              'DOI-BLM-MT-0000-2019-0001-EA--July_2019_Oil_Gas_Lease_Sale_EA_3.12.19.txt',
#                              'DOI-BLM-NM-0040-2016-0028-EA--June_2017_Lease_Sale_EA_Protest_Period.txt',
#                              'DOI-BLM-NM-0040-2016-0028-EA--Ex._B_-_EA_Comment.txt',
#                              'DOI-BLM-UT-W010-2019-0001-EA--SLFO_DOI-BLM-UT-W010-2019-0001-EA_(Box_Elder).txt',
#                              'DOI-BLM-UT-Y010-2015-0238-EA--GV3D_EA.txt',
#                              'DOI-BLM-NV-S010-2014-0066-EA--Lone_Mountain_Community_Pit_Sand_&_Gravel_Sales_PDF.txt',
#                              '53048--53048_108254_FSPLT3_4634360.txt',
#                              '53048--53048_108254_FSPLT3_4634355.txt',
#                              'DOI-BLM-WY-D040-2017-0022-EA--2._EA_ATSW&GDB_-_July,_2017.txt',
#                              '53048--53048_108254_FSPLT3_4630484.txt',
#                              '45123--45123_99614_FSPLT3_2982914.txt',
#                              '40841--40841_92933_FSPLT3_2354859.txt',
#                              '40207--40207_91215_FSPLT3_1662936.txt',
#                              'DOI-BLM-ORWA-B050-2013-0021-EA--UptonMountainEA11-16-15FINAL.txt',
#                              'DOI-BLM-CO-N050-2019-0040-EA--NWD_EA_Comment_Sept2019.txt',
#                              'EA-1956--EA-1956-DEA-2014.txt',
#                              '34320--34320_74093_FSPLT2_291931.txt',
#                              '34320--34320_74093_FSPLT2_291931.txt',
#                              'EA-1982--EA-1982%20Parker-Davis%202014-12.txt',
#                              'DOI-BLM-UT-W010-2019-0001-EA--SLFO_OG_DOI-BLM-UT-W010-2019-0001-EA_(Box_Elder)_01-25-2019.txt',
#                              'EA-1329-S1--ea-1329-s1-wildfire-hazard-reduction-forest-health-improvement-program-04-2019.txt')]
# 
# weirds = grep('_-_',docs$FILE_NAME,value = T)
# docs = docs[!FILE_NAME %in% weirds[gsub('_-_','_',weirds) %in% docs$FILE_NAME],]
# 
# 
# bfiles = list.files('scratch/boilerplate/big_text_files/',pattern = 'rds',full.names = T)
# rds_list = lapply(bfiles,readRDS)
# 
# lapply(seq_along(rds_list),function(x){
# flist_dt <- rds_list[[x]]
# flist_dt$File = gsub('\\.pdf\\.txt$','.txt',flist_dt$File)
# flist_dt$File <- gsub('_{2,}','_',flist_dt$File,perl = T)
# flist_dt$PROJECT_ID = str_remove(flist_dt$File,'(--|_).*')
# flist_dt$PID<-NA
# file_dash = grepl('--',flist_dt$File)
# flist_dt$PID[file_dash] = paste(flist_dt$File[file_dash],flist_dt$Page[file_dash],sep = '--')
# flist_dt$PID[!file_dash] = paste(flist_dt$PROJECT_ID[!file_dash],flist_dt$File[!file_dash],flist_dt$Page[!file_dash],sep = '--')
# flist_dt = flist_dt[!grepl('\\.{10,}',text,perl = T),]
# test = flist_dt[,list(sum(duplicated(text)),.N),by=.(PROJECT_ID)]
# high_duplication = test[order(-V1/N),][V1/N>=0.1,]
# if(nrow(high_duplication)>0){
# for(i in 1:nrow(high_duplication)){
#   print(high_duplication$PROJECT_ID[i])
#   dup_pages = flist_dt[PROJECT_ID==high_duplication$PROJECT_ID[i],][order(-File,Page),][duplicated(text),][,.N,by=.(File)]
#   pages = flist_dt[PROJECT_ID==high_duplication$PROJECT_ID[i],][order(-File,Page),][,.N,by=.(File)]
#   setnames(dup_pages,'N','dups')
#   pages = merge(pages,dup_pages,all = T)  
#   docs = docs[!FILE_NAME%in% pages[dups/N>=0.30,]$File,]
# }
# flist_dt = flist_dt[File %in% docs$FILE_NAME,]}
# 
# test = flist_dt[,list(sum(duplicated(text)),.N),by=.(PROJECT_ID)]
# high_duplication = test[order(-V1/N),][V1/N>=0.1,]
# 
# if(nrow(high_duplication)>0){
# for(i in 1:nrow(high_duplication)){
#   print(high_duplication$PROJECT_ID[i])
#   dup_pages = flist_dt[PROJECT_ID==high_duplication$PROJECT_ID[i],][order(File,Page),][duplicated(text),][,.N,by=.(File)]
#   pages = flist_dt[PROJECT_ID==high_duplication$PROJECT_ID[i],][order(-File,Page),][,.N,by=.(File)]
#   setnames(dup_pages,'N','dups')
#   pages = merge(pages,dup_pages,all = T)  
#   docs = docs[!FILE_NAME%in% pages[dups/N>=0.20,]$File,]
# }}
# flist_dt = flist_dt[File %in% docs$FILE_NAME,]
# saveRDS(flist_dt,file = bfiles[[x]])
# })
# 
# 
# fwrite(docs,'scratch/boilerplate/document_candidates.csv')
# 
# 








