
scratch_folder = '../../../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/'
aligns = readRDS(paste0(scratch_folder,'cross_eis/local_aling_list.RDS'))
keep = which(sapply(aligns,function(x) x$score>400))
combos = readRDS(paste0(scratch_folder,'cross_eis/combos_searched.RDS'))
doc_meta = readRDS(paste0(scratch_folder,'cross_eis/text_metadata.RDS'))
doc_meta = doc_meta[,.(EIS,File,Paragraph,EIS_File_Par)]
eis_meta = fread('../../input/epa_master_repository/eis_record_detail.csv')

saveRDS(aligns[keep],'local_align_400.RDS')
saveRDS(combos[keep,],'combos_searched_400.RDS')
saveRDS(doc_meta,'text_metadata.RDS')
saveRDS(eis_meta,'eis_record_detail.RDS')

list.files('../../../../../../net/tmp/tscott1/tuolumne_scratch/scratch/reuse/cross_eis/')

