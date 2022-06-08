#table(projects$AGENCY,projects$PROJECT_TYPE)
projects = fread('../bucket_mount/tuolumne/scratch/boilerplate/project_candidates_eis_only.csv')
projects = projects[Document=='Final',]
projects = projects[grepl('^201[3-9]|^2020',PROJECT_ID),]
documents = fread( '../bucket_mount/tuolumne/scratch/boilerplate/document_candidates_eis_only.csv')
documents = documents[PROJECT_ID %in% projects$PROJECT_ID,]
scratch_loc = 'scratch/boilerplate/hash_candidates/'

scores = readRDS('../bucket_mount/tuolumne/scratch/eis_scores_scratch.rds')
scores$a_id = str_remove(scores$a,'_.*')
scores$b_id = str_remove(scores$b,'_.*')
scores$a_agency = projects$AGENCY[match(scores$a_id,projects$EIS.Number)]
scores$b_agency = projects$AGENCY[match(scores$b_id,projects$EIS.Number)]
require(pbapply)

t1 = full_tlist[scores$a]
t2 = full_tlist[scores$b]

cross_agency = scores[a_agency!=b_agency,]
cross_agency[grepl( "20200032_Lewistown_PRMP_FEIS_Vol_1_Feb2020.txt" ,a),]
unique(str_remove(cross_agency$a,'[0-9]{1,}$'))


ident = mapply(function(x,y) identical(x,y),x = t1,y = t2,SIMPLIFY = T)


scores[a_id %in%  c('20130294','20140244') & b_id %in%  c('20130294','20140244')&a_id!=b_id,]

projects[EIS.Number %in% c('20130294','20140244'),]

documents[EIS.Number %in% scores[a_agency!=b_agency&a_id!=b_id,.N,by=.(a_id,b_id)][order(-N)][1:15,]$a_id,]$FILE_NAME


scores[a_agency!=b_agency,.N,by=.(a_id,b_id)][order(-N)][1:15,]$b_id

cross_agency[as.numeric(str_extract(cross_agency$a,'[0-9]{1,}$'))<90&!grepl('APPENDIX|APP',toupper(a))&!grepl('APPENDIX|APP',toupper(b)),]

cross_agency[grepl("20180134_CoronadoFEISVol1_Chapter1Thru4.txt",a),]

documents

require(htmlTable)


cross_agency = scores[a_agency!=b_agency,]
cross_agency[grepl( "20190275_Gondola_FEIS-EIR_V1.txt" ,a),]
unique(str_remove(cross_agency$a,'[0-9]{1,}$'))
htmlTable(cbind(
  full_tlist['20190265_OKT_FEIS_EPA_Vol_5.txt5'],
  full_tlist['20200204_Osage_County_Oil_and_Gas_EIS.txt1408']),rows =F)

projects[EIS.Number%in%c(20190265,20200204)]

htmlTable(cbind(
full_tlist['20130057_Riparian_Sanctuary_Final_EIS-EIR_0213_Web.txt55'],
full_tlist['20130171_Riverside-Corona_Feeder_Final_EIS.txt312']),rows =F)

projects[EIS.Number=='20170153']
20130294 20140244
table(ident)

scores[!ident & a_agency!=b_agency,][,list(.N,mean(score)),by=.(a_id,b_id)][order(-N)][1:20,]
non_ident = scores[!ident,]


full_tlist["20160318_DEIRS_comment_letter_1565-1600_Part1.txt436" ]

full_tlist[scores[a_id %in% c('20160318','20190179') & b_id %in% c('20160318','20190179') & a_id!=b_id,]$a]
scores[a_id %in% c('20160318','20190179') & b_id %in% c('20160318','20190179') & a_id!=b_id,]$a


projects[EIS.Number%in%c('20150091','20190236')]

head(non_ident)

full_tlist['20130004_noaa_4766_DS1.txt250']
full_tlist['20130027_08_Chapter_4_Environmental_Consequences.txt6']


length(ident)
table(ident)

ident


ident = pbsapply(1:nrow(scores),function(x) {
  identical(full_tlist[scores$a,fu]ll_tlist[scores$b[x]])
},cl = 10)


table(ident)

dim(scores)




scores[1000,]

unique(scores[a_agency!=b_agency,][order(-score)][score>200,]$a)
nchar(full_tlist['20170209_OA2_FEIS_Vol_2_FINAL_171010.txt1'])
nchar(full_tlist['20200133_NPR-A_Final_IAP-EIS_DOI_Vol2_508_compliant.txt526'])

scores2 = scores[str_remove(scores$a,'_.*')!=str_remove(scores$b,'_.*'),]
library(textreuse)

align_local('The Okanogan Forest Plan would be amended by adding the following to
MA15B 22B and the Wenatchee Forest Plan would be amended by adding the
following to Table IV 15 page IV 77 Pack and saddle stock outfitter
guides shall not be allowed to increase the existing amount of barren
core bare mineral soil in established campsites In campsites where
the existing amount of barren core exceeds 2,800 square feet outfitter
guides shall not use more than 2,800 square feet of the barren core All
pack and saddle stock outfitter guides shall use the same delineated
2,800 square foot area for each camp and shall not use any area outside
of the delineated 2,800 square foot area',
'The Okanogan Forest Plan would be amended by adding the following to
MA15B 22B and the Wenatchee Forest Plan would be amended by adding the
following to Table IV 15 page IV 77 Pack and saddle stock outfitter
guides shall not be allowed to increase the existing amount of barren
core bare mineral soil in established campsites In campsites where
the existing amount of barren core exceeds 2,800 square feet outfitter
guides shall not use more than 2,800 square feet of the barren core All
pack and saddle stock outfitter guides shall use the same delineated
2,800 square foot area for each camp and shall not use any area outside
of the delineated 2,800 square foot area' )

align_local('The Okanogan Forest Plan would be amended by adding the following to
MA15B 22B and the Wenatchee Forest Plan would be amended by adding the
following to Table IV 15 page IV 77 Pack and saddle stock outfitter
guides shall not be allowed to increase the existing amount of barren
core bare mineral soil in established campsites In campsites where
the existing amount of barren core exceeds 2,800 square feet outfitter
guides shall not use more than 2,800 square feet of the barren core All
pack and saddle stock outfitter guides shall use the same delineated
2,800 square foot area for each camp and shall not use any area outside
of the delineated 2,800 square foot area',
'The Okanogan Forest Plan would be amended by adding the following to
MA15B 22B and the Wenatchee Forest Plan would be amended by adding the
following to Table IV 15 page IV 77 Pack and saddle stock outfitter
guides shall not be allowed to increase the existing amount of barren
core bare mineral soil in established campsites In campsites where
the existing amount of barren core exceeds 2,800 square feet outfitter
guides shall not use more than 2,800 square feet of the barren core All
pack and saddle stock outfitter guides shall use the same delineated
2,800 square foot area for each camp and shall not use any area outside
of the delineated 2,800 square foot area')



align_local(full_tlist["20130051_03_FEIS_Summary.txt56"],full_tlist["20130051_05_FEIS_Chapter_2_-_Alternatives.txt50"],match = 1,mismatch = -1)
nchar(full_tlist["20130051_03_FEIS_Summary.txt56"])
nchar('The Okanogan Forest Plan would be amended by adding the following to
MA15B 22B and the Wenatchee Forest Plan would be amended by adding the
following to Table IV 15 page IV 77 Pack and saddle stock outfitter
guides shall not be allowed to increase the existing amount of barren
core bare mineral soil in established campsites In campsites where
the existing amount of barren core exceeds 2,800 square feet outfitter
guides shall not use more than 2,800 square feet of the barren core All
pack and saddle stock outfitter guides shall use the same delineated
2,800 square foot area for each camp and shall not use any area outside
of the delineated 2,800 square foot area')

The Okanogan Forest Plan would be amended by adding the following to
MA15B 22B and the Wenatchee Forest Plan would be amended by adding the
following to Table IV 15 page IV 77 Pack and saddle stock outfitter
guides shall not be allowed to increase the existing amount of barren
core bare mineral soil in established campsites In campsites where
the existing amount of barren core exceeds 2,800 square feet outfitter
guides shall not use more than 2,800 square feet of the barren core All
pack and saddle stock outfitter guides shall use the same delineated
2,800 square foot area for each camp and shall not use any area outside
of the delineated 2,800 square foot area


scores[a_agency == 'Federal Energy Regulatory Commission']

library(textreuse)
?align_local

summary(scores2$score)
dim(scores2)


summary(test$score)
dim(test)
