

blm_docs[PROJECT_ID=='DOI-BLM-WY-D030-2013-0253-EA',]

blm_sub_docs[PROJECT_ID=='DOI-BLM-WY-D030-2013-0253-EA']


blm_docs = blm_docs[!Set_Type%in%c('Scoping','Notice of Competitive Lease Sale','Preliminary EA','Scoping Documents','Public Comment Letters',
                                   'Maps','Comments on Preliminary EA','Appendices','Public Scoping','Morgan Group Proposed Decisions',
                                   'Rangeland Health Assessments','Final Grazing Decisions',
                                   'Draft Resource Reports','Resource Reports','Other References','Channel Stability Monitoring Report','Decision',
                                   'Master Development Plan Documents','News Release','Other Documentation','Decision Record',
                                   'Final Rangeland Health Assessment and Evaluation Report','Bagdad Mine Compensatory Mitigation Documentation',
                                   'Timber Sale Decision',
                                   'Morgan Group Final Decisions','Draft Environmental Assessment','Morgan Group 5 Rangeland Health Assessment',
                                   'Preliminary Environmental Assessment','Preliminary Environmental Assessment Documents','Protests'),]
blm_docs = blm_docs[!Set_Name%in%c('Draft Resource Reports','Channel Stability Monitoring Report','Resource Reports','Rangeland Health Assessments',
                                   'Other References','Draft Resource Reports'),]

blm_docs = blm_docs[!grepl('^APPENDIX',toupper(Set_Name)),]
blm_docs = blm_docs[!grepl('PUBLIC COMMENTS',toupper(Set_Name)),]
blm_docs = blm_docs[!grepl('^MAPS',toupper(Set_Name)),]
blm_docs = blm_docs[!grepl('FINAL DECISIONS',toupper(Set_Type)),]
blm_docs = blm_docs[!grepl('SUBMISSION',toupper(Set_Type)),]
blm_docs = blm_docs[!grepl('PRELIMINARY',toupper(Set_Type)),]
blm_docs = blm_docs[!grepl('PROTESTS AND RESPONSES',toupper(Set_Type)),]
blm_docs = blm_docs[!grepl('RANGELAND HEALTH',toupper(Set_Type)),]
blm_docs = blm_docs[!grepl('CONSULTATION',toupper(Set_Type)),]
blm_docs = blm_docs[!grepl('Biological Assessment|Supporting|Draft|Scoping|Notice of Availability|Federal Register|Public Comment',Set_Name),]

blm_docs = blm_docs[Set_Name!='Background NEPA and Planning Documents',]
blm_docs = blm_docs[Set_Name!='Cooperating Agencies - MOUs',]
blm_docs = blm_docs[Set_Type!='Public Comment Period Documents',]
blm_docs = blm_docs[Set_Type!='Comments on the Preliminary EA',]
blm_docs = blm_docs[Set_Name!='Background Documents',]
