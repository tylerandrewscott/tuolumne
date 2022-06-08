https://pdf-preparer-files.s3.amazonaws.com/PREPARER_20130004_noaa_4766_DS1.pdf
temp = fread('~/Desktop/file_list.csv')
base = 'https://pdf-preparer-files.s3.amazonaws.com/'

require(RCurl)
test = url.exists(paste0(base,temp$url))

batch = fread('~/Downloads/Batch_4376082_batch_results.csv',fill = T)
temp$url_exists = test

batch$Reject = ifelse(!batch$Input.url %in% temp$url[temp$url_exists],'',"URL doesn't lead to file for review")
batch$Approve = ifelse(batch$Input.url %in% temp$url[temp$url_exists],'X','')


table(batch$Reject)
fwrite(batch,'~/Desktop/Batch_4376082_batch_results.csv')
batch[Approve=='']
table(batch$Reject)
table(batch$Approve)

table(batch$Approve)
