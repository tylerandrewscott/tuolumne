
library(data.table)
library(stringr)

lda = readRDS('scratch/boilerplate/lda_combo_results_V2.RDS')
lda$a_id = str_remove(lda$a,'(--|_).*')
lda$b_id = str_remove(lda$b,'(--|_).*')
lda$a = str_replace(lda$a,"([0-9]{1,}--)\\1",'\\1')
lda$b = str_replace(lda$b,"([0-9]{1,}--)\\1",'\\1')
lda$a_p = str_extract(lda$a,'[0-9]{1,}$')
lda$b_p =  str_extract(lda$b,'[0-9]{1,}$')
lda$a_file = gsub('^--','',str_extract(lda$a,'(?:--).+(?=--)'))
lda$b_file = gsub('^--','',str_extract(lda$b,'(?:--).+(?=--)'))
fwrite(lda,'scratch/boilerplate/lda_combo_results_V2.csv')

