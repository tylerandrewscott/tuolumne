cleanText <- function(flist_dt,cut_prop = 0.1) {
  cut = cut_prop
  flist_dt$EIS.Number <- str_remove(flist_dt$File,'_.*')
  flist_dt <- flist_dt[EIS.Number %in% projects$EIS.Number,]
  flist_dt$text = gsub('\"\"','',flist_dt$text,fixed = T)
  chars = nchar(flist_dt$text)
  periods = stringr::str_count(flist_dt$text,"\\.")
  numbers = stringr::str_count(flist_dt$text,"[0-9]")
  caps = stringr::str_count(flist_dt$text,'[A-Z]')
  tildes = stringr::str_count(flist_dt$text,'~')
  quotes = stringr::str_count(flist_dt$text,'\\"')
  spaces = stringr::str_count(flist_dt$text,'\\s')
  flist_dt  = flist_dt[chars>400&{periods/chars}<cut&{quotes/chars}<cut&{tildes/chars}<cut&{numbers/chars}<cut&{caps/chars}<cut&{spaces/chars}<{cut*2},]
}