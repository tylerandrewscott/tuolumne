cleanText <- function(dt,cut_prop = 0.1) {
  cut = cut_prop
  dt$text = gsub('\"\"','',dt$text,fixed = T)
  chars = nchar(dt$text)
  periods = stringr::str_count(dt$text,"\\.")
  numbers = stringr::str_count(dt$text,"[0-9]")
  caps = stringr::str_count(dt$text,'[A-Z]')
  tildes = stringr::str_count(dt$text,'~')
  quotes = stringr::str_count(dt$text,'\\"')
  spaces = stringr::str_count(dt$text,'\\s')
  dt[chars>400&{periods/chars}<cut&{quotes/chars}<cut&{tildes/chars}<cut&{numbers/chars}<cut&{caps/chars}<cut&{spaces/chars}<{cut*2},]
}