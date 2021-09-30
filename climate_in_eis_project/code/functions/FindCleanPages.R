
FindCleanPages = function(file){
  if(!require(stringr)){
    {print('please install stringr');break}
  }
    pars = file$text
    pars <- stringr::str_replace_all(pars,'\\s{2,}',' ')
    chars = nchar(pars)
    periods = stringr::str_count(pars,"\\.")
    numbers = stringr::str_count(pars,"[0-9]")
    caps = stringr::str_count(pars,'[A-Z]')
    spaces = stringr::str_count(pars,'\\s')
    filtered_pars = chars>300&{periods/chars}<0.1&{numbers/chars}<0.1&{caps/chars}<0.1&!grepl('http',pars)&{spaces/chars}<0.2
    return(file[filtered_pars,])
}