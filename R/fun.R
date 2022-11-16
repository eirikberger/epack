#' Get information based on municipality number.
#'
#' @param komnr_string municipality number.
#' @return An integer
#' @examples
#' FromKomnrCounty('1205')
#'
#' @export
FromKomnrCounty <- function(komnr_string, type='countynr'){
  library(stringr)
  tmp <- as.character(komnr_string)
  num_county <- as.integer(str_match(tmp, "^([0-9]{1,2})[0-9]{2}")[,2])
  return(num_county)
}

#' @param komnr_string municipality number.
#' @return A string
#' @examples
#' FromKomnrCountyname('1205')
#'
#' @export
FromKomnrCountyname <- function(komnr_string, type='countynr'){
  library(stringr)
  tmp <- as.character(komnr_string)
  num_county <- as.integer(str_match(tmp, "^([0-9]{1,2})[0-9]{2}")[,2])
  string_county <- '(missing)'
  
  if (num_county==1){string_county = 'Østfold'}
  if (num_county==2){string_county = 'Akershus'}
  if (num_county==3){string_county = 'Oslo'}
  
  if (num_county==4){string_county = 'Hermark'}
  if (num_county==5){string_county = 'Oppland'}
  if (num_county==6){string_county = 'Buskerud'}
  
  if (num_county==7){string_county = 'Vestfold'}
  if (num_county==8){string_county = 'Telemark'}
  if (num_county==9){string_county = 'Aust-Agder'}
  
  if (num_county==10){string_county = 'Vest-Agder'}
  if (num_county==11){string_county = 'Rogaland'}
  if (num_county==12){string_county = 'Hordaland'}
  
  if (num_county==13){string_county = 'Bergen'}
  if (num_county==14){string_county = 'Sogn og Fjordane'}
  if (num_county==15){string_county = 'Møre og Romsdal'}
  
  if (num_county==16){string_county = 'Sør-Trøndelag'}
  if (num_county==17){string_county = 'Nord-Trøndelag'}
  if (num_county==18){string_county = 'Nordland'}
  
  if (num_county==19){string_county = 'Troms'}
  if (num_county==20){string_county = 'Finnmark'}
  
  return(string_county)
}

#' @param komnr_string municipality number.
#' @return An integer
#' @examples
#' FromKomnrKomnr('1205')
#'
#' @export
FromKomnrKomnr <- function(komnr_string, type='countynr'){
  library(stringr)
  tmp <- as.character(komnr_string)
  num_knr <- as.integer(str_match(tmp, "[0-9]{2}$"))
  return(num_knr)
}
