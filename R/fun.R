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
FromKomnrCountyname <- Vectorize(function(komnr_string, type='countynr'){
  tmp <- as.character(komnr_string)
  num_county <- as.integer(stringr::str_match(tmp, "^([0-9]{1,2})[0-9]{2}")[,2])
  string_county <- '(missing)'

  try(if (num_county==1){string_county <- 'Østfold'})
  try(if (num_county==2){string_county <- 'Akershus'})
  try(if (num_county==3){string_county <- 'Oslo'})

  try(if (num_county==4){string_county <- 'Hermark'})
  try(if (num_county==5){string_county <- 'Oppland'})
  try(if (num_county==6){string_county <- 'Buskerud'})

  try(if (num_county==7){string_county <- 'Vestfold'})
  try(if (num_county==8){string_county <- 'Telemark'})
  try(if (num_county==9){string_county <- 'Aust-Agder'})

  try(if (num_county==10){string_county <- 'Vest-Agder'})
  try(if (num_county==11){string_county <- 'Rogaland'})
  try(if (num_county==12){string_county <- 'Hordaland'})

  try(if (num_county==13){string_county <- 'Bergen'})
  try(if (num_county==14){string_county <- 'Sogn og Fjordane'})
  try(if (num_county==15){string_county <- 'Møre og Romsdal'})

  try(if (num_county==16){string_county <- 'Sør-Trøndelag'})
  try(if (num_county==17){string_county <- 'Nord-Trøndelag'})
  try(if (num_county==18){string_county <- 'Nordland'})

  try(if (num_county==19){string_county <- 'Troms'})
  try(if (num_county==20){string_county <- 'Finnmark'})

  print(num_county)

  return(string_county)
})

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
