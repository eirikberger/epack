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
FromKomnrCountyname <- function(komnr_string) {
  county_name_map <- c(
    '1'  = 'Østfold',
    '2'  = 'Akershus',
    '3'  = 'Oslo',
    '4'  = 'Hedemark',
    '5'  = 'Oppland',
    '6'  = 'Buskerud',
    '7'  = 'Vestfold',
    '8'  = 'Telemark',
    '9'  = 'Aust-Agder',
    '10' = 'Vest-Agder',
    '11' = 'Rogaland',
    '12' = 'Hordaland',
    '13' = 'Bergen',
    '14' = 'Sogn og Fjordane',
    '15' = 'Møre og Romsdal',
    '16' = 'Sør-Trøndelag',
    '17' = 'Nord-Trøndelag',
    '18' = 'Nordland',
    '19' = 'Troms',
    '20' = 'Finnmark'
  )

  num_cores <- detectCores() / 4 - 1

  result <- mclapply(komnr_vector, FromKomnrCountyname_single, mc.cores = num_cores)

  return(result)
}

#' @param komnr_string municipality number.
#' @return A string
#' @examples
#' FromKomnrCountyname_single('1205')
#'
#' @export
FromKomnrCountyname_single <- function(komnr_string) {
  tmp <- as.character(komnr_string)
  num_county <- as.integer(stringr::str_match(tmp, "^([0-9]{1,2})[0-9]{2}")[,2])

  string_county <- county_name_map[as.character(num_county)]
  if (is.null(string_county)) {
    string_county <- '(missing)'
  }

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
