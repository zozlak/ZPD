#' @title Pobiera dozwolone wartości punktowe kryteriów oceny
#' @description 
#' Zwraca albo pełen plan zapytania (pelnyPlan = TRUE) albo skrótowe
#' podsuomowanie szacowanego kosztu wykonania zapytania.
#' 
#' W wypadku zwracania tylko podsumowania wyswietlane są dolny i górny szacunek
#' w jednostkach szacowania czasu planera wraz z komentarzem opisującym rząd
#' wielkości czasu, jakiemu dana wartość odpowiada.
#' @param dane ramka danych dplyr-a
#' @param pelnyPlan czy zwracać pełny plan zapytania czy tylko łączny koszt (zawsze TRUE gdy format inny niż TEXT)
#' @param format format zwracanych danych: TEXT, XML, JSON lub YAML
#' @import dplyr
#' @export
oszacuj_czas_wykonania = function(
  dane,
  pelnyPlan = FALSE,
  format = 'TEXT'
){
  stopifnot(
    is.tbl(dane),
    is.vector(pelnyPlan), is.logical(pelnyPlan), length(pelnyPlan) == 1,
    is.vector(format), is.character(format), length(format) == 1, format %in% c('TEXT', 'XML', 'JSON', 'YAML')
  )
  if(!any(class(dane) %in% 'tbl_sql')){
    stop(e('Dane zostały już pobrane z serwera'))
  }
  query = paste0(
    'EXPLAIN (FORMAT ', format, ') ',
    dane$query$sql
  )
  results = DBI::dbGetQuery(dane$src$con, e(query))
  
  if(pelnyPlan == FALSE & format == 'TEXT'){
    results = results[1, 1]
    results = sub('.*cost=([.0-9]+).*', '\\1', results)
    results = as.numeric(strsplit(results, '[.][.]')[[1]])
    names(results) = cut(results, breaks = c(0, 10^4, 10^5, 10^6, 10^12), labels = e(c('sekundy', '< minuty', 'kilka minut', 'kilkadziesiąt minut')))
  }
  return(results)
}
