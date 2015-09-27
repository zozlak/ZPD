#' @title Pobiera z bazy normy (np. zrównywania lub ekwikwantylowe)
#' @param src uchwyt źródła danych dplyr-a (np. z funkcji polacz())
#' @import dplyr
#' @export
pobierz_normy = function(
  src
){
  stopifnot(is.src(src))
  
  return(tbl(src, sql(e("SELECT * FROM normy"))))
}
attr(pobierz_normy, 'grupa') = 'normy'