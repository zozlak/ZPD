#' @title Pobiera parametry kryteriów i pseudokryteriów obliczone w skalowaniach
#' @description 
#' @param src uchwyt źródła danych dplyr-a 
#' @import dplyr
#' @export
pobierz_parametry = function(
  src
){
  stopifnot(is.src(src))
  data = tbl(src, sql(e("SELECT * FROM widoki.pobierz_parametry")))
  return(data)  
}
attr(pobierz_parametry, 'grupa') = 'parametry'
