#' @title Pobiera informacje o kryteriach oceny i pytaniach oraz w których
#'   testach występują
#' @description Każde kryterium oceny występuje w danych tyle razy, w ilu
#' różnych testach występuje.
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
pobierz_kryteria_oceny = function(
  src
){
  stopifnot(is.src(src))
  data = tbl(src, sql(e("SELECT * FROM widoki.pobierz_kryteria_oceny")))
  return(data)
}
attr(pobierz_kryteria_oceny, 'grupa') = 'kryteriaOceny'
