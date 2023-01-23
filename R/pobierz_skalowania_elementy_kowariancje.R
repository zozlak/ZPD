#' @title Pobiera dane z tablicy skalowania_elementy_kowariancje
#' @description 
#' Pobiera dane z tablicy skalowania_elementy_kowariancje
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
pobierz_skalowania_elementy_kowariancje = function(
    src
){
  stopifnot(is.src(src))
  
  query = "SELECT * FROM skalowania_elementy_kowariancje"
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_skalowania_elementy_kowariancje, 'grupa') = 'parametry'
