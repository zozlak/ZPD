#' @title Pobiera dozwolone wartości punktowe kryteriów oceny
#' @description 
#' Pobiera dozwolone wartości punktowe kryteriów oceny
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
pobierz_schematy_pkt = function(
  src
){
  stopifnot(is.src(src))

  query = "
    SELECT 'k_' || id_kryterium AS kryterium, wartosc AS wartosc_pkt
    FROM kryteria_oceny JOIN sl_schematy_pkt_wartosci USING (schemat_pkt)
    ORDER BY 1
  "
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_schematy_pkt, 'grupa') = 'kryteriaOceny'
