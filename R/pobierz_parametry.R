#' @title Pobiera parametry kryteriów i pseudokryteriów obliczone w skalowaniach
#' @param src uchwyt źródła danych dplyr-a 
#' @import dplyr
#' @export
pobierz_parametry = function(
  src
){
  stopifnot(is.src(src))
  
  query = "
    SELECT 
      id_skali, skalowanie, 
      COALESCE('k_' || id_kryterium, 'p_' || id_pseudokryterium) AS kryterium, 
      parametr, model, wartosc, bs
    FROM
      skalowania_elementy
      LEFT JOIN skale_elementy USING (id_skali, kolejnosc)
    ORDER BY id_skali, kolejnosc
  "
  data = tbl(src, sql(query))
  return(data)  
}
attr(pobierz_parametry, 'grupa') = 'parametry'
