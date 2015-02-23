#' @title Pobiera dystraktory kryteriów oceny
#' @description
#' W wypadku pobierania z bazy wyników w postaci niewypunktowanej wybrana przez 
#' ucznia odpowiedź zakodowana jest liczbowo. Dane pobierane funkcją 
#' pobierz_schematy_odp() pozwalają przekodować je na faktyczne oznaczenia użyte
#' w teście.
#' 
#' Innym zastosowaniem może być sprawdzanie, czy zbiór danych z wynikami testu 
#' nie zawiera wartości spoza możliwych do wyboru dla danego zadania odpowiedzi.
#' @param src uchwyt źródła danych dplyr-a 
#' @import dplyr
#' @export
pobierz_schematy_odp = function(
  src
){
  stopifnot(is.src(src))
  
  query = "
    SELECT 'k_' || id_kryterium AS kryterium, dystraktor, kolejnosc AS kolejnosc_dystr
    FROM 
      pytania 
      JOIN kryteria_oceny USING (id_pytania) 
      JOIN sl_schematy_odp_dystr USING (schemat_odp)
    ORDER BY 1, 3
  "
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_schematy_odp, 'grupa') = 'kryteriaOceny'
