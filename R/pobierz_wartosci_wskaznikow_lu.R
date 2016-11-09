#' @title Pobiera informacje o liczbie uczniów uwzględnionych przy wyliczaniu
#'   wskaźników w podziale na przedmioty (dokładniej części egzaminu)
#' @description 
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
pobierz_wartosci_wskaznikow_lu = function(
  src
){
  stopifnot(is.src(src))
  
  query = "
    SELECT 
      id_ww, rodzaj_egzaminu, czesc_egzaminu, 
      lu AS przedm_lu, 
      lu_ewd AS przedm_lu_ewd, 
      lu_wszyscy AS przedm_lu_wszyscy
    FROM
      liczba_zdajacych
      JOIN sl_kategorie_lu USING (rodzaj_wsk, wskaznik, kategoria_lu)
    WHERE
      czesc_egzaminu IS NOT NULL
    ORDER BY 1, 2, 3
  "
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_wartosci_wskaznikow_lu, 'grupa') = 'wartosciWskaznikow'
