#' @title Pobiera informacje o testach / egzaminach
#' @description 
#' Pobiera informacje o testach / egzaminach
#' @param src uchwyt źródła danych dplyr-a
#' @import dplyr
#' @export
pobierz_testy = function(
  src
){
  stopifnot(is.src(src))

  query = "
    SELECT 
      id_testu, ewd AS dane_ewd, arkusz, 
      COALESCE(a.rodzaj_egzaminu, t.rodzaj_egzaminu) AS rodzaj_egzaminu,
      COALESCE(a.czesc_egzaminu, t.czesc_egzaminu) AS czesc_egzaminu, 
      extract(year FROM COALESCE(a.data_egzaminu, t.data)) AS rok,
      COALESCE(a.data_egzaminu, t.data) AS data_testu, 
      arkusz IS NOT NULL AS czy_egzamin,
      COALESCE(opis, arkusz) AS opis_testu,
      prefiks
    FROM 
      testy t
      LEFT JOIN arkusze a USING (arkusz)
      LEFT JOIN sl_czesci_egzaminow sce ON (COALESCE(t.rodzaj_egzaminu, a.rodzaj_egzaminu), COALESCE(t.czesc_egzaminu, a.czesc_egzaminu))  = (sce.rodzaj_egzaminu, sce.czesc_egzaminu)
	"
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_testy, 'grupa') = 'testy'
