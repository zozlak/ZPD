#' @title Pobiera informacje o testach / egzaminach
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
      opis AS opis_testu
    FROM 
      testy t
      LEFT JOIN arkusze a USING (arkusz)
	"
  data = tbl(src, sql(e(query)))
  return(data)
}
attr(pobierz_testy, 'grupa') = 'testy'
