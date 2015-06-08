#' @title Pobiera informacje o kryteriach oceny i pytaniach oraz w których
#'   testach występują
#' @description Każde kryterium oceny występuje w danych tyle razy, w ilu
#' różnych testach występuje.
#' 
#' Parametry \code{testy} i \code{skale} wpływają z jednej strony (w mniejszym
#' stopniu) na szybkość pobierania danych (pobieranie każdej z tych informacji
#' wydłuża czas ok. dwukrotnie), z drugiej zaś strony na liczbę zwracanych
#' wierszy (rzędu 10^4 bez informacji o testach i skalach, ok. dwukrotnie więcej
#' z informacją o testach lub skalach, rzędu 10^5 z informacją o testach i o
#' skalach).
#' @param src uchwyt źródła danych dplyr-a
#' @param testy czy pobierać informacje o występowaniu kryteriów w testach
#'   (zmienne id_testu, kolejnosc_w_tescie, popr_dystraktor)
#' @param skale czy pobierać informacje o występowaniu kryteriów w skalach
#'   (zmienne id_skali, kolejnosc_w_skali)
#' @import dplyr
#' @export
pobierz_kryteria_oceny = function(
  src,
  testy = TRUE,
  skale = TRUE
){
  stopifnot(
    is.src(src),
    is.vector(testy), is.logical(testy), length(testy) == 1, all(!is.na(testy)),
    is.vector(skale), is.logical(skale), length(skale) == 1, all(!is.na(skale))
  )
  
  data = tbl(src, sql(e("SELECT * FROM widoki.pobierz_kryteria_oceny")))
  if(testy){
    query = "
      SELECT 
        'k_' || id_kryterium AS kryterium, 
        id_testu, 
        kolejnosc AS kolejnosc_w_tescie, 
        popr_dystraktor
      FROM testy_kryteria
      UNION
      SELECT DISTINCT
        'p_' || id_pseudokryterium AS kryterium,
        id_testu, 
        NULL::int, NULL::int
      FROM
        pseudokryteria_oceny_kryteria 
        JOIN testy_kryteria USING (id_kryterium)"
    data = data %>%
      left_join(tbl(src, sql(e(query))))
  }
  if(skale){
    query = "
      SELECT
        COALESCE('k_' || id_kryterium, 'p_' || id_pseudokryterium) AS kryterium,
        id_skali,
        kolejnosc AS kolejnosc_w_skali
      FROM skale_elementy"
    data = data %>%
      left_join(tbl(src, sql(e(query))))
  }

  return(data)
}
attr(pobierz_kryteria_oceny, 'grupa') = 'kryteriaOceny'
