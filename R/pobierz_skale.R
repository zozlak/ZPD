#' @title Pobiera skale i skalowania
#' @description
#' Pobiera listę skal połączoną z listą skalowań oraz testami powiązanymi z daną
#' skalą. Tak więc każda skala występować będzie tyle razy, ile wynosi iloczyn
#' liczby testów, z którymi jest powiązana oraz liczby skalowań, które na niej
#' bazują.
#' 
#' Domyślnie pomijane są skale i skalowania, które nie zostały oznaczone jako
#' do_prezentacji. W większości wypadków powoduje to, że danej skali przypisane
#' jest co najwyżej jedno skalowanie, co powinno ułatwiać wybór poszukiwanego
#' skalowania. Jeśli chcemy uzyskać listę wszystkich skal, należy parametr
#' "doPrezentacji" ustawić na NA.
#' 
#' Domyślnie pomijane są wszystie skale i skalowania KTT. Aby wyszukać skale i
#' skalowania KTT, ustaw parametr "ktt" na TRUE.
#' @param src uchwyt źródła danych dplyr-a
#' @param doPrezentacji czy wyświetlać tylko skale i skalowania oznaczone do
#'   publicznej prezentacji
#' @param czyKtt czy czy wyświetlać skale i skalowania KTT
#' @import dplyr
#' @export
pobierz_skale = function(
  src,
  doPrezentacji = TRUE,
  czyKtt = FALSE
){
  stopifnot(
    is.src(src),
    is.null(doPrezentacji) | 
      is.vector(doPrezentacji) & is.logical(doPrezentacji) & length(doPrezentacji) == 1,
    is.vector(czyKtt), is.logical(czyKtt), length(czyKtt) == 1, czyKtt %in% c(TRUE, FALSE)
  )
  
  query = "
    SELECT 
      id_skali, s.opis AS opis_skali, nazwa AS nazwa_skali, rodzaj_skali, 
      s.do_prezentacji AS skala_do_prezentacji,
      COALESCE(t.rodzaj_egzaminu, a.rodzaj_egzaminu) AS rodzaj_egzaminu ,
      COALESCE(t.czesc_egzaminu, a.czesc_egzaminu) AS czesc_egzaminu, 
      id_testu,
      extract(year from COALESCE(t.data, a.data_egzaminu)) AS rok, 
      skalowanie, ss.opis AS opis_skalowania, estymacja, ss.data AS data_skalowania,
      ss.do_prezentacji AS skalowanie_do_prezentacji,
      n.id_skali IS NOT NULL AS normy_ekwikwantylowe,
      eap.posiada_eap, pv.posiada_pv
    FROM
      skale s
      LEFT JOIN skalowania ss USING (id_skali)
      LEFT JOIN (
        SELECT DISTINCT id_skali FROM normy_ekwikwantylowe
      ) AS n USING (id_skali)
      JOIN skale_testy st USING (id_skali)
      JOIN testy t USING (id_testu)
      LEFT JOIN arkusze a USING (arkusz)
      LEFT JOIN (
        SELECT id_skali, skalowanie, true AS posiada_eap
        FROM skalowania ss1
        WHERE
          EXISTS (
            SELECT 1 
            FROM skalowania_obserwacje so1 
            WHERE 
              estymacja = 'EAP' 
              AND (ss1.id_skali, ss1.skalowanie) = (so1.id_skali, so1.skalowanie)
          )
      ) AS eap USING (id_skali, skalowanie)
      LEFT JOIN (
        SELECT id_skali, skalowanie, true AS posiada_pv
        FROM skalowania ss2
        WHERE
          EXISTS (
            SELECT 1 
            FROM skalowania_obserwacje so2
            WHERE 
              estymacja = 'PV' 
              AND (ss2.id_skali, ss2.skalowanie) = (so2.id_skali, so2.skalowanie)
          )
      ) AS pv USING (id_skali, skalowanie)
  "
  
  where = c()
  if(!is.null(doPrezentacji) & !is.na(doPrezentacji)){
    where = append(
      where, 
      sprintf(
        "s.do_prezentacji = %s AND (ss.do_prezentacji = %s OR ss.do_prezentacji IS NULL)",
        ifelse(doPrezentacji == TRUE, 'true', 'false'),
        ifelse(doPrezentacji == TRUE, 'true', 'false')
      )
    )
  }
  if(czyKtt == FALSE){
    where = append(where, "s.rodzaj_skali <> 'ktt'")
  }
  if(length(where) > 0){
    query = sprintf('%s WHERE %s', query, paste0(where, collapse = ' AND '))
  }
  
  data = tbl(src, sql(e(query)))
  return(data)  
}
attr(pobierz_skale, 'grupa') = 'skale'
