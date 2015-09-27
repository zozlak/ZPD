#' @title Pobiera informacje o skalach i skalowaniach
#' @description
#' Pobiera listę skal, skalowań i grup wyróżnionych w ramach skalowań połączoną
#' z listą testów przypisanych danej skali.
#' 
#' Ponieważ domyślna forma pobierania danych może być zbyt szczegółowa, za 
#' pomocą parametrów \code{skalowania} oraz \code{grupy} można pominąć 
#' pobieranie informacji skalowaniach i/lub grupach wyróżnionych w ramach
#' skalowań.
#' 
#' Domyślnie pomijane są skale i skalowania, które nie zostały oznaczone jako
#' do_prezentacji. W większości wypadków powoduje to, że danej skali przypisane
#' jest co najwyżej jedno skalowanie, co powinno ułatwiać wybór poszukiwanego
#' skalowania. Jeśli chcemy uzyskać listę wszystkich skal, należy parametr
#' \code{doPrezentacji} ustawić na NA.
#' 
#' Domyślnie pomijane są wszystie skale i skalowania KTT. Aby wyszukać skale i
#' skalowania KTT, ustaw parametr "ktt" na TRUE.
#' @param src uchwyt źródła danych dplyr-a
#' @param doPrezentacji czy wyświetlać tylko skale i skalowania oznaczone do
#'   publicznej prezentacji
#' @param czyKtt czy czy wyświetlać skale i skalowania KTT
#' @param testy czy pobierać informację o powiązaniu skal z testami
#' @param skalowania czy pobierać informacje o skalowaniach w ramach skal
#' @param grupy czy pobierać informacje o grupach w ramach skalowań (tylko gdy parametr skalowania = TRUE)
#' @import dplyr
#' @export
pobierz_skale = function(
  src,
  doPrezentacji = TRUE,
  czyKtt = FALSE,
  skalowania = TRUE,
  grupy = TRUE
){
  stopifnot(
    is.src(src),
    is.vector(doPrezentacji), is.logical(doPrezentacji), length(doPrezentacji) == 1,
    is.vector(czyKtt), is.logical(czyKtt), length(czyKtt) == 1, all(czyKtt %in% c(TRUE, FALSE)),
    is.vector(skalowania), is.logical(skalowania), length(skalowania) == 1, all(skalowania %in% c(TRUE, FALSE)),
    is.vector(grupy), is.logical(grupy), length(grupy) == 1, all(grupy %in% c(TRUE, FALSE))
  )
  
  select = "
    id_skali, s.opis AS opis_skali, rodzaj_skali, 
    s.do_prezentacji AS skala_do_prezentacji,
    COALESCE(t.rodzaj_egzaminu, a.rodzaj_egzaminu) AS rodzaj_egzaminu ,
    COALESCE(t.czesc_egzaminu, a.czesc_egzaminu) AS czesc_egzaminu, 
    id_testu,
    extract(year from COALESCE(t.data, a.data_egzaminu)) AS rok 
  "
  from = "
    skale s
    LEFT JOIN skale_testy st USING (id_skali)
    LEFT JOIN testy t USING (id_testu)
    LEFT JOIN arkusze a USING (arkusz)
  "
  where = c()
  if(!is.na(doPrezentacji)){
    where = append(
      where, 
      paste0("s.do_prezentacji = ", ifelse(doPrezentacji, 'true', 'false'))
    )
  }
  
  if(skalowania){
    select = paste0(
      select,
      ",
        skalowanie, ss.opis AS opis_skalowania, ss.estymacja, ss.data AS data_skalowania,
        ss.do_prezentacji AS skalowanie_do_prezentacji,
        n.id_skali IS NOT NULL AS posiada_normy,
        COALESCE(eap.n > 0, false) AS posiada_eap,
        COALESCE(pv.n > 0, false) AS posiada_pv
      "
    )
    from = paste0(
      from,
      "
        LEFT JOIN skalowania ss USING (id_skali)
        LEFT JOIN (
          SELECT DISTINCT id_skali, skalowanie FROM normy
        ) AS n USING (id_skali, skalowanie)
        LEFT JOIN (
          SELECT * FROM widoki.skalowania_obserwacje_eap_pv WHERE estymacja = 'EAP'
        ) AS eap USING (id_skali, skalowanie)
        LEFT JOIN (
          SELECT * FROM widoki.skalowania_obserwacje_eap_pv WHERE estymacja = 'PV'
        ) AS pv USING (id_skali, skalowanie)
      "
    )
    if(!is.na(doPrezentacji)){
      where = append(
        where, 
        paste0("(ss.do_prezentacji = ", ifelse(doPrezentacji, 'true', 'false'), " OR ss.do_prezentacji IS NULL )")
      )
    }
  }
  
  if(grupy & skalowania){
    select = paste0(select, ", sg.grupa")
    from = paste0(from, " LEFT JOIN skalowania_grupy sg USING (id_skali, skalowanie) ")
  }

  if(czyKtt == FALSE){
    where = append(where, "s.rodzaj_skali <> 'ktt'")
  }

  where = ifelse(length(where) > 0, paste('WHERE', paste0(where, collapse = ' AND ')), '')
    
  query = paste(
    "SELECT", select, 
    "FROM", from,
    where
  )

  data = tbl(src, sql(e(query)))
  return(data)  
}
attr(pobierz_skale, 'grupa') = 'skale'
