#' @title Zwraca wskaźniki zapisane w bazie
#' @description
#' Każdy wskaźnik jest powielony w zwracanej tabeli tyle razy, ile wynosi 
#' iloczyn liczby typów szkół, do których ma zastosowanie, liczby częśći 
#' egzaminu, do których ma zastosowanie, liczby okresów, dla których wskaźnik
#' był obliczany oraz liczby skalowań, na których wynikach się opiera. Dzięki
#' temu możliwe jest łatwe filtrowanie po typach szkół i/lub przedmiotach bez
#' konieczności tworzenia dodatkowych grup danych. Aby uzyskać listę, w której
#' kolumna "id_wskaznika" ma unikalne wartości nelaży usunąć kolumny
#' "typ_szkoly" i "przedmiot" oraz skorzystać z czasownika "distinct()".
#' 
#' Domyśle odfiltrowywanie jedynie wskaźników do prezentacji służy ułatwieniu 
#' wyszukiwania wskaźników przez większość użytkowników.
#' 
#' Parametr "wszystkieKolumny" pozwala pobrać również kolumny mające jedynie 
#' zastosowania techiczne, np. kolor prezntacji na stronie WWW, dopełniacz nazwy
#' egzaminu wyświetlany przy osi wykresu na stronie WWW, itp.
#' @param src uchwyt źródła danych dplyr-a
#' @param doPrezentacji czy pobierać tylko wskaźniki oznaczone do publicznej
#'   prezentacji
#' @param wszystkieKolumny czy pobierać także techniczne kolumny używane do
#'   prezentacji wskaźników na stronach WWW
#' @import dplyr
#' @export
pobierz_wskazniki = function(
  src,
  doPrezentacji = TRUE,
  wszystkieKolumny = FALSE
){
  stopifnot(
    is.src(src),
    is.vector(doPrezentacji) & is.logical(doPrezentacji) & length(doPrezentacji) == 1,
    is.vector(wszystkieKolumny), is.logical(wszystkieKolumny), length(wszystkieKolumny) == 1, wszystkieKolumny %in% c(TRUE, FALSE)
  )
  
  query = sprintf(
    "SELECT %s, rok_do, tsz.typ_szkoly, rodzaj_egzaminu, czesc_egzaminu, ws.id_skali, ws.skalowanie
    FROM 
      sl_wskazniki w
      LEFT JOIN sl_wskazniki_typy_szkol tsz USING (rodzaj_wsk, wskaznik)
      LEFT JOIN (SELECT * FROM sl_kategorie_lu WHERE rodzaj_egzaminu IS NOT NULL) p USING (rodzaj_wsk, wskaznik)
      LEFT JOIN wskazniki USING (rodzaj_wsk, wskaznik)
      LEFT JOIN wskazniki_skalowania ws USING (rodzaj_wsk, wskaznik, rok_do)",
    ifelse(
      wszystkieKolumny == TRUE, 
      'w.okres, w.nazwa, w.kolejnosc, w.do_prezentacji AS wsk_do_prezentacji, 
        w.kolor, w.skrot, w.os_y, w.egz_dop, w.opis AS opis_wsk, w.grupa, w.rodzaj_wsk, w.wskaznik', 
      'w.rodzaj_wsk, w.wskaznik, w.okres, w.do_prezentacji AS wsk_do_prezentacji, w.skrot, w.opis AS opis_wsk'
    )
  )
  if(!is.na(doPrezentacji)){
    query = sprintf(
      "%s WHERE do_prezentacji = %s",
      query,
      ifelse(doPrezentacji == TRUE, 'true', 'false')
    )
  }
  data = tbl(src, sql(query))
  return(data)  
}
attr(pobierz_wskazniki, 'grupa') = 'wskazniki'
