#  Copyright 2013-2015 Mateusz Zoltak
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU Lesser General Public License as published by
#    the Free Software Foundation; either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
#    Niniejszy program jest wolnym oprogramowaniem; mozesz go
#    rozprowadzac dalej i/lub modyfikowac na warunkach Mniej Powszechnej
#    Licencji Publicznej GNU, wydanej przez Fundacje Wolnego
#    Oprogramowania - wedlug wersji 3 tej Licencji lub (wedlug twojego
#    wyboru) ktorejs z pozniejszych wersji.
#
#    Niniejszy program rozpowszechniany jest z nadzieja, iz bedzie on
#    uzyteczny - jednak BEZ JAKIEJKOLWIEK GWARANCJI, nawet domyslnej
#    gwarancji PRZYDATNOSCI HANDLOWEJ albo PRZYDATNOSCI DO OKRESLONYCH
#    ZASTOSOWAN. W celu uzyskania blizszych informacji siegnij do
#    Powszechnej Licencji Publicznej GNU.
#
#    Z pewnoscia wraz z niniejszym programem otrzymales tez egzemplarz
#    Powszechnej Licencji Publicznej GNU (GNU General Public License);
#    jesli nie - napisz do Free Software Foundation, Inc., 59 Temple
#    Place, Fifth Floor, Boston, MA  02110-1301  USA

#' @title Zwraca wskaźniki zapisane w bazie
#' @description
#' Każdy wskaźnik jest powielony w zwracanej tabeli tyle razy, ile wynosi 
#' iloczyn liczby typów szkół, do których ma zastosowanie, liczby częśći
#' egzaminu, do których ma zastosowanie oraz liczby skalowań, na których
#' wynikach się opiera. Dzięki temu możliwe jest łatwe filtrowanie po typach
#' szkół i/lub przedmiotach bez konieczności tworzenia dodatkowych grup danych.
#' Aby uzyskać listę, w której kolumna "id_wskaznika" ma unikalne wartości
#' nelaży usunąć kolumny "typ_szkoly" i "przedmiot" oraz skorzystać z czasownika
#' "distinct()".
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
    "SELECT %s, tsz.typ_szkoly, rodzaj_egzaminu, czesc_egzaminu, ws.id_skali, ws.skalowanie
    FROM 
      sl_wskazniki w
      LEFT JOIN sl_wskazniki_typy_szkol tsz USING (rodzaj_wsk, wskaznik)
      LEFT JOIN (SELECT * FROM sl_kategorie_lu WHERE rodzaj_egzaminu IS NOT NULL) p USING (rodzaj_wsk, wskaznik)
      LEFT JOIN wskazniki USING (rodzaj_wsk, wskaznik)
      LEFT JOIN wskazniki_skalowania ws USING (rodzaj_wsk, wskaznik, rok_do)",
    ifelse(
      wszystkieKolumny == TRUE, 
      'w.*', 
      'rodzaj_wsk, wskaznik, okres, do_prezentacji, skrot, opis'
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
  data = data %>%
    rename_(.dots = list(opis_wskaznika = 'opis'))
  return(data)  
}
attr(pobierz_wskazniki, 'grupa') = 'wskazniki'
