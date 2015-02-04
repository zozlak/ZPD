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
      COALESCE(t.rodzaj_egzaminu_, a.rodzaj_egzaminu) AS rodzaj_egzaminu ,
      COALESCE(t.czesc_egzaminu_, a.czesc_egzaminu) AS czesc_egzaminu, 
      id_testu,
      extract(year from COALESCE(t.data, a.data_egzaminu)) AS rok, 
      skalowanie, ss.opis AS opis_skalowania, estymacja, ss.data AS data_skalowania,
      ss.do_prezentacji AS skalowanie_do_prezentacji,
      n.id_skali IS NOT NULL AS normy_ekwikwantylowe      
    FROM
      skale s
      LEFT JOIN skalowania ss USING (id_skali)
      LEFT JOIN (
        SELECT DISTINCT id_skali FROM normy_ekwikwantylowe
      ) AS n USING (id_skali)
      JOIN skale_testy st USING (id_skali)
      JOIN testy t USING (id_testu)
      LEFT JOIN arkusze a USING (arkusz)
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
  
  data = tbl(src, sql(query))
  return(data)  
}
attr(pobierz_skale, 'grupa') = 'skale'
