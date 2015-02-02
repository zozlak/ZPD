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
#

#' @title Sumuje punkty w ramach testu
#' @description
#' Parametr "usunKryteria" dziala tylko dla danych w postaci szerokiej. Dla 
#' danych w postaci dlugiej nastepuje agregacja, ktora sila rzeczy usuwa
#' informacje o poszczegolnych kryteriach.
#' @param dane wynik dzialania dowolnej z funkcji pobierz_wyniki_...()
#' @param usunKryteria czy usuwac ze zbioru kolumny z wynikami za poszczegolne kryteria
#' @import dplyr
#' @export
zsumuj_punkty = function(
  dane,
  usunKryteria = TRUE
){
  stopifnot(
    is.data.frame(dane) | is.src(dane),
    is.vector(usunKryteria), is.logical(usunKryteria), length(usunKryteria) == 1, usunKryteria %in% c(T, F)
  )
  
  colNames = colnames(dane)
  if(sum(colNames == 'kryterium') == 1 & sum(colNames == 'ocena') == 1){
    # postać długa
    groupCols = colNames[! colNames %in% c('kryterium', 'odpowiedz', 'ocena')]
    dane = dane %>% 
      group_by_(.dots = as.list(groupCols)) %>%
      summarize_(.dots = list('wynik' = 'sum(ocena)')) %>%
      ungroup()
  } else if(sum(grepl('^[pk]_[0-9]+$', colNames)) > 0){
    # postać szeroka
    sumCols = grep('^[pk]_[0-9]+$', colNames, value = T)
    sumForm = list(wynik = paste0(sumCols, collapse = '+'))
    dane = dane %>% 
      mutate_(.dots = sumForm)
    if(usunKryteria == TRUE){
      dane = dane %>% select_(.dots = as.list(paste0('-', sumCols)))
    }
  }
  
  return(dane)
}
