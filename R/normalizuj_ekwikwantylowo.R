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

#' @title Normalizuje wyniki ekwikwantylowo
#' @description
#' Znormalizowane wyniki doklejane są do danych jako nowa zmienna z sufiksem
#' "_norm".
#' 
#' Wyniki normalizowane są do rozkładu normalnego o średniej 100 i odchyleniu
#' standardowym 15.
#' 
#' IdSkali, której norma ma zostać zastosowana jest typowo przekazywane przez
#' atrybut parametru "dane" (funkcje pobierz_wyniki_...() ustawiają go 
#' automatycznie, jeśli stosowaly skale przy pobieraniu danych). Jest to 
#' bezpieczny, a przez to najbardziej polecany sposob uzycia.
#' 
#' Jesli jednak jestesmy swiadomi ryzyka zastosowania normy niewlasciwej do 
#' danych, mozliwe jest wymuszenie skali, ktorej norma ma zostac uzyta za 
#' pomoca parametru "idSkali".
#' 
#' Normalizacji mozna tez dokonac na podstawie danych - uznane one zostana
#' wtedy za populacyjne, na ich podstawie obliczone zostana normy, ktore
#' nastepnie zostana zastosowane do danych.
#' @param dane ramka danych zawierająca wyniki
#' @param kolWynik nazwa kolumny zawierającej wyniki
#' @param src uchwyt źródła danych dplyr-a (gdy normalizacja na podstawie norm w bazie)
#' @param idSkali skala, której norma z bazy ma zostać zastosowana
#' @param zBazy czy normalizowac na podstawie norm w bazie, czy na podstawie danych
#' @import dplyr
#' @export
normalizuj_ekwikwantylowo = function(
  dane,
  src      = NULL,
  kolWynik = 'wynik',
  idSkali  = NULL,
  zBazy    = TRUE
){
  resultCol = paste0(kolWynik, '_norm')
  if(zBazy == TRUE){
    stopifnot(
      !is.null(src)
    )
    if(is.null(idSkali)){
      idSkali = attr(dane, 'idSkali')
    }
    if(is.null(idSkali)){
      stop('Nie okreslono skali, której normy mają zostać zastosowane')
    }
    norms = tbl(src, sql("SELECT * FROM normy_ekwikwantylowe")) %>%
      filter_(~id_skali == idSkali) %>%
      select_('wartosc', 'wartosc_zr') %>%
      rename_(.dots = setNames(list('wartosc', 'wartosc_zr'), c(kolWynik, resultCol)))
    if(nrow(norms %>% collect()) == 0){
      stop('W bazie nie ma określonych norm dla tej skali')
    }
  }else{
    dane = as.data.frame(dane)
    norms = suppressMessages(EWDdane::normy_ekwikwantylowe(dane[, kolWynik]))
    norms = data_frame(as.numeric(names(norms)), norms)
    names(norms) = c(kolWynik, resultCol)
  }
  
  dane = dane %>%
    left_join(norms, copy = TRUE)
  
  return(dane)
}
