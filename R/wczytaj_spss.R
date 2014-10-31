#  Copyright 2013 Mateusz Zoltak
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

#' @title Wczytuje wskazany plik SPSS stosujac standardowe przeksztalcenia
#' @description 
#' Funkcja:
#' \itemize{
#' 	\item wczytuje plik SPSS z to.data.frame=T, use.missings=F, trim.factor.names=T
#' 	\item zamienia nazwy wszystkich zmiennych na pisane malymi literami
#' 	\item zamienia wszystkie factor-y na zwykle zmienne typu character
#' 	\item wykonuje str_trim() na wszystkich zmiennych
#' }
#' @param plik sciezka do pliku SPSS
#' @return [data.frame] wczytane dane
#' @examples
#' \dontrun{
#' 	wczytaj_spss('gim10_07.sav')
#' }
#' @import stringr
#' @import foreign
#' @export
wczytaj_spss = function(plik){
  dane = suppressWarnings(suppressMessages(read.spss(plik, to.data.frame=T, use.missings=F, trim.factor.names=T)))
  names(dane) = tolower(names(dane))
  for(i in 1:ncol(dane)){
    if(is.factor(dane[, i]))
      dane[, i] = as.character(dane[, i])
    if(is.character(dane[, i]))
      dane[, i] = str_trim(dane[, i])
  }
  return(dane)
}
