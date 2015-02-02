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

#' @title Pobiera dozwolone wartości punktowe kryteriów oceny
#' @description 
#' Zwraca albo pełen plan zapytania (pelnyPlan = TRUE) albo skrótowe
#' podsuomowanie szacowanego kosztu wykonania zapytania.
#' 
#' W wypadku zwracania tylko podsumowania wyswietlane są dolny i górny szacunek
#' w jednostkach szacowania czasu planera wraz z komentarzem opisującym rząd
#' wielkości czasu, jakiemu dana wartość odpowiada.
#' @param dane ramka danych dplyr-a
#' @param pelnyPlan czy zwracać pełny plan zapytania czy tylko łączny koszt (zawsze TRUE gdy format inny niż TEXT)
#' @param format format zwracanych danych: TEXT, XML, JSON lub YAML
#' @import dplyr
#' @export
oszacuj_czas_wykonania = function(
  dane,
  pelnyPlan = FALSE,
  format = 'TEXT'
){
  stopifnot(
    is.tbl(dane),
    is.vector(pelnyPlan), is.logical(pelnyPlan), length(pelnyPlan) == 1,
    is.vector(format), is.character(format), length(format) == 1, format %in% c('TEXT', 'XML', 'JSON', 'YAML')
  )
  if(!any(class(dane) %in% 'tbl_sql')){
    stop('Dane zostały już pobrane z serwera')
  }
  query = paste0(
    'EXPLAIN (FORMAT ', format, ') ',
    dane$query$sql
  )
  results = DBI::dbGetQuery(dane$src$con, query)
  
  if(pelnyPlan == FALSE & format == 'TEXT'){
    results = results[1, 1]
    results = sub('.*cost=([.0-9]+).*', '\\1', results)
    results = as.numeric(strsplit(results, '[.][.]')[[1]])
    names(results) = cut(results, breaks = c(0, 10^4, 10^5, 10^6, 10^12), labels = c('sekundy', '< minuty', 'kilka minut', 'kilkadziesiąt minut'))
  }
  return(results)
}