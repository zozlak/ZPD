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

#' @title Pobiera dystraktory kryteriów oceny
#' @description
#' W wypadku pobierania z bazy wyników w postaci niewypunktowanej wybrana przez 
#' ucznia odpowiedź zakodowana jest liczbowo. Dane pobierane funkcją 
#' pobierz_schematy_odp() pozwalają przekodować je na faktyczne oznaczenia użyte
#' w teście.
#' 
#' Innym zastosowaniem może być sprawdzanie, czy zbiór danych z wynikami testu 
#' nie zawiera wartości spoza możliwych do wyboru dla danego zadania odpowiedzi.
#' @param src uchwyt źródła danych dplyr-a 
#' @import dplyr
#' @export
pobierz_schematy_odp = function(
  src
){
  stopifnot(is.src(src))
  
  query = "
    SELECT 'k_' || id_kryterium AS kryterium, dystraktor, kolejnosc AS kolejnosc_dystr
    FROM 
      pytania 
      JOIN kryteria_oceny USING (id_pytania) 
      JOIN sl_schematy_odp_dystr USING (schemat_odp)
    ORDER BY 1, 3
  "
  data = tbl(src, sql(query))
  return(data)
}
attr(pobierz_schematy_odp, 'grupa') = 'kryteriaOceny'
