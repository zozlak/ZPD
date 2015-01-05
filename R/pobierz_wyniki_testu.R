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
#

#' @title Pobiera wyniki testu
#' @param src uchwyt źródła danych dplyr-a
#' @param idTestu identyfikator testu do pobrania
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @import dplyr
#' @export
pobierz_wyniki_testu = function(
  src,
  idTestu, 
  punktuj        = TRUE,
  idSkali        = NULL,
  skroc          = FALSE
){
  query = sprintf(
    "SELECT zbuduj_widok_testu('tmp_view', %d, %s, %s, %s, true);",
    idTestu,
    ifelse(punktuj, 'true', 'false'),
    ifelse(is.null(idSkali), 'null', idSkali),
    ifelse(skroc, 'true', 'false')
  )
  # R Postgresql DBI driver is extremely stupid and switches every message from
  # a database into R error.
  # This means "DROP VIEW IF EXISTS view_name;" executed on the backstage of the
  # zbuduj_widok_czesci_egzaminu() call will generate an R error if a view named
  # "view_name" doesn't exist.
  # So we need to make sure it exists before calling zbuduj_widok_czesci_egzaminu()
  dbGetQuery(src$con, "CREATE TEMPORARY VIEW tmp_view AS SELECT 1")
  dbGetQuery(src$con, query)
  data = tbl(src, sql("SELECT * FROM tmp_view"))
  return(data)	
}	

#' @rdname polacz
#' @export
get_test_results = pobierz_wyniki_testu