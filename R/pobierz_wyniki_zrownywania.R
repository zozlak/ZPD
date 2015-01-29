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

#' @title Pobiera ramke danych z wynikami egzaminacyjnymi testow zrownujacych
#' @param src uchwyt źródła danych dplyr-a
#' @param rodzajEgzaminu rodzaj egzaminu, ktorego wyniki maja zostac pobrane
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param rok rok, z ktorego dane maja zostac pobrane
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @import dplyr
#' @export
pobierz_wyniki_zrownywania = function(
  src,
	rodzajEgzaminu, 
	rok, 
	punktuj = TRUE, 
	idSkali = NULL,
	skroc   = TRUE
){
  stopifnot(
    is.src(src),
    is.vector(rodzajEgzaminu), is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
    is.vector(rok), is.numeric(rok), length(rok) == 1, 
    is.vector(punktuj), is.logical(punktuj), length(punktuj) == 1, punktuj %in% c(T, F),
    is.null(idSkali) | is.vector(idSkali) & is.numeric(idSkali) & length(idSkali) == 1,
    is.vector(skroc), is.logical(skroc), length(skroc) == 1, skroc %in% c(T, F)
  )
  
  regExp = paste0('^zrównywanie;', rodzajEgzaminu, ';', rok, ';.*$')
	tests = pobierz_testy(src) %>% 
    collect() %>%
    filter_(~grepl(regExp, opis_testu))
	if(nrow(tests) == 0){
		stop('w bazie nie ma takiego zrownywania')
	}
		
	tmpName = sub('[.]', '_', paste0('t', as.numeric(Sys.time(), runif(1))))
  query = sprintf(
    "SELECT zbuduj_widok_zrownywania(%s, %s, %d, %s, %s, %s, true)",
    escape(tmpName),
    escape(rodzajEgzaminu),
    rok,
    ifelse(punktuj, 'true', 'false'),
    ifelse(is.null(idSkali), 'null', as.numeric(idSkali)),
    ifelse(skroc, 'true', 'false')
  )
	DBI::dbGetQuery(src$con, query)
	data = tbl(src, sql(paste0("SELECT * FROM ", tmpName)))

	attr(data, 'idSkali') = idSkali
  
	return(data)
}
attr(pobierz_wyniki_zrownywania, 'grupa') = 'wyniki'
attr(pobierz_wyniki_zrownywania, 'testArgs') = list(
  'rodzajEgzaminu' = 'sprawdzian', 'rok' = 2013, 'idSkali' = 41
)
