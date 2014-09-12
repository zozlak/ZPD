#	Copyright 2013 Mateusz Zoltak
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
#' @description
#' _
#' @param rodzajEgzaminu rodzaj egzaminu, ktorego wyniki maja zostac pobrane
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param rok rok, z ktorego dane maja zostac pobrane
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return data frame
#' @export
pobierz_zrownywanie=function(
	rodzajEgzaminu, 
	rok, 
	punktuj=TRUE, 
	idSkali=NULL,
	skroc=TRUE,
	zrodloDanychODBC='EWD'
){
	P = odbcConnect(zrodloDanychODBC, readOnlyOptimize=T)
	tryCatch({
		if(!is.character(rodzajEgzaminu) | !is.vector(rodzajEgzaminu) | length(rodzajEgzaminu)>1)
			stop('rodzajEgzaminu nie jest lancuchem znakow')
		if(!is.numeric(rok))
			stop('rok nie jest liczba')
		if(!is.logical(punktuj) | !is.vector(punktuj) | length(punktuj)>1)
			stop('punktuj nie jest wartoscia logiczna')
		if((!is.numeric(idSkali) | !is.vector(idSkali) | length(idSkali)>1) & !is.null(idSkali))
			stop('idSkali nie jest liczba')
		if(!is.logical(skroc) | !is.vector(skroc) | length(skroc)>1)
			stop('skroc nie jest wartoscia logiczna')
		if(!is.null(idSkali))
			idSkali = as.character(idSkali)
		else idSkali = NA
		
		ile = .sqlQuery(
			P, 
			"SELECT count(*) FROM testy WHERE opis LIKE ?", 
			paste0('zrównywanie;%;', rok, ';%')
		)
		if(ile[1, 1] == 0){
			stop('w bazie nie ma takiego zrownywania')
		}
		
		tmp = .sqlQuery(
			P, 
			"SELECT zbuduj_widok_zrownywania('tmp', ?, ?, ?, ?, ?);", 
			list(rodzajEgzaminu, rok, punktuj, idSkali, skroc)
		)
		dane = .sqlQuery(P, "SELECT * FROM tmp")
		odbcClose(P)
		return(dane)
	},
	error=function(e){
		odbcClose(P)
		.stop(e)
	})
}
