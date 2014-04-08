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

#' @title Pobiera ramke danych z wynikami egzaminacyjnymi wskazanej czesci egzaminu
#' @description
#' _
#' @param rodzajEgzaminu rodzaj egzaminu, ktorego wyniki maja zostac pobrane
#' @param czescEgzaminu czesc egzaminu, ktorego wyniki maja zostac pobrane
#' @param rokEgzaminu rok egzaminu, ktorego wyniki maja zostac pobrane
#' @param czyEwd wybor, czy maja byc pobrane wyniki gromadzone przez EWD, czy PAOU
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return data frame
#' @export
pobierz_czesc_egzaminu=function(
	rodzajEgzaminu, 
	czescEgzaminu, 
	rokEgzaminu, 
	czyEwd, 
	punktuj=TRUE, 
	idSkali=NULL,
	skroc=TRUE,
	zrodloDanychODBC='EWD'
){
	P=odbcConnect(as.character(zrodloDanychODBC))
	tryCatch({
		if(!is.character(rodzajEgzaminu) | !is.vector(rodzajEgzaminu) | length(rodzajEgzaminu)>1)
			stop('rodzajEgzaminu nie jest lancuchem znakow')
		if(!is.character(czescEgzaminu) | !is.vector(czescEgzaminu) | length(czescEgzaminu)>1)
			stop('czescEgzaminu nie jest lancuchem znakow')
		if(!is.numeric(rokEgzaminu) | !is.vector(rokEgzaminu) | length(rokEgzaminu)>1)
			stop('rokEgzaminu nie jest liczba')
		if(!is.logical(czyEwd) | !is.vector(czyEwd) | length(czyEwd)>1)
			stop('czyEwd nie jest wartoscia logiczna')
		if(!is.logical(punktuj) | !is.vector(punktuj) | length(punktuj)>1)
			stop('punktuj nie jest wartoscia logiczna')
		if((!is.numeric(idSkali) | !is.vector(idSkali) | length(idSkali)>1) & !is.null(idSkali))
			stop('idSkali nie jest liczba')
		if(!is.logical(skroc) | !is.vector(skroc) | length(skroc)>1)
			stop('skroc nie jest wartoscia logiczna')
		if(czyEwd){
			czyEwd='true'	
		} else czyEwd='false'
		if(punktuj){
			punktuj='true'
		} else punktuj='false'
		if(skroc){
			skroc='true'
		} else skroc='false'
		if(is.null(idSkali)){
			idSkali='null'
		} else idSkali=as.character(idSkali)
		
		tmp=.sqlQuery(P, sprintf("SELECT zbuduj_widok_czesci_egzaminu('tmp', '%s', '%s', %d, %s, %s, %s, %s);", 
														 .e(rodzajEgzaminu),
														 .e(czescEgzaminu),
														 rokEgzaminu, 
														 czyEwd, 
														 punktuj,
														 idSkali,
														 skroc))
		dane=.sqlQuery(P, "SELECT * FROM tmp")
		odbcClose(P)
		return(dane)
	},
	error=function(e){
		odbcClose(P)
		.stop(e)
	})
}