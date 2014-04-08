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

#' @title Wyszukuje skale w bazie danych
#' @description
#' _
#' @param nazwa nazwa skali (zwyczajowo w formacie "ZESPOL;EGZ;LATA", np. "paou;s;2002-2011" lub "ewd;gh_h;2012")
#' @param rodzajEgzaminu jesli skala dotyczy jednej konkretnej czesci egzaminu, rodzaj tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param czescEgzaminu jesli skala dotyczy jednej konkretnej czesci egzaminu, czesc tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param rok jesli skala dotyczy jednej konkretnej czesci egzaminu, rok tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param jednoznacznie czy w wypadku znalezienia wiecej niz jednej pasujace skali powinien byc rzucany wyjatek?
#' @param dokladnie sposob porownywania nazwy skali, rodzaju i czesci egzaminu (TRUE - literalna zgodnosc wartosci, FALSE - zawieranie ciagu znakow)
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return [data.frame] informacje o znalezionych skalach
#' @export
znajdz_skale = function(
	nazwa=NULL, 
	rodzajEgzaminu=NULL, 
	czescEgzaminu=NULL, 
	rok=NULL, 
	jednoznacznie=FALSE,
	dokladnie=FALSE, 
	zrodloDanychODBC='EWD'
){
	if((!is.vector(nazwa) | !is.character(nazwa) | length(nazwa)>1) & !is.null(nazwa))
		stop('parametr "nazwa" nie jest pojedynczym lancuchem znakow')
	if((!is.vector(rodzajEgzaminu) | !is.character(rodzajEgzaminu) | length(rodzajEgzaminu)>1) & !is.null(rodzajEgzaminu))
		stop('parametr "rodzajEgzaminu" nie jest pojedynczym lancuchem znakow')
	if((!is.vector(czescEgzaminu) | !is.character(czescEgzaminu) | length(czescEgzaminu)>1) & !is.null(czescEgzaminu))
		stop('parametr "czescEgzaminu" nie jest pojedynczym lancuchem znakow')
	if((!is.vector(rok) | !is.numeric(rok) | length(rok)>1) & !is.null(rok))
		stop('parametr "rok" nie jest pojedyncza liczba')
	if(!is.vector(dokladnie) | !is.logical(dokladnie) | length(dokladnie)>1)
		stop('parametr "dokladnie" nie jest pojedyncza wartoscia logiczna')
	
	if(dokladnie){
		operator=' = '
		wzorzec=''
	} else{
		operator=' ILIKE '
		wzorzec='%'
	}
	
	warunek=''
	if(!is.null(nazwa))
		if(nazwa!='')
			warunek=paste(warunek, sprintf("nazwa %s '%s%s%s'", operator, wzorzec, nazwa, wzorzec), sep=' AND ')
	if(!is.null(rodzajEgzaminu))
		if(rodzajEgzaminu!='')
			warunek=paste(warunek, sprintf("rodzaj_egzaminu %s '%s%s%s'", operator, wzorzec, rodzajEgzaminu, wzorzec), sep=' AND ')
	if(!is.null(czescEgzaminu))
		if(czescEgzaminu!='')
			warunek=paste(warunek, sprintf("czesc_egzaminu %s '%s%s%s'", operator, wzorzec, czescEgzaminu, wzorzec), sep=' AND ')
	if(!is.null(rok))
		if(rok!='')
			warunek=paste(warunek, sprintf("date_part('year', data_egzaminu)=%d", rok), sep=' AND ')
	if(nchar(warunek)>0)
		warunek=paste('WHERE', substring(warunek, 5))
	
	P=odbcConnect(zrodloDanychODBC)
	tryCatch({
		tmp=.sqlQuery(P, sprintf("SELECT * FROM skale %s;", warunek))
		if(jednoznacznie & nrow(tmp)>1)
			stop('znaleziono wiecej niz jedna skale pasujaca do podanych kryteriow')
		odbcClose(P)
		return(tmp)
	},
	error=function(e){
		odbcClose(P)
		.stop(e)
	})
}
