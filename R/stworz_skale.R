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

#' @title Tworzy w bazie nowa skale i zwraca jej id_skali
#' @description
#' _
#' @param nazwa nazwa skali (zwyczajowo w formacie "ZESPOL;EGZ;LATA", np. "paou;s;2002-2011" lub "ewd;gh_h;2012")
#' @param opis bardziej dokladny opis skali (opcjonalny, mozna ustawic na "")
#' @param rodzajEgzaminu jesli skala dotyczy jednej konkretnej czesci egzaminu, rodzaj tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param czescEgzaminu jesli skala dotyczy jednej konkretnej czesci egzaminu, czesc tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param rok jesli skala dotyczy jednej konkretnej czesci egzaminu, rok tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return [numeric] id_skali utworzonej skali
#' @export
stworz_skale = function(
	nazwa, 
	opis, 
	rodzajEgzaminu, 
	czescEgzaminu, 
	rok, 
	zrodloDanychODBC='EWD'
){
	P=odbcConnect(zrodloDanychODBC)
	tryCatch({
		odbcSetAutoCommit(P, FALSE)
		.sqlQuery(P, "BEGIN;")
		
		if(!is.vector(nazwa) | !is.character(nazwa) | length(nazwa)>1)
			stop('nazwa nie jest pojedynczym lancuchem znakow')
		if(nazwa=='')
			stop('nazwa skali nie moze byc pusta')
		if(!is.vector(opis) | !is.character(opis) | length(opis)>1)
			stop('opis nie jest pojedynczym lancuchem znakow')
		if((!is.vector(rodzajEgzaminu) | !is.character(rodzajEgzaminu) | length(rodzajEgzaminu)>1) & !is.null(rodzajEgzaminu))
			stop('rodzajEgzaminu nie jest pojedynczym lancuchem znakow')
		if((!is.vector(czescEgzaminu) | !is.character(czescEgzaminu) | length(czescEgzaminu)>1) & !is.null(czescEgzaminu))
			stop('czescEgzaminu nie jest pojedynczym lancuchem znakow')
		if((!is.vector(rok) | !is.numeric(rok) | length(rok)>1) & !is.null(rok))
			stop('rok nie jest pojedyncza liczba')
		czyEgzamin=is.null(rodzajEgzaminu)+is.null(czescEgzaminu)+is.null(rok)
		if(czyEgzamin>0 & czyEgzamin<3)
			stop('rodzajEgzaminu, czescEgzaminu oraz rok musza byc zdefiniowane wszystkie na raz lub wszystkie musza miec wartosc NULL')
		if(czyEgzamin==0){
			if(1!=.sqlQuery(P, sprintf("SELECT count(*) FROM sl_egzaminy WHERE rodzaj_egzaminu='%s' AND czesc_egzaminu='%s' AND date_part('year', data_egzaminu)=%d",
																 .e(rodzajEgzaminu), .e(czescEgzaminu), rok))[1, 1]){
				stop(sprintf("w bazie nie ma egzaminu '%s', '%s', %d", rodzajEgzaminu, czescEgzaminu, rok))
			}
		}
		if(0!=.sqlQuery(P, sprintf("SELECT count(*) FROM skale WHERE nazwa='%s'", .e(nazwa))))
			stop(sprintf("w bazie istnieje juz skala o nazwie '%s'", nazwa))
		
		idSkali=.sqlQuery(P, "SELECT nextval('skale_id_skali_seq')")[1, 1]
		if(czyEgzamin>0){
			.sqlQuery(P, sprintf("INSERT INTO skale (id_skali, opis, nazwa) VALUES (%d, '%s', '%s');", idSkali, .e(opis), .e(nazwa)))
		}else{
			.sqlQuery(P, sprintf("INSERT INTO skale (id_skali, opis, nazwa, rodzaj_egzaminu, czesc_egzaminu, data_egzaminu) 
														SELECT %d, '%s', '%s', rodzaj_egzaminu, czesc_egzaminu, data_egzaminu
														FROM sl_egzaminy WHERE rodzaj_egzaminu='%s' AND czesc_egzaminu='%s' AND date_part('year', data_egzaminu)=%d;",
													 idSkali, .e(opis), .e(nazwa), .e(rodzajEgzaminu), .e(czescEgzaminu), rok))
		}
		odbcEndTran(P, TRUE)
		odbcClose(P)
		return(idSkali)	
	},
	error=function(e){
		odbcClose(P)
		.stop(e)
	})
}
