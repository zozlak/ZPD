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
#' @param rodzaj rodzaj skali (ewd/zrównywanie/ktt)
#' @param doPrezentacji czy skala ma być oznaczona jako przeznaczona do prezentacji
#' @param idTestow wektor id testów, z którymi ma być powiązana skala
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return [numeric] id_skali utworzonej skali
#' @export
#' @import RODBCext
stworz_skale = function(
	nazwa, 
	opis, 
	rodzaj,
	doPrezentacji,
	idTestow, 
	zrodloDanychODBC = 'EWD'
){
  stopifnot(
    is.vector(nazwa), is.character(nazwa), length(nazwa) == 1, !is.na(nazwa), nazwa != '', 
    is.vector(opis), is.character(opis), length(opis) == 1, !is.na(opis),
    is.vector(rodzaj), is.character(rodzaj), length(rodzaj) == 1, !is.na(rodzaj),
    is.vector(doPrezentacji), is.logical(doPrezentacji), length(doPrezentacji) == 1, !is.na(doPrezentacji),
    is.vector(idTestow), is.numeric(idTestow),
    is.vector(zrodloDanychODBC), is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1
  )
  P = odbcConnect(zrodloDanychODBC)
	on.exit({
	  odbcClose(P)
	})
  
  idTestow = na.exclude(idTestow)
  stopifnot(
    ! nazwa %in% .sqlQuery(P, "SELECT DISTINCT nazwa FROM skale")[, 1],
    rodzaj %in% .sqlQuery(P, "SELECT rodzaj_skali FROM sl_rodzaje_skal")[, 1],
    length(idTestow) > 0,
    all(idTestow %in% .sqlQuery(P, "SELECT id_testu FROM testy")[, 1])
  )
  
	odbcSetAutoCommit(P, FALSE)
	.sqlQuery(P, "BEGIN")
		
  idSkali = .sqlQuery(P, "SELECT nextval('skale_id_skali_seq')")[1, 1]
  zap = "INSERT INTO skale (id_skali, opis, nazwa, rodzaj_skali, do_prezentacji) VALUES (?, ?, ?, ?, ?)"
  .sqlQuery(P, zap, list(idSkali, opis, nazwa, rodzaj, doPrezentacji))
  zap = "INSERT INTO skale_testy (id_skali, id_testu) VALUES (?, ?)"
  .sqlQuery(P, zap, list(rep(idSkali, length(idTestow)), idTestow))
  
  odbcEndTran(P, TRUE)
	return(idSkali)	
}
