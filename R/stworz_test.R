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

#' @title Tworzy nowy test (także jako kompilację wskazanych testów)
#' @description
#' _
#' @param ewd czy będzie to test ewd [logical]
#' @param opis opis testu
#' @param data data testu
#' @param rodzajEgzaminu rodzaj egzaminu powiązany z testem (lub NA) 
#' @param czescEgzaminu część egzaminu powiązana z testem (lub NA)
#' @param idTestow wektor id_testu testów, których kryteria oceny mają zostać skopiowane do tworzonego testu
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return [numeric] id_testu utworzonego testu
#' @export
#' @import RODBCext
stworz_test = function(
	ewd,
	opis, 
	data,
  rodzajEgzaminu = NA_character_,
	czescEgzaminu = NA_character_,
	idTestow = NULL,
	zrodloDanychODBC = 'EWD'
){
  stopifnot(
    is.vector(ewd), is.logical(ewd), length(ewd) == 1, !is.na(ewd),
    is.vector(opis), is.character(opis), length(opis) == 1, !is.na(opis),
    is.vector(data), length(data) == 1, all(!is.na(data)), class(as.Date(data)) == 'Date',
    is.vector(rodzajEgzaminu), is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
    is.vector(czescEgzaminu), is.character(czescEgzaminu), length(czescEgzaminu) == 1,
    is.null(idTestow) | is.vector(idTestow) & is.numeric(idTestow),
    is.vector(zrodloDanychODBC), is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1
  )
	P = odbcConnect(zrodloDanychODBC)
	on.exit({
	  odbcClose(P)
	})
	
	odbcSetAutoCommit(P, FALSE)
	.sqlQuery(P, "BEGIN;")
		
	idTestu = .sqlQuery(P, "SELECT nextval('testy_id_testu_seq')")[1, 1]
	.sqlQuery(P,
		"INSERT INTO testy (id_testu, opis, data, ewd, rodzaj_egzaminu, czesc_egzaminu) VALUES (?, ?, ?, ?, ?, ?)",
		list(idTestu, opis, data, ewd, rodzajEgzaminu, czescEgzaminu)
	)
		
	if(!is.null(idTestow)){
		idTestow = na.exclude(idTestow)
		if(length(idTestow) > 0){
			.sqlQuery(P, "CREATE TEMPORARY SEQUENCE tmp_kolejnosc")
		  zap = sprintf(
		    "INSERT INTO testy_kryteria (id_testu, id_kryterium, kolejnosc) 
					SELECT %d, t.*, nextval('tmp_kolejnosc')
					FROM
						(
							SELECT DISTINCT id_kryterium 
							FROM testy_kryteria 
							WHERE id_testu IN (%s)
              ORDER BY 1
						) AS t",
		    idTestu,
		    paste0(idTestow, collapse = ', ')
		  )
			.sqlQuery(P, zap)
		}
	}
		
	odbcEndTran(P, TRUE)

	return(idTestu)
}