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
#' @param idTestow wektor id_testu testów, których kryteria oceny mają zostać skopiowane do tworzonego testu
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return [numeric] id_testu utworzonego testu
#' @export
stworz_test = function(
	ewd,
	opis, 
	data,
	idTestow=NULL,
	zrodloDanychODBC='EWD'
){
	if(!is.logical(ewd) | !is.vector(ewd) | length(ewd) > 1){
		stop('argument ewd musi mieć wartość TRUE lub FALSE')
	}
	if(
		!is.character(opis) | !is.vector(opis) | length(opis) == 0
		| !is.character(data) | !is.vector(data) | length(data) == 0
	){
		stop('data i opis muszą być pojedynczymi stringami')
	}
	tryCatch(
		as.Date(data),
		error=function(e){
			stop('data powinna być w formcie RRRR-MM-DD')
		}
	)
	if(!is.null(idTestow) & (!is.numeric(idTestow) | !is.vector(idTestow))){
		stop('idTestow musi być wektorem numerycznym')
	}
	
	P=odbcConnect(zrodloDanychODBC)
	tryCatch({
		odbcSetAutoCommit(P, FALSE)
		.sqlQuery(P, "BEGIN;")
		
		idTestu = .sqlQuery(P, "SELECT nextval('testy_id_testu_seq')")[1, 1]
		.sqlQuery(P, sprintf(
			"INSERT INTO testy (id_testu, opis, data, ewd) VALUES (%s, '%s', '%s', %s)",
			.e(idTestu), .e(opis), .e(data), ifelse(ewd, 'true', 'false')
		))
		
		if(!is.null(idTestow)){
			idTestow = as.numeric(idTestow)
			idTestow = idTestow[!is.na(idTestow)]
			if(length(idTestow) > 0){
				.sqlQuery(P, "CREATE TEMPORARY SEQUENCE tmp_kolejnosc")
				.sqlQuery(P, sprintf(
					"INSERT INTO testy_kryteria (id_testu, id_kryterium, kolejnosc) 
						SELECT %d, t.*, nextval('tmp_kolejnosc')
						FROM
							(
								SELECT DISTINCT id_kryterium 
								FROM testy_kryteria 
								WHERE id_testu IN (%s)
							) AS t",
					idTestu, paste0(idTestow, collapse=', ')
				))
			}
		}
		
		odbcEndTran(P, TRUE)
		odbcClose(P)
		
		return(idTestu)
	},
	error=function(e){
		odbcClose(P)
		.stop(e)
	})
}