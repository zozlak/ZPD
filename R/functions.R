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

#' @title Wyparsowuje znaki konca stringu
#' @description
#' Funkcja do wyparsowywania niebezpiecznych znakow przy budowaniu zapytan SQL
#' @details
#' _
#' @param str lancuch znakow do wyparsowania
#' @return character
.e = function(str){
	return(gsub("'", "''", str))
}

#' @title Wykonuje zapytanie sql i obsluguje bledy
#' @description
#' _
#' @details
#' _
#' @param P otwarte polaczenie ODBC
#' @param sql polecenie SQL do wykonania
#' @return data.frame
#' @import RODBCext
.sqlQuery = function(P, sql, dane = NULL){
	odbcClearError(P)
	tmp = sqlExecute(P, sql, dane, fetch = T, errors = F, stringsAsFactors = F, dec = '.')
	if(!is.data.frame(tmp)){
		if(tmp[1] == -2){
			return(NULL) # brak danych
		}
	}
	blad = odbcGetErrMsg(P)
	if(length(blad) > 0){
		stop(paste0(blad, collapse = '\n'))
	}
	return(tmp)
}

#' @title Ponownie rzuca przekazanym wyjatkiem zachowujac jego pierwotne wywolanie
#' @description
#' _
#' @details
#' _
#' @param e wyjatek przechwycony funkcja tryCatch()
#' @return void
.stop = function(e){
	stop(paste(deparse(conditionCall(e)), conditionMessage(e), sep = '\n'), call. = F)
}