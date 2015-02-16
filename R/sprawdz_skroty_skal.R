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

#' @title Sprawdza poprawnosc i tworzy w bazie brakujace skroty skal
#' @description
#' _
#' @param skroty wektor wartosci id_skrotu
#' @param P obiekt otwartego polaczenie ODBC
#' @return [logical] TRUE w wypadku powodzenia funkcji
sprawdz_skroty_skal = function(skroty, P){
	skrotyBaza = .sqlQuery(P, "SELECT id_skrotu FROM skroty_skal")[, 1]
	skroty = gsub(',', '.', skroty)
	tmp = unique(skroty[!(skroty %in% skrotyBaza) & !is.na(skroty)])
	tmp = strsplit(tmp, '[|]')
	for(i in tmp){
		idSkrotu = paste(i, collapse = '|')
		i = strsplit(i, '[;]')
		if(length(i) != 2){
			stop(sprintf('"%s" nie jest poprawnym identyfikatorem skrotu skali - nie podano wartosci wyjsciowych', idSkrotu))
		}
		we = suppressWarnings(as.numeric(i[[1]]))
		wy = suppressWarnings(as.numeric(i[[2]]))
		if(length(we) != length(wy)){
			stop(sprintf('"%s" nie jest poprawnym identyfikatorem skrotu skali - rozne liczby wartosci wejsciowych i wyjsciowych', idSkrotu))
		}
		if(any(is.na(we) | is.na(wy))){
			stop(sprintf('"%s" nie jest poprawnym identyfikatorem skrotu skali - co najmniej jedna z wartosci wejsciowych lub wyjsciowych nie jest liczba', idSkrotu))
		}
		.sqlQuery(P, "INSERT INTO skroty_skal (id_skrotu) VALUES (?)", idSkrotu)
	  zap = "INSERT INTO skroty_skal_mapowania (id_skrotu, wartosc, nowa_wartosc) VALUES (?, ?, ?)"
		.sqlQuery(P, zap, list(rep(idSkrotu, length(we)), we, wy))
	}
	return(TRUE)
}
