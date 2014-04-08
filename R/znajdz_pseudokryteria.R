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

#' @title Odnajduje i tworzy pseudokryteria na podstawie list ich elementow
#' @description
#' _
#' @param kryteria macierz id_kryterium - kazdy wiersz opisuje jedno pseudokryterium
#' @param opisy wektor opisow pseudokryteriow
#' @param P obiekt otwartego polaczenie ODBC
#' @return [numeric vector] wektor odnalezionych i/lub utworzonych id_pseudokryteriow
znajdz_pseudokryteria = function(kryteria, opisy, P){
	kryteria = as.matrix(kryteria) # na wypadek, gdyby "kryteria" byly wektorem
	opisyBaza = .sqlQuery(P, "SELECT opis FROM pseudokryteria_oceny;")[, 1]
	pkrytBaza = pobierz_pseudokryteria(P)
	idPkryt = rep(NA, nrow(kryteria))
	sumy = rowSums(!is.na(kryteria))
	if(any(sumy == 1)){
		stop(sprintf('w wierszu/ach %s zdefiniowano pseudokryterium skladajace sie z jednego kryterium oceny', paste((1:length(sumy))[sumy], sep=',')))
	}
	sumy = sumy == 0
	for(i in 1:nrow(kryteria)){
		if(sumy[i]){
			next
		}
		wiersz = kryteria[i, ]
		tmp = match(paste(wiersz[order(wiersz)], collapse='|'), pkrytBaza)
		if(!is.na(tmp)){
			idPkryt[i] = as.numeric(names(pkrytBaza)[tmp])
		}else{
			if(is.null(opisy)){
				stop(sprintf('w wierszu %d trzeba utworzyc nowe pseudokryterium, jednak nie podano dla niego opisu', i))
			}
			if(is.na(opisy[i]) | 0 == nchar(gsub(' ', '', opisy[i]))){
				stop(sprintf('w wierszu %d trzeba utworzyc nowe pseudokryterium, jednak nie podano dla niego opisu', i))
			}
			if(opisy[i] %in% opisyBaza){
				stop(sprintf('w wierszu %d trzeba utworzyc nowe pseudokryterium, jednak podany dla niego opis wystepuje juz w bazie', i))
			}
			
			idPkryt[i] = .sqlQuery(P, "SELECT nextval('pseudokryteria_id_pseudokryterium_seq');")[1, 1]
			.sqlQuery(P, sprintf("INSERT INTO pseudokryteria_oceny (id_pseudokryterium, opis) VALUES (%d, '%s');",
													 idPkryt[i], .e(opisy[i])))
			for(j in wiersz){
				if(is.na(j)){
					next
				}
				.sqlQuery(P, sprintf("INSERT INTO pseudokryteria_oceny_kryteria (id_pseudokryterium, id_kryterium) VALUES (%d, %s);",
														 idPkryt[i], as.character(j)))
			}
		}
	}	
	return(idPkryt)
}
