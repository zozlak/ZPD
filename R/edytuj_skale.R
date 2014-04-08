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

#' @title Zapisuje elementy skali do bazy danych
#' @description
#' Patrz http://zpd.ibe.edu.pl/doku.php?id=odbcskale
#' @param idSkali identyfikator skali, ktora ma zostac zapisana (typowo uzyskany z funkcji "stworz_skale()")
#' @param elementy ramka danych opisujaca elementy skali - patrz http://zpd.ibe.edu.pl/doku.php?id=odbcskale
#' @param nadpisz czy nadpisac skale, jesli jest juz zdefiniowana w bazie danych
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return [data.frame] zapisane elementy skali
#' @export
edytuj_skale = function(
	idSkali, 
	elementy, 
	nadpisz=FALSE, 
	zrodloDanychODBC='EWD'
){
	P=odbcConnect(zrodloDanychODBC)
	tryCatch({
		odbcSetAutoCommit(P, FALSE)
		.sqlQuery(P, "BEGIN;")
		
		if(!is.data.frame(elementy)){
			stop('parametr "elementy" nie jest ramka danych')
		}
		
		kryteria = as.matrix(elementy[, grep('^id_kryterium_[0-9]+$', names(elementy))])
		kolOpis = grep('^opis$', names(elementy))
		#<-- na wypadek factor-ow
		if(ncol(kryteria) > 0){
			for(i in 1:ncol(kryteria)){
				kryteria[, i] = as.numeric(as.character(kryteria[, i]))
			}
		}
		elementy$id_kryterium = as.numeric(as.character(elementy$id_kryterium))
		elementy$id_pseudokryterium = as.numeric(as.character(elementy$id_pseudokryterium))
		elementy$opis = as.character(elementy$opis)
		elementy$id_skrotu = as.character(elementy$id_skrotu)
		#-->
		
		#<-- testy niewymagajace polaczenia z baza
		if((!is.vector(idSkali) | !is.numeric(idSkali) | length(idSkali)>1) & !is.null(idSkali)){
			stop('parametr "idSkali" nie jest pojedyncza liczba')
		}
		if(is.null(elementy$id_skrotu)){
			stop('parametr "elementy" nie zawiera kolumny "id_skrotu"')
		}
		if(any(is.null(elementy$id_kryterium), is.null(elementy$id_pseudokryterium))){
			stop('parametr "elementy" nie zawiera kolumn "id_kryterium" i/lub "id_pseudokryterium"')
		}
		
		tmp = rowSums(cbind(!is.na(elementy$id_kryterium), !is.na(elementy$id_pseudokryterium)))
		if(any(tmp > 1)){
			stop('dla niektorych elementow skali zdefiniowano jednoczesnie id_kryterium oraz id_pseudokryterium')
		}
		if(any(tmp & rowSums(!is.na(kryteria))>0)){
			stop('dla niektorych elementow skali zdefiniowano jednoczesnie id_kryterium/id_pseudokryterium, jak i wartosci w kolumnach id_kryterium_N')
		}
		rm(tmp)
		#-->		
		#<-- testy zwiazane ze skala
		if(1 != .sqlQuery(P, sprintf("SELECT count(*) FROM skale WHERE id_skali=%d;", idSkali))[1, 1]){
			stop(sprintf('w bazie nie ma skali o id_skali %d', idSkali))
		}
		if(any(
			0 != .sqlQuery(P, sprintf("SELECT count(*) FROM skalowania_elementy WHERE id_skali=%d;", idSkali))[1, 1],
			0 != .sqlQuery(P, sprintf("SELECT count(*) FROM skalowania_obserwacje WHERE id_skali=%d;", idSkali))[1, 1]
		)){
			stop(sprintf('nie mozna edytowac skali - ma ona juz wpisane do bazy parametry zadan i/lub estymacje umiejetnosci uczniow', idSkali))
		}
		if(!nadpisz & 0!=.sqlQuery(P, sprintf("SELECT count(*) FROM skale_elementy WHERE id_skali=%d;", idSkali))[1, 1]){
			stop(sprintf('skala %d ma juz w bazie zdefiniowane elementy - ustaw parametr "nadpisz" na TRUE, aby ja nadpisac', idSkali))
		}
		#-->
		
		krytBaza = .sqlQuery(P, "SELECT id_kryterium FROM kryteria_oceny")[, 1]
		pkrytBaza = pobierz_pseudokryteria(P)
		#<-- weryfikacja elementow skali - istnienie id_kryterium/id_pseudokryteriu w bazie
		if(ncol(kryteria)>0){
			for(i in 1:ncol(kryteria)){
				tmp = !is.na(kryteria[, i]) & !(kryteria[, i] %in% krytBaza)
				if(any(tmp)){
					stop(sprintf("kryterium o id_kryterium %s nie ma w bazie", paste(kryteria[tmp, i], collapse=', ')))
				}
			}
		}
		tmp = !is.na(elementy$id_kryterium) & !(elementy$id_kryterium %in% krytBaza)
		if(any(tmp)){
			stop(sprintf("kryterium/ow o id_kryterium %s nie ma w bazie", paste(elementy$id_kryterium[tmp], collapse=', ')))
		}
		tmp = !is.na(elementy$id_pseudokryterium) & !(elementy$id_pseudokryterium %in% names(pkrytBaza))
		if(any(tmp)){
			stop(sprintf("pseudokryterium/ow o id_pseudokryterium %s nie ma w bazie", paste(elementy$id_pseudokryterium[tmp], collapse=', ')))
		}
		rm(tmp)
		#-->
		
		#<-- odnajdowanie id_pseudokryterium i ew. tworzenie pseudokryteriow
		tmp = znajdz_pseudokryteria(kryteria, elementy$opis, P) 
		elementy$id_pseudokryterium[!is.na(tmp)] = tmp[!is.na(tmp)]
		#-->
		#<-- tworzenie brakujacych skrotow skal
		sprawdz_skroty_skal(elementy$id_skrotu, P)
		#-->
		
		#<-- duplikaty
		tmp = duplicated(elementy$id_kryterium) & !is.na(elementy$id_kryterium)
		if(any(tmp)){
			stop(sprintf('kryteria o id_kryterium %s wystepuja wiele razy w parametrze "elementy"', paste(elementy$id_kryterium[tmp], collapse=',')))
		}
		tmp = duplicated(elementy$id_pseudokryterium) & !is.na(elementy$id_pseudokryterium)
		if(any(tmp)){
			stop(sprintf('pseudokryteria o id_pseudokryterium %s wystepuja wiele razy w parametrze "elementy"', paste(elementy$id_pseudokryterium[tmp], collapse=',')))
		}
		rm(tmp)
		#-->
		
		if(nadpisz){
			.sqlQuery(P, sprintf("DELETE FROM skale_elementy WHERE id_skali=%d;", idSkali))
		}
		
		#<-- zapis do bazy
		for(i in 1:ncol(elementy)){ # aby zamiana NA na 'null' nie powodowała ostrzeżeń
			if(is.factor(elementy[, i])){
				elementy[, i]=as.character(elementy[, i])
			}
		}
		elementy[is.na(elementy)] = 'null'
		tmp = elementy$id_skrotu != 'null'
		elementy$id_skrotu[tmp] = paste("'", .e(elementy$id_skrotu[tmp]), "'", sep='')
		for(i in 1:nrow(elementy)){
			.sqlQuery(P, sprintf("INSERT INTO skale_elementy (kolejnosc, id_skali, id_kryterium, id_pseudokryterium, id_skrotu) VALUES (%d, %d, %s, %s, %s)", 
													 i, idSkali, elementy$id_kryterium[i], elementy$id_pseudokryterium[i], elementy$id_skrotu[i]))
		}
		#-->
		odbcEndTran(P, TRUE)
		odbcClose(P)
		return(elementy)
	},
	error=function(e){
		odbcClose(P)
		.stop(e)
	})
}
