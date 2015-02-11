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
#' Patrz http://zpd.ibe.edu.pl/doku.php?id=r_zpd_skale
#' @param idSkali identyfikator skali, ktora ma zostac zapisana (typowo uzyskany z funkcji "stworz_skale()")
#' @param elementy ramka danych opisujaca elementy skali - patrz http://zpd.ibe.edu.pl/doku.php?id=odbcskale
#' @param nadpisz czy nadpisac skale, jesli jest juz zdefiniowana w bazie danych
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return [data.frame] zapisane elementy skali
#' @export
#' @import RODBCext
edytuj_skale = function(
	idSkali, 
	elementy, 
	nadpisz = FALSE, 
	zrodloDanychODBC = 'EWD'
){
  stopifnot(
    is.vector(idSkali), is.numeric(idSkali), length(idSkali) == 1, !is.na(idSkali),
    is.data.frame(elementy)
  )
  
	P = odbcConnect(zrodloDanychODBC)
	on.exit({
	  odbcClose(P)
	})

	odbcSetAutoCommit(P, FALSE)
	.sqlQuery(P, "BEGIN")
		

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
	stopifnot(
	  !is.null(elementy$id_skrotu),
	  !is.null(elementy$id_kryterium),
	  !is.null(elementy$id_pseudokryterium),
	  all(as.numeric(!is.na(elementy$id_kryterium)) + as.numeric(!is.na(elementy$id_pseudokryterium)) <= 1)
	)
  if(any((!is.na(elementy$id_kryterium) | !is.na(elementy$id_pseudokryterium)) & rowSums(!is.na(kryteria)) > 0)){
	  stop('dla niektorych elementow skali zdefiniowano jednoczesnie id_kryterium/id_pseudokryterium, jak i wartosci w kolumnach id_kryterium_N')
	}
	#-->		
	#<-- testy zwiazane ze skala
  stopifnot(
    idSkali %in% .sqlQuery(P, "SELECT id_skali FROM skale")[, 1],
    nadpisz | !idSkali %in% .sqlQuery(P, "SELECT DISTINCT id_skali FROM skale_elementy")[, 1]
  )
	if(any(
	  0 != .sqlQuery(P, "SELECT count(*) FROM skalowania_elementy WHERE id_skali = ?", idSkali)[1, 1],
	  0 != .sqlQuery(P, "SELECT count(*) FROM skalowania_obserwacje WHERE id_skali = ?", idSkali)[1, 1]
	)){
	  stop('nie mozna edytowac skali - ma ona juz wpisane do bazy parametry zadan i/lub estymacje umiejetnosci uczniow')
	}
	#-->
	
	krytBaza = .sqlQuery(P, "SELECT id_kryterium FROM kryteria_oceny")[, 1]
	pkrytBaza = pobierz_pseudokryteria(P)
	#<-- weryfikacja elementow skali - istnienie id_kryterium/id_pseudokryteriu w bazie
	if(ncol(kryteria) > 0){
	  for(i in 1:ncol(kryteria)){
	    tmp = !is.na(kryteria[, i]) & !(kryteria[, i] %in% krytBaza)
	    if(any(tmp)){
	      stop("kryterium o id_kryterium ", paste(kryteria[tmp, i], collapse = ', '), " nie ma w bazie")
	    }
	  }
	}
	tmp = !is.na(elementy$id_kryterium) & !(elementy$id_kryterium %in% krytBaza)
	if(any(tmp)){
	  stop("kryterium/ow o id_kryterium ", paste(elementy$id_kryterium[tmp], collapse = ', '), " nie ma w bazie")
	}
	tmp = !is.na(elementy$id_pseudokryterium) & !(elementy$id_pseudokryterium %in% names(pkrytBaza))
	if(any(tmp)){
	  stop("pseudokryterium/ow o id_pseudokryterium ", paste(elementy$id_pseudokryterium[tmp], collapse=', '), " nie ma w bazie")
	}
	rm(tmp)
	#-->
	
	#<-- odnajdowanie id_pseudokryterium i ew. tworzenie pseudokryteriow
	tmp = znajdz_pseudokryteria(kryteria, elementy$opis, P) 
	elementy$id_pseudokryterium[!is.na(tmp)] = tmp[!is.na(tmp)]
	#-->

	#<-- weryfikacja istnienia kryteriów/pseudokryteriów w testach powiązanych ze skalą
	tmp = .sqlQuery(P, "SELECT id_pseudokryterium, id_kryterium FROM pseudokryteria_oceny_kryteria")
	tmp = tmp$id_kryterium[tmp$id_pseudokryterium %in% na.exclude(elementy$id_pseudokryterium)]
	kryt = unique(na.exclude(append(elementy$id_kryterium, tmp)))
	tmp = .sqlQuery(P, "SELECT id_kryterium FROM testy_kryteria JOIN skale_testy USING (id_testu) WHERE id_skali = ?", idSkali)[, 1]
	if(any(is.na(match(kryt, tmp)))){
	  stop("nie wszystkie (pseudo)kryteria oceny skali pochodzą z testów powiązanych ze skalą")
	}
  rm(tmp)
	#-->
	
	#<-- tworzenie brakujacych skrotow skal
	sprawdz_skroty_skal(elementy$id_skrotu, P)
	#-->
	
	#<-- duplikaty
	tmp = duplicated(elementy$id_kryterium) & !is.na(elementy$id_kryterium)
	if(any(tmp)){
	  stop(sprintf('kryteria o id_kryterium %s wystepuja wiele razy w parametrze "elementy"', paste(elementy$id_kryterium[tmp], collapse = ',')))
	}
	tmp = duplicated(elementy$id_pseudokryterium) & !is.na(elementy$id_pseudokryterium)
	if(any(tmp)){
	  stop(sprintf('pseudokryteria o id_pseudokryterium %s wystepuja wiele razy w parametrze "elementy"', paste(elementy$id_pseudokryterium[tmp], collapse = ',')))
	}
	rm(tmp)
	#-->
	
	if(nadpisz){
	  .sqlQuery(P, "DELETE FROM skale_elementy WHERE id_skali = ?", idSkali)
	}
	
	#<-- zapis do bazy
  zap = "INSERT INTO skale_elementy (kolejnosc, id_skali, id_kryterium, id_pseudokryterium, id_skrotu) VALUES (?, ?, ?, ?, ?)"
  tmp = data.frame(
    1:nrow(elementy), 
    rep(idSkali, nrow(elementy)), 
    elementy$id_kryterium, 
    elementy$id_pseudokryterium, 
    elementy$id_skrotu
  )
  .sqlQuery(P, zap, tmp)
	#-->
  
	odbcEndTran(P, TRUE)
	return(elementy)
}
