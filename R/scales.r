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

#' @title Zwraca wektor pozwalajacy identyfikowac pseudokryteria
#' @description
#' Nazwy elementow zwracanego wektora to id_pseudokryterium
#' Elementy zwracanego wektora to posortowane rosnaco id_kryterium
#' nalezacych do danego pseudokryterium, zlaczone w lancuch znakow
#' separatorem '|'
#' @details
#' _
#' @return character wektor pseudokryteriow znajdujacych sie w bazie
.zwr_pseudokryteria=function(P){
	wynik=sqlQuery(P, "SELECT id_pseudokryterium, id_kryterium FROM pseudokryteria_oceny_kryteria ORDER BY id_kryterium")
	wynik=by(wynik, wynik$id_pseudokryterium, function(x){return(paste(x$id_kryterium, collapse='|'))})
	tmp=names(wynik)
	wynik=as.character(unlist(wynik))
	names(wynik)=tmp
	return(wynik)	
}

#' @title Odnajduje i tworzy pseudokryteria na podstawie list ich elementow
#' @description
#' _
#' @details
#' _
#' @param kryteria macierz id_kryterium - kazdy wiersz opisuje jedno pseudokryterium
#' @param opisy wektor opisow pseudokryteriow
#' @param P obiekt otwartego polaczenie ODBC
#' @return numeric wektor odnalezionych i/lub utworzonych id_pseudokryteriow
.znajdz_pseudokryteria=function(kryteria, opisy, P){
	kryteria=as.matrix(kryteria) # na wypadek, gdyby "kryteria" byly wektorem
	opisyBaza=.sqlQuery(P, "SELECT opis FROM pseudokryteria_oceny;")[, 1]
	pkrytBaza=.zwr_pseudokryteria(P)
	idPkryt=rep(NA, nrow(kryteria))
	sumy=rowSums(!is.na(kryteria))
	if(any(sumy==1))
		stop(sprintf('w wierszu/ach %s zdefiniowano pseudokryterium skladajace sie z jednego kryterium oceny', paste((1:length(sumy))[sumy], sep=',')))
	sumy=sumy==0
	for(i in 1:nrow(kryteria)){
		if(sumy[i])
			next
		wiersz=kryteria[i, ]
		tmp=match(paste(wiersz[order(wiersz)], collapse='|'), pkrytBaza)
		if(!is.na(tmp)){
			idPkryt[i]=as.numeric(names(pkrytBaza)[tmp])
		}else{
			if(is.null(opisy))
				stop(sprintf('w wierszu %d trzeba utworzyc nowe pseudokryterium, jednak nie podano dla niego opisu', i))
			if(is.na(opisy[i]) | 0==nchar(gsub(' ', '', opisy[i])))
				stop(sprintf('w wierszu %d trzeba utworzyc nowe pseudokryterium, jednak nie podano dla niego opisu', i))
			if(opisy[i] %in% opisyBaza)
				stop(sprintf('w wierszu %d trzeba utworzyc nowe pseudokryterium, jednak podany dla niego opis wystepuje juz w bazie', i))
			
			idPkryt[i]=.sqlQuery(P, "SELECT nextval('pseudokryteria_id_pseudokryterium_seq');")[1, 1]
			.sqlQuery(P, sprintf("INSERT INTO pseudokryteria_oceny (id_pseudokryterium, opis) VALUES (%d, '%s');",
													idPkryt[i], .e(opisy[i])))
			for(j in wiersz){
        if(is.na(j))
          next
				.sqlQuery(P, sprintf("INSERT INTO pseudokryteria_oceny_kryteria (id_pseudokryterium, id_kryterium) VALUES (%d, %s);",
														idPkryt[i], as.character(j)))
			}
		}
	}	
	return(idPkryt)
}

#' @title Sprawdza poprawnosc i tworzy w bazie brakujace skroty skal
#' @description
#' _
#' @details
#' _
#' @param skroty wektor wartosci id_skrotu
#' @param P obiekt otwartego polaczenie ODBC
#' @return logical TRUE w wypadku powodzenia funkcji
.weryfikuj_skroty_skal=function(skroty, P){
	skrotyBaza=.sqlQuery(P, "SELECT id_skrotu FROM skroty_skal")[, 1]
	skroty=gsub(',', '.', skroty)
	tmp=unique(skroty[!(skroty %in% skrotyBaza) & !is.na(skroty)])
	tmp=strsplit(tmp, '[|]')
	for(i in tmp){
		idSkrotu=paste(i, collapse='|')
		i=strsplit(i, '[;]')
		we=as.numeric(i[[1]])
		wy=as.numeric(i[[2]])
		if(length(we)!=length(wy))
			stop(sprintf('"%s" nie jest poprawnym identyfikatorem skrotu skali - rozne liczby wartosci wejsciowych i wyjsciowych', idSkrotu))
		if(any(is.na(we) | is.na(wy)))
			stop(sprintf('"%s" nie jest poprawnym identyfikatorem skrotu skali - co najmniej jedna z wartosci wejsciowych lub wyjsciowych nie jest liczba', idSkrotu))
		.sqlQuery(P, sprintf("INSERT INTO skroty_skal (id_skrotu) VALUES ('%s')", .e(idSkrotu)))
		for(j in 1:length(we)){
			.sqlQuery(P, sprintf("INSERT INTO skroty_skal_mapowania (id_skrotu, wartosc, nowa_wartosc) VALUES ('%s', %f, %f)",
													idSkrotu, we[j], wy[j]))
		}
		return(TRUE)
	}
}

#' @title Tworzy w bazie nowa skale i zwraca jej id_skali
#' @description
#' _
#' @details
#' _
#' @param nazwa nazwa skali (zwyczajowo w formacie "ZESPOL;EGZ;LATA", np. "paou;s;2002-2011" lub "ewd;gh_h;2012")
#' @param opis bardziej dokladny opis skali (opcjonalny, mozna ustawic na "")
#' @param rodzajEgzaminu jesli skala dotyczy jednej konkretnej czesci egzaminu, rodzaj tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param czescEgzaminu jesli skala dotyczy jednej konkretnej czesci egzaminu, czesc tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param rok jesli skala dotyczy jednej konkretnej czesci egzaminu, rok tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return numeric - id_skali utworzonej skali
#' @examples
#' a=1
#' @export
stworz_skale=function(
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
			if(1!=.sqlQuery(P, sprintf("SELECT count(*) FROM egzaminy WHERE rodzaj_egzaminu='%s' AND czesc_egzaminu='%s' AND date_part('year', data_egzaminu)=%d",
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
														FROM egzaminy WHERE rodzaj_egzaminu='%s' AND czesc_egzaminu='%s' AND date_part('year', data_egzaminu)=%d;",
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

#' @title Wyszukuje skale w bazie danych
#' @description
#' _
#' @details
#' _
#' @param nazwa nazwa skali (zwyczajowo w formacie "ZESPOL;EGZ;LATA", np. "paou;s;2002-2011" lub "ewd;gh_h;2012")
#' @param rodzajEgzaminu jesli skala dotyczy jednej konkretnej czesci egzaminu, rodzaj tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param czescEgzaminu jesli skala dotyczy jednej konkretnej czesci egzaminu, czesc tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param rok jesli skala dotyczy jednej konkretnej czesci egzaminu, rok tego egzaminu (patrz http://zpd.ibe.edu.pl/doku.php?id=czesci_egzaminu)
#' @param jednoznacznie czy w wypadku znalezienia wiecej niz jednej pasujace skali powinien byc rzucany wyjatek?
#' @param dokladnie sposob porownywania nazwy skali, rodzaju i czesci egzaminu (TRUE - literalna zgodnosc wartosci, FALSE - zawieranie ciagu znakow)
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return data.frame
#' @examples
#' a=1
#' @export
znajdz_skale=function(
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

#' @title Zapisuje elementy skali do bazy danych
#' @description
#' Patrz http://zpd.ibe.edu.pl/doku.php?id=odbcskale
#' @details
#' _
#' @param idSkali identyfikator skali, ktora ma zostac zapisana (typowo uzyskany z funkcji "stworz_skale()")
#' @param elementy ramka danych opisujaca elementy skali - patrz http://zpd.ibe.edu.pl/doku.php?id=odbcskale
#' @param nadpisz czy nadpisac skale, jesli jest juz zdefiniowana w bazie danych
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return data.frame
#' @examples
#' a=1
#' @export
edytuj_skale=function(
	idSkali, 
	elementy, 
	nadpisz=FALSE, 
	zrodloDanychODBC='EWD'
){
	P=odbcConnect(zrodloDanychODBC)
	tryCatch({
		odbcSetAutoCommit(P, FALSE)
		.sqlQuery(P, "BEGIN;")

		if(!is.data.frame(elementy))
			stop('parametr "elementy" nie jest ramka danych')

		kryteria=as.matrix(elementy[, grep('^id_kryterium_[0-9]+$', names(elementy))])
		kolOpis=grep('^opis$', names(elementy))
		#<-- na wypadek factor-ow
		if(ncol(kryteria)>0){
			for(i in 1:ncol(kryteria))
				kryteria[, i]=as.numeric(as.character(kryteria[, i]))
		}
		elementy$id_kryterium=as.numeric(as.character(elementy$id_kryterium))
		elementy$id_pseudokryterium=as.numeric(as.character(elementy$id_pseudokryterium))
		elementy$opis=as.character(elementy$opis)
		elementy$id_skrotu=as.character(elementy$id_skrotu)
		#-->
		
		#<-- testy niewymagajace polaczenia z baza
		if((!is.vector(idSkali) | !is.numeric(idSkali) | length(idSkali)>1) & !is.null(idSkali))
			stop('parametr "idSkali" nie jest pojedyncza liczba')
		if(is.null(elementy$id_skrotu))
			stop('parametr "elementy" nie zawiera kolumny "id_skrotu"')
		if(any(is.null(elementy$id_kryterium), is.null(elementy$id_pseudokryterium)))
			stop('parametr "elementy" nie zawiera kolumn "id_kryterium" i/lub "id_pseudokryterium"')
		
		tmp=rowSums(cbind(!is.na(elementy$id_kryterium), !is.na(elementy$id_pseudokryterium)))
		if(any(tmp>1))
			stop('dla niektorych elementow skali zdefiniowano jednoczesnie id_kryterium oraz id_pseudokryterium')
		if(any(tmp & rowSums(!is.na(kryteria))>0))
			stop('dla niektorych elementow skali zdefiniowano jednoczesnie id_kryterium/id_pseudokryterium, jak i wartosci w kolumnach id_kryterium_N')
		rm(tmp)
		#-->		
		#<-- testy zwiazane ze skala
		if(1!=.sqlQuery(P, sprintf("SELECT count(*) FROM skale WHERE id_skali=%d;", idSkali))[1, 1])
			stop(sprintf('w bazie nie ma skali o id_skali %d', idSkali))
		if(any(
			0!=.sqlQuery(P, sprintf("SELECT count(*) FROM skalowania_elementy WHERE id_skali=%d;", idSkali))[1, 1],
			0!=.sqlQuery(P, sprintf("SELECT count(*) FROM skalowania_obserwacje WHERE id_skali=%d;", idSkali))[1, 1]
		)){
			stop(sprintf('nie mozna edytowac skali - ma ona juz wpisane do bazy parametry zadan i/lub estymacje umiejetnosci uczniow', idSkali))
		}
		if(!nadpisz & 0!=.sqlQuery(P, sprintf("SELECT count(*) FROM skale_elementy WHERE id_skali=%d;", idSkali))[1, 1])
			stop(sprintf('skala %d ma juz w bazie zdefiniowane elementy - ustaw parametr "nadpisz" na TRUE, aby ja nadpisac', idSkali))
		#-->
		
		krytBaza=.sqlQuery(P, "SELECT id_kryterium FROM kryteria_oceny")[, 1]
		pkrytBaza=.zwr_pseudokryteria(P)
		#<-- weryfikacja elementow skali - istnienie id_kryterium/id_pseudokryteriu w bazie
		if(ncol(kryteria)>0){
			for(i in 1:ncol(kryteria)){
				tmp=!is.na(kryteria[, i]) & !(kryteria[, i] %in% krytBaza)
				if(any(tmp))
					stop(sprintf("kryterium o id_kryterium %s nie ma w bazie", paste(kryteria[tmp, i], collapse=', ')))
			}
		}
		tmp=!is.na(elementy$id_kryterium) & !(elementy$id_kryterium %in% krytBaza)
		if(any(tmp))
			stop(sprintf("kryterium/ow o id_kryterium %s nie ma w bazie", paste(elementy$id_kryterium[tmp], collapse=', ')))
		tmp=!is.na(elementy$id_pseudokryterium) & !(elementy$id_pseudokryterium %in% names(pkrytBaza))
		if(any(tmp))
			stop(sprintf("pseudokryterium/ow o id_pseudokryterium %s nie ma w bazie", paste(elementy$id_pseudokryterium[tmp], collapse=', ')))
		rm(tmp)
		#-->
		
		#<-- odnajdowanie id_pseudokryterium i ew. tworzenie pseudokryteriow
		tmp=.znajdz_pseudokryteria(kryteria, elementy$opis, P) 
		elementy$id_pseudokryterium[!is.na(tmp)]=tmp[!is.na(tmp)]
		#-->
		#<-- tworzenie brakujacych skrotow skal
		.weryfikuj_skroty_skal(elementy$id_skrotu, P)
		#-->
		
		#<-- duplikaty
		tmp=duplicated(elementy$id_kryterium) & !is.na(elementy$id_kryterium)
		if(any(tmp))
			stop(sprintf('kryteria o id_kryterium %s wystepuja wiele razy w parametrze "elementy"', paste(elementy$id_kryterium[tmp], collapse=',')))
		tmp=duplicated(elementy$id_pseudokryterium) & !is.na(elementy$id_pseudokryterium)
		if(any(tmp))
			stop(sprintf('pseudokryteria o id_pseudokryterium %s wystepuja wiele razy w parametrze "elementy"', paste(elementy$id_pseudokryterium[tmp], collapse=',')))
		rm(tmp)
		#-->
		
		if(nadpisz)
			.sqlQuery(P, sprintf("DELETE FROM skale_elementy WHERE id_skali=%d;", idSkali))
		
		#<-- zapis do bazy
    for(i in 1:ncol(elementy)){ # aby zamiana NA na 'null' nie powodowała ostrzeżeń
      if(is.factor(elementy[, i])) 
        elementy[, i]=as.character(elementy[, i])
    }
		elementy[is.na(elementy)]='null'
		tmp=elementy$id_skrotu!='null'
		elementy$id_skrotu[tmp]=paste("'", .e(elementy$id_skrotu[tmp]), "'", sep='')
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
