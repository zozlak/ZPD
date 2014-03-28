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

#' @title Pobiera ramke danych ze scalonymi danymi jednorocznymi egzaminu gimnazjalnego
#' @description
#' aaa
#' @details
#' bbb
#' @param rok rok egzaminu, dla ktorego maja zostac pobrane dane
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return data frame
#' @examples
#' a=1
#' @export
pobierz_dane_jednoroczne_gimnazjum=function(
	rok, 
	zrodloDanychODBC='EWD'
){
	P=odbcConnect(zrodloDanychODBC, readOnlyOptimize=T)
	czesciEgzaminu=.sqlQuery(P, sprintf("SELECT rodzaj_egzaminu, czesc_egzaminu, prefiks 
																		 FROM egzaminy JOIN sl_czesci_egzaminow USING (rodzaj_egzaminu, czesc_egzaminu)
																		 WHERE date_part('year', data_egzaminu)=%d AND rodzaj_egzaminu='egzamin gimnazjalny'", rok))
	szkoly=.sqlQuery(P, "SELECT id_szkoly, publiczna, dla_doroslych, specjalna, przyszpitalna, paou FROM szkoly")
	.sqlQuery(P, sprintf("SELECT zbuduj_widok_powtarzajacy('tmp', '%s', %d, true)", 'egzamin gimnazjalny', rok))
	powtarzajacy=.sqlQuery(P, "SELECT * FROM tmp")
	odbcClose(P)
	
	dane=as.data.frame(matrix(NA, nrow=0, ncol=1, dimnames=list(c(), c('id_obserwacji'))))
	for(i in 1:nrow(czesciEgzaminu)){
		tmp=pobierz_czesc_egzaminu('egzamin gimnazjalny', czesciEgzaminu$czesc_egzaminu[i], rok, T, T, zrodloDanychODBC=zrodloDanychODBC)
		tmp=tmp[grep('(([A-C]1)|[?])-[0-9]+$', tmp$opis_testu), ]
		names(tmp)[grep('id_testu', names(tmp))]=sprintf('id_testu_%s', czesciEgzaminu$prefiks[i])
		names(tmp)[grep('opis_testu', names(tmp))]=sprintf('ark_%s', czesciEgzaminu$prefiks[i])
		names(tmp)[grep('dysleksja', names(tmp))]=sprintf('dysl_%s', czesciEgzaminu$prefiks[i])
		names(tmp)=sub('^z_', sprintf('%s_', czesciEgzaminu$prefiks[i]), names(tmp))
		dane=join(dane, tmp, by='id_obserwacji', type='full')
	}
	dane$rok_g=rok
	dane$powtarzajacy= dane$id_obserwacji %in% powtarzajacy$id_obserwacji
	names(dane)[grep('id_szkoly', names(dane))]='id_szkoly_g'
	
	dane=as.data.frame(matrix(NA, nrow=0, ncol=1, dimnames=list(c(), c('id_obserwacji'))))
	for(i in (rok-3):(rok-5)){
		tmp=pobierz_czesc_egzaminu('sprawdzian', '', i, T, T, zrodloDanychODBC=zrodloDanychODBC)
		tmp=tmp[grep('(([A-C]1)|[?])-[0-9]+$', tmp$opis_testu), ]
		tmp=tmp[tmp$id_obserwacji %in% dane$id_obserwacji & !(tmp$id_obserwacji %in% spr$id_obserwacji), ]
		tmp$rok_s=i
		spr=join(spr, tmp, by='id_obserwacji', type='full')
		rm(tmp)
	}
	names(spr)[grep('id_testu', names(spr))]='id_testu_s'
	names(spr)[grep('opis_testu', names(spr))]='ark_s'
	names(spr)[grep('dysleksja', names(spr))]='dysl_s'
	names(spr)[grep('id_szkoly', names(spr))]='id_szkoly_s'
	names(spr)=sub('^z_', 's_', names(spr))
	dane=join(dane, spr, by='id_obserwacji', type='left')
	
	dane=join(dane, szkoly, by='id_szkoly',  type='left')
	
	return(dane)
}

#' @title Pobiera ramke danych z wynikami egzaminacyjnymi wskazanego arkusza egzaminacyjnego
#' @description
#' aaa
#' @details
#' bbb
#' @param idArkusza kod arkusza, ktorego wyniki maja zostac pobrane
#' @param czyEwd wybor, czy maja byc pobrane wyniki gromadzone przez EWD, czy PAOU
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return data frame
#' @examples
#' a=1
#' @export
pobierz_arkusz=function(
	idArkusza, 
	czyEwd, 
	punktuj=TRUE, 
	idSkali=NULL,
	skroc=TRUE,
	zrodloDanychODBC='EWD'
){
	P=odbcConnect(zrodloDanychODBC, readOnlyOptimize=T)
	tryCatch({
		if(!is.numeric(idArkusza) | !is.vector(idArkusza) | length(idArkusza)>1)
			stop('idArkusza nie jest liczba')
		if(!is.logical(czyEwd) | !is.vector(czyEwd) | length(czyEwd)>1)
			stop('czyEwd nie jest wartoscia logiczna')
		if(!is.logical(punktuj) | !is.vector(punktuj) | length(punktuj)>1)
			stop('punktuj nie jest wartoscia logiczna')
		if((!is.numeric(idSkali) | !is.vector(idSkali) | length(idSkali)>1) & !is.null(idSkali))
			stop('idSkali nie jest liczba')
		if(!is.logical(skroc) | !is.vector(skroc) | length(skroc)>1)
			stop('skroc nie jest wartoscia logiczna')
		if(czyEwd){
			czyEwd='true'
		} else czyEwd='false'
		if(punktuj){
			punktuj='true'
		} else punktuj='false'
		if(skroc){
			skroc='true'
		} else skroc='false'
		if(is.null(idSkali)){
			idSkali='null'
		} else idSkali=as.character(idSkali)
	
		tmp=.sqlQuery(P, sprintf("SELECT zbuduj_widok_arkusza('tmp', '%s', %s, %s, %s, %s);",
														.e(idArkusza), 
														czyEwd, 
														punktuj,
														idSkali,
														skroc))
		dane=.sqlQuery(P, "SELECT * FROM tmp")
		odbcClose(P)
		return(dane)
	},
	error=function(e){
		odbcClose(P)
		.stop(e)
	})
}

#' @title Pobiera ramke danych z wynikami egzaminacyjnymi wskazanej czesci egzaminu
#' @description
#' aaa
#' @details
#' bbb
#' @param rodzajEgzaminu rodzaj egzaminu, ktorego wyniki maja zostac pobrane
#' @param czescEgzaminu czesc egzaminu, ktorego wyniki maja zostac pobrane
#' @param rokEgzaminu rok egzaminu, ktorego wyniki maja zostac pobrane
#' @param czyEwd wybor, czy maja byc pobrane wyniki gromadzone przez EWD, czy PAOU
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return data frame
#' @examples
#' a=1
#' @export
pobierz_czesc_egzaminu=function(
	rodzajEgzaminu, 
	czescEgzaminu, 
	rokEgzaminu, 
	czyEwd, 
	punktuj=TRUE, 
	idSkali=NULL,
	skroc=TRUE,
	zrodloDanychODBC='EWD'
){
	P=odbcConnect(as.character(zrodloDanychODBC))
	tryCatch({
		if(!is.character(rodzajEgzaminu) | !is.vector(rodzajEgzaminu) | length(rodzajEgzaminu)>1)
			stop('rodzajEgzaminu nie jest lancuchem znakow')
		if(!is.character(czescEgzaminu) | !is.vector(czescEgzaminu) | length(czescEgzaminu)>1)
			stop('czescEgzaminu nie jest lancuchem znakow')
		if(!is.numeric(rokEgzaminu) | !is.vector(rokEgzaminu) | length(rokEgzaminu)>1)
			stop('rokEgzaminu nie jest liczba')
		if(!is.logical(czyEwd) | !is.vector(czyEwd) | length(czyEwd)>1)
			stop('czyEwd nie jest wartoscia logiczna')
		if(!is.logical(punktuj) | !is.vector(punktuj) | length(punktuj)>1)
			stop('punktuj nie jest wartoscia logiczna')
		if((!is.numeric(idSkali) | !is.vector(idSkali) | length(idSkali)>1) & !is.null(idSkali))
			stop('idSkali nie jest liczba')
		if(!is.logical(skroc) | !is.vector(skroc) | length(skroc)>1)
			stop('skroc nie jest wartoscia logiczna')
		if(czyEwd){
			czyEwd='true'	
		} else czyEwd='false'
		if(punktuj){
			punktuj='true'
		} else punktuj='false'
		if(skroc){
			skroc='true'
		} else skroc='false'
		if(is.null(idSkali)){
			idSkali='null'
		} else idSkali=as.character(idSkali)
		
		tmp=.sqlQuery(P, sprintf("SELECT zbuduj_widok_czesci_egzaminu('tmp', '%s', '%s', %d, %s, %s, %s, %s);", 
													 .e(rodzajEgzaminu),
													 .e(czescEgzaminu),
													 rokEgzaminu, 
													 czyEwd, 
													 punktuj,
													 idSkali,
													 skroc))
		dane=.sqlQuery(P, "SELECT * FROM tmp")
		odbcClose(P)
		return(dane)
	},
	error=function(e){
	 	odbcClose(P)
	 	.stop(e)
	})
}

#' @title Pobiera ramke danych z wynikami egzaminacyjnymi wskazanego testu
#' @description
#' aaa
#' @details
#' bbb
#' @param idTestu identyfikator, ktorego wyniki maja zostac pobrane
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return data frame
#' @examples
#' a=1
#' @export
pobierz_test=function(
	idTestu, 
	punktuj=TRUE, 
	idSkali=NULL,
	skroc=TRUE,
	zrodloDanychODBC='EWD'
){
	P=odbcConnect(zrodloDanychODBC, readOnlyOptimize=T)
	tryCatch({
		if(!is.numeric(idTestu) | !is.vector(idTestu) | length(idTestu)>1)
			stop('idTestu nie jest liczba')
		if(!is.logical(punktuj) | !is.vector(punktuj) | length(punktuj)>1)
			stop('punktuj nie jest wartoscia logiczna')
		if((!is.numeric(idSkali) | !is.vector(idSkali) | length(idSkali)>1) & !is.null(idSkali))
			stop('idSkali nie jest liczba')
		if(!is.logical(skroc) | !is.vector(skroc) | length(skroc)>1)
			stop('skroc nie jest wartoscia logiczna')
		if(punktuj){
			punktuj='true'
		} else punktuj='false'
		if(skroc){
			skroc='true'
		} else skroc='false'
		if(is.null(idSkali)){
			idSkali='null'
		} else idSkali=as.character(idSkali)
	
		tmp=.sqlQuery(P, sprintf("SELECT zbuduj_widok_testu('tmp', %d, %s, %s, %s);", 
														idTestu, 
														punktuj,
														idSkali,
														skroc))
		dane=.sqlQuery(P, "SELECT * FROM tmp")
		odbcClose(P)
		return(dane)
	},
	error=function(e){
		odbcClose(P)
		.stop(e)
	})
}

#' @title Pobiera ramke danych z wynikami egzaminacyjnymi testow zrownujacych
#' @description
#' aaa
#' @details
#' bbb
#' @param rodzajEgzaminu rodzaj egzaminu, ktorego wyniki maja zostac pobrane
#' @param punktuj wybor, czy dane maja byc pobrane w postaci dystraktorow, czy punktow
#' @param rok rok, z ktorego dane maja zostac pobrane
#' @param idSkali identyfikator skali, ktora ma zostac zastosowana do danych
#' @param skroc czy do danych zastosowac skrocenia skal opisane w skali
#' @param zrodloDanychODBC nazwa zrodla danych ODBC, ktorego nalezy uzyc
#' @return data frame
#' @examples
#' a=1
#' @export
pobierz_zrownywanie=function(
	rodzajEgzaminu, 
	rok, 
	punktuj=TRUE, 
	idSkali=NULL,
	skroc=TRUE,
	zrodloDanychODBC='EWD'
){
	P=odbcConnect(zrodloDanychODBC, readOnlyOptimize=T)
	tryCatch({
		if(!is.character(rodzajEgzaminu) | !is.vector(rodzajEgzaminu) | length(rodzajEgzaminu)>1)
			stop('rodzajEgzaminu nie jest lancuchem znakow')
		if(!is.numeric(rok))
			stop('rok nie jest liczba')
		if(!is.logical(punktuj) | !is.vector(punktuj) | length(punktuj)>1)
			stop('punktuj nie jest wartoscia logiczna')
		if((!is.numeric(idSkali) | !is.vector(idSkali) | length(idSkali)>1) & !is.null(idSkali))
			stop('idSkali nie jest liczba')
		if(!is.logical(skroc) | !is.vector(skroc) | length(skroc)>1)
			stop('skroc nie jest wartoscia logiczna')
		if(punktuj){
			punktuj='true'
		} else punktuj='false'
		if(skroc){
			skroc='true'
		} else skroc='false'
		if(is.null(idSkali)){
			idSkali='null'
		} else idSkali=as.character(idSkali)
		
		tmp=.sqlQuery(P, sprintf("SELECT zbuduj_widok_zrownywania('tmp', '%s', %d, %s, %s, %s);", 
														.e(rodzajEgzaminu),
														rok, 
														punktuj,
														idSkali,
														skroc))
		dane=.sqlQuery(P, "SELECT * FROM tmp")
		odbcClose(P)
		return(dane)
	},
	error=function(e){
		odbcClose(P)
		.stop(e)
	})
}
