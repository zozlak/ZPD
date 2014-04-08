#	Copyright 2014 Mateusz Zoltak
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

#' @title Przygotowuje zbiory matur poprawkowych do importu do bazy
#' @description
#' Przygotowuje zbiory z wynikami matur poprawkowych do importu do bazy
#' skryptem 'import_test.php'.
#' Sprawdza zgodność kolumn w pliku z planem testu w bazie, jeśli trzeba, 
#' zamienia kody dystraktorów (angielski i problem T/F v. A/B), a następnie 
#' zmienia ich nazwy na stosowne 'k_idKryterium'.
#' @param rok rok egzaminu
#' @param przedmiot przedmiot egzaminacyjny
#' @param plik plik CSV z wynikami matury poprawkowej
#' @param kodowanie kodowanie używane przy otwieraniu pliku
#' @param zrodloDanychODBC nazwa zrodla danych ODBC
#' @return void
#' @export
przygotuj_mature_popr = function(rok, przedmiot, plik, kodowanie='Windows-1250', zrodloDanychODBC='EWD'){
	dane = read.csv2(plik, stringsAsFactors=F, fileEncoding=kodowanie)
	P = odbcConnect(zrodloDanychODBC)
	schematyOdp = sqlQuery(P, "SELECT * FROM sl_schematy_odp_dystr ORDER BY schemat_odp, kolejnosc", stringsAsFactors=F)
	kryteria = sqlQuery(P, sprintf("
    SELECT id_kryterium, schemat_odp, typ, l_punktow
    FROM 
			pytania 
			JOIN kryteria_oceny k USING (id_pytania) 
			JOIN testy_kryteria USING (id_kryterium) 
			JOIN testy USING (id_testu) 
			JOIN arkusze USING (arkusz)
		WHERE 
			rodzaj_egzaminu = 'matura poprawkowa' 
			AND czesc_egzaminu LIKE '%s%%' 
			AND extract(year FROM data_egzaminu) = %d 
			AND ewd = false
		ORDER BY kolejnosc, k.opis, id_kryterium",
		.e(przedmiot), rok), stringsAsFactors=F)
	idTestu = sqlQuery(P, sprintf(
		"SELECT id_testu 
		 FROM testy JOIN arkusze USING (arkusz) 
		 WHERE 
			 rodzaj_egzaminu = 'matura poprawkowa' 
			 AND czesc_egzaminu LIKE '%s%%' 
			 AND extract(year FROM data_egzaminu) = %d 
		   AND ewd = false", 
		.e(przedmiot), rok
	), stringsAsFactors=F)[1, 1]
	odbcClose(P)
	
	names(dane) = sub('.*OKE.*', 'oke', names(dane))
	names(dane) = sub('id_cke|id_egz', 'cke', names(dane))
	names(dane) = sub('id_szkoly', 'id_szk_oke', names(dane))
	
	wzorKolZadan = '^POP_|^z_|^pkt$'
	kolumny = grep(wzorKolZadan, names(dane), value=T)
	if(length(kolumny) != nrow(kryteria)){
		stop(paste0('niezgodna liczba kryteriów oceny [plik] ', length(kolumny), ' != ', nrow(kryteria), ' [baza]'))
	}
	for(i in 1:length(kolumny)){
		kryterium = kryteria[i, ]
		kol = kolumny[i]
		
		if(kryterium$typ == 'otwarte'){
			tmp = sum(is.na(dane[, kol]))
			dane[, kol] = as.numeric(dane[, kol])
			if(sum(is.na(dane[, kol])) != tmp){
				stop(paste0('w kryterium ', kol, '(', kryterium$id_kryterium, ') - kolumna nie jest numeryczna'))
			}
			if(max(dane[, kol], na.rm=T) > kryterium$l_punktow){
				stop(paste0(
					'w kryterium ', kol, '(', kryterium$id_kryterium, ') maksymalna wartość ', 
					max(dane[, kol]),' większa niż dopuszczalna ', kryterium$l_punktow
				))
			}
		}else{
			schemat = schematyOdp$dystraktor[schematyOdp$schemat_odp == kryterium$schemat_odp]
			
			tmp = na.exclude(unique(sub('-1|-2|[*]|[?]|<|>|^$', NA, dane[, kol])))
			if(length(tmp) != length(schemat)){
				print(list('plik' = tmp, 'baza' = schemat))
				stop(paste0('w kryterium ', kol, '(', kryterium$id_kryterium, ') niezgodne liczby dystraktorów'))
			}
			
			# obsługa pytań TRUE/FALSE
			if(kryterium$schemat_odp == 'T..F'){
				dane[, kol] = sub('A', 'T', dane[, kol])
				dane[, kol] = sub('B', 'F', dane[, kol])
				tmp = na.exclude(unique(sub('-1|-2|[*]|[?]|<|>|^$', NA, dane[, kol])))
			}
			
			if(length(setdiff(tmp, schemat)) != 0){
				print(data.frame('plik' = tmp, 'baza' = schemat))
				stop(paste0('w kryterium ', kol, '(', kryterium$id_kryterium, ') niezgodne etykiety dystraktorów'))	
			}
		}
	}
	
	# dopasowywanie szkoły
	if(is.null(dane$id_szk_oke)){
		cat('Dopasowywanie id_szkoly_oke - to może zająć chwilę...\n')
		P = odbcConnect(zrodloDanychODBC)
		testyObs = sqlQuery(P, sprintf(
			"SELECT id, id_szkoly_oke 
			 FROM 
			 	 arkusze a
			   JOIN testy USING (arkusz)
			   JOIN testy_obserwacje tob USING (id_testu)
			   JOIN szkoly_dane sd ON ( (tob.id_szkoly, extract(year FROM data_egzaminu)) = (sd.id_szkoly, sd.rok) )
			   JOIN obserwacje_id USING (id_obserwacji)
			 WHERE
			   typ_id = 'cke'
			   AND rodzaj_egzaminu = 'matura'
				 AND czesc_egzaminu LIKE '%s%%'
				 AND extract(year FROM data_egzaminu) = %d 
		     AND ewd = true",
			.e(przedmiot), rok
		), stringsAsFactors=F)
		odbcClose(P)
		
		dane$id_szk_oke = testyObs$id_szkoly_oke[match(dane$cke, testyObs$id)]
		dane$id_szk_oke[is.na(dane$id_szk_oke)] = '?'
	}
	
	# usunięcie duplikatów
	dane$cke = as.numeric(dane$cke)
	duplikaty = unique(dane$cke[duplicated(dane$cke)]) 
	filtr = ! dane$cke %in% duplikaty
	dane = dane[filtr, ]
	if(any(!filtr)){
		cat(paste0('usunięto ', sum(!filtr), ' duplikatów\n'))
	}
	if(sum(is.na(dane$cke)) > 0){
		cat(paste0(sum(is.na(dane$cke)), ' uczniów bez id_cke\n'))
	}
	
	# zamiana nazw kolumn
	kolumny = grep(wzorKolZadan, names(dane))
	names(dane)[kolumny] = paste0('k_', kryteria$id_kryterium)
	
	cat(paste0('zapisuję do pliku ', idTestu, '.csv\n'))
	write.csv2(dane, paste0(idTestu, '.csv'), row.names=F, na='', fileEncoding='UTF-8')
}