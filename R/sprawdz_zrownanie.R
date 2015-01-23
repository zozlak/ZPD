#	Copyright 2013 Mateusz Żółtak
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

#' @title Weryfikuje, czy dane zrownania zostaly poprawnie zaimportowane do bazy
#' @description
#' Funkcja:
#' \itemize{
#' 	\item pobiera dane wskazanego zrownania (rodzaj egzaminu i rok) z bazy
#' 	\item dla kazdego testu zrownujacego sprawdza jego dane w bazie z plikiem "id_testu.csv"
#'   pliki te powinny sie znajdowac w katalogu roboczym
#' }
#' @param rodzajEgzaminu rodzaj egzaminu
#' @param rok rok badania zrownujacego
#' @param kolumnaId nazwa kolumny przechowujacej "zewnetrzne id"
#' @param src zrodlo danych dplyr-a
#' @return [logical] TRUE, gdy dane zweryfikowane poprawnie, FALSE w p.p.
#' @examples
#' \dontrun{
#' 	sprawdz_zrownanie('sprawdzian', 2014, 'zbiór.zrównywanie')
#' }
#' @import dplyr
#' @export
sprawdz_zrownanie = function(rodzajEgzaminu, rok, kolumnaId, src){
  
	cat('Pobieranie wyników zrównań z bazy...')
	daneBaza = pobierz_wyniki_zrownywania(rodzajEgzaminu, rok, F)
	
	testy = unique(daneBaza$id_testu)
	if(length(testy) == 0){
		stop('W bazie brak testow zrownujacych')
	}
	
	sumyBaza = pobierz_wyniki_zrownywania(src, rodzajEgzaminu, rok, T)
	sumyBaza$suma = rowSums(sumyBaza[, grep('^k_', names(sumyBaza))], na.rm=T)
	sumyBaza = sumyBaza[, -grep('^k_', names(sumyBaza))]
	daneBaza = plyr::join(daneBaza, sumyBaza, by=intersect(names(daneBaza), names(sumyBaza)))
	rm(sumyBaza)
	cat('\n')	
	
	cat('Pobierane informacji o zadaniach i uczniach...')
	schematyOdp = tbl(src, sql("SELECT id_kryterium, kolejnosc, dystraktor
												 			FROM sl_schematy_odp_dystr JOIN pytania USING (schemat_odp) JOIN kryteria_oceny USING (id_pytania)"))
	obserwacjeId = tbl(src, sql("SELECT id_obserwacji, id
															FROM obserwacje_id
															WHERE typ_id = 'zbiór zrównywanie'"))
	testyMaks = tbl(src, sql("SELECT id_testu, sum(l_punktow) AS maks
											 			FROM
															(
												 				SELECT 
																	id_testu, opis, 
																	CASE opis IN ('tresc', 'komp', 'styl', 'jez', 'zap', 'wal') 
													 					WHEN true THEN avg(l_punktow) 
													 					ELSE sum(l_punktow) 
																	END as l_punktow 
												 				FROM testy_kryteria JOIN kryteria_oceny USING (id_kryterium) 
												 				WHERE opis <> 'wal' 
												 				GROUP BY 1, 2
												 			) AS t 
											 			GROUP BY 1"))
	cat('\n\n')
	
	daneBaza = plyr::join(daneBaza, obserwacjeId, by='id_obserwacji', type='left')
	rm(obserwacjeId)
	
	bledy = c()
	
	for(test in testy){
		bledy = c(
			bledy, 
			tryCatch({
				cat(sprintf('Weryfikacja testu %s\n', test))
					
				plik = sprintf('%s.csv', test)
				if(!file.exists(plik)){
					stop(sprintf('W katalogu roboczym nie ma pliku "%s"', plik))	
				}
				daneTmp = read.csv2(plik, stringsAsFactors=F, colClasses = 'character')
					
				idTmp = daneTmp[, kolumnaId]
				sumaTmp = ilorazTmp = NULL
				if(!is.null(daneTmp$suma)){
					sumaTmp = as.numeric(sub(',', '.', daneTmp$suma))
				}
				if(!is.null(daneTmp$iloraz)){
					ilorazTmp = as.numeric(sub(',', '.', daneTmp$iloraz))
				}
				daneTmp = daneTmp[, grep('^k_', names(daneTmp))]
				
				idKryteriow = sub('^k_', '', names(daneTmp))
				for(i in 1:ncol(daneTmp)){
					#<-- zmapuj kody dystraktorow na liczby
					wartosci = schematyOdp[schematyOdp$id_kryterium == idKryteriow[i], ]
					for(j in 1:nrow(wartosci)){
						daneTmp[daneTmp[, i] %in% wartosci$dystraktor[j], i] = wartosci$kolejnosc[j]
					}
					daneTmp[daneTmp[, i] %in% c('Brak odpowiedzi', 'brak odpowiedzi', 'odmowa odpowiedzi', '-8'), i] = -1
					daneTmp[daneTmp[, i] %in% c('Wielokrotne zaznaczenie', 'wielokrotne zaznaczenie', 'trudno powiedzieć', '-9'), i] = -2
					#-->
					
					filtr = is.na(daneTmp[, i])
					tryCatch({daneTmp[, i] = as.numeric(daneTmp[, i])}, warning=function(w){
						stop(paste0('niezmapowane odpowiedzi w kryterium ', names(daneTmp)[i]))						
					})
				}
				
				daneBazaTmp = daneBaza[which(daneBaza$id_testu == test), ]
				if(nrow(daneTmp) != nrow(daneBazaTmp)){
					stop('rozne liczby obserwacji')
				}
				
				kol = match(names(daneTmp), names(daneBazaTmp))
				if(any(is.na(kol))){
					stop('niezmapowane zadania')
				}
				wrsz = match(idTmp, daneBazaTmp$id)
				if(any(is.na(wrsz))){
					stop('niezmapowani uczniowie')
				}
				if(any(daneTmp != daneBazaTmp[wrsz, kol] & (!is.na(daneBazaTmp[wrsz, kol]) | !is.na(daneTmp)))){
					stop('rozne wartosci')
				}
				
				if(!is.null(sumaTmp)){
					filtr = sumaTmp != daneBazaTmp$suma[wrsz]
					if(any(filtr)){
						stop(sprintf('niezgodne sumy w wierszach c(%s)', paste(which(filtr), collapse=',')))
					}
				}else{
					cat('  nie sprawdzano sum punktow\n')
				}
				
				if(!is.null(ilorazTmp)){
					licznik = testyMaks$maks[testyMaks$id_testu == test]
					filtr = (ilorazTmp - daneBazaTmp$suma[wrsz] / licznik) >= 0.00005
					if(any(filtr)){
						stop(sprintf('niezgodne ilorazy w wierszach [%d] c(%s)', licznik, paste(which(filtr), collapse=',')))
					}
				}else{
					cat('  nie sprawdzano ilorazow punktow\n')
				}
			},
			error=function(e){
				bl = conditionMessage(e)
				names(bl) = test
				return(bl)
			})
		)
	}
	if(length(bledy) == 0){
		return(TRUE)
	}
	return(bledy)
}