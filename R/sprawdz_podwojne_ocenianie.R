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

#' @title Sprawdza poprawność pliku wyników podwójnego oceniania
#' @description
#' Sprawdzane są:
#' \itemize{
#' 	\item zgodność w ramach pliku podwójnego oceniania, tzn. czy w wypadku dwóch
#'   zgodnych ocen egzaminatorów ocena ostateczna też jest im równa
#' 	\item zgodność ostatecznej oceny z pliku podwójnego oceniania z danymi w
#'   zbiorze uczniów
#' 	\item zawieranie się wszystkich identyfikatorów uczniów ze zbioru podwójnego
#'   oceniania w zbiorze identyfikatorów uczniów ze zbioru uczniów
#' }
#' 
#' Dane dopasowywane są na podstawie kolumny "id_ucz".
#' 
#' Odpowiadające sobie kolumny z ocenami powinny kończyć się sufiksami:
#' \itemize{
#' 	\item "_ost" - ocena ostateczna w zbiorze podwójnego oceniania
#' 	\item "_i" - ocena pierwszego egzaminatora w zbiorze podwójnego oceniania
#' 	\item "_ii" - ocena drugiego egzaminatora w zbiorze podwójnego oceniania
#' 	\item bez sufiksu - ocena w zbiorze uczniowskim
#' }
#' @param plik ścieżka do pliku z danymi podwójnego oceniania (plik powinien mieć "format IBE")
#' @param codebook ścieżka do pliku codebooka danych z podwójnego oceniania (plik powinien mieć "format IBE")
#' @param dane ramka danych z wynikami uczniów
#' @param zeszyt umożliwia odfiltrowanie tylko wskazanych zeszytów - potrzebny do weryfikacji danych gimnazjalnych
#' @param wzor wyrażenie regularne dopasowujący nazwy zmiennych z ostatecznymi wartościami ocen
#' @return [logical] TRUE, gdy dane zweryfikowane poprawnie, FALSE w p.p.
#' @examples
#' \dontrun{
#' 	# zwykłe sprawdzenie
#' 	dane = wczytaj_ibe('spr.csv', 'spr_c.csv')
#' 	sprawdz_podwojne_ocenianie('spr_podw_kod.csv', 'spr_podw_kod_c.csv', dane)
#' 
#' 	# dane gimnazjalne - oddzielnie dla części hum i mat-przyr
#' 	dane = wczytaj_ibe('gim.csv', 'gim_c.csv')
#' 	sprawdz_podwojne_ocenianie('gim_podw_kod.csv', 'gim_podw_kod_c.csv', dane, c('GH-P1A-14', 'GH-P1B-14'), '^pol.*_ost$')
#' 	sprawdz_podwojne_ocenianie('gim_podw_kod.csv', 'gim_podw_kod_c.csv', dane, c('GM-M1A-14', 'GM-M1B-14'), '^mat.*_ost$')
#' }
#' @export
sprawdz_podwojne_ocenianie = function(plik, codebook, dane, zeszyt=NULL, wzor='^z[0-9].*_ost$'){
	flaga = TRUE
	podwKod = wczytaj_ibe(plik, codebook, oo='-2', tp='-1')
	names(podwKod) = tolower(names(podwKod))
	if(!is.null(zeszyt)){
		if(length(intersect(zeszyt, unique(podwKod$id_zt))) != length(zeszyt)){
			stop('niepoprawnie określony zestaw zeszytów')
		}
		podwKod = podwKod[podwKod$id_zt %in% zeszyt, ]
	}
	podwKod = join(podwKod, dane, by='id_ucz', type='left', match='all')
	if(length(unique(podwKod$id_ucz)) != nrow(podwKod)){
		stop('Duplikaty id_ucz')
	}
	filtr = is.na(podwKod$klasa)
	if(any(filtr)){
		stop(paste0('Brak ', sum(filtr),' uczniów w zbiorze z wynikami: ', paste0(podwKod$id_ucz[filtr], collapse=', ')))
	}
	zadania = grep(wzor, names(podwKod), value=T)
	if(length(zadania) == 0){
		stop('niepoprawny filtr zadań')
	}
	for(kol in zadania){
		kol2 = sub('_ost$', '', kol)
		podwKod[, kol] = as.character(podwKod[, kol])
		podwKod[, kol2] = as.character(podwKod[, kol2])
		filtr = !is.na(podwKod[, kol]) & podwKod[, kol] != podwKod[, kol2]
		if(any(filtr| is.na(filtr))){
			cat(paste0('Niezgodne wyniki w kolumnie ', kol2, ' dla uczniów: ', paste0(podwKod$id_ucz[filtr], collapse=', '), '\n'))
			flaga = FALSE
		}
		
		kolI = sub('_ost$', '_i', kol)
		kolII = sub('_ost$', '_ii', kol)
		podwKod[, kolI] = as.character(podwKod[, kolI])
		podwKod[, kolII] = as.character(podwKod[, kolII])
		filtr = !is.na(podwKod[, kol]) & podwKod[, kolI] == podwKod[, kolII] & podwKod[, kol] != podwKod[, kolI]
		if(any(filtr| is.na(filtr))){
			cat(paste0('Ostateczna ocena różna mimo że I i II ocena równe w kolumnie ', kol, ' dla uczniów: ', paste0(podwKod$id_ucz[filtr], collapse=', '), '\n'))
			flaga = FALSE
		}
	}
	return(flaga)
}

