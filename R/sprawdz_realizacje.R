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

#' @title Sprawdza poprawność pliku podsumowującego realizację badania
#' @description
#' Dopasowanie pomiędzy plikiem realizacji a plikiem wyników uczniów odbywa się
#' na podstawie zmiennych "id_szk" oraz "id_kl". Funkcja zakłada więc, że 
#' zmienne takie istnieją w obydwu zbiorach.
#' 
#' Sprawdzane są:
#' \itemize{
#' 	\item zawieranie wszystkich kombinacji {"id_szk", "id_kl"} w obydwu zbiorach
#' 	\item zgodność liczby wystąpień danej kombinacji {"id_szk", "id_kl"} z wartością
#'   kolumny "l_bad" ("l_przedmGim" gdy podano argument "przedmGim")
#' 	\item zależności pomiędzy kolumnami "l_uczkl", "l_wykl", "l_bad" ("l_przedmGim" 
#'   gdy podano argument "przedmGim"), "l_ucz_szk", "l_dys", "l_dys_szk",
#'   "l_oddz"
#' }
#' @param plik ścieżka do pliku z danymi z realizacji (plik powinien mieć "format IBE")
#' @param codebook ścieżka do pliku codebooka danych z realizacji (plik powinien mieć "format IBE")
#' @param dane ramka danych z wynikami uczniów (wystarczą kolumny "id_szk", "id_kl", "wykl")
#' @param przedmGim opcjonalny wzór nazwy kolumny przechowującej w pliku realizacji liczbę zrealizowanych testów
#' @return [logical] TRUE, gdy dane zweryfikowane poprawnie, FALSE w p.p.
#' @examples
#' \dontrun{
#' 	dane = wczytaj_ibe('spr.csv', 'spr_c.csv')
#' 	sprawdz_realizacje('spr_realizacja.csv', 'spr_realizacja_c.csv', dane)
#' }
#' @export
sprawdz_realizacje = function(plik, codebook, dane, przedmGim=NULL){
	flaga = T
	if(is.null(dane$przedm)){
		dane$przedm = ''
	}
	dane = dane[dane$wykl == 'brak', ]
	
	realizacja = wczytaj_ibe(plik, codebook)
	names(realizacja) = tolower(names(realizacja))
	if(is.null(realizacja$przedm)){
		realizacja$przedm = ''
	}
	
	if(!is.null(przedmGim)){
		names(realizacja) = sub(paste0('l_', przedmGim), 'l_bad', names(realizacja))
		names(dane) = sub(paste0('udzial_', przedmGim), 'udzial', names(dane))
	}
	
	dane$id_szk = stringi::stri_trim(paste(dane$przedm, dane$id_szk))
	realizacja$id_szk = stringi::stri_trim(paste(realizacja$przedm, realizacja$id_szk))
	
	# wszyscy niewykluczeni uczniowie - sprawdzenie zgodności id_szk i id_kl
	tmp = aggregate(dane[, 'id_ucz'], list('id_szk' = dane$id_szk, 'id_kl' = dane$id_kl), length)
	if(length(setdiff(tmp$id_szk, realizacja$id_szk)) != 0){
		cat(paste0('w zbiorze wyników są szkoły, których nie ma w podsumowaniu realizacji: ', paste0(setdiff(tmp$id_szk, realizacja$id_szk), collapse=', '), '\n'))
		flaga = F
	}
	tmp = plyr::join(realizacja, tmp, type='left', match='all')
	filtr = is.na(tmp$x)
	if(any(filtr)){
		cat(paste0('w podsumowaniu realizacji są szkoły i/lub klasy, których nie ma w zbiorze wyników: ', paste0(tmp$id_szk[filtr], collapse=', '), '\n'))
		flaga = F
	}
	
	# tylko uczniowie, którzy wzięli udział w badaniu - sprawdzenie 
	dane = dane[dane$udzial == 'tak', ]
	tmp = aggregate(dane[, 'id_ucz'], list('id_szk' = dane$id_szk, 'id_kl' = dane$id_kl), length)
	tmp = plyr::join(realizacja, tmp, type='left', match='all')
	filtr = tmp$x - tmp$l_bad != 0 & !is.na(tmp$x)
	if(any(filtr)){
		cat(paste0('niezgodna liczba uczniów, którzy wzięli udział w badaniu w szkołach: ', paste0(tmp$id_szk[filtr], collapse=', '), '\n'))
		flaga = F
	}
	
	# proste testy w ramach zbioru danych z realizacji
	filtr = tmp$l_uczkl > tmp$l_ucz_szk
	if(any(filtr)){
		cat(paste0('liczba uczniów w klasie większa od liczby uczniów w szkole: ', paste0(tmp$id_szk[filtr], collapse=', '), '\n'))
		flaga = F
	}
	filtr = tmp$l_wykl + tmp$l_bad > tmp$l_uczkl
	if(any(filtr)){
		cat(paste0('liczba uczniów badanych i wykluczonych w klasie większa od liczby uczniów w klasie: ', paste0(tmp$id_szk[filtr], collapse=', '), '\n'))
		flaga = F
	}
	filtr = tmp$l_dys > tmp$l_uczkl
	if(any(filtr)){
		cat(paste0('liczba dyslektyków w klasie większa od liczby uczniów w klasie: ', paste0(tmp$id_szk[filtr], collapse=', '), '\n'))
		flaga = F
	}
	filtr = tmp$l_dys_szk > tmp$l_ucz_szk
	if(any(filtr)){
		cat(paste0('liczba dyslektyków w szkole większa od liczby uczniów w szkole: ', paste0(tmp$id_szk[filtr], collapse=', '), '\n'))
		flaga = F
	}
	filtr = abs(tmp$l_uczkl - tmp$l_ucz_szk / tmp$l_oddz) > tmp$l_uczkl
	if(any(filtr)){
		cat(paste0('nieproporcjonalne oddziały: ', paste0(tmp$id_szk[filtr], collapse=', '), '\n'))
		flaga = F
	}
	
	return(flaga)
}