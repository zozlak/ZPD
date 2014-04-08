#	Copyright 2011-2014 Mateusz Żółtak
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

#' @title Funkcja wczytująca wskazany zbiór danych w formacie zgodnym ze standardami IBE
#' @description
#' Funkcja wczytująca wskazany zbiór danych w formacie zgodnym ze standardami IBE.
#' Poza wczytaniem samych danych podczytuje z książki kodowej danego zbioru danych
#' etykiety zmiennych oraz skale.
#' @details
#' Ponieważ R rozróżnia tylko skale nominalną, porządkową i całą resztę i tylko 
#' dwa pierwsze rodzaje mogą być etykietowane, to mapowanie przebiega następująco:
#' \itemize{
#' 	\item zmienne posiadające listę etykiet i mające wg książki kodowej skalę nominalną
#'   lub dychotomiczną konwertowane są na zmienne nominalne R-a (oraz przypisywane
#'   są im etykiety)
#' 	\item zmienne posiadające listę etykiet i mające wg ksiązki kodowej skalę inną niż
#'   nominalna lub dychotomiczna konwertowane są na zmienne porządkowe R-a (oraz 
#'   przypisywane są im etykiety) bez zwracania uwagi na to, jaką konkretnie skalę
#'   miały określoną w książce kodowej (!!!)
#' 	\item zmienne nie posiadające listy etykiet, a mające wg książki kodowej skalę
#'   nominalną lub dychotomiczną konwertowane są na zmienne nominalne R-a
#'   z etykietami równymi liście ich unikalnych wartości
#' 	\item zmienne nie posiadające listy etykiet, a mające wg książki kodowej skalę
#'   porządkową konwertowane są na zmienne porządkowe R-a z etykietami równymi
#'   liście ich unikalnych wartości
#' 	\item pozostałe zmienne nie posiadające w książce kodowej listy etykiet nie są
#'   w żaden sposób modyfikowane
#' }
#' 
#' Druga kwestia, którą trzeba uwzględnić w R to to, że obsługuje on tylko jeden
#' brak danych (NA), podczas gdy standardy IBE rozróżniają trzy typy braków danych
#' (nie dotyczy, trudno powiedzieć/zaznaczono więcej niż jedną odpowiedź, odmowa
#' odpowiedzi/nie wybrano żadnej odpowiedzi). Dla każdej wartości specjalnej IBE
#' możliwe są dwa scenariusze:
#' \itemize{
#' 	\item mapowanie wartości specjalnej IBE na NA;
#' 	\item pozostawienie wartości specjalnych jako zwykłej wartości, przy czym jeśli
#'   zmienna ma określone etykietowanie, to pierwotna wartość kodu nie zostanie
#'   zachowana (etykietować w R można tylko zmienne nominalne i porządkowe, a tego
#'   rodzaju zmienne R przekodowuje na kolejne wartości od 1 począwszy i przypisuje
#'   każdej wartości etykietę pochodzącą z pierwotnej wartości zmiennej) i z góry
#'   znana będzie tylko wartość etykiety
#' }
#' Z tego powodu dla każdej z trzech wartości specjalnych IBE określających powód
#' braku danych dostępny jest przełącznik umożliwiający wskazanie etykiety, jaka
#' zostanie przypisana danemu brakowi danych lub wartości NA, jeśli ma on zostać
#' skonwertowany na brak danych R
#' @param plik_danych nazwa pliku ze zbiorem danych w formacie zgodnym ze standardami IBE do wczytania
#' @param ksiazka_kodow nazwa pliku z książką kodów w formacie zgodnym ze standardami IBE opisującej wczytywany zbiór danych
#' @param oo etykieta, jaka zostanie przypisana brakom danych IBE 'odmowa odpowiedzi/nie zaznaczono żadnej odpowiedzi' lub NA, jeśli danybrak danych IBE ma zostać skonwertowany na brak danych R (patrz opis)
#' @param tp etykieta, jaka zostanie przypisana brakom danych IBE 'trudno powiedzieć/wybrano więcej niż jedną odpowiedź' lub NA, jeśli dany brak danych IBE ma zostać skonwertowany na brak danych R (patrz opis)
#' @param nd etykieta, jaka zostanie przypisana brakom danych IBE 'nie dotyczy' lub NA, jeśli dany brak danych IBE ma zostać skonwertowany na brak danych R (patrz opis)
#' @param kolumna_skala numer kolumny książki kodów zawierającej opis skali zmiennych (jeśli książka kodów jest ściśle zgodna ze standardami IBE, to będzie to kolumna 6)
#' @param kolumna_etykiety numer kolumny książki kodów zawierającej etykiety zmiennych (jeśli książa kodów jest ściśle zgodna ze standardami IBE, to będzie to kolumna 11)
#' @param kolumna_typ numer kolumny książki kodów zawierającej typy zmiennych (jeśli książka kodów jest ściśle zgodna ze standardami IBE, to będzie to kolumna 5)
#' @param kolumna_rozmiar numer kolumny książki kodów zawierającej rozmiar zmiennych (jeśli książka kodów jest ściśle zgodna ze standardami IBE, to będzie to kolumna 7)
#' @param kolumna_dokladnosc numer kolumny książki kodów zawierającej dokładność zmiennych będących liczbami rzeczywistymi (jeśli książka kodów jest ściśle zgodna ze standardami IBE, to będzie to kolumna 8)
#' @param postep czy wyświetlać informacje o liczbie przetworzonych zmiennych
#' @return [data.frame] wczytane dane
#' @examples
#' \dontrun{
#' 	wczytaj_ibe('zr_spr.csv', 'zr_spr_c.csv', oo=-1, tp=-2)
#' }
#' @export
wczytaj_ibe = function(plik_danych, ksiazka_kodow, oo=NA, tp=NA, nd=NA, kolumna_skala=6, kolumna_etykiety=11, kolumna_typ=5, kolumna_rozmiar=7, kolumna_dokladnosc=8, postep=F){
	dane = read.csv2(file=plik_danych, fileEncoding='WINDOWS-1250', stringsAsFactors=F);
	codebook = read.csv2(file=ksiazka_kodow, fileEncoding='WINDOWS-1250', stringsAsFactors=F);
	for(i in 1:ncol(dane)){
		if(postep){
			print(c(i, codebook[i, 1]))
		}
		if(codebook[i, kolumna_typ] %in% c('liczba całkowita', 'terc')){
			baza = rep('9', codebook[i, kolumna_rozmiar]-1);
			ooWz = paste(baza, '9', sep='', collapse='');
			tpWz = paste(baza, '8', sep='', collapse='');
			ndWz = paste(baza, '7', sep='', collapse='');
		}else if(codebook[i, kolumna_typ] %in% c('liczba rzeczywista', 'waga', 'terc')){
			baza = rep('9', codebook[i, kolumna_rozmiar]-codebook[i, kolumna_dokladnosc]-1);
			ooWz = paste(baza, '9', sep='', collapse='');
			tpWz = paste(baza, '8', sep='', collapse='');
			ndWz = paste(baza, '7', sep='', collapse='');
		}else if(codebook[i, kolumna_typ] %in% c('tekst')){
			ooWz = 'ODMOWA ODPOWIEDZI';
			tpWz = 'TRUDNO POWIEDZIEĆ';
			ndWz = 'NIE DOTYCZY';
		}else if(codebook[i, kolumna_typ] %in% c('data')){
			ooWz = '9999-00-00 00:00:00';
			tpWz = '9998-00-00 00:00:00';
			ndWz = '9997-00-00 00:00:00';
		}
		if(is.na(oo)){
			dane[!is.na(dane[, i]) & dane[, i] == ooWz, i] = NA;
		}else{
			dane[!is.na(dane[, i]) & dane[, i] == ooWz, i] = oo;
		}
		if(is.na(tp)){
			dane[!is.na(dane[, i]) & dane[, i] == tpWz, i] = NA;
		}else{
			dane[!is.na(dane[, i]) & dane[, i] == tpWz, i] = tp;
		}
		if(is.na(nd)){
			dane[!is.na(dane[, i]) & dane[, i] == ndWz, i] = NA;
		}else{
			dane[!is.na(dane[, i]) & dane[, i] == ndWz, i] = nd;
		}
		
		if(!is.na(codebook[i, kolumna_etykiety]) & codebook[i, kolumna_etykiety] != ''){
			tmp = gsub(pattern='(^\\s+)|(\\s+$)', replacement='', x=as.character(codebook[i,kolumna_etykiety]), fixed=FALSE, perl=TRUE)
			tmp = strsplit(x=tmp, split='\n', fixed=TRUE)[[1]];
			tmp = strsplit(x=tmp, split=':', fixed=TRUE);
			poziomy = sapply(tmp, function(x){
				return(x[1])
			})
			etykiety = sapply(tmp, function(x){
				return(paste(x[-1], collapse=':'))
			})
			if(!is.na(nd) & ! nd %in% poziomy){
				poziomy = c(poziomy, nd)
				etykiety = c(etykiety, 'nie dotyczy')
			}
			if(!is.na(tp) & ! tp %in% poziomy){
				poziomy = c(poziomy, tp)
				etykiety = c(etykiety, 'trudno powiedzieć')
			}
			if(!is.na(oo) & ! oo %in% poziomy){
				poziomy = c(poziomy, oo)
				etykiety = c(etykiety, 'odmowa odpowiedzi')
			}
			porzadkowa = TRUE;
			if(codebook[i, kolumna_skala] == 'nominalna' || codebook[i, kolumna_skala] == 'dychotomiczna'){
				porzadkowa=FALSE;
			}
			dane[, i] = factor(dane[, i], levels=poziomy, labels=etykiety, ordered=porzadkowa);
		}else if(codebook[i, kolumna_skala] == 'nominalna' || codebook[i, kolumna_skala] == 'dychotomiczna'){
			dane[, i] = factor(dane[, i], levels=unique(dane[, i]), ordered=FALSE);
		}else if(codebook[i, kolumna_skala] == 'porządkowa'){
			dane[, i] = factor(dane[, i], levels=unique(dane[, i]), ordered=TRUE);
		}
	}
	return(dane);
}
