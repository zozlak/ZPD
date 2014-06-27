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

#' @title Oblicza PWE na podstawie PV agregujac wg wskazanych zmiennych
#' @description
#' _
#' @param dane macierz z wartosciami PV do zagregowania
#' @param kolGrupy nazwa kolumny (lub wektor nazw kolumn), wg których PWE mają zostać zagregowane
#' @param bladZrwn błąd zrównywania
#' @param wariancjaPop wariancja wyników w populacji (uzyskana np. z funkcji pobierz_wariancje_populacji())
#' @param kolPV wyrażenie regularne dopasowujące kolumny z wartościami PV
#' @param kolNrPV nazwa kolumny z numerem PV (gdy dane w postaci długiej)
#' @return [data.frame] zagregowane wartości PWE
#' @export
agreguj_pwe = function(dane, kolGrupy, bladZrwn, wariancjaPop, kolPV='^[pP][vV]_[0-9]+|wynik$', kolNrPV='nr_pv'){
	require(plyr)
	
	#<-- Sprawdzenie argumentów i doprowadzenie danych do postaci długiej
	if(any(!is.numeric(bladZrwn), !is.vector(bladZrwn), length(bladZrwn) != 1, is.na(bladZrwn))){
		stop('argument bladZrwn musi być jednoelementowym wektorem numerycznym i posiadać wartość inną niż NA')
	}

	if(any(!is.numeric(wariancjaPop), !is.vector(wariancjaPop), length(wariancjaPop) != 1, is.na(wariancjaPop))){
		stop('argument wariancjaPop musi być jednoelementowym wektorem numerycznym i posiadać wartość inną niż NA')
	}
	
	dane = ramka_danych_pv(dane, kolPV, kolNrPV, kolGrupy)
	#-->
	
	#<-- liczenie
	ilePV = length(unique(dane$nr_pv))
	wynik = ddply(dane, kolGrupy, function(d, blZrwn2, varPop, ilePV){
		srednia = mean(d$wynik)
		d = ddply(d, c('nr_pv'), function(x){
			x = c(
				quantile(x$wynik, probs=c(0, 0.25, 0.5, 0.75, 1)),
				mean(x$wynik),
				nrow(x)
			)
			names(x) = c('min', 'q1', 'mediana', 'q3', 'maks', 'srednia', 'n')
			return(x)
		})
		bs = sqrt((1 + 1 / ilePV) * var(d$srednia) + varPop / mean(d$n) + blZrwn2)
		d$srednia = srednia
		d$bs = bs
		d = apply(d, 2, mean)
		return(d)
	}, bladZrwn^2, wariancjaPop, ilePV)
	
	return(wynik)
	#-->
}