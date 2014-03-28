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

#' @title Dzieli plik danych z badan zrownujacych na testy
#' @description
#' Funkcja:
#' - filtruje rekordy pasujace do zadanego zeszytu
#' - zmienia nazwy kolumn z wynikami zadan na id_kryteriow
#' - dodaje kolumne "zrwn" zawierajace kolumne "id_ucz" przeksztalcona do postaci uzywanego w bazie identyfikatora "zrwn"
#' - dubluje kolumne "id_szk" pod nazwa "id_szk_oke"
#' - zapisuje tak odfiltrowany i skonwertowany zbior pod nazwa "id_testu.csv"
#' @details
#' _
#' @param dane ramka danych
#' @param zeszyt string z nazwa zeszytu (taka, jak w kolumnie wskazywanej przez "kolIdZeszytu")
#' @param zeszytBaza string z nazwa zeszytu w bazie danych ("zrownywanie;RODZAJ_EGZ;ROK;KOD")
#' @param kolIdZeszytu numer kolumny w "dane" przechowujacej nazwy zeszytow
#' @param kolUdzial numer kolumny w "dane" przechowujacej informacje o udziale ucznia w badaniu
#' @param kolIdSzkOKE numer kolumny w "dane" przechowujace id OKE szkoly
#' @param prefiksZadan prefiks kolumn zawierajacych wyniki zadan dla danego zeszytu
#' @param rok rok testu zrownujacego (niezbedny do skonstruowania identyfikatora 'zrwn')
#' @param zrodloDanychODBC nazwa zrodla danych ODBC
#' @return void
#' @examples
#' a=1
#' @export
wydziel_testy = function(dane, zeszyt, zeszytBaza, kolIdZeszytu, kolUdzial, kolIdSzkOKE, prefiksZadan, rok, zrodloDanychODBC='EWD'){
	P = odbcConnect(zrodloDanychODBC)
	
	cat(sprintf('%s\n', zeszyt))
	
	dane[, kolIdZeszytu] = gsub(' ', '', dane[, kolIdZeszytu])
	filtrW = dane[, kolIdZeszytu]==zeszyt & !is.na(dane[, kolIdZeszytu]) & dane[, kolUdzial]=='tak' & !is.na(dane[, kolUdzial])
	dane=dane[filtrW, ]
	
	idTestu = sqlQuery(P, sprintf("SELECT id_testu FROM testy WHERE opis ILIKE '%s'", zeszytBaza))[1,1]
	kryteria = as.character(sqlQuery(P, sprintf("SELECT 'k_'||id_kryterium::text 
																							 FROM testy_kryteria JOIN kryteria_oceny USING (id_kryterium)
																							 WHERE id_testu=%d 
																							 ORDER BY kolejnosc, opis, id_kryterium", idTestu))[, 1])
	
	for(i in names(dane)){
		tmp = grep(sprintf('^%s_pkt$', i), names(dane)) # czy jest taka sama kolumna, tylko z sufiksem "_pkt"?
		if(length(tmp) > 0){
			dane = dane[, -tmp] # jesli tak, usun
		}
	}
	kolZadania = grep(sprintf('^%s', prefiksZadan), names(dane))
	if(length(kolZadania) != length(kryteria)){
		stop('liczba kryteriow w tescie nie zgadza sie z liczba kolumn w zbiorze')
	}
	
	names(dane)[kolZadania] = kryteria # przezwij kolumny na id_kryteriow
	
	tmp = as.character(dane$id_ucz)
	tmp = sub('^ +', '', tmp)
	tmp = sub(' +$', '', tmp)
	tmp = sub(' ', '|', tmp)
	tmp = sub('0*([1-9][0-9]*)$', '|\\1', tmp)
	tmp = paste(rok, tmp, sep='|')
	dane$zrwn = tmp
	
	names(dane)[kolIdSzkOKE] = 'id_szk_oke'
	
	write.csv2(dane, sprintf('%d.csv', idTestu), row.names=F, na='', fileEncoding='UTF-8')
	
	odbcClose(P)
}
